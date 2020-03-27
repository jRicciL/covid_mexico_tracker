shinyServer(function(input, output, session) {
  
  # =========================================================================
  # Reactive Resources
  # =========================================================================
  
  # Validates if a requested date is into the dataset
  validate_date <- reactive({
    selected_date <- input$pickDate
    sel_date_int <- date_to_int(selected_date)
    first_date_int <- date_to_int(first_date)
    last_date_int <- date_to_int(last_date)
    if (sel_date_int < first_date_int | sel_date_int > last_date_int){
      # If not valid, raise a warning and return the actual date
      warning_date_mss <- paste0('<h5 class="alert alert-danger">',
                         'Por favor selecciona una fecha entre el ',
                         format(as.Date(first_date), format = "%d de %B del %Y"),
                         ' y el ',
                         format(as.Date(last_date), format = "%d de %B del %Y"),
                         '</h5>')
      output$date_warning <- renderText({warning_date_mss})
      return(last_date)
    } else {
      # If valid, return the requested date
      output$date_warning <- renderText({''})
      return(selected_date)
    }
  })
  
  # Dataframe to get cases at a given date
  total_cases_date <- reactive({
    selected_date <- validate_date()
    df_total_reps[df_total_reps$Fecha == selected_date, ]
  })
  
  # Dataframe to get a time series of commulated cases given $case_categories
  df_line_plot <- reactive({
    df_ <- df_total_reps
    # Get the case categories
    categories <- c(c('Fecha', 'Pos_rep'), input$case_categories)
    df_ <- df_total_reps[, categories]
    return(df_)
  })
  
  # Get the acumulated cases at a requested date
  cases_requested <- reactive({
    selected_date <- validate_date()
    cases_date <- df_pos_states[df_total_reps$Fecha == selected_date, ]
    cases_date <- unlist(cases_date)
    return(cases_date)           
  })
  
  
  # =========================================================================
  # OUTPUTS: Observe Resources
  # =========================================================================
  # Format date and render it
  observe({
    selected_date <- validate_date()
    formated_date <- format(as.Date(selected_date),
                            format = "%d/%B/%Y")
    output$text_date <- renderText({formated_date})
  })
  
  # Render text of number of cases per category for the National Numbers Panel
  observe({
    # Get the cases of a requested date
    all_reps <- total_cases_date()
    n_pos <- all_reps['Pos_rep'][[1]]
    n_susp <- all_reps['Susp_rep'][[1]]
    n_neg <- all_reps['Neg_rep'][[1]]
    n_recov <- all_reps['Recovered'][[1]]
    n_dead <- all_reps['Deceased'][[1]]
    n_tested <- all_reps['Tested_tot'][[1]]
    # Render the values
    output$text_pos <- renderText({n_pos})
    output$text_susp <- renderText({n_susp})
    output$text_neg <- renderText({n_neg})
    output$text_recov <- renderText({n_recov})
    output$text_deaths <- renderText({n_dead})
    output$text_tested <- renderText({n_tested})
  })
  
  # PlotlyProxy observer to change the scale of y axis in the time plot
  observeEvent(input$scale_log, {
    yax_lp[['type']] <- input$scale_log
    plotlyProxy("timePlot", session) %>%
      plotlyProxyInvoke("relayout", 
                        list(yaxis= yax_lp))
  })
  
  
  # =========================================================================
  # OUTPUTS: 
  # =========================================================================
  # *********** MEXICO MAP ***********
  output$mapMx <- renderLeaflet({
    # Update the requested cases
    cases_per_state <- cases_requested()
    # Parse the values the map object
    # Actual case numbers
    mexico$cases_per_state <- cases_per_state 
    # Sqrt of case numbers for a better color palette
    mexico$relative_n_cases <- (sqrt(cases_per_state)) 
    # Set NA to states with 0 cases in order to paint them as white
    mexico$relative_n_cases[cases_per_state == 0] <- NA 
    # Get color palette
    color_palette <- get_pal()
    
    # Text for pop object at click selection 
    state_popup <- paste0("<strong>Estado: </strong>", 
                          mexico$name, 
                          "<br><strong>Número de casos: </strong>", 
                          mexico$cases_per_state)
    
    # Create the Leaflet Map
    leaflet(data = mexico) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(fillColor = ~map_pal(relative_n_cases), 
                  fillOpacity = 0.8, 
                  color = "#444", 
                  weight = 1, 
                  popup = state_popup)  %>%
      addLegend("bottomleft", 
                pal = map_pal, 
                values = cases_per_state,
                title = "Número de Casos<br>",
                opacity = 1,
                labFormat = labelFormat(
                    prefix = "", suffix = "", between = " - ",
                    transform = function(x) as.integer(x*x) )
      )
  })
  
  
  # *********** TIME LINE PLOT ***********
  output$timePlot <- renderPlotly({
    # Get the dataframe with the requested columns
    df_ <- df_line_plot()
    # Max y value of the whole df
    max_y_value = apply(df_, 2, max, na.rm = TRUE)
    DATE_FIRST_POS_CASE = '2020-02-28'
    
    # Creates the plot and add the Positive Cases
    fig <- plot_ly(type = 'scatter', mode = 'markers+lines') %>%
           add_trace(x = df_$Fecha,
                     y = df_[['Pos_rep']],
                     marker = list(size = 14,
                                   color = 'rgb(231, 87, 74)'),
                     line = list(color = 'rgb(231, 87, 74)',
                                 width = 5),
                     text = paste0(
                       '<b>Positivos</b>',
                       '<br><b>Casos:</b> ', df_[['Pos_rep']],
                       '<br><b>Fecha:</b> ', df_$Fecha),
                     name = 'Casos Positivos',
                     hovertemplate = paste('%{text}')) %>%
          # add a vertical segment indicating the first confirmed case in the country
          add_segments(x = DATE_FIRST_POS_CASE, 
                       xend = DATE_FIRST_POS_CASE, 
                       y = 0, 
                       yend = max_y_value, 
                       showlegend = FALSE, opacity = 0.5,
                       line = list(color = 'black', dash = 'dash', 
                                   linewidth = 3)) %>%
          add_text(x = DATE_FIRST_POS_CASE,  
                   y = 0,
                   textposition = "up right",
                   text = ' Primer caso<br> positivo<br> reportado<br><br><br><br><br>',
                   showlegend = FALSE)
    
    # Plot the requested extra categories (if any)
    for (column in input$case_categories) {
      # Skip Fecha and Pos_rep
      if (column == 'Fecha' | column == 'Pos_rep') {
        next
      }
      # Get the category color and name
      color_ <- switch (column,
                'Susp_rep' = 'rgb(80,147,148)',
                'Neg_rep' = 'rgb(81,157,72)',
                'Tested_tot' = 'rgb(195,148,202)'
      )
      
      name_ <- switch (column,
                'Susp_rep' = 'Sospechosos',
                'Neg_rep' = 'Negativos',
                'Tested_tot' = 'Núm. Pruebas Realizadas'
      )
      
      # Add the category trace
      fig <- fig %>% 
           add_trace(x = df_$Fecha,
                     y = df_[[column]],
                     text = paste0(
                       '<b>', name_,'</b>',
                       '<br><b>Casos:</b> ', df_[[column]],
                       '<br><b>Fecha:</b> ', df_$Fecha),
                     name = name_,
                     marker = list(size = 10,
                                   symbol = 'diamond',
                                   color = color_),
                     line = list(color = color_,
                                 dash = 'dash',
                                 width = 2),
                     hovertemplate = paste('%{text}')) 
    }
    
    # Update the y axis param at requested scale 
    yax_lp[['type']] <- input$scale_log
    # Set the layout and general plotly configuration
    fig <- fig %>%
      layout(xaxis = ax_lp, yaxis = yax_lp, 
             paper_bgcolor = 'rgba(0,0,0,0)', 
             font = font_plotly,
             legend = list(title = list(text = '<b>Categorías:</b>'),
                           x = 0.05, y =0.95)) %>%
      config(modeBarButtonsToRemove = modebar_plotly_conf,
             displaylogo = FALSE,
             displayModeBar = FALSE)
    
  })
})