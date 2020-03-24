library(shiny)
library(shinythemes)
library(plotly)
library(leaflet)
library(shinycssloaders)

# Spinner Options
options(spinner.color="#FF7467", spinner.color.background="#ffffff", spinner.size=1.5)

# COVID data
raw_data <- read.csv('https://raw.githubusercontent.com/carranco-sga/Mexico-COVID-19/master/Mexico_COVID19.csv', stringsAsFactors =  FALSE)
data_list <- readRDS('data/data.rds')

# PREPROCESSING DATA:
# Preprocessing main values: Positive, Recovered, Local, Imported and Deaths
df_total_reps <- raw_data[, c('Fecha', 'Pos_rep', 'Susp_rep', 
                              'Neg_rep', 'Recovered', 'Deceased', 'Tested_tot')]

# STATES
ord_state_names <- data_list$ord_state_names
# Positive Cases (TOTAL)
df_pos_states <- raw_data[, -grep("\\_.*$", colnames(raw_data))]
df_pos_states[, c('Fecha', 'Pos', 'Recovered', 'Deceased')] <- NULL
colnames(df_pos_states) <- ord_state_names
df_pos_states <- df_pos_states[ , order(names(df_pos_states))]


# First date
first_date <- head(raw_data$Fecha, n = 1)[1]
# Last date
last_date <- tail(raw_data$Fecha, n = 1)[1]
last_date_formated <- format(as.Date(last_date), format = "%d de %b del %Y")

# Format date to int func
date_to_int <- function(text_date){
    date_int <- as.integer(format(as.Date(text_date),
                      format = "%Y%m%d"))
    return(date_int)
}

# Create the palette with the max values until now
pal <- colorBin("YlOrRd", sqrt(c(1,50)), 
                bins = 6, na.color = '#FFFFFF')


# Load map data
mexico <- data_list$mexico

# TIME PLOT CONF PLOTLY
#### LINE PLOT
conf_ <- c(#"zoomIn2d", "zoomOut2d", #"select2d", 
    "toImage", "autoScale2d",
    "toggleSpikelines", "hoverCompareCartesian", 
    "hoverClosestCartesian")
font_plotly <- list(
    size = 14
)
ax_lp <- list(marging = list(pad = 0), linecolor = 'rgb(0,0,0)', 
              linewidth = 3, showline = T,
              title = '<b>Fecha</b>')
yax_lp <- ax_lp
yax_lp[['title']] <- '<b>Número de Casos</b>'


# Define UI for application that draws a histogram
ui <- fluidPage(
    theme=shinytheme('journal'),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ), 
    
    sidebarLayout(
        sidebarPanel(
            HTML('<h1><span class="red_color">COVID-19:</span><span class="yellow_color"> MxTracker</span></h1>'),
            p(span('Última actualización:', style = 'font-weight: bold;'), span(last_date_formated)),
            HTML("<p style = 'text-align: left;'><b>Fuente:</b> <a href='https://www.gob.mx/salud/documentos/informacion-internacional-y-nacional-sobre-nuevo-coronavirus-2019-ncov' target='_blank'>SSalud, Mx.</a>, a través de <a href='https://github.com/carranco-sga/Mexico-COVID-19' target='_blank'>carranco-sga</a>.<p/>"),
            
            hr(),
            
            fluidRow(
                column(12,
                    h3('Fecha'),
                    column(12,
                           dateInput(
                               inputId = 'pickDate',
                               label = 'Selecciona la fecha:',
                               value = last_date,
                               format = "dd/mm/yy",
                               language = 'es'
                           ),
                ), 
                class = 'col-md-7'),
                column(12,
                       htmlOutput('date_warning'),
                class = 'col-md-5')
            ),
            
            fluidRow(
                column(12,
                       h3('Línea de Tiempo'),
                       column(12,
                          radioButtons("scale_log", 
                                       label = p("Selecciona la escala de visualización:"), 
                                       choices = list("Datos crudos" = 'raw', 
                                                      "Escala Logarítmica" = 'log'),
                                       selected = 'raw'),
                          class='col-md-6'
                        ),
                       column(12,
                          checkboxGroupInput("case_categories", 
                             label = p("Categorías complementárias:"), 
                             choices = list("Casos Sospechosos" = 'Susp_rep', 
                                            "Casos Negativos" = 'Neg_rep',
                                            "Número de Pruebas Realizadas" = 'Tested_tot'),
                             selected = NULL),
                          class='col-md-6'
                        ),                 
                )
            ),

            hr(),
            
            fluidRow(
 HTML(
'
<div class="col-sm-12">
<h3 class="alert-heading" style="color: #FFC27E;">Usabilidad</h3>

  <ul class="fade_white_color">
    <li>Selecciona la fecha de interés.</li>
    <li>Da click en el mapa para más información de cada estado.</li>
    <li>Pasa el cursor sobre algún punto de interés en la <b>línea de tiempo</b> para más información al respecto. Vuelve a la <b>visualización inicial</b> con doble click sobre la gráfica.</li>
  </ul>
</div>
'),
# DISCLAIMER
            HTML(
'
<input type="checkbox"  id="spoiler" /> 
<label for="spoiler" ><h3  class="alert-heading red_color"><span class="fas fa-exclamation-triangle"></span> Disclaimer</h3></label>
<div class="alert  spoiler" role="alert">
<p style="color: #E6C496; background-color: #1a1a1a; padding: 10px; border-radius: 5px;">Los datos de esta aplicación son obtenidos a partir de los Comunicados Técnicos diários de la <a href="https://www.gob.mx/salud/documentos/informacion-internacional-y-nacional-sobre-nuevo-coronavirus-2019-ncov" target="_blank">Secretaría de Salud, Mx</a>. No obstante, siempre cabe la posibilidad de problemas de actualización o errores en el código de esta aplicación, por lo cual <b>recuerda siempre verificar con a la información oficial. Evitemos propagar información falsa.</b></p><hr>
</div>
')
            ), # Ends Raw HTML
            HTML("
<p style = 'text-align: right;'>Author: <a href='https://github.com/jRicciL' target='_blank'>J. Ricci-López (2020) &copy;</a>, PhD student at <a href='https://www.cicese.edu.mx/' target='_blank'>CICESE</a>.<p/>
                 "),
            
            width = 3,
            style = 'padding: 3rem'),
        
### MAIN PANEL
        
        mainPanel(
            # MAP ROW *******************
            fluidRow(
                # Numeric Info
                column(12, 
                       div(h3('Datos Nacionales',
                              class = 'text-center'),
                           style = 'margin-bottom: 0px; z-index: 100'),
                       hr(),
                       fluidRow(
                           column(6,
                               h4('Fecha:'), class = "info_column_names"),
                           column(6,
                                  fluidRow(
                                      column(12,
                                        h4(textOutput('text_date')),
                                        class = "info_row_right"),
                                  class='info_row')
                                ),
                           
                           class = 'info_main_row'
                       ),
                       
                       fluidRow(
                           column(6,
                                  h4('Número de CONFIRMADOS:'), 
                                  class = "info_column_names"),
                           column(6,
                                  fluidRow(
                                      column(8,
                                             h3(textOutput('text_pos')),
                                             class = "info_row_right"),
                                      column(4,
                                             icon('fas fa-user-plus', 
                                                  class = 'icon', lib = "font-awesome"),
                                             class = "info_row_right"),
                                      class='info_row positive_bg_color')
                           ),
                           class = 'info_main_row'
                       ),
                       
                       fluidRow(
                           column(6,
                                  h4('Número de SOSPECHOSOS:'), 
                                  class = "info_column_names"),
                           column(6,
                                  fluidRow(
                                      column(8,
                                             h3(textOutput('text_susp')),
                                             class = "info_row_right"),
                                      column(4,
                                             icon('fas fa-user-clock', 
                                                  class = 'icon', lib = "font-awesome"),
                                             class = "info_row_right"),
                                      class='info_row suspect_bg_color')
                           ),
                           class = 'info_main_row'
                       ),
                       
                       fluidRow(
                           column(6,
                                  h4('Número de NEGATIVOS:'), 
                                  class = "info_column_names"),
                           column(6,
                                  fluidRow(
                                      column(8,
                                             h3(textOutput('text_neg')),
                                             class = "info_row_right"),
                                      column(4,
                                             icon('fas fa-user-minus', 
                                                  class = 'icon', lib = "font-awesome"),
                                             class = "info_row_right"),
                                      class='info_row negative_bg_color')
                           ),
                           class = 'info_main_row'
                       ),
                       
                       fluidRow(
                           column(6,
                                  h4('Número de RECUPERADOS:'), 
                                  class = "info_column_names"),
                           column(6,
                                  fluidRow(
                                      column(8,
                                             h3(textOutput('text_recov')),
                                             class = "info_row_right"),
                                      column(4,
                                             icon('fas fa-user-shield', 
                                                  class = 'icon', lib = "font-awesome"),
                                             class = "info_row_right"),
                                      class='info_row recovered_bg_color')
                           ),
                           class = 'info_main_row'
                       ),
                       
                       fluidRow(
                           column(6,
                                  h4('Número de DECESOS:'), 
                                  class = "info_column_names"),
                           column(6,
                                  fluidRow(
                                      column(8,
                                             h3(textOutput('text_deaths')),
                                             class = "info_row_right"),
                                      column(4,
                                             icon('fas fa-cross', 
                                                  class = 'icon', lib = "font-awesome"),
                                             class = "info_row_right"),
                                      class='info_row death_bg_color')
                           ),
                           class = 'info_main_row'
                       ),
                       
                       fluidRow(
                           column(6,
                                  h4('Número de pruebas realizadas:'), 
                                  class = "info_column_names"),
                           column(6,
                                  fluidRow(
                                      column(8,
                                             h3(textOutput('text_tested')),
                                             class = "info_row_right"),
                                      column(4,
                                             icon('fas fa-vial', 
                                                  class = 'icon', lib = "font-awesome"),
                                             class = "info_row_right"),
                                      class='info_row tested_bg_color')
                           ),
                           class = 'info_main_row'
                       ),
                       
                       
                       class = "col-lg-4"),
                # Mexcio Map
                column(12, 
                    div(h3(paste0('Número de casos confirmados por Estado'),
                                  #'(', textOutput('text_date'), ')'),
                           class = 'text-center'),
                           style = 'margin-bottom: 0px; z-index: 100'),
                    leafletOutput(
                        outputId = 'mapMx',
                        height = "85%"
                        ),
                    class = "col-lg-8",
                    style = "height: 550px"
                    ),
            ),
            
            # Time Series
            fluidRow(
                column(12,
                       div(h3('Linea de Tiempo: ',
                              span('Número de casos por día',
                                   style = 'font-weight: normal;'),
                              class = 'text-center'),
                           style = 'margin-bottom: -35px; z-index: 1000'),
                       withSpinner(plotlyOutput(
                           outputId = 'timePlot'))
                )
            ),
            width = 9)
    ),

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    #****** OBSERVERS ****
    # DATES
    observe({
        selected_date <- validate_date()
        formated_date <- format(as.Date(selected_date),
                                format = "%d/%B/%Y")
        output$text_date <- renderText({formated_date})
    })
    # NUM TOTAL CASES
    total_cases_date <- reactive({
        selected_date <- validate_date()
        df_total_reps[df_total_reps$Fecha == selected_date, ]
    })
    observe({ # Positives
        all_reps <- total_cases_date()
        n_pos <- all_reps['Pos_rep'][[1]]
        n_susp <- all_reps['Susp_rep'][[1]]
        n_neg <- all_reps['Neg_rep'][[1]]
        n_recov <- all_reps['Recovered'][[1]]
        n_dead <- all_reps['Deceased'][[1]]
        n_tested <- all_reps['Tested_tot'][[1]]
        
        output$text_pos <- renderText({n_pos})
        output$text_susp <- renderText({n_susp})
        output$text_neg <- renderText({n_neg})
        output$text_recov <- renderText({n_recov})
        output$text_deaths <- renderText({n_dead})
        output$text_tested <- renderText({n_tested})
    })
    
    
    #****** REACTIVES ****
    df_line_plot <- reactive({
        df_ <- df_total_reps
        # Filter values:
        categories <- c(c('Fecha', 'Pos_rep'), input$case_categories)
        df_ <- df_total_reps[, categories]
        return(df_)
    })
    
    validate_date <- reactive({
        # If not valid, return the actual date
        selected_date <- input$pickDate
        sel_date_int <- date_to_int(selected_date)
        first_date_int <- date_to_int(first_date)
        last_date_int <- date_to_int(last_date)
        if (sel_date_int < first_date_int | sel_date_int > last_date_int){
            # Raise a warning
            output$date_warning <- renderText({
                paste0('<h5 class="alert alert-danger">',
                       'Por favor selecciona una fecha entre el ',
                       format(as.Date(first_date), format = "%d de %B del %Y"),
                       ' y el ',
                       format(as.Date(last_date), format = "%d de %B del %Y"),
                       '</h5>')})
            return(last_date)
        } else {
            output$date_warning <- renderText({''})
            return(selected_date)
        }
    })
    
    cases_requested <- reactive({
        selected_date <- validate_date()
        cases_date <- df_pos_states[raw_data$Fecha == selected_date, ]
        cases_date <- unlist(cases_date)
        return(cases_date)           
        })
    
    #****** OUTPUTS ******
    output$mapMx <- renderLeaflet({
        # Update the requested cases
        cases_per_state <- cases_requested()
        mexico$cases_per_state <- cases_per_state
        mexico$relative_n_cases <- abs(sqrt(cases_per_state))
        mexico$relative_n_cases[cases_per_state == 0] <- NA
        
        state_popup <- paste0("<strong>Estado: </strong>", 
                              mexico$name, 
                              "<br><strong>Número de casos: </strong>", 
                              mexico$cases_per_state)
        leaflet(data = mexico) %>%
            addProviderTiles("CartoDB.Positron") %>%
            addPolygons(fillColor = ~pal(relative_n_cases), 
                        fillOpacity = 0.8, 
                        color = "#444", 
                        weight = 1, 
                        popup = state_popup)  %>%
            addLegend("bottomleft", 
                      pal = pal, 
                      values = cases_per_state,
                      title = "Número de Casos<br>",
                      opacity = 1,
                      labFormat = labelFormat(
                          prefix = "", suffix = "", between = " - ",
                          transform = function(x) as.integer(x*x) )
            )
    })
    
    
    observeEvent(input$scale_log, {
        yax_lp[['type']] <- input$scale_log
        plotlyProxy("timePlot", session) %>%
            plotlyProxyInvoke("relayout", 
                              list(yaxis= yax_lp))
    })
    
    #**** Linear Plot ******
    output$timePlot <- renderPlotly({
        fig <- plot_ly(type = 'scatter', mode = 'markers+lines')
        df_ <- df_line_plot()
        max_y_value = apply(df_, 2, max, na.rm = TRUE)

        # Add the Positive Cases
        fig <- add_trace(fig, x = df_$Fecha,
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
        add_segments(x = '2020-02-28', xend = '2020-02-28', 
              y = 0, yend = max_y_value, 
              showlegend = FALSE, opacity = 0.5,
              line = list(color = 'black', dash = 'dash', 
                          linewidth = 3)) %>%
        add_text(x = '2020-02-28',  
                 y = 0,
                 textposition = "up right",
                 text = ' Primer caso<br> positivo<br> reportado<br><br><br><br><br>',
                 showlegend = FALSE)
        
        # Filter per value
        for (column in input$case_categories) {
            if (column == 'Fecha' | column == 'Pos_rep') {
                next
            }
            # Get the color
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
            fig <- fig %>% add_trace(x = df_$Fecha,
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
        yax_lp[['type']] <- input$scale_log
        fig <- fig %>%
            layout(xaxis = ax_lp, yaxis = yax_lp, 
                   paper_bgcolor = 'rgba(0,0,0,0)', 
                   font = font_plotly,
                   legend = list(title = list(text = '<b>Categorías:</b>'),
                                 x = 0.05, y =0.95)) %>%
            config(modeBarButtonsToRemove = conf_, displaylogo = FALSE,
                   displayModeBar = F)

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
