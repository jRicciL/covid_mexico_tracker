shinyUI(
  fluidPage(
    
    # =========================================================================
    # Headtags
    # =========================================================================
    theme=shinytheme('journal'),  
    tags$head(
      HTML("<title>México: Covid-19 Tracker</title>"),
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    fluidRow(
      # =========================================================================
      # Main Panel
      # =========================================================================
      column(12,
             # ***** Mexico Map and National Numbers *****   
             fluidRow(
               # =========================================================================
               # Sidebar Panel
               # =========================================================================
               column(12,
                      class = 'col-xs-12 col-sm-12 col-md-6 col-lg-3 well',
                      style = 'padding: 3rem;',
                      div(style='font-size: 0.9em;',
                        h1(span('COVID-19:', class = 'red_color'), span('MxTracker', class='yellow_color')),
                        p(span('Última actualización:', style= 'font-weight: bold;'), span(last_date_formated)),
                        p(span('Fuente:', style='font-weight: bold;'),
                          a('SSalud, Mx.', href= 'https://www.gob.mx/salud/documentos/informacion-internacional-y-nacional-sobre-nuevo-coronavirus-2019-ncov', target='blank_'),
                          span('a través de'),
                          a('@carranco-sga', href= 'https://github.com/carranco-sga/Mexico-COVID-19', target='blank_'),
                        ),
                        
                        class='header'
                      ),
                      hr(class='hr_main_light'),
                      
                      # ***** INPUT: Date *****    
                      fluidRow(
                        column(6,
                               h3('Fecha'),
                               column(12,
                                      dateInput(
                                        inputId = 'pickDate',
                                        label = 'Selecciona la fecha:',
                                        value = last_date,
                                        format = "dd/mm/yy",
                                        language = 'es'
                                      ),
                                      class = 'col-sm-8 col-md-8 col-lg-10'),
                               column(12,
                                      htmlOutput('date_warning'),
                                      class = 'col-sm-8  col-md-8 col-lg-10'),
                               class = 'col-xs-12 col-sm-6 col-md-12'
                        ),
                        # ***** INPUT: MAP *****          
                        column(6,
                               h3('Mapa'),
                               column(6,
                                      selectInput(
                                        inputId = 'mapData',
                                        label = 'Mostrar Casos:',
                                        choices = list("Positivos" = 'positivos', 
                                                       "Decesos" = 'decesos',
                                                       "Sospechosos" = 'sospechosos'), 
                                        selected = 'positivos'
                                      ),
                                      class = 'col-sm-8 col-md-8 col-lg-10'),
                               
                               class = 'col-xs-12 col-sm-6 col-md-12'
                        ),
                        
                        
                      ),
                      hr(class='hr_main_light'),
                      # Quedate en casa
                      h2(a('#QuedateEnCasa', href='https://twitter.com/hashtag/quedatencasa', target='_blank',
                           style='color: #58EBD7'), class='header'),
                      br(),
                      # # ***** Disclaimer *****       
                       
                      # ***** Footer *****
                      HTML("<div class='header'>
                      <p style = 'text-align: right; font-size: 0.9em;' class='header'><a href='https://github.com/jRicciL'  target='_blank'>J. Ricci-López (2020) &copy;</a>, PhD student at <a href='https://www.cicese.edu.mx/' target='_blank'>CICESE</a>.<p/>
                      </div>
  
              "),
              ),
               
               # =================
               # National Numbers
               # =================
               column(12,
                      class = "col-xs-12 col-sm-12 col-md-6 col-lg-3 numbers_panel",
                      div(h3('Resumen nacional',
                             class = 'text-center'),
                          style = 'margin-bottom: 0px; z-index: 100'),
                      hr(),
                      hr(class='hr_main'),
                      # ***** Date *****
                      fluidRow(
                        column(6,
                               h4('Fecha:'), class = "info_column_names"),
                        column(6,
                               fluidRow(
                                 column(12,
                                        h4(textOutput('text_date')),
                                        class = "info_row_right date_column"),
                                 class='info_row')
                        ),
                        class = 'info_main_row'
                      ),
                      # ***** Positive Cases *****
                      fluidRow(
                        column(6,
                               h4('Número de POSITIVOS:'), 
                               class = "info_column_names"),
                        column(6,
                               fluidRow(
                                 column(6,
                                        h3(textOutput('text_pos')),
                                        class = "info_row_right col-xs-6"),
                                 column(6,
                                        icon('fas fa-user-plus', 
                                             class = 'icon', lib = "font-awesome"),
                                        class = "icon_column col-xs-6"),
                                 class='info_row positive_bg_color')
                        ),
                        class = 'info_main_row'
                      ),
                      # ***** Suspected Cases *****
                      fluidRow(
                        column(6,
                               h4('Número de SOSPECHOSOS:'), 
                               class = "info_column_names"),
                        column(6,
                               fluidRow(
                                 column(6,
                                        h3(textOutput('text_susp')),
                                        class = "info_row_right col-xs-6"),
                                 column(6,
                                        icon('fas fa-user-clock', 
                                             class = 'icon', lib = "font-awesome"),
                                        class = "icon_column col-xs-6"),
                                 class='info_row suspect_bg_color')
                        ),
                        class = 'info_main_row'
                      ),
                      # ***** Negative Cases *****
                      fluidRow(
                        column(6,
                               h4('Número de NEGATIVOS:'), 
                               class = "info_column_names"),
                        column(6,
                               fluidRow(
                                 column(6,
                                        h3(textOutput('text_neg')),
                                        class = "info_row_right col-xs-6"),
                                 column(6,
                                        icon('fas fa-user-minus', 
                                             class = 'icon', lib = "font-awesome"),
                                        class = "icon_column col-xs-6"),
                                 class='info_row negative_bg_color')
                        ),
                        class = 'info_main_row'
                      ),
                      # ***** Recovered Cases *****
                      conditionalPanel(
                        condition = "input.pickDate < '2020-03-22'",
                        fluidRow(
                          column(6,
                                 h4('Número de RECUPERADOS:'), 
                                 class = "info_column_names col-sm-12"),
                          column(6,
                                 fluidRow(
                                   column(6,
                                          h3(textOutput('text_recov')),
                                          class = "info_row_right col-xs-6"),
                                   column(6,
                                          icon('fas fa-user-shield', 
                                               class = 'icon', lib = "font-awesome"),
                                          class = "icon_column col-xs-6"),
                                   class='info_row recovered_bg_color')
                          ),
                          class = 'info_main_row'
                        ),
                      ),
                      
                      # ***** Deceased Cases *****
                      fluidRow(
                        column(6,
                               h4('Número de DECESOS:'), 
                               class = "info_column_names"),
                        column(6,
                               fluidRow(
                                 column(6,
                                        h3(textOutput('text_deaths')),
                                        class = "info_row_right col-xs-6"),
                                 column(6,
                                        icon('fas fa-cross', 
                                             class = 'icon', lib = "font-awesome"),
                                        class = "icon_column col-xs-6"),
                                 class='info_row death_bg_color')
                        ),
                        class = 'info_main_row'
                      ),
                      # ***** Testing *****
                      fluidRow(
                        column(6,
                               h4('Número de pruebas realizadas:'), 
                               class = "info_column_names"),
                        column(6,
                               fluidRow(
                                 column(6,
                                        h3(textOutput('text_tested')),
                                        class = "info_row_right col-xs-6"),
                                 column(6,
                                        icon('fas fa-vial', 
                                             class = 'icon', lib = "font-awesome"),
                                        class = "icon_column col-xs-6"),
                                 class='info_row tested_bg_color')
                        ),
                        class = 'info_main_row'
                      ),
                      
                      # *** DISCLAIMER ***
                      div(
                        h3('Los datos de esta aplicación son obtenidos a partir de los Comunicados Técnicos diários de la', 
                           a('Secretaría de Salud, Mx', href="https://www.gob.mx/salud/documentos/informacion-internacional-y-nacional-sobre-nuevo-coronavirus-2019-ncov", target="_blank", style ="color: #FF7467;"), '. No obstante, pueden haber problemas de actualización o errores en el código de esta aplicación, por lo cual recuerda siempre verificar con a la información oficial.', class='small', 
                             style='font-weight: normal; text-align: right; margin: 0 4rem; color: #666450'),
                          class='float-right'),
                      hr(),
                      ),
              
                      
               
               # =================
               # Mexico Map
               # =================
               column(12, 
                      
                      div(h3(
                        span('Número de casos', style='font-weight: normal'), 
                        span(textOutput('map_title_cases', inline = TRUE)), 
                        span('por Estado:', style='font-weight: normal'),
                        span(textOutput('map_title_date', inline = TRUE), class='small'),
                        class = 'text-center'),
                        style = 'margin-bottom: 0px; z-index: 100'),
                      hr(),
                      leafletOutput(
                        outputId = 'mapMx',
                        height = "90%"
                      ),
                      
                      fluidRow(
                        # column(6,
                        #        checkboxInput("normalizeCasesMap", 
                        #                      label = h4("Casos por cada 100, 000 habitantes.",
                        #                                 style='font-weight: normal; color: black; margin: 0.2rem 0;'), 
                        #                      value = FALSE),
                        #        
                        # ),
                        column(6,
                              p('Da click sobre un estado para más información.',
                                style='color: black; font-weight: bold;'),
                        ),
                        column(6,
                               p("GIS data map:", a('Kyle Walker', href='http://personal.tcu.edu/kylewalker/'),
                                 style='color: #999;', class='small'),
                               style='text-align: right;'
                        ),
                      ),
                      class = "col-xs-12 col-sm-12 col-lg-6 sm_container",
                      style = "height: 580px; padding: 0 2em;",   
               ),
             ),
             hr(),
             hr(class='hr_main'),
             # =================
             # TIME LINE PLOT
             # =================
             fluidRow(
               column(12,
                      # =================
                      # Time Line
                      # =================
                      column(12, class= 'col-lg-12',
                             div(h3(span('Línea de Tiempo Nacional: ', 
                                         style='font-weight: bold;'),
                                    span('Número de casos',
                                         style = 'font-weight: normal;'),
                                    span(textOutput('lp_title', inline = TRUE),
                                         style = 'font-weight: bold'),
                                    span('por día',
                                         style = 'font-weight: normal'),
                                    class = 'text-center')
                             ),
                             div(
                               withSpinner(
                                 plotlyOutput(
                                   outputId = 'timePlot'
                                 ),
                               ),
                             )
                      ),
                      class = 'col-xs-12  col-lg-9 col-lg-push-3 sm_container',
                      style = 'min-height: 620px;'
                  ),
               
               column(12,
                        # =================
                        # Time Line Inputs
                        # =================   
                        column(12,
                               style='padding: 0 4rem',
                               class = 'col-xs-12 col-sm-12 col-md-12',
                               h3(icon("fas fa-clock"), 
                                  'Datos Nacionales', style='font-weight: normal; color: #333'),
                               hr(class='hr_main_red'),
                               # From the first confirmed case
                               column(12,
                                      awesomeCheckbox("from_first_pos_case", 
                                                    label = h4("A partir del primer caso confirmado",
                                                               style='font-weight: bold; color: #444; margin: -1rem 1rem; padding: 0'),
                                                    status='danger',
                                                    value = TRUE),
                                      icon = icon("check"),
                                      class='col-xs-12 col-sm-6 col-lg-12', 
                                      style = 'padding: 0 2rem;', style='margin-top: 0rem;'
                               ),
                               # Complementary categories
                               column(12,
                                      awesomeCheckboxGroup("case_categories", 
                                         label = h4("Categorías complementárias:",
                                                    style='font-weight: bold; color: #444; margin: 1rem 0;'), 
                                         choices = list("Casos Sospechosos" = 'Susp_rep', 
                                                        "Casos Negativos" = 'Neg_rep',
                                                        "Número de Pruebas Realizadas" = 'Tested_tot'),
                                         selected = c('Susp_rep', 'Neg_rep', 'Tested_tot')),
                                      class='col-xs-12 col-sm-6 col-lg-12 plot_input_labels'
                               ),
                               
                               # Cummulative or new cases
                               column(12,
                                      awesomeRadio("cum_or_new_cases", 
                                                   label = h4("Casos por día:",
                                                              style='font-weight: bold; color: #444; margin: 1rem 0;'), 
                                                   choices = list("Acumulados" = 'cum', 
                                                                  "Nuevos" = 'new'),
                                                   status='succes', inline=TRUE),
                                      class='col-xs-12 col-sm-6 col-lg-12 plot_input_labels', style='margin-top: 1rem;'
                               ),
                               
                               # Scale
                               column(12,
                                      awesomeRadio("scale_log", 
                                                   label = h4("Selecciona la escala de visualización:",
                                                              style='font-weight: bold; color: #444; margin: 1rem 0;'), 
                                                   choices = list("Lineal" = 'raw', 
                                                                  "Logarítmica (log10)" = 'log'),
                                                   status='succes',
                                                   selected = 'raw', inline = F),
                                      class='col-xs-12 col-sm-6 col-lg-12 plot_input_labels', style='margin-top: 1rem;'
                               ),
                               
                          ),
                      class = 'col-xs-12 col-lg-3 col-lg-pull-9 sideSecondary',
                      style = 'padding: 3rem; color: black;'
                    ),

             ),
             
             hr(),
             hr(class='hr_main'),     
             hr(),
             
             fluidRow(
               
               
               column(12,
                      class = 'col-xs-12  col-lg-9 col-lg-push-3',
                      h3(span('Desglose de los Datos:'),
                         span(textOutput('sec2_title_date', inline = TRUE), style='font-weight: normal'),
                         style = 'text-align: center;'),
                      hr(),
                      column(12, class = 'col-sm-12 col-md-6 col-lg-6 sm-container',
                             h4(span('Distribución de Sexos:'),
                                span('Casos positivos', style='font-weight: normal'), class='text-center'),
                             div(
                               withSpinner(
                                 plotlyOutput(
                                   outputId = 'pieSex'
                                 ),
                               ),
                             )
                      ),
                      
                      column(12, class = 'col-sm-12 col-md-6 col-lg-6',
                             h4(span('Distribución de Edades:'),
                                span('Casos positivos', style='font-weight: normal'), class='text-center'),
                             div(
                               
                               withSpinner(
                                 plotlyOutput(
                                   outputId = 'histAges'
                                 ),
                               ),
                               style = 'padding: 0 1rem;'
                             ),
                             div(
                               checkboxInput("splitBySex", 
                                             label = h4("Distribuir por sexos",
                                                        style='font-weight: normal; color: black; margin: 0.2rem 0;'), 
                                             value = TRUE),
                               style='padding-left: 80px;'
                             ),
                             
                      ),
                      
                      # column(12, class = 'col-sm-12 col-md-3 col-lg-3',
                      #        h4(span('País Fuente:'),
                      #           span('Casos positivos', style='font-weight: normal'), class='text-center'),
                      #        div(
                      #          withSpinner(
                      #            plotlyOutput(
                      #              outputId = 'importCountry'
                      #            ),
                      #          ),
                      #          style = 'padding: 0 0;'
                      #        )
                      # ),
                      

               ),
               
               # =====================
               # Stacker bars and hist
               # =====================
               column(12,
                      # =================
                      #  Inputs
                      # =================   
                      column(12,
                             style='padding: 0 4rem',
                             class = 'col-xs-12 col-sm-12 col-md-12 sm_container',
                             h3(icon('fas fa-info-circle'),
                                'Información sobre los casos', style='font-weight: normal;', class='sec3-header'),
                             hr(class='hr_main_red'),
                             br(),
                             # Go to date button
                             column(12,
                                    actionButton(
                                      inputId = 'selState',
                                      label = h4('Modificar fecha',
                                                 style='font-weight: normal; color: black; margin: 0.2rem 0;'),
                                      onclick ="document.getElementById('pickDate').scrollIntoView();"
                                    ),
                                    class='col-xs-12 col-sm-6 col-lg-12', 
                                    style = 'padding: 0;', style='margin-top: 1rem;'
                             ),
                             br(),
                             br(),
                             # Select data per a requested state?
                             column(12,
                                    checkboxInput("filterByState", 
                                                  label = h4("Filtrar datos por estado.",
                                                             style='font-weight: bold; color: #444; margin: 0 1rem;'), 
                                                  value = FALSE),
                                    class='col-xs-12 col-sm-6 col-lg-12', 
                                    style = 'padding: 0 2rem;', style='margin-top: 1rem;'
                             ),
                             # State Selection
                             conditionalPanel(
                               condition = "input.filterByState === true",
                               column(12,
                                      selectInput(
                                        inputId = 'selState',
                                        label = h4('Selecciona el Estado:',
                                                   style='font-weight: bold; color: black; margin: 0.2rem 0;'),
                                        choices = MX_SATES, 
                                        selected = 'positivos'
                                      ),
                                      class='col-xs-12 col-sm-6 col-lg-12', 
                                      style = 'padding: 0 2rem;', style='margin-top: 1rem;'
                               ),
                               h4('Pendiente...')
                             ),
                             
                             # Complementary categories
                             column(12,
                                    p(''),
                                    class='col-xs-12 col-sm-6 col-lg-12 plot_input_labels'
                             ),
                      ),
                      class = 'col-xs-12 col-lg-3 col-lg-pull-9 sideSecondary',
                      style = 'padding: 3rem'
               ),
               
               fluidRow(
                                       column(12, class = 'col-sm-12 col-md-12 col-lg-12',
                             h3(span('Línea de Tiempo por Estado:'),
                                span('Casos positivos', style='font-weight: normal'), class='text-center'),
                             div(
                               withSpinner(
                                 plotlyOutput(
                                   outputId = 'statesTimePlot'
                                 ),
                               ),
                               br(),
                               fluidRow(
                                 style='margin-top: 4em;',
                                 
                                 column(12,
                                        div(
                                          awesomeCheckbox("normalizeCases", 
                                                        label = h4(span("Normalizar los casos:", style='font-weight: bold;'), 
                                                                   span("Casos por cada 100,000 habitantes"),
                                                                   style='font-weight: normal; color: black; margin: 0.2rem 0;'), 
                                                        value = FALSE),
                                          style='padding-left: 80px;'
                                        ),
                                 ),
                                 column(12,
                                   div(
                                     awesomeCheckbox("hideLabelsLineStates", 
                                                   label = h4(span("Mostrar Nombres de los estados", style='font-weight: bold;'),
                                                              style='font-weight: normal; color: black; margin: 0.2rem 0;'), 
                                                   value = TRUE),
                                     style='padding-left: 80px;'
                                   ),
                                 ),
                               ),
                               
                               style = 'padding: 0 1rem;'
                             )
                      ),
               )
               
             ),
             
             class = 'col-xs-12 col-md-12 col-lg-12',
      )
    ),
    class='main_row_layout',
    div(
      includeHTML('include_html/footer.html')
    ),
  )
)