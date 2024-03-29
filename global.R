# =========================================================================
# Load libraries and scripts
# =========================================================================
library(shiny)
library(shinythemes)
library(plotly)
library(leaflet)
library(shinycssloaders)
library(shinyWidgets)
library(shinyalert)
library(languageserver)
source('data/parser.R')
Sys.setlocale(locale="es_ES.UTF-8")
# =========================================================================
# GET parsed data from parser.R script
# =========================================================================
data <- preprocessing_data()

DATE_FIRST_POS_CASE = '2020-02-28'

df_total_reps <- data$df_total_reps
df_pos_states <- data$df_pos_states
df_sup_states <- data$df_sup_states
df_deceased_states <- data$df_deceased_states
raw_daily_data <- data$raw_daily_data
max_pos_today <- data$max_pos_today
max_sup_today <- data$max_sup_today
max_deceased_today <- data$max_deceased_today
first_date <- data$first_date
last_date <- data$last_date
last_date_formated <- format(as.Date(last_date), 
                             format = "%d de %b del %Y")
mexico <- data$mexico
MX_SATES <- mexico$names_corrected
MX_POP <- mexico$population_states
MX_POP <- as.vector(MX_POP)
names(MX_POP) <- MX_SATES

  # =========================================================================
# helper function date_to_int
#
# @description: convert a given string date (ISO format)  to int
# @param text_date: string date ISO format
# @return: int (year-month-day)
# =========================================================================
date_to_int <- function(text_date){
  date_int <- as.integer(format(as.Date(text_date),
                                format = "%Y%m%d"))
  return(date_int)
}

# ****** Color Palettes ******
# Color palette used for active cases in mexico map
get_pal <- function(colors_str = "YlOrRd", n_bins = 6,  reverse = FALSE, max_today) {
  pal_ <- colorBin(colors_str, sqrt(c(1, max_today)), reverse = reverse, 
           bins = n_bins, na.color = '#FFFFFF')
  return(pal_)
}

# =========================================================================
# Plotling utilities
# =========================================================================

# ****** Spinner Options ******
options(spinner.type = 2,
        spinner.color="#FF7467", 
        spinner.color.background="#F1EFDA", 
        spinner.size= 0.8)

# ****** Plotly configurations *****
# Modebar conf
modebar_plotly_conf <- c( "toImage", "autoScale2d",
  "toggleSpikelines", "hoverCompareCartesian", 
  "hoverClosestCartesian")

# Font
font_plotly <- list(
    size = 14
  )

# Axes for linearplot
ax_lp <- list(marging = list(pad = 0), linecolor = 'rgb(0,0,0)', 
              linewidth = 3, showline = T, title = '<b>Fecha</b>')
yax_lp <- ax_lp

spectral_palette <- c('#a70b44', '#af1446', '#b81e48', '#c1274a', '#c9314c', '#d43d4f',
                      '#d9444d', '#de4c4b', '#e3534a', '#e85b48', '#ee6445', '#f36b43',
                      '#f57547', '#f67f4b', '#f88950', '#fa9656', '#91d3a4', '#84cea5', 
                      '#79c9a5', '#6ec5a5',
                      '#64c0a6', '#5cb7aa', '#52abae', '#49a2b2', '#4199b6', '#3990ba',
                      '#3387bc', '#3b7cb7', '#4273b3', '#496aaf', '#5061aa', '#5758a6')