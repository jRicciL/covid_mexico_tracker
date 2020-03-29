# =========================================================================
# Load libraries and scripts
# =========================================================================
library(shiny)
library(shinythemes)
library(plotly)
library(leaflet)
library(shinycssloaders)
source('data/parser.R')

# =========================================================================
# GET parsed data from parser.R script
# =========================================================================
data <- preprocessing_data()

df_total_reps <- data$df_total_reps
df_pos_states <- data$df_pos_states
df_sup_states <- data$df_sup_states
df_deceased_states <- data$df_deceased_states
max_pos_today <- data$max_pos_today
max_sup_today <- data$max_sup_today
max_deceased_today <- data$max_deceased_today
first_date <- data$first_date
last_date <- data$last_date
last_date_formated <- format(as.Date(last_date), 
                             format = "%d de %b del %Y")
mexico <- data$mexico

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
options(spinner.color="#FF7467", spinner.color.background="#ffffff", spinner.size=1.5)

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

