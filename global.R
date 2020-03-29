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
max_today <- data$max_today
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

get_pal <- function(colors_str = "YlOrRd", n_bins = 6 ) {
  pal_ <- colorBin(colors_str, sqrt(c(1,max_today)), 
           bins = n_bins, na.color = '#FFFFFF')
  return(pal_)
}

# =========================================================================
# Plotling utilities
# =========================================================================

# ****** Color Palettes ******
# Color palette used for active cases in mexico map
map_pal <- colorBin("Spectral", sqrt(c(1,max_today)), 
                bins = 6, na.color = '#FFFFFF')
error_pal <- colorBin("Greys", sqrt(c(1,max_today)), 
                       bins = 6, na.color = '#FFFFFF')

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

