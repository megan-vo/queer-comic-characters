# Add libraries and data sets
library(ggplot2)
library(shiny)
library(RColorBrewer)
library(shinythemes)
library(plotly)
library(dplyr)

###############
# Data Frames #
###############

# Read in Marvel and DC data sets
whole.marvel <- read.csv("data/marvel-wikia-data.csv", stringsAsFactors = FALSE)
whole.dc <- read.csv("data/dc-wikia-data.csv", stringsAsFactors = FALSE)

# Filter for only the GSM characters
gsm.marvel <- filter(whole.marvel, GSM != "") %>% mutate()
gsm.dc <- filter(whole.dc, GSM != "")

# Defines server function
my.server <- function(input, output) {
  #################
  # GSM HISTOGRAM #
  #################
  
  # Creates reactive function that takes in user input and changes the data to be viewed
  polarity.candidate <- reactive({
    data <- sample.polarity
  })
}

# Creates server 
shinyServer(my.server)
  