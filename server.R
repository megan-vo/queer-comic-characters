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

# Rename Marvel's "Year" to all caps
colnames(whole.marvel)[13] <- "YEAR"

# Filter for only the GSM characters and add company name
gsm.marvel <- filter(whole.marvel, GSM != "") %>% mutate(COMPANY = "MARVEL")
gsm.dc <- filter(whole.dc, GSM != "") %>%  mutate(COMPANY = "DC")

# Defines server function
my.server <- function(input, output) {
  #################
  # GSM HISTOGRAM #
  #################
  
  # Creates reactive function that takes in user input and changes the data to be viewed
  company <- reactive({
    # Combine the two dataframes as a default
    data <- bind_rows(gsm.marvel, gsm.dc) %>% filter(YEAR != "")
    
    # Based on what the user checkboxes, filter for that data only
    if(input$company.data == "Marvel") {
      data <- filter(data, Company == "Marvel")
    } else {
      data <- filter(data, Company == "DC")
    }
  })
}

# Creates server 
shinyServer(my.server)
  