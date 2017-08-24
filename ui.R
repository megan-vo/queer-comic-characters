# Add libraries and data sets
library(ggplot2)
library(shiny)
library(RColorBrewer)
library(shinythemes)
library(plotly)
library(dplyr)

my.ui <- fluidPage(  
  checkboxGroupInput('company.data', label = 'View by Publishing Company',
                     choices = c("DC", "Marvel"), 
                     selected = c("DC", "Marvel"))
)

# Make UI from my.ui
shinyUI(my.ui)