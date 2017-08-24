# Add libraries and data sets
library(ggplot2)
library(shiny)
library(RColorBrewer)
library(shinythemes)
library(plotly)
library(dplyr)

my.ui <- fluidPage(  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput('company.data', label = 'View by Publishing Company',
                         choices = c("DC", "MARVEL"), 
                         selected = c("DC", "MARVEL"))
    ),
    
    mainPanel(
      plotOutput("histogram")
    )
  )
)

# Make UI from my.ui
shinyUI(my.ui)