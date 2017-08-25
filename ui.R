# Add libraries and data sets
library(ggplot2)
library(shiny)
library(RColorBrewer)
library(shinythemes)
library(plotly)
library(dplyr)

my.ui <- fluidPage(  
  
    
     # style = "position:relative",
      plotlyOutput("histogram"),


    fluidRow(
      column(4,
             checkboxGroupInput('company.data', label = 'View by Publishing Company',
                                choices = c("DC", "MARVEL"), 
                                selected = c("DC", "MARVEL"))
      ),
      column(4,
             radioButtons("feature", label = "Choose",
                          choices = c("ALIGNMENT (Good/Bad/Neutral)", "GENDER", "GSM (Gender/Sexuality Minority)"), 
                          selected = c("GENDER"))
      )
  )
)

# Make UI from my.ui
shinyUI(my.ui)