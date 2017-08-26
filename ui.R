# Add libraries and data sets
library(ggplot2)
library(shiny)
library(RColorBrewer)
library(shinythemes)
library(plotly)
library(dplyr)

my.ui <- fluidPage(  
  
    # style = "position:relative",
    plotOutput("histogram", width = "100%", height = 180,
                hover = hoverOpts(id = "plot_hover", delay = 100, delayType = "debounce")),
      
    fluidRow(
      column(4,
             checkboxGroupInput('company.data', label = 'View by Publishing Company',
                                choices = c("DC", "MARVEL"), 
                                selected = c("DC", "MARVEL"))
      ),
      column(4,
             radioButtons("feature", label = "View by Category",
                          choices = c("ALIGNMENT (Good/Bad/Neutral)", "GENDER", "GSM (Gender/Sexuality Minority)"), 
                          selected = c("GENDER"))
      ),
      column(4,
             uiOutput("hist_info")
      ),
      fluidRow(
        column(6,
               plotOutput("top_marvel")
        ),
        column(6,
               plotOutput("top_dc")       
        )
      )
  )
)

# Make UI from my.ui
shinyUI(my.ui)