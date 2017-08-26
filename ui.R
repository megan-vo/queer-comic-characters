# Add libraries and data sets
library(ggplot2)
library(shiny)
library(RColorBrewer)
library(shinythemes)
library(plotly)
library(dplyr)

my.ui <- fluidPage(  
    #theme = shinytheme("slate"), 
  
    # style = "position:relative",
    plotOutput("histogram", width = "100%", height = 180,
                hover = hoverOpts(id = "plot_hover", delay = 100, delayType = "debounce")),
      
    fluidRow(
      column(4, align = "right",
             checkboxGroupInput('company.data', label = 'View by Publishing Company',
                                choices = c("DC", "MARVEL"), 
                                selected = c("DC", "MARVEL"))
      ),
      column(4,
             uiOutput("hist_info")
      ),
      column(4, align = "left",
             radioButtons("feature", label = "View by Category",
                          choices = c("ALIGNMENT (Good/Bad/Neutral)", "GENDER", "GSM (Gender/Sexuality Minority)"), 
                          selected = c("GENDER"))
      )),
      fluidRow(
        column(2, align = "right",
               #plotlyOutput("top_marvel", height = 300)
               actionButton('marvel_backward', width = 20, label = "",
                            icon = icon("angle-left", lib = "font-awesome"), class = "btn-primary")
        ),
        column(2, align = "center",
               uiOutput("marvel_top_name")
        ),
               
        column(2, align = "left",
               actionButton('marvel_forward', width = 20, label = "",
                            icon = icon("angle-right", lib = "font-awesome"), class = "btn-primary")
        ),
        column(2, align = "right",
               #plotlyOutput("top_marvel", height = 300)
               actionButton('dc_backward', width = 20, label = "",
                            icon = icon("angle-left", lib = "font-awesome"), class = "btn-primary")
        ),
        column(2, align = "center",
               uiOutput("dc_top_name")
        ),
        
        column(2, align = "left",
               actionButton('dc_forward', width = 20, label = "",
                            icon = icon("angle-right", lib = "font-awesome"), class = "btn-primary")
        )
      ),
      fluidRow(
        column(6, align = "center",
               uiOutput("marvel_top_info")
        )
      )
)

# Make UI from my.ui
shinyUI(my.ui)