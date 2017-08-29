# Add library
library(shinythemes)

########
# NOTE #
########
# The reference for the comprehensive comparison plot and widgets of the project comes from Shiny's
  # movie explorer example which can be found here: https://github.com/rstudio/shiny-examples/tree/master/051-movie-explorer

my.ui <- fluidPage(
    #theme = shinytheme("slate"), 
    fluidRow(align = "center",
      h2("QUEER COMPANY"),
      h5(em("Breaking down the composition of queer MARVEL and DC characters from 1940 to 2014"))
    ),
    # style = "position:relative",
    fluidRow(align = "center",
             plotOutput("histogram", width = "100%", height = 180,
                        hover = hoverOpts(id = "plot_hover", delay = 100, delayType = "debounce"))
    ),
      
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
      )
    ),
    fluidRow(align = "center",
             # Placeholder text 
               p("At vero eos et accusamus et iusto odio dignissimos ducimus qui blanditiis 
                 praesentium voluptatum deleniti atque corrupti quos dolores et quas molestias 
                 excepturi sint occaecati cupiditate non provident, similique sunt in culpa qui 
                 officia deserunt mollitia animi, id est laborum et dolorum fuga. Et harum 
                 quidem rerum facilis est et expedita distinctio. Nam libero tempore, cum soluta 
                 nobis est eligendi optio cumque nihil impedit quo minus id quod maxime placeat facere 
                 possimus, omnis voluptas assumenda est, omnis dolor repellendus. Temporibus autem quibusdam et
                 aut officiis debitis aut rerum necessitatibus saepe eveniet ut et voluptates 
                 repudiandae sint et molestiae non recusandae. Itaque earum rerum hic tenetur a sapiente 
                 delectus, ut aut reiciendis voluptatibus maiores alias consequatur aut 
                 perferendis doloribus asperiores repellat.")
    ),
    
    ##########
    # TOP 10 #
    ##########
      fluidRow(align = "center", div(style = "height:75px;background-color: black;"),
               h2("Are You In or Are You Out?"),
               h5(em("The top 5 queer characters from MARVEL and DC by number of appearances"))
      ),
      fluidRow(
        column(6, align = "center",
              h4("MARVEL Characters")
        ),
        column(6, align = "center",
              h4("DC Characters")
        )  
      ),
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
        ),
        column(6, align = "center",
               uiOutput("dc_top_info")       
        )
      ),
    fluidRow(align = "center",
             
             # Placeholder text
             p("At vero eos et accusamus et iusto odio dignissimos ducimus qui blanditiis 
               quidem rerum facilis est et expedita distinctio. Nam libero tempore, cum soluta 
               nobis est eligendi optio cumque nihil impedit quo minus id quod maxime placeat facere 
               possimus, omnis voluptas assumenda est, omnis dolor repellendus. Temporibus autem quibusdam et
               aut officiis debitis aut rerum necessitatibus saepe eveniet ut et voluptates 
               repudiandae sint et molestiae non recusandae. Itaque earum rerum hic tenetur a sapiente 
               delectus, ut aut reiciendis voluptatibus maiores alias consequatur aut 
               perferendis doloribus asperiores repellat.")
    ),
    fluidRow(
      column(9,
        plotOutput("characters_plot", height = 480, width = "100%",
                   hover = hoverOpts(id = "compare_hover", delay = 100, delayType = "debounce"),
                   dblclick = "dblclick",
                   brush = brushOpts(
                     id = "brush",
                     resetOnNew = TRUE)),
        column(6,
          uiOutput("character_info")     
        ),
        column(6, align = "center",
          uiOutput("percentages")             
        )
      ),
      column(3,
             wellPanel(
               sliderInput("appearances", "# of Appearances", min = 1, max = 4500, value = c(50, 1500)),
               sliderInput("years", "Year of First Appearance", min = 1939, max = 2014, value = c(1939, 2014)),
               textInput("character", "Character name contains (ex., Akihiro)"),
               tags$small(paste0(
                 "Note: Most characters are searchable under their real",
                 " name and not their alias."
               ))
             ),
             wellPanel(
               radioButtons("compare", label = "Compare by", choices = c("Males to Females", "Gender or Sexuality Minority Status"),
                            selected = "Males to Females")
             )
      )
    ),
    fluidRow(
      
    )
)

# Make UI from my.ui
shinyUI(my.ui)