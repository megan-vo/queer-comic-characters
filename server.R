# Add libraries and data sets
library(ggplot2)
library(shiny)
library(RColorBrewer)
library(shinythemes)
library(plotly)
library(dplyr)

########
# Note #
########
# Source code for tooltip wellPanel from https://gitlab.com/snippets/16220


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

# Change the column "SEX" to "GENDER"
colnames(gsm.marvel)[which(names(gsm.marvel) == "SEX")] <- "GENDER"
colnames(gsm.dc)[which(names(gsm.dc) == "SEX")] <- "GENDER"

# Extracts the top 10 characters that appeared the most times for DC and MARVEL
marvel_10 <- read.csv("data/marvel_10.csv", stringsAsFactors = FALSE)
dc_10 <- read.csv("data/dc_10.csv", stringsAsFactors = FALSE)


# Defines server function
my.server <- function(input, output) {
  #################
  # GSM HISTOGRAM #
  #################
  
  # Creates reactive function that takes in user input and changes the data to be viewed
  company <- reactive({
    # Combine the two dataframes as a default
    data <- bind_rows(gsm.marvel, gsm.dc) %>% filter(YEAR != "") 
    
    # Arranges data so that color groups stay together
    if(input$feature == "ALIGNMENT (Good/Bad/Neutral)") {
      data <- arrange(data, ALIGN)
    } else if(input$feature == "GENDER") {
      data <- arrange(data, GENDER)
    } else {
      data <- arrange(data, GSM)
    }
    
    # Based on what the user checkboxes, filter for that data only
    if(length(input$company.data) == 1) {
      data <- filter(data, COMPANY == input$company.data)
    } 
    # Count the number of characters per year
    data <- data %>% 
      group_by(YEAR) %>% mutate(COUNT = 1:n()) 
    
    # For those with no alignment, assigns 'N/A' to row
    data$ALIGN[data$ALIGN == ""] <- "N/A"
    
    return(data)
  })
  
  # Renders histogram plot based on user input
  output$histogram <- renderPlot({
    # Calls upon the data from company() to create a histogram
    # Based on radio button user input, changes the legend of the plot
    if (input$feature == "ALIGNMENT (Good/Bad/Neutral)") {
      histogram <- ggplot(data = company(), aes(color = ALIGN))
    } else if (input$feature == "GENDER") {
      histogram <- ggplot(data = company(), aes(color = GENDER))
    } else {
      histogram <- ggplot(data = company(), aes(color = GSM))
    }
    histogram <- histogram + geom_count(aes(x = YEAR, y = COUNT),
      size = 5, position = position_dodge(width = 0.1)) +
      theme_bw() + 
     # theme(plot.background = element_rect(fill = "grey")) +

      
      # Keeps the ratio of height:width
      # coord_fixed(1.2) * coord_fixed() for some reason does not work well with nearPoints()
      
      # Add more ticks to x and y axis
      scale_x_continuous(breaks = round(seq(1940, max(company()$YEAR), by = 3), 1)) +
      
      # Prevents the graph scale from getting smaller
      ylim(1, 13) +
      scale_color_brewer(palette = "Pastel1") 
    
    # If they want to distinguish between companies, changes shape
    return(histogram)
  })
  
  # Renders the hover information into a little panel
  output$hist_info <- renderUI({
    data <- company()
    hover <- input$plot_hover
    
    # Finds the information from the rows nearest to the hover
    point <- nearPoints(data, coordinfo = hover, xvar = "YEAR", yvar = 
                          "COUNT", threshold = 5, maxpoints = 1, addDist = TRUE)
    style <- paste0("background-color: rgba(255, 255, 255, 0.85); ")

    # Tooltip but not really a tooltip because it's stationary
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Name: </b>", point$name, "<br/>",
                    "<b> Gender: </b>", point$GENDER, "<br/>",
                    "<b> Year of First Appearance: </b>", point$YEAR, "<br/>", #</br> breaks to next line
                    "<b> Current Status: </b>", point$ALIVE, "<br/>",
                    "<b> Alignment: </b>", point$ALIGN, "<br/>",
                    "<b> GSM: </b>", point$GSM, "<br/>",
                    "<b> Publisher: </b>", point$COMPANY, "<br/>")))
    )
  })
  
  ########################
  # Bar Graph Comparison #
  ########################
  
  # Gives the summary statistics based on what the user is viewing
  stats_data <- reactive({
    data <- company()
    if(length(input$company.data) == 1) {
      data <- data %>% 
        filter(input$company.data == COMPANY) %>% 
        select(COMPANY, input$feature) %>% 
        group_by(input$feature) %>% 
        summarize(
          n = n()
        )
    } 
    
    return(data)
  })
  
  
  #############
  # Pie Chart #
  #############
  

  ##########
  # Top 10 #
  ##########
  # Create a reactive data frame containing ranks that will change based on forward/backward buttons
  dc_rank <- reactiveValues()
  dc_rank$rankings <- data.frame(RANKINGS = c(1, 2, 3, 4, 5)) # Stores current rank positions as a column in frame
  
  # If user hits the forward button, shifts rankings up with wrap-around
  dc_shift <- observeEvent(input$dc_forward, {
    dc_rank$rankings$RANKINGS <- c(tail(dc_rank$rankings$RANKINGS, -1), head(dc_rank$rankings$RANKINGS, 1))
  })
  
  # If user hits the back button, shifts rankings downward with wrap-around
  dc_shift <- observeEvent(input$dc_backward, {
    dc_rank$rankings$RANKINGS <- c(tail(dc_rank$rankings$RANKINGS, 1), head(dc_rank$rankings$RANKINGS, -1))
  })
  
  # Renders names of top 5 DC appearances based on forward/backward action buttons
  output$dc_top_name <- renderUI({
    data <- dc_10 %>% 
      filter(RANK == dc_rank$rankings$RANKINGS[1]) # match the current dc_rankings first element in column with dc_10 df's RANKING
    name <- data$name
    
    style <- paste0("background-color: rgba(255, 255, 255, 0.85); padding: 5px;")
    
    # Tooltip but not really a tooltip because it's stationary
    wellPanel(
      style = style,
      p(HTML(paste0("<b> </b>", data$name,"<br/>")))
    )
  })
    
    # Create a reactive data frame containing ranks that will change based on forward/backward buttons
    m_rank <- reactiveValues()
    m_rank$rankings <- data.frame(RANKINGS = c(1, 2, 3, 4, 5)) # Stores current rank positions as a column in frame
    
    # If user hits the forward button, shifts rankings up with wrap-around
    m_shift <- observeEvent(input$marvel_forward, {
      m_rank$rankings$RANKINGS <- c(tail(m_rank$rankings$RANKINGS, -1), head(m_rank$rankings$RANKINGS, 1))
    })
    
    # If user hits the back button, shifts rankings downward with wrap-around
    m_shift <- observeEvent(input$marvel_backward, {
      m_rank$rankings$RANKINGS <- c(tail(m_rank$rankings$RANKINGS, 1), head(m_rank$rankings$RANKINGS, -1))
    })
    
    # Render the name of the top Marvel character based on user input
    output$marvel_top_name <- renderUI({
      data <- marvel_10 %>% 
        filter(RANK == m_rank$rankings$RANKINGS[1]) # match the current m_rankings first element in column with marvel_10 df's RANKING
      name <- data$name
      
      style <- paste0("background-color: rgba(255, 255, 255, 0.85); padding: 5px;")
      
      # Tooltip but not really a tooltip because it's stationary
      wellPanel(
        style = style,
        p(HTML(paste0("<b> </b>", data$name,"<br/>")))
      )
    })
    
    # Output the character profile of current character being viewed
    output$marvel_top_info <- renderUI({
      data <- marvel_10 %>% 
        filter(RANK == m_rank$rankings$RANKINGS[1]) # match the current m_rankings first element in column with marvel_10 df's RANKING
      name <- data$name
      
      style <- paste0("background-color: rgba(255, 255, 255, 0.85); padding: 5px;")

      # Tooltip but not really a tooltip because it's stationary
      wellPanel(
        style = style,
        p(HTML("<img src=", data$PIC, "width=100, height=120> <br/>",
          paste0("<b> #", data$RANK, "</b> <br/>",
                      "<b> Appearances: </b>", data$APPEARANCES,"<br/>",
                      "<b> Year of First Appearance: </b>", data$YEAR,"<br/>",
                      "<b> Alignment: </b>", data$ALIGN,"<br/>",
                      "<b> Gender: </b>", data$GENDER,"<br/>",
                      "<b> GSM: </b>", data$GSM,"<br/>",
                      "<h6> Artist(s) - ", data$Artist, "</h6>", # Image credits
                      "<h6> Source: ", data$Source, "</h6>", # Image credits
                      "<h6> Date of Publishing: ", data$Date, "</h6>", # Image credits
                      "<h6> <a href=", data$Wikia, ">Image Wikia Link</a>" # Image link
                 )))
      )
    })
    
    # Output the character profile of current character being viewed
    output$dc_top_info <- renderUI({
      data <- dc_10 %>% 
        filter(RANK == dc_rank$rankings$RANKINGS[1]) # match the current m_rankings first element in column with marvel_10 df's RANKING
      name <- data$name
      
      style <- paste0("background-color: rgba(255, 255, 255, 0.85); padding: 5px;")
      
      # Tooltip but not really a tooltip because it's stationary
      wellPanel(
        style = style,
        p(HTML("<img src=", data$PIC, "width=120, height=120> <br/>",
               paste0("<b> #", data$RANK, "</b> <br/>",
                      "<b> Appearances: </b>", data$APPEARANCES,"<br/>",
                      "<b> Year of First Appearance: </b>", data$YEAR,"<br/>",
                      "<b> Alignment: </b>", data$ALIGN,"<br/>",
                      "<b> Gender: </b>", data$GENDER,"<br/>",
                      "<b> GSM: </b>", data$GSM,"<br/>",
                      "<h6> Artist(s) - ", data$Artist, "</h6>", # Image credits
                      "<h6> Source: ", data$Source, "</h6>", # Image credits
                      "<h6> Date of Publishing: ", data$Date, "</h6>", # Image credits
                      "<h6> <a href=", data$Wikia, ">Image Wikia Link</a>" # Image link
               )))
      )
    })
}


# Creates server 
shinyServer(my.server)
  