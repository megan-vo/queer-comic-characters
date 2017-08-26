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

# Change the column "SEX" to "GENDER"
colnames(gsm.marvel)[which(names(gsm.marvel) == "SEX")] <- "GENDER"
colnames(gsm.dc)[which(names(gsm.dc) == "SEX")] <- "GENDER"


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
      p(HTML(paste0("<b> Name: ", point$name, "<br/>",
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
  # Extracts the top 5 characters that appeared the most times for MARVEL
  marvel_10 <- reactive({
    data <- gsm.marvel %>% 
      filter(APPEARANCES != "") 
    data <- head(arrange(data, desc(APPEARANCES)), 5) # Arranges it from most appearances and decreases downward
    data <- select(data, name, ALIGN, GENDER, APPEARANCES, YEAR, GSM)
    data$ALIGN[data$ALIGN == ""] <- "N/A"
    return(data)
  })
  
  # Extracts the top 5 characters that appeared the most times for DC
  dc_10 <- reactive({
    data <- gsm.dc %>% 
      filter(APPEARANCES != "") 
    data <- head(arrange(data, desc(APPEARANCES)), 5) # Arranges it from most appearances and decreases downward
    data <- select(data, name, ALIGN, GENDER, APPEARANCES, YEAR, GSM)
    data$ALIGN[data$ALIGN == ""] <- "N/A"
    return(data)
  })
  
  output$top_marvel <- renderPlot(height = 300, {
    bar <- ggplot(marvel_10(), aes(x = name, y = APPEARANCES, fill = name)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_minimal() +
      xlab("NAME") +
      scale_fill_brewer(palette = "Reds", guide = FALSE) # removes legend
    return(bar)
  })
  
  output$top_dc <- renderPlot(height = 300, {
    bar <- ggplot(dc_10(), aes(x = name, y = APPEARANCES, fill = name)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_minimal() +
      xlab("NAME") +
      scale_fill_brewer(palette = "Blues", guide = FALSE) # removes legend
    return(bar)
  })
}


# Creates server 
shinyServer(my.server)
  