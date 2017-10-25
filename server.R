# Add libraries and data sets
library(ggplot2)
library(shiny)
library(RColorBrewer)
library(shinythemes)
library(dplyr)

########
# Note #
########
# Source code referenced for tooltip wellPanel from Pawel's https://gitlab.com/snippets/16220 
# The reference for the comprehensive comparison plot and widgets of the project comes from Shiny's
  # movie explorer example which can be found here: https://github.com/rstudio/shiny-examples/tree/master/051-movie-explorer by RStudio
# Zoom brush/double click code referenced from: https://gallery.shinyapps.io/105-plot-interaction-zoom/ by RStudio

###############
# Data Frames #
###############

# Read in Marvel and DC data sets
whole.marvel <- read.csv("data/marvel-wikia-data.csv", stringsAsFactors = FALSE)
whole.dc <- read.csv("data/dc-wikia-data.csv", stringsAsFactors = FALSE)

# Rename Marvel's "Year" to all caps
colnames(whole.marvel)[13] <- "YEAR"

# Change the column "SEX" to "GENDER"
colnames(whole.marvel)[which(names(whole.marvel) == "SEX")] <- "GENDER"
colnames(whole.dc)[which(names(whole.dc) == "SEX")] <- "GENDER"

# Filter for only the GSM characters and add company name
gsm.marvel <- filter(whole.marvel, GSM != "") %>% mutate(COMPANY = "MARVEL")
gsm.dc <- filter(whole.dc, GSM != "") %>%  mutate(COMPANY = "DC")

# Replace homosexual with gay/lesbian
gsm.marvel$GSM <- replace(gsm.marvel$GSM, gsm.marvel$GSM == "Homosexual Characters", "Gay/Lesbian Characters")
gsm.dc$GSM <- replace(gsm.dc$GSM, gsm.dc$GSM == "Homosexual Characters", "Gay/Lesbian Characters")

# Extracts the top 10 characters that appeared the most times for DC and MARVEL
marvel_10 <- read.csv("data/marvel_10.csv", stringsAsFactors = FALSE)
dc_10 <- read.csv("data/dc_10.csv", stringsAsFactors = FALSE)


# Defines server function
my.server <- function(input, output) {
  #################
  # GSM HISTOGRAM #
  #################
  
  # Creates reactive function that takes in user input and changes the data to be viewed. Returns data frame
  # containing desired data
  company <- reactive({
    # Combine the two dataframes as a default
    data <- bind_rows(gsm.marvel, gsm.dc) %>% filter(YEAR != "") 
    
    # Replace male/female with man/woman and get rid of 'characters' ending 
    data$GENDER <- replace(data$GENDER, data$GENDER =="Female Characters", "Woman")
    data$GENDER <- replace(data$GENDER, data$GENDER =="Genderfluid Characters", "Genderfluid")
    data$GENDER <- replace(data$GENDER, data$GENDER =="Genderless Characters", "Genderless")
    data$GENDER <- replace(data$GENDER, data$GENDER =="Male Characters", "Man")
    
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
      scale_color_brewer(palette = "Pastel2") 
    
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

    # Renders a panel that displays information of character based on hovered points
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
  
  # Generate and return analysis of dot plot based on data user is viewing 
  output$comp.analysis <- renderText({
    total <- nrow(company()) # the total number of characters being displayed on the plot
    analysis <- paste0("You are currently viewing ",  total, " GSM characters in ")
    woman <- 3 # default position of "woman" column in vector of columns
    man <- 2 # default position of "men" column in vector of columns
    
    # Changes text output based on company being viewed
    if(length(input$company.data) == 1 & input$company.data == "DC") {
      companies <- "DC"
      gender <- "is also 1 genderless character"
    } else if(length(input$company.data) == 1) {
      companies <- "Marvel"
      gender <- "are also 2 genderfluid characters" 
    } else {
      companies <- "DC and Marvel"
      woman <- 4 # Since another category is added, position of column name shifts down one in vector
      man <- 3
      gender <- "are also 2 genderfluid and 1 genderless characters"
    }
    analysis <- paste0(analysis, companies, ". ")
    
    # Changes text output based on feature being viewed
    if(input$feature == "GENDER") {
      categories <- company() %>% 
        group_by(GENDER) %>% 
        summarize(n = n())
      analysis <- paste0(analysis, "Of the total number of characters being viewed, ", 
                         categories$n[woman], " are women and ", categories$n[man], " are men. There  ",
                         gender, ". From the 1940s up until 1974, GSM character introductions were few and
                         far between. It wasn't until the late 1980s that queer characters were being included more often. Perhaps not surprisingly, gender non-conforming characters have
                         gained less visibility than characters who identify as women or men. So while diversity in sexuality has obtained more traction throughout the years, diversity in 
                         non-binary gender representation has been slower to catch up.")
    } else if(input$feature == "GSM (Gender/Sexuality Minority)") {
      categories <- company() %>% 
        group_by(GSM) %>% 
        summarize(n = n())
      analysis <- paste0(analysis, "Of the total number of characters being viewed, ", categories$n[1], " are bisexual, while ",
                         categories$n[2], " are gay or lesbian. Most of the genderfluid (2), pansexual (2), and transgender (3) characters
                         are from the Marvel Universe, with the exception of Kate Godwin who is part of the DC Universe. That would also indicate that DC's queer representation has been mainly limited
                         to gay/lesbian and bisexual characters (although that could have changed since 2014). Regardless, the majority
                         of GSM characters in both universes are gay or lesbian. It must be noted that in this category, Loki (who is genderfluid and bisexual)
                         is categorized as 'bisexual' here and 'genderfluid' in the Gender category, so that both his gender and
                         sexuality are represented somehow on this plot.")
    } else {
      
      # Group and count the number of characters by their alignment
      categories <- company() %>% 
        group_by(ALIGN) %>% 
        summarize(n = n())
      
      # Calculates number of "good characters" compared to all of the characters in that universe in percentages
      marvel.align <- filter(whole.marvel, ALIGN == "Good Characters") %>% 
        nrow()
      marvel.align <- marvel.align * 100 / nrow(whole.marvel) # percentage of characters in Marvel that are Good
      dc.align <- filter(whole.dc, ALIGN == "Good Characters") %>% 
        nrow()
      dc.align <- dc.align * 100 / nrow(whole.dc) # percentage of characters in DC that are Good
      
      analysis <- paste0(analysis, "Of the total number of characters being viewed, ", categories$n[1], " are considered 'bad', while ",
                         categories$n[2], " are 'good' and ", categories$n[4], " are 'neutral'. The remaining ", categories$n[3], " do not
                         have a given alignment. This calculates to about ", round(categories$n[2] * 100 / total, 2), "% of GSM characters that are
                         good. In contrast, roughly ", round(dc.align, 2), "% of all DC characters and ", round(marvel.align, 2), "% of all Marvel characters 
                         are considered 'good' in their respective universes. This suggests that queer characters in both universes tend to be cast
                         as good at a higher rate than non-queer characters.")
    }
    return(analysis)
  })

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
  
  # Renders names of top 5 DC appearances based on forward/backward action buttons
  output$dc_top_name <- renderUI({
    profile_name(dc_10, dc_rank$rankings$RANKINGS[1])
  })
  
  # Render the name of the top Marvel character based on user input
  output$marvel_top_name <- renderUI({
    profile_name(marvel_10, m_rank$rankings$RANKINGS[1])
  })
  
  # Output the character profile of current character being viewed
  output$marvel_top_info <- renderUI({
    character_profile(marvel_10, m_rank$rankings$RANKINGS[1])
  })
  
  # Output the character profile of current character being viewed
  output$dc_top_info <- renderUI({
    character_profile(dc_10, dc_rank$rankings$RANKINGS[1])
  })
  
  # Outputs the artist information 
  output$dc_artists <- renderUI({
    artist(dc_10, dc_rank$rankings$RANKINGS[1], "DC Comics, Inc.")
  })
  
  # Outputs artist information
  output$marvel_artists<- renderUI({
    artist(marvel_10, m_rank$rankings$RANKINGS[1], "Marvel Comics")
  })
  
  # Function that outputs character profile given the data frame of top 5 characters and the
  # reactive value data frame with column rankings
  character_profile <- function(data.frame, col.rank) {
    data <- data.frame %>% 
      filter(RANK == col.rank)
    style <- paste0("background-color: rgba(255, 255, 255, 0.85); padding: 5px;")
    
    # Renders a panel containing information about characters
    wellPanel(
      style = style,
      p(HTML(#"<img src=", data$PIC, "width=120, height=120> <br/>",
             paste0("<b> #", data$RANK, "</b> <br/>",
                    "<b> Appearances: </b>", data$APPEARANCES,"<br/>",
                    "<b> Year of First Appearance: </b>", data$YEAR,"<br/>",
                    "<b> Alignment: </b>", data$ALIGN,"<br/>",
                    "<b> Gender: </b>", data$GENDER,"<br/>",
                    "<b> GSM: </b>", data$GSM,"<br/><br/>",
                    "<b> Trivia: </b>", data$Trivia, "<br/>")
             ))
    )
  }
  
  # Function that outputs image references and citations given the data frame, character ranking, and publisher string
  artist <- function(data.frame, col.rank, publisher) {
    data <- data.frame %>% 
      filter(RANK == col.rank)
    style <- paste0("background-color: rgba(255, 255, 255, 0.85); padding: 5px;")
    
    # Renders a panel containing information about image
    wellPanel(
      style = style,
      p(HTML(
             paste0("<h6> Artist(s) - ", data$Artist, "</h6>", # Image credits
                    "<h6> Source: ", data$Source, "</h6>", # Image credits
                    "<h6> Date of Publishing: ", data$Date, "</h6>", # Image credits
                    "<h6> Publisher: ", publisher, "</h6>",
                    "<h6> <a href=", data$Wikia, ">Image Wikia Link</a>" # Image link
             ))))
  }
  
  # Given the top 5 company data frame, matches with the reactive value data frame "col.rank" with the 
  # correct ranking and renders the profile name on a panel
  profile_name <- function(data.frame, col.rank) {
    data <- data.frame %>% 
      filter(RANK == col.rank)
    style <- paste0("background-color: rgba(255, 255, 255, 0.85); padding: 5px;")
    name <- data$name
    
    # Renders a panel containing character name
    wellPanel(
      style = style,
      p(HTML(paste0(data$name)))
    )
  }

  ############################
  # COMPREHENSIVE COMPARISON #
  ############################
  
  # Eliminate any rows of data that don't have information about the number of appearances for both dataframes
  # Also add company name as a separate column
  m.appear <- whole.marvel %>% 
    mutate(COMPANY = "MARVEL")
  dc.appear <- whole.dc %>% 
    mutate(COMPANY = "DC")
  total.appear <- bind_rows(dc.appear, m.appear) %>% 
    filter(APPEARANCES != "") %>% 
    filter(GENDER != "") # excludes characters that are considered entities (i.e. not male, female, genderless, or genderfluid)

  # Based on user input, filters data for only relevant characters
  compare.data <- reactive({
    appearances <- input$appearances
    years <- input$years
    character <- input$character
    compare <- input$compare
    
    # Filter based on slider input for year range and appearance range
    data <- total.appear %>% 
      filter(APPEARANCES >= appearances[1],
             APPEARANCES <= appearances[2],
             YEAR >= years[1],
             YEAR <= years[2]
             )
    
    # Filter based on radio button input
    if(input$compare == "Males to Females") {
      data <- filter(data, GENDER %in% c("Male Characters", "Female Characters"))
      colnames(data)[which(names(data) == "GENDER")] <- "SEX"
    } else {
      # Add a column specifying whether a character is a gender/sexuality minority
      gsm.data <- data %>% 
        filter(GSM != "") %>% 
        mutate(`GSM Status` = "GSM")
      non.gsm.data <- data %>% 
        filter(GSM == "") %>% 
        mutate(`GSM Status` = "Not a GSM")
     
      # Bind the data together again
      data <- bind_rows(gsm.data, non.gsm.data)
    }
    
    # Filter based on search input
    if(character != "") {
      char <- data %>% 
        filter(grepl(character, name, ignore.case = TRUE)) # find character names with given substring
      if(nrow(char) != 0) { # if the df has 0 rows in it, display the original graph (otherwise show the changed graph)
        data <- char
      }
    }
    return(data)
  })
  
  # Resets x and y bounds based on user zoom in/out
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  # Interactivity of plot using double clicks and brushes
  observeEvent(input$dblclick, {
    brush <- input$brush
    
    # Set the plot's axes and zoom in if the user selects an area and double clicks
    if(!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
      # If the user double clicks again, zoom out
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  # Renders plot comparing all the characters from both DC and Marvel
  output$characters_plot <- renderPlot({
    data <- compare.data()
    if(input$compare == "Males to Females") {
      plot <- ggplot(data, aes(color = SEX)) 
    } else {
      plot <- ggplot(data, aes(color = `GSM Status`)) 
    }
    plot <- plot +
      geom_point(aes(x = YEAR, y = APPEARANCES), size = 2, alpha = 0.25, stroke = 2) +
      theme_gray() +
      scale_color_brewer(palette = "Set2") +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y) # reset axes of plot based on zoom in/out

    return(plot)
  })
  
  # Render the tooltip for hovered over points
  output$character_info <- renderUI({
    hover <- input$compare_hover
    point <- nearPoints(compare.data(), coordinfo = hover, xvar = "YEAR", yvar = 
                          "APPEARANCES", threshold = 5, maxpoints = 1, addDist = TRUE)

    style <- paste0("background-color: rgba(255, 255, 255, 0.85); ")
    wellPanel(
      style = style,
      h5("Current Character: "),
      p(HTML(paste0("<b>Name: </b>", point$name,"<br/>",
                    "<b>Year of First Appearance: </b>", point$YEAR,"<br/>",
                    "<b> # of Appearances: </b>", point$APPEARANCES,"<br/>",
                    "<b> Company/Publisher: </b>", point$COMPANY, "<br/>"
                    )))
    )
  })
  
  # Outputs the summary statistics based on user input
  output$percentages <- renderUI({
    total <- nrow(compare.data()) # total number of rows
    
    marvel <- compare.data() %>% 
      filter(COMPANY == "MARVEL")
    marvel.total <- nrow(marvel) # total number of Marvel characters
    
    dc <- compare.data() %>% 
      filter(COMPANY == "DC")
    dc.total <- nrow(dc) # total number of DC characters
    
    # Calculates the percentages of each ratio
    if(input$compare == "Males to Females") {
      total.percent <- nrow(filter(compare.data(), SEX == "Female Characters")) * 100 / total 
      marvel.percent <- nrow(filter(marvel, SEX == "Female Characters")) * 100 / marvel.total 
      dc.percent <- nrow(filter(dc, SEX == "Female Characters")) * 100 / dc.total 
      header <- "female"
    } else {
      total.percent <- nrow(filter(compare.data(), `GSM Status` == "GSM")) * 100 / total 
      marvel.percent <- nrow(filter(marvel, `GSM Status` == "GSM")) * 100 / marvel.total 
      dc.percent <- nrow(filter(dc, `GSM Status` == "GSM")) * 100 / dc.total
      header <- "GS minority"
    }
    
    # If summary stats don't exist for the company, replace 'NaN' with 0%
    if (total.percent == "NaN") { # three if statements for each possible case
      total.percent = 0
    } 
    if (marvel.percent == "NaN") {
      marvel.percent = 0
    } 
    if (dc.percent == "NaN"){
      dc.percent = 0
    } 
    
    style <- paste0("background-color: rgba(255, 255, 255, 0.85); ")
    wellPanel(
      style = style,
      h4(paste0("Summary Stats")),
      p(HTML(paste0("<b>Total: </b>", round(total.percent, 2), "% ", header, "<br/>",
                    "<b>Marvel: </b>", round(marvel.percent, 2),"% ", header, "<br/>",
                    "<b>DC: </b>", round(dc.percent, 2),"% ", header, "<br/>"
      )))
    )
  })
}
# Creates server 
shinyServer(my.server)
  