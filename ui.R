# Add library
library(shinythemes)

########
# NOTE #
########
# The reference for the comprehensive comparison plot and widgets of the project comes from Shiny's
  # movie explorer example which can be found here: https://github.com/rstudio/shiny-examples/tree/master/051-movie-explorer from RStudio

my.ui <- fluidPage(
    theme = shinytheme("simplex"), 
    
#########
# INTRO #
#########
    # Heading for the beginning of the project
    fluidRow(align = "center",
     h2(strong("ANOTHER KIND OF LEAGUE")),
     h5("Exploring the queer composition of Marvel and DC leagues through data"),
     h6(em("by Megan Vo Bui, September 2017")),
     h4(strong("Introduction"))
    ),
    fluidRow(
     p("The purpose of this project is to examine the patterns of queer character inclusion in both Marvel and DC
       comics. Comics historically have been created, written, and aimed at primarily",
       tags$a(href="https://fivethirtyeight.com/features/women-in-comic-books/", " white men"), "according to research
       done by FiveThirtyEight's Walt Hickey using data scraped from ",
       tags$a(href="http://marvel.wikia.com/wiki/Marvel_Database", "Marvel wikia's database"), "and ",
       tags$a(href="http://dc.wikia.com/wiki/DC_Comics_Database", "DC wikia's database.")),
     p("The good news is that diversifying efforts for DC and Marvel comics have been on the uprise. 
        Characters like Kamala Khan (the new Ms. Marvel), and America Chavez (Miss America) have started to grace
        Marvel covers, and these characters have been coupled with more ",
        tags$a(href="http://www.huffingtonpost.com/entry/diversity-in-comics-whats-been-done-and-what-needs_us_5933b75fe4b0649fff211a07", "diverse writers"),
        " as well. Even with these steps to make the comic realm more inclusive, there is still more work to be done."),
     p("This project explores the break down of queer character inclusion from 1939 and 2014. It must be noted that the
       data come from the same data scraped by FiveThirtyEight in August 2014, which means it also comes from both Marvel and DC
       wikia databases. Since these databases are edited by fans, information might not be entirely accurate. Because of this
       caveat, this project is meant to be a fun, quick glimpse into the queer Marvel and DC realm. Moreover, the number of appearances
       and characters have both increased since 2014, which is not reflected here. You can access the data ",
       tags$a(href="https://github.com/fivethirtyeight/data/tree/master/comic-characters", " on GitHub")),
     p("I'd also like to note some changes I made to the csv files. I changed the 'SEX' category to the more accurate
       term of 'GENDER'. Furthermore, Black Widow's sexuality was originally listed as 'bisexual', but without any hard
       evidence and official statements, I decided to not count her in the pool of queer characters. Deadpool's sexuality
       was changed to 'pansexual' as it was stated by Tim Miller (director of Deadpool) in an", 
       tags$a(href="http://collider.com/deadpool-ryan-reynolds-tim-miller-interview/", " interview.")),
     p("Lastly, I'd like to acknowledge that in working on this project, content used from the databases mentioned above is
       allowed under the Creative Commons License, which you can read more about ",
       tags$a(href="http://marvel.wikia.com/wiki/Marvel_Database:Copyrights", " here."),
       " All characters either belong to the Marvel and DC publishing companies. Any images used are only for the purposes of this
       personal research project, and the artists, source, date, and image link are all attributed when shown.")
    ),
    
############
# DOT PLOT #
############
    # Beginning of PART 1 of project with dot plot
    fluidRow(align = "center",
      h3(strong("PART 1: ALL IN GOOD COMPANY")),
      h5(em("Breaking down the composition of queer MARVEL and DC characters from 1939 to 2014"))
    ),
    fluidRow(
      p(strong("Behind the Data: "), "The data shown only takes into account characters that are labeled as  
        gender or sexuality minorities ", strong("(shortened as 'GSM' from here on out)."), "There are 147 dots in total, 
        each representing a character that is a GSM."),
      p(strong("What Am I Viewing? "), "The x-axis represents the year the character first appeared while
        the y-axis represents the number of characters introduced that year. The characters are color coded and grouped
        together by the category chosen at the bottom right of the graph. You may view the graph according to publishing
        company as well, which can be changed via the checkboxes on the bottom left of the graph. More information about
        a character can be viewed by hovering over the dot to be examined. For optimal viewing, I recommend
        expanding your browsing window to avoid overcrowding of dots (if you haven't already).")
    ),
    
    # Creates the dot plot with hover options
    fluidRow(align = "center",
             h5(strong("Count of Queer Characters by Year of First Appearance")),
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
    fluidRow(
      p(strong("A closer look: "), textOutput("comp.analysis") )
    ),
    
##########
# TOP 10 #
##########
      fluidRow(align = "center", 
               h3(strong("PART 2: THE IN AND OUT")),
               h5(em("The top 5 queer characters from MARVEL and DC by number of appearances"))
      ),
      fluidRow(
        p(strong("Before You Get Started: "), "The characters below are ranked by number of appearances since their first year of appearance.
          Data for these number of appearances were scraped on September 2, 2014.
          Each character has an image that references the artists, date, source (volume, cover, etc.), and wikia link of that image. Use the
          left and right arrows to cycle through the character rankings and to view a different character profile.")
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
    fluidRow(
      p(strong("A closer look: "), "Overall, Marvel's top 5 GSM characters seem to have been
        introduced earlier on average and appear more often than DC's top 5 GSM characters. The average
        year of first appearance for the top 5 Marvel characters is roughly 1972, with the earliest year
        of introduction being 1949 (Loki) and the latest year for the top 5 being 1991 (Deadpool). This contrasts
        with DC's average introduction year (1981), with the Pied Piper appearing the earliest (1959) and The Question
        appearing the latest (1992). In fact, when this data were scraped, Marvel's #5 character profile, Mystique, 
        appeared the same number of times as DC's #1 character, John Constantine. Perhaps Marvel's GSM characters
        tend to be more popular or are cast into more prolific roles. The average number of appearances for Marvel's
        top 5 was about 567, which is 322 higher than DC's average of roughly 245."),

      p("Alignment of DC's top 5 characters tends to lean towards 'Good', while Marvel's characters show a little 
        more moral ambiguity with Hercules being the only 'Good' aligned character and the rest either 'Neutral' or 'Bad'.
        Marvel also showcases more bisexual characters in their top 5 whereas DC mainly has homosexual characters. 
        Moreover, Marvel's top 5 includes pansexual and genderfluid characters. This is similar to the findings in Part 1
        where Marvel showed a more diverse GSM community than DC (whose characters were either bisexual or homosexual).")
    ),
    
###############
# COMPARISONS #
###############
    fluidRow(align = "center", 
      h3(strong("PART 3: ALL TOGETHER")),
      h5(em("Comparing the compositions of gender and sexuality minorities and majorities throughout 1939-2014"))
    ),
    fluidRow(
      p(strong("Behind the Data: "), "The data include all characters scraped from the databases, with the exception of
        characters considered as 'entities' (not male, female, genderless, or genderfluid) which were excluded for the purposes
        of simplifying comparisons. The two comparisons drawn were between male/female characters and 
        GSM and non-GSM (heterosexual or cis-gender) characters to look at inclusion of these minorities over time."),
      p(strong("What Am I Viewing? "), " This visualization shows the inclusivity of female and GSM characters over time.
        The x-axis represents the first year of appearance and the y-axis represents the number of appearances each character
        has as of September 2014. Use the slider inputs to the right to examine a certain range of years or appearances. You may
        also type in a character's name to view a particular character in the search bar. This tool can also be used to view characters
        with particular substrings in their names. For example, typing 'M' will automatically result in showing the plots for characters
        with names containing a 'M' irrespective of case. You may also switch color encodings to compare male/female distribution
        or GSM/non-GSM distribution."),
      p("You may also ", strong("hover "), "over dots to view more information about a character. To examine crowded areas
        of the plot, you can also zoom by ", strong("brushing over the desired area and double clicking"), " on the selected area. To zoom
        out, you can ", strong("double click"), " on the graph, which will reset to the original viewing dimensions.")
    ),
    fluidRow(align = "center",
      column(9,
             h5(strong("Gender and Sexuality Minority Ratios Over Time"))       
      )
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
               sliderInput("appearances", "# of Appearances", min = 1, max = 4100, value = c(50, 1500)),
               sliderInput("years", "Year of First Appearance", min = 1939, max = 2014, value = c(1939, 2014)),
               textInput("character", "Character name contains (ex., Akihiro)"),
               tags$small(paste0(
                 "Note: Most characters are searchable under their real",
                 " name and not their alias."
               ))
             ),
             wellPanel(
               radioButtons("compare", label = "Compare by", choices = c("Males to Females", "GSM Status"),
                            selected = "Males to Females")
             )
      )
    ),
    fluidRow(align = "center", 
      p("Analysis")
    )
)

# Make UI from my.ui
shinyUI(my.ui)