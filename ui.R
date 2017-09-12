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
    column(10, offset = 2,
           h2(strong("ANOTHER KIND OF LEAGUE")),
           h5("Exploring the queer composition of Marvel and DC leagues through data"),
           h6(em("by Megan Vo Bui, September 2017"))     
    )
  ),

  # Create a table of contents navigation for the user
  navlistPanel(widths = c(2, 10),
    tabPanel("Introduction", 
      fluidRow(align = "center",
               h4(strong("Introduction"))
      ),
      fluidRow(
        column(width = 10, offset = 1,
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
               p("This project explores the break down of queer character inclusion from 1940 and 2014. It must be noted that the
                 data come from the same data scraped by FiveThirtyEight in August 2014, which means it also comes from both Marvel and DC
                 wikia databases. Since these databases are edited by fans, information might not be entirely accurate. Because of this
                 caveat, this project is meant to be a fun, quick glimpse into the queer Marvel and DC realm. Moreover, the number of appearances
                 and characters have both increased since 2014, which is not reflected here. You can access the data ",
                 tags$a(href="https://github.com/fivethirtyeight/data/tree/master/comic-characters", " on GitHub")),
               p("I'd also like to note some changes I made to the csv files. I changed the 'SEX' category to the more accurate
                 term of 'GENDER'. Subsequently, I have also changed 'male'/'female' terminology to 'man'/'woman'. However, I have retained
                 the category 'SEX' to compare male/female character ratios in Part 3. In addition to this change in terminology, I have
                 swapped the word 'homosexual' to gay/lesbian as it may hold negative and derogatory weight for gay or queer identifying individuals.
                 Furthermore, Black Widow's sexuality was originally listed as 'bisexual', but without any hard
                 evidence and official statements, I decided to not count her in the pool of queer characters. Deadpool's sexuality
                 was changed to 'pansexual' as stated by Tim Miller (director of 'Deadpool') in an", 
                 tags$a(href="http://collider.com/deadpool-ryan-reynolds-tim-miller-interview/", " interview.")),
               p("Lastly, I'd like to acknowledge that in working on this project, text-based content used from the databases mentioned above is
                 allowed under the Creative Commons License, which you can read more about ",
                 tags$a(href="http://marvel.wikia.com/wiki/Marvel_Database:Copyrights", " here."),
                 " All characters either belong to the Marvel and DC publishing companies. Any images used are only for the purposes of this
                 personal research project, and the artists, source, date, and image link are all attributed when shown.")
        )
      )
    ),
    
    tabPanel("Before You Start"),
    
############
# DOT PLOT #
############
    # Beginning of PART 1 of project with dot plot
    tabPanel("Part 1: Who's in the League?",
      fluidRow(align = "center",
        h3(strong("PART 1: WHO'S IN THE LEAGUE?")),
        h5(em("Breaking down the composition of queer MARVEL and DC characters from 1940 to 2014"))
      ),
      fluidRow(
        column(width = 10, offset = 1,
               p(strong("Behind the Data: "), "The data shown only takes into account characters that are labeled as  
                 gender or sexuality minorities ", strong("(shortened as 'GSM' from here on out)."), "There are 147 dots in total, 
                 each representing a character that is a GSM."),
               p(strong("What Am I Viewing? "), "The x-axis represents the year the character first appeared while
                 the y-axis represents the number of characters introduced that year. The characters are color coded and grouped
                 together by the category chosen at the bottom right of the graph. You may view the graph according to publishing
                 company as well, which can be changed via the checkboxes on the bottom left of the graph. More information about
                 a character can be viewed by hovering over the dot to be examined. For optimal viewing, I recommend
                 expanding your browsing window to avoid overcrowding of dots (if you haven't already).")
        )
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
        column(width = 10, offset = 1,
               p(strong("Breakdown: "), textOutput("comp.analysis")),
               p(strong("Overall Distribution: "), "Looking at the overall graph, the distribution (regardless of color codings) seems to be skewed left toward earlier years. It might be a point
                 of interest that the year with the most GSM characters introduced overall is 2003, with both Marvel and DC
                 producing the most (or tied for the most) number of queer characters that year compared to other years. Marvel seems to have
                 a more pronounced skew than DC, and has consequently included more queer characters from 2000 onwards in terms of raw character count.
                 Marvel's median year of introduction is 2001. Contrastingly, DC's median year is between 1993 and 1994, with many of their queer-identifying
                 characters having been introduced in the 1980s and 1990s."),
               
               p(strong("Keep In Mind: "), "Although it may be inferred
                 that Marvel has introduced more GSM characters, it must be acknowledged that these characters may not have identified as queer upon introduction
                 In fact, from 1954 to 1989, due to the", tags$a(href="http://cbldf.org/the-comics-code-of-1954/", "Comics Code Authority's "),  "requirements, characters were", 
                 tags$a(href="http://www.history.com/news/how-the-code-authority-kept-lgbt-characters-out-of-comics", " pressured to not
                 be introduced or identify as queer"), " (Kistler 2017). Therefore, it may be more accurate to say that Marvel has been more diverse in queer character representation
                 overall in terms of raw character count (percentage-wise is a different matter and is explored in Part 3). Still, this does not fully illustrate the issue, as it may be the case that Marvel has more queer characters or
                 character leads, but that does not mean that it has more writers that identify as such behind those comics. For example, ",
                 tags$a(href="http://comicsalliance.com/marvel-dc-female-audience-female-characters-female-creators/", "Wheeler's article "), "points
                 out that Marvel had more female lead characters, but DC had more female writers working on their female driven comics (2014). Because of this, the
                 visualization only tells a half story that is strictly quantitative. The quality and representation in those stories and characters
                 are separate battles from these numbers, which only scratch the surface.")
        )
      )
    ),
    
##########
# TOP 10 #
##########
    tabPanel("Part 2: The Top 10",
        fluidRow(align = "center", 
                 h3(strong("PART 2: The Top 10")),
                 h5(em("Exploring the 5 most popular character profiles from both Marvel and DC"))
        ),
  
        
        fluidRow(
          column(width = 10, offset = 1,
                 p(strong("Before You Get Started: "), "The characters below are ranked by number of appearances since their first year of appearance.
                   Data for these number of appearances were scraped on September 2, 2014.
                   Each character has an image that references the artists, date, source (volume, cover, etc.), and wikia link of that image. Use the
                   left and right arrows to cycle through the character rankings and to view different character profiles.")
          )
        ),
        fluidRow(
          column(6, align = "center", # Columns go up til 12 so this goes half way
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
        column(width = 10, offset = 1,
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
                 Marvel also showcases more bisexual characters in their top 5 whereas DC mainly has gay or lesbian characters. 
                 Moreover, Marvel's top 5 includes pansexual and genderfluid characters. This is similar to the findings in Part 1
                 where Marvel showed a more diverse GSM community than DC (whose characters were either bisexual or gay/lesbian).")      
        )
      )
  ),
    
###############
# COMPARISONS #
###############
    tabPanel("Part 3: Revisualizing Representation",
      fluidRow(align = "center", 
        h3(strong("PART 3: REVISUALIZING REPRESENTATION")),
        h5(em("Comparing the ratios of sex and gender and sexuality minorities and majorities throughout 1939-2014"))
      ),
      fluidRow(
        column(width = 10, offset = 1,
               p(strong("Behind the Data: "), "The data include all characters scraped from the databases, with the exception of
                 characters considered as 'entities' (not male, female, genderless, or genderfluid) which were excluded for the purposes
                 of simplifying comparisons. The two comparisons drawn were between male/female characters and 
                 GSM and non-GSM (heterosexual or cisgender) characters to look at inclusion of minorities over time."),
               p(strong("What Am I Viewing? "), " This visualization shows the inclusivity of female and GSM characters over time.
                 The x-axis represents the first year of appearance and the y-axis represents the the number of character appearances as of September 2014. Use the slider inputs to the right to examine a certain range of years or appearances. You may
                 also type in a character's name to view a particular character in the search bar. This tool can further be used to view characters
                 with particular substrings in their names. For example, typing 'M' will automatically result in showing the plots for characters
                 with names containing a 'M' irrespective of case. You may switch color encodings to compare male/female distribution
                 or GSM/non-GSM distribution."),
               p("In addition, you can ", strong("hover "), "over dots to view more information about a character. To examine crowded areas
                 of the plot, you can also zoom by ", strong("brushing over the desired area and double clicking"), " on the selected area. To zoom
                 out, you can ", strong("double click"), " on the graph, which will reset to the original viewing dimensions.")
        )
      ),
      fluidRow(align = "center",
        column(9,
               h5(strong("Sex, Gender and Sexuality Minority Ratios Over Time"))       
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
                 sliderInput("appearances", "# of Appearances", min = 1, max = 4043, value = c(50, 1500)),
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
      fluidRow(
        column(width = 10, offset = 1,
              p(strong("A closer look at female/male ratios: "), "When looking at the overall timeline, male characters make up just about 3/4 of the
                Marvel and DC universes combined. DC seems to have approximately a 5% lead on Marvel in terms of female-to-male
                character ratio. The percentages of female characters show only a small increase (reaching to just about a third) when looking at characters introduced
                after 2000. This illustrates that nearly every year, more male characters are introduced than female, even with
                the increasing attention on the lack of female representation in comics. Roughly a fourth of characters appearing more than 1000 times in both
                companies are female, with the highest number of female appearances being 1713. Characters that appear more times than that are all 
                male."),
              p("However, when looking at characters introduced from 2000 to 2014, the top three characters by number of appearances are female.
                On top of that, of the characters that appear more than 100 times in that timeframe, roughly 50% are female. So while the introduction
                of female characters has been slow to increase, it can be inferred that female characters are gaining visibility through a higher
                increase in number of appearances. Granted, the number of appearances does not speak to ", strong("how"), " those females are
                being represented in comics -- only that they have appeared more often."),
              p(strong("A closer look at GSM ratios:"), " When taking a look at all characters created since 1940, 1% or less of characters for both companies
                       (separately and combined) are considered gender and/or sexuality minorities, with DC having higher raw numbers. 
                       Those numbers increase slightly past 1% when entering into the 2000s, and similar to findings on Part 1, reach a peak in 2003
                       where 2.88% of Marvel characters introduced were GSM and 5.05% of DC characters were GSM. "),
              p("")
        )
      )
    ),
    tabPanel("Part 4: Future Discussions",
      fluidRow(align = "center", 
               h3(strong("PART 4: FUTURE DISCUSSIONS"))
      ),
      fluidRow(
          column(width = 10, offset = 1,
                 p(strong("Caveats: ")),
                 p(strong("A closer look at GSM ratios: "))      
          )
      )
    ),
    tabPanel("References",
      fluidRow(align = "center",
               h4(strong("References"))
      )
    )
  )
)

# Make UI from my.ui
shinyUI(my.ui)