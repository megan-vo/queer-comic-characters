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
           h5("Exploring the queer and gender composition of Marvel and DC leagues through data"),
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
               p(strong("Batwoman, Deadpool, Loki, Mystique."), " All complex characters in their own right. We could dissect, categorize,
                 and analyze their personas in many different ways. However, this project
                 is going to zoom in on one particular aspect these characters share: they're queer. And they're not the only ones that are."),
               p("The main purpose of this project is to examine the patterns of gender and sexuality minority inclusion in both Marvel and DC
               comics. Comics historically have been created, written, and aimed at primarily white men according to",
                 tags$a(href="https://fivethirtyeight.com/features/women-in-comic-books/", " research"),  
                 " done by FiveThirtyEight's Walt Hickey (2014). But by looking quantitatively into characters
                 that diverge from the norm, we can catch a glimpse at what other stories are being told throughout the years."),
               p("The good news is that diversifying efforts for DC and Marvel comics have been generally on the uprise. 
                 Famous superheroes are being recasted as different ethnicities, while some characters are being reintroduced with more nuanced
                 sexualities (i.e. ", tags$a(href="http://www.comicosity.com/exclusive-interview-greg-rucka-on-queer-narrative-and-wonder-woman/", "Wonder Woman"), 
                 ") (Santori 2016). Furthermore, these characters have been coupled with more ",
                 tags$a(href="http://www.huffingtonpost.com/entry/diversity-in-comics-whats-been-done-and-what-needs_us_5933b75fe4b0649fff211a07", "diverse writers"),
                 " as well (Dern 2017). Even with these steps to make the comic realm more inclusive, there is still more work to be done."),
              p("So whether you are queer, interested in diversity in comics, or enjoy looking at data for fun like me (or a combination of the three),
                this project will hopefully shed a few insights and lead to even more curiosity and exploration.")
        )
      )
    ),
    
    tabPanel("Before You Start",
             fluidRow(align = "center",
                      h4(strong("Before You Start")),
                      h5(em("Prior to diving into the interesting content, let's lay out some foundations of the project."))
             ),
             p(strong("Data: "), "You can access the data ",
               tags$a(href="https://github.com/fivethirtyeight/data/tree/master/comic-characters", " on GitHub."),
             "Here are the links to ", tags$a(href="http://marvel.wikia.com/wiki/Marvel_Database", "Marvel wikia's database"), "and ",
             tags$a(href="http://dc.wikia.com/wiki/DC_Comics_Database", "DC wikia's database ")), 
             
             p(strong("Caveats: "), "This project explores the break down of queer character inclusion from 1940 and 2014. It must be noted that the
               data come from the same data scraped by FiveThirtyEight in August 2014, which means it also comes from both Marvel and DC
               wikia databases. Since these databases are edited by fans, information might not be entirely accurate. Because of this
               caveat, this project is meant to be a fun, quick glimpse into the queer Marvel and DC realm. Moreover, the number of appearances
               and characters have both increased since 2014, which is not reflected here. "),
             
             p("Another thing to keep in mind is that the data were scraped from one continuous universe from each publisher to avoid
               duplications of the same character. That means characters are either from Earth-616 (Marvel) or New Earth (DC), with the New 52
               characters from DC Comics being excluded. You can read more about it within the 7th note in Hickey's", tags$a(href="https://fivethirtyeight.com/features/women-in-comic-books/", " article"),
               " mentioned in the introduction."),

             p(strong("Changes: "), "I'd also like to note some changes I made to the csv files. I changed the 'SEX' category to the more accurate
               term of 'GENDER'. Subsequently, I have also changed 'male'/'female' terminology to 'man'/'woman'. However, I have retained
               the category 'SEX' to compare male/female character ratios in Part 3. In addition to this change in terminology, I have
               swapped the word 'homosexual' to gay/lesbian as it may hold negative and derogatory weight for gay or queer identifying individuals."),
             
             p("Furthermore, I had to omit some top 5 characters from the pool of queer characters (i.e. Hercules and Black Widow) due to either a lack of hard evidence
               or because their sexuality was only addressed in an alternate universe. Deadpool's sexuality
               was changed to 'pansexual' as stated by Tim Miller (director of 'Deadpool') in an", 
               tags$a(href="http://collider.com/deadpool-ryan-reynolds-tim-miller-interview/", " interview."), " DC's Kate Godwin was also added as a GSM character."),
             p(strong("Images and Content: "), "Lastly, I'd like to acknowledge that in working on this project, text-based content used from the databases mentioned above is
               allowed under the Creative Commons License, which you can read more about ",
               tags$a(href="http://marvel.wikia.com/wiki/Marvel_Database:Copyrights", " here."),
               " All characters either belong to the Marvel and DC publishing companies. Any images used are only for the purposes of this
               personal research project, and the artists, source, date, and image link are all attributed when shown.")
    ),
    
############
# DOT PLOT #
############
    # Beginning of PART 1 of project with dot plot
    tabPanel("Part 1: Who's in the League?",
      fluidRow(align = "center",
        h3(strong("PART 1: WHO'S IN THE LEAGUE?")),
        h5(em("Breaking down the composition of queer and gender non-conforming MARVEL and DC characters from 1940 to 2014"))
      ),
      fluidRow(
        column(width = 10, offset = 1,
               p(strong("Behind the Data: "), "The data shown only takes into account characters that are labeled as  
                 gender or sexuality minorities ", strong("(shortened as 'GSM' from here on out)."), "There are 147 dots in total, 
                 each representing a character that is a GSM."),
               p(strong("What Am I Viewing? "), "The ", em("x-axis represents the year"), " the character first appeared while
                 the ", em(" y-axis represents the number of characters"), " introduced that year. The characters are color coded and grouped
                 together by the category chosen at the bottom right of the graph. You may view the graph according to publishing
                 company as well, which can be changed via the checkboxes on the bottom left of the graph. More information about
                 a character can be viewed by ", strong(" hovering "), " over the dot to be examined. For optimal viewing, I recommend
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
               p(strong("Categorical Breakdown: "), textOutput("comp.analysis")),
               p(strong("Overall Distribution: "), "Looking at the overall graph, the distribution (regardless of color codings) seems to be skewed left toward earlier years. It might be a point
                 of interest that the year with the most GSM characters introduced overall is 2003, with both Marvel and DC
                 producing the most (or tied for the most) number of queer characters that year compared to other years. Marvel seems to have
                 a more pronounced skew than DC, and has consequently included more queer characters from 2000 onwards in terms of raw character count.
                 Marvel's median year of introduction is 2001. Contrastingly, DC's median year is between 1993 and 1994, with many of their queer-identifying
                 characters having been introduced in the 1980s and 1990s."),
               
               p(strong("Keep In Mind: "), "Although it may be inferred
                 that Marvel has introduced more GSM characters, it must be acknowledged that these characters may not have identified as queer upon introduction.
                 In fact, from 1954 to 1989, due to the", tags$a(href="http://cbldf.org/the-comics-code-of-1954/", "Comics Code Authority's "),  "requirements, characters were", 
                 tags$a(href="http://www.history.com/news/how-the-code-authority-kept-lgbt-characters-out-of-comics", " pressured to not
                 be introduced or identify as queer"), " (Kistler 2017). Therefore, it may be more accurate to say that Marvel has been more diverse in queer character representation
                 overall in terms of raw character count (percentage-wise is a different matter and is explored in Part 3). Still, this does not fully illustrate the issue, as it may be the case that Marvel has more queer characters or
                 character leads, but that does not mean that it has more writers that identify as such behind those comics. For example, ",
                 tags$a(href="http://comicsalliance.com/marvel-dc-female-audience-female-characters-female-creators/", "Wheeler's article "), "points
                 out that Marvel had more female lead characters, but DC had more female writers working on their female driven comics (2014). Because of this, the
                 visualization only tells a partial story that is strictly quantitative. The quality and representation in those stories and characters
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
                 h5(em("The 5 most popular character profiles from both Marvel and DC"))
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
                 uiOutput("marvel_top_info"),
                 uiOutput("marvel_artists")
          ),
          column(6, align = "center",
                 uiOutput("dc_top_info") ,   
                 uiOutput("dc_artists")
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
                 of simplifying comparisons. Note that some characters are missing due to lack of appearance data. The two comparisons drawn were between male/female characters and 
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
        column(width = 12, 
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
                       (separately and combined) are considered gender and/or sexuality minorities, with DC having higher percentages. 
                       Those numbers increase slightly past 1% when entering into the 2000s, and similar to findings on Part 1, reach a peak in 2003
                       where 2.88% of Marvel characters introduced were GSM and 5.05% of DC characters were GSM. Moreover, while Marvel has a higher
                       raw count of GSM characters, DC seems to have a higher ratio of queer character inclusion. In terms of appearances,
                       however, Marvel has a higher percentage of queer characters that appear 18 times or more, and the gap between the two
                       percentages generally widens as the number of appearances increase.")
        )
      )
    ),
    tabPanel("Part 4: Future Discussions",
      fluidRow(align = "center", 
               h3(strong("PART 4: FUTURE DISCUSSIONS")),
               h5(em("Interested in knowing more? There are many more bases to cover, so here are the next steps."))
      ),
      fluidRow(
          column(width = 10, offset = 1,
                 p(strong("How Can You Take This Further? "), "As with most projects, there are a lot of areas that this project
                   leaves unexplored. Since this project is not definitive, there is a lot of room to look into."),
                 p("Perhaps one of the biggest areas that this project does not fully address is the exact time/year that the character
                   is introduced as queer. Because the data primarily contain the year the character was first created and many characters do not come out
                   until much later (ex. Catwoman and Iceman), it might be worth looking into the trends and progression of the years characters begin to", strong(em(" identify ")),
                   "as queer. That may paint a more accurate depiction of gender and sexuality minority inclusion in DC and Marvel comics. As with most data sets, this data are
                   also imperfect and not always 100% consistent."),
                 p("Another place for improvement is that the data are about 3 years old. A lot of new characters have been introduced since then and
                   existing characters have appeared more and have been recasted too. A good question to consider is how we have progressed since 2014 (if at all)."),
                 p("Moreover, since this project only covers characters from one DC and one Marvel universe, it might be interesting to look into other universes
                   and perhaps compare how queer characters are being portrayed. Some characters identify differently in other universes."),
                 p("As mentioned before, this project only looks at the nature of queer and gender representation in Marvel and DC comics quantitatively. To understand fully
                   how these groups are being represented, we also must look at their depictions and appearances at a qualitative level (which means actually ", 
                   strong(em("reading ")), "the comics for the way characters are portrayed)."),
                 p("Finally, because this project was meant to be a fun, surface introduction, there is much more room for a more in-depth, academic
                   analysis and research into the topics explored here. I would also like to acknowledge that I do not know the Marvel and DC realms as extensively as many others do.
                   I encourage those that do know more to go further. Starting with one of the issues mentioned above is a good step forward."),
                 p(strong("Possible Topics of Exploration: "),
                   tags$ul(
                     tags$li("Looking at the demographics of the creators and writers of comic books and lead characters"),
                     tags$li("Exploring more qualitatively at how queer characters are being portrayed in comic books. Is inclusion
                             of queer characters being treated more as an 'arms race' or are their stories being accurately and well told?"),
                     tags$li("Examining the differences of how these minority characters are being portrayed in the books vs. film/TV adaptations"),
                     tags$li("Branching out and researching into other comic publishers. This could also be extended to how
                             queer characters are being represented internationally. How do they compare?"),
                     tags$li("Looking into how characters identify themselves. Because gender and sexuality is complicated, it might be hard to discern
                             what characters identify as (ex. as a trans woman or as a woman). This would require a qualitative analysis as well"),
                     tags$li("Analyzing the depiction of queer characters of color")
                   )
                 ),
                 p(strong("Some Articles to Get You Thinking: ")),
                 fluidRow(
                   column(6,
                        p(tags$ul(
                            tags$li(tags$a(href="https://www.wired.com/2015/07/diversity-in-comics/", "Racial Diversity in Comics and Comic Creators")),
                            tags$li(tags$a(href="https://www.dailydot.com/parsec/marvel-lgbt-characters-black-writers-controversy/", "Marvel, Gender, Race, and LGBTQ Issues")),
                            tags$li(tags$a(href="https://www.nytimes.com/2015/12/24/fashion/coming-out-as-gay-superheroes.html", "NYTimes Gay Superheroes")))
                          
                        )   
                   ),
                   column(6,
                        p(tags$ul(
                            tags$li(tags$a(href="https://www.theatlantic.com/entertainment/archive/2013/10/why-we-worry-about-dc-comicss-gay-characters/310076/", "DC and Gay Representation")),
                            tags$li(tags$a(href="http://www.nationalgeographic.com/magazine/2017/06/explore-minorities-in-comics/", "Exploring Minorities in Comics (With Brief Character Spotlights)")))
                        )   
                   )
                )
          )
      )
    ),
    tabPanel("References",
      fluidRow(align = "center",
               h4(strong("References")),
               column(10, offset = 1,
               h5(em("Special thanks to Jaelien Pinheiro for editing and guiding me through the complexities of
                     gender, sexuality and related terminology for this project. She is currently studying to become a Doctor of Psychology 
                     in Clinical Psychology (Psy.D.) at The Wright Institute.")))
      ),
      fluidRow(
        column(6,
               img(src='References1.png', width = "100%", height = 600)
        ),
        column(6,
               img(src='References2.png', width = "100%", height = 600)       
        )
      )
    )
  )
)

# Make UI from my.ui
shinyUI(my.ui)