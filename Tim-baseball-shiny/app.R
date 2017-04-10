#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# ---------- LOAD LIBRARIES ----------

library(tidyverse)
library(shiny)
library(readr)
library(reshape2)
library(gridExtra)
library(shinyjs)
library(scales)

# ---------- LOAD DATA ----------

# read in MLB data
teams <- read_csv('Teams.csv', col_names = TRUE)

# ---------- WRANGLE DATA ----------

# need to change the 2B and 3B colnames to be valid
colnames(teams)[names(teams) == "2B"] <- "doubles"
colnames(teams)[names(teams) == "3B"] <- "triples"

# now, we need to filter data to only include data from 1973 onward
# we do this by filtering by yearID

teamsData <- filter(teams, yearID >= 1973)

# I also need to create a calculated column for winning percentage
# which normalizes the wins/losses data to account for different
# numbers of games played in different seasons
# I'll also calculate/normalize the other statistical columns
# that I want to make available

teamsData <- mutate(teamsData, 
                    Winning_percentage = signif(W/G, 4),
                    Runs_per_game = signif(R/G, 3),
                    Hits_per_game = signif(H/G, 3),
                    HR_per_game = signif(HR/G, 3),
                    Stolen_bases_per_game = signif(SB/G, 3),
                    Runs_allowed_per_game = signif(RA/G, 3),
                    Run_margin_per_game = Runs_per_game - Runs_allowed_per_game,
                    Average_attendance = signif(attendance/Ghome, 5)
)

# next, I'm going to only select those columns 
# I'm interested in proceeding with
teamsData <- select(teamsData,
                    name,
                    yearID,
                    WSWin,
                    Winning_percentage,
                    Runs_per_game,
                    Hits_per_game,
                    HR_per_game,
                    Stolen_bases_per_game,
                    Runs_allowed_per_game,
                    ERA,
                    Run_margin_per_game,
                    FP,
                    Average_attendance)

# now, I'm going to change all NA values in WSWin to "N"
teamsData['WSWin'][is.na(teamsData['WSWin'])] <- "N"

# next, I need to resolve some team name changes,
# so I'm consolidating to only use the current team names

teamsData['name'][teamsData['name'] == "California Angels"] <- 
  "Los Angeles Angels"
teamsData['name'][teamsData['name'] == "Anaheim Angels"] <- 
  "Los Angeles Angels"
teamsData['name'][teamsData['name'] == "Los Angeles Angels of Anaheim"] <- 
  "Los Angeles Angels"
teamsData['name'][teamsData['name'] == "Florida Marlins"] <- 
  "Miami Marlins"
teamsData['name'][teamsData['name'] == "Montreal Expos"] <- 
  "Washington Nationals"
teamsData['name'][teamsData['name'] == "Tampa Bay Devil Rays"] <- 
  "Tampa Bay Rays"

# ***some visualization text encoding work:
 # create a column with team nickname, which I then adjust for a few
teamsData$nick <- lapply(strsplit(teamsData$name, "\\s+"), tail, n = 1L)
teamsData$nick[teamsData$nick == "Diamondbacks"] <- "D-backs"
teamsData$nick[teamsData$name == "Boston Red Sox"] <- "Red Sox"
teamsData$nick[teamsData$name == "Chicago White Sox"] <- "White Sox"
teamsData$nick[teamsData$name == "Toronto Blue Jays"] <- "Blue Jays"

 # create a new column with yearID and nick combined, with new line sep
 # unique for every row; will be used for the hue encoding on in-depth data
teamsData$year_nick <- paste(teamsData$yearID, teamsData$nick, sep = "\n")

 # I'm also adding a "size" column that I'll use for some interactivity
teamsData$size <- rep(5,nrow(teamsData))

 # finally, I'm changing a couple of variable names to be more user-friendly
names(teamsData)[names(teamsData) == "FP"] <- "Fielding_percentage"

# ---------- BUILD USEFUL VARIABLES ----------

# and now, we can build a list of teamChoices to use in the dropdown
teamChoices <- unique(teamsData['name'])
# and sort them alphabetically
teamChoices <- teamChoices[order(teamChoices$name),]

# and create max and min years variables from the data
minYear <- min(teamsData$yearID)
maxYear <- max(teamsData$yearID)

# and create max and min winPct variables from the data
minWin <- min(teamsData$Winning_percentage)
maxWin <- max(teamsData$Winning_percentage)

# set maximum variable number for in-depth plots
plots_max <- 4

# ---------- SHINY UI CODE ----------

# Define UI for the application
ui <- fluidPage(
   # include shinyjs
  useShinyjs(),
  
   # Application title
   # titlePanel("Major League Baseball team data comparison, 1973-2015"),
  fluidRow(
    column(width = 1),
    column(width = 10,
           h1("Investigate Major League Baseball team data from 1973-2015"),
           br()
           ),
    column(width = 1)
  ),
   fluidRow(
    # Sidebar with two inputs - team choice and years slider 
    column(width = 1
      ),
    column(width = 2,
      wellPanel(
        h4("Layer 1: Compare teams at high level"),
        p("Choose teams(s) and a yearly range to compare winning percentage over time."),
        selectizeInput(inputId = "teams",
                    label = "Select team(s) to display:",
                    choices = teamChoices$name,
                    selected = "Arizona Diamondbacks",
                    multiple = TRUE),
        sliderInput(inputId = "years",
                    label = "Choose years to display:",
                    dragRange = TRUE,
                    min = minYear,
                    max = maxYear,
                    value = c(minYear,maxYear),
                    step = 1,
                    sep = ""),
        p("Reset your plot point choices to re-enable Layer 1 controls."),
        actionButton("reset_input", "Reset Layer 1"),
        br(),
        br(),
        tags$div(class="header", checked=NA,
                 tags$p("By Tim Blankemeyer for INFX 562A at Univ. of Wash."),
                 tags$a(href="http://seanlahman.com/baseball-archive/statistics/", "Source Data:\nSeanLahman.com", target="_blank")
                 )
        )
      ),
    column(width = 8,
           div(
             style = "position:relative",
             # Show a plot of winning percentage by years
             plotOutput("linePlot",
                        width = "100%",
                        height = "560px",
                        clickOpts(id="linePlot_click"),
                        hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")),
             uiOutput("hover_info")
           )
      ),
    column(width = 1
      )
    ),
   fluidRow(
     column(width = 1),
     column(width = 10,
            hr()),
     column(width = 1)
   ),
   # show 4 plots based on selected points.
  # shinyjs::hidden(
  # div(id="barplots", 
       fluidRow(
         column(width = 1
            ),
         column(width = 2,
                wellPanel(
                  h4("Layer 2: Compare teams' in-depth stats"),
                  checkboxGroupInput("variables", 
                                     "Select a max of 4 statistics:",
                                     c("Winning percentage" = "Winning_percentage",
                                       "HR per game" = "HR_per_game",
                                       "Run margin per game" = "Run_margin_per_game",
                                       "Earned-run average (ERA)" = "ERA",
                                       "Runs per game" = "Runs_per_game",
                                       "Hits per game" = "Hits_per_game",
                                       "Stolen bases per game" = "Stolen_bases_per_game",
                                       "Runs allowed per game" = "Runs_allowed_per_game",
                                       "Average attendance" = "Average_attendance"),
                                     selected = c("Winning_percentage", 
                                                  "HR_per_game",
                                                  "Run_margin_per_game",
                                                  "ERA"))
                )
            ),
         column(width = 8,
            plotOutput("click_info")
            ),
         column(width = 1
            )
         )
       )
#    )
#   )

# ---------- SHINY SERVER CODE ----------

# Define server logic for the app
server <- function(input, output, session) {
  # store selected teams in reactive data element
    ip <- reactive({
      filter(teamsData,
             name %in% input$teams,
             yearID >= input$years[1] & yearID <= input$years[2])
    })
    
    # To show a kind of "highlight" on clicked rows
    # Via: http://shiny.rstudio.com/gallery/plot-interaction-exclude.html
    # For storing which rows have been excluded
    vals <- reactiveValues(
      keeprows = rep(TRUE, nrow(isolate(ip())))
    )

  # output the rendered line plot, using reactive team data from inputs
  output$linePlot <- renderPlot({
    
    req(length(ip()) >= 1)
    
    # Plot the kept and excluded points as two separate data sets
    keep    <- ip()[ vals$keeprows, , drop = FALSE]
    exclude <- ip()[!vals$keeprows, , drop = FALSE]
    
    p <- ggplot(ip(), aes(yearID, Winning_percentage, group=name)) +
      geom_line(aes(color = name), size = 1.2) +
      geom_point(aes(fill=WSWin), 
                 pch = 21, 
                 color ="transparent",
                 size = 5,
                 alpha = 0.7) +
      geom_point(data = exclude,
                 aes(fill=WSWin),
                 pch = 21, 
                 color = "transparent",
                 size = 10,
                 alpha = 1) +
      scale_fill_manual(breaks = "Y",
                        labels = "Won World Series",
                        values = c("Y" = "red", "N" = "black")) +
      scale_size_identity() +
      guides(fill = guide_legend(override.aes = list(size=5))) +
      guides(color = guide_legend(override.aes = list(size=2))) +
      scale_colour_hue(l=70, c=100) +
      scale_x_continuous(breaks = seq(from=input$years[1],to=round(input$years[2],0),by=2)) +
      geom_hline(yintercept=0.5) +
      theme(legend.position = "bottom", 
            legend.box = "horizontal",
            legend.title = element_blank(),
            legend.text = element_text(size = 14),
            panel.background = element_rect(fill = "#f0f0f0"),
            panel.grid.major = element_line(color = '#d9d9d9'),
            plot.title = element_text(size = 16, face = 'bold', hjust = 0),
            axis.text = element_text(size = 13),
            axis.title.x = element_text(size = 16, margin=margin(20,0,0,0)),
            axis.title.y = element_text(size = 16, margin=margin(0,20,0,0))) +
      labs(title = "Select or deselect plot points to compare in-depth stats in Layer 2 (Note: This will freeze Layer 1 inputs)", 
            x = "Year", 
            y = "Winning Percentage")
    p
  })
  
  # Toggle the points that are clicked
  observeEvent(input$linePlot_click, {
    shinyjs::disable('teams')
    shinyjs::disable('years')
    res <- nearPoints(ip(), input$linePlot_click, allRows = TRUE, maxpoints = 1, threshold = 15)
    
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  # adding hover tooltip
  # Much help from: https://gitlab.com/snippets/16220
  output$hover_info <- renderUI({
    req(input$plot_hover)
    hover <- input$plot_hover
    point <- nearPoints(ip(), hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      class = "well-sm",
      tags$b("Team:"), point$nick, br(),
      tags$b("Year:"), point$yearID, br(),
      tags$b("Win Pct.:"), point$Winning_percentage, br()
    )
  })
  
  # for in-depth data:
  # save clicked items in reactiveValues object
  tc <- reactiveValues(selectedData = NULL)
  
  # function that adds/deletes clicked points
  observeEvent(input$linePlot_click, {
    X1 <- nearPoints(ip(), input$linePlot_click, maxpoints = 1, threshold = 15)
    
    if (is.null(tc$selectedData)) {
      tc$selectedData <- X1
    } else {
      if (nrow(merge(X1, tc$selectedData)) > 0) {
        ind <- anyDuplicated(rbind(tc$selectedData, X1), fromLast=TRUE)
        tc$selectedData <- tc$selectedData[-ind,]
      } else {
        tc$selectedData <- rbind(tc$selectedData, X1)
      }
    }
  })
  
  # function that resets the selected data points
  observeEvent(input$reset_input, {
    tc$selectedData = NULL
    vals$keeprows <- rep(TRUE, nrow(ip()))
    shinyjs::enable('teams')
    shinyjs::enable('years')
  })
  
  # output all of the clicked data points (or unclicked, as it were)
  
    output$click_info <- renderPlot({
      # require the input to exist
      req(!is.na(tc$selectedData['year_nick']))
      # require that the input variables length is max 4
      req(length(input$variables) < 5, cancelOutput = TRUE)
      
      # validate the selection of at least one input variable
      validate(
        need(!is.null(input$variables), 
             "Please select at least one statistic to investigate.")
      )
      
      # save both items of reactive data to access in ggplot
      toPlot <- tc$selectedData
      
      # storage for plots
      plots <- list()
      
      # loop through the input variables to produce 4 plots
      for (i in 1:length(input$variables)) {
        plots[[paste0("e", i)]] <- ggplot(toPlot, aes_string(x="year_nick", y=input$variables[i])) +
          geom_bar(aes(fill=as.factor(year_nick)), stat = "identity", position=position_dodge(), color="black") +
          scale_fill_manual(values = c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69','#fccde5','#d9d9d9','#bc80bd','#ccebc5','#ffed6f')) +
          theme(legend.position="none") +
          theme(axis.text.x = element_text(size = 13, angle = 45, vjust = 1, hjust=1),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                plot.title = element_text(size = 14)) +
          labs(title = gsub("_", " ", as.character(input$variables[i])))
      }
      
      # use grid.arrange to plot - the do.call allows for variable amounts
      do.call("grid.arrange", c(plots, ncol=4))
      
  })
    
    # function to keep us at maximum 4 selected variables
    observe({
      if(length(input$variables) > plots_max)
      {
        updateCheckboxGroupInput(session, "variables", selected = tail(input$variables, plots_max))
      }
    })
}

# ---------- SHINY APP LINKAGE ----------

# Run the application 
shinyApp(ui = ui, server = server)

