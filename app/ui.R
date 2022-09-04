library(shiny)
library(shinyjs)
library(plotly)
library(bslib)

# Define UI
shinyUI(
  navbarPage(
    "diAnalytics",
    useShinyjs(),
    theme = bs_theme(version = 4, bootswatch = "journal", primary = 'rgb(205, 12, 24)'),
    
    # --------------------------------
    #  Daily overview of blood sugar - 
    # --------------------------------
    tabPanel("Daily",
     sidebarLayout(
       sidebarPanel(
         dateInput('daywise_ts','Select a day to display:'),
       ),
       # Show a plot of the generated distribution
       mainPanel(
         plotlyOutput("dailyTimeSeries"),
       )
     )
    ),
    
    # ----------------------
    #  Pattern Recognition - 
    # ----------------------
    tabPanel("Pattern Recognition",
      sidebarLayout(
        sidebarPanel(
          dateRangeInput('pattern_date_range', 'Select a range to start pattern analysis:',
                    start = '2022-07-01', end = '2022-07-14'),
          sliderInput('pattern_window_size', 'How many data points would consider a pattern?',  
                      value = 40, min = 10, max = 80),
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          plotlyOutput('patternRecResult', height = 240),
          plotlyOutput('patternOverview', height = 300)
        )
      ),
      
      # display the individual patterns in more detail
      fluidRow(
        column(12,
               style = 'margin-bottom: 20px',
               align = 'center',
               h2('Details'))
      ),
      fluidRow(
        style = 'margin-left: 40px; margin-right: 40px',
        align = 'center',
        column(4,
          align = 'center',
          id = 'pattern-details-1',
          selectInput('motif1_select', 'Pattern 1: Examples', choices=c()),
          plotlyOutput('motif1')
        ),
        column(4,
          id = 'pattern-details-2',
          selectInput('motif2_select', 'Pattern 2: Examples', choices=c()),
          plotlyOutput('motif2')
        ),
        column(4,
          id = 'pattern-details-3',
          selectInput('motif3_select', 'Pattern 3: Examples', choices=c()),
          plotlyOutput('motif3')
        ),
      )
    )
  )
)
