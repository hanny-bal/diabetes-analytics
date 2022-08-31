library(shiny)
library(shinyjs)

# Define UI
shinyUI(
  navbarPage(
    "diAnalytics",
    useShinyjs(),
    
    # --------------------------------
    #  Daily overview of blood sugar - 
    # --------------------------------
    tabPanel("Daily",
     sidebarLayout(
       sidebarPanel(
         dateInput('daywise_ts','Select a day to display'),
       ),
       # Show a plot of the generated distribution
       mainPanel(
         plotOutput("overviewTimeSeries"),
       )
     )
    ),
    
    # ----------------------
    #  Pattern Recognition - 
    # ----------------------
    tabPanel("Pattern Recognition",
      sidebarLayout(
        sidebarPanel(
          dateRangeInput('pattern_date_range', 'Select a range to start pattern analysis',
                    start = '2022-07-01', end = '2022-07-14'),
          sliderInput('pattern_window_size', 'How many data points would consider a pattern?',  
                      value = 40, min = 10, max = 80),
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("patternRecResult"),
        )
      ),
      
      # display the individual patterns in more detail
      fluidRow(
        column(4,
          id = 'pattern-details-1',
          selectInput('motif1_select', 'Pattern 1: Examples', choices=c()),
          plotOutput('motif1')
        ),
        column(4,
          id = 'pattern-details-2',
          selectInput('motif2_select', 'Pattern 2: Examples', choices=c()),
          plotOutput('motif2')
        ),
        column(4,
          id = 'pattern-details-3',
          selectInput('motif3_select', 'Pattern 3: Examples', choices=c()),
          plotOutput('motif3')
        ),
      )
    )
  )
)
