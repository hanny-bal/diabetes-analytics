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
    
    # ------------------------
    #  Overview / AGP Report - 
    # ------------------------
    tabPanel('Overview',
       sidebarLayout(
         sidebarPanel(
           dateRangeInput('overview_date_range', 'Select a date range to analyze:'),
         ),
         mainPanel(
           column(6,
              h1('Overview')),
              div(
                style = 'margin: 20px',
                tags$ul(
                  tags$li(span('Average glucose: '), 
                     textOutput('average_glucose', inline = TRUE),
                     style = 'font-weight: bold'),
                  tags$li(span('Estimated A1C: '), 
                          textOutput('estimated_a1c', inline = TRUE)),
                  tags$li(span('Standard deviation: '), 
                    textOutput('standard_deviation', inline = TRUE)),
                  tags$li(span('Coefficient of variation: '), 
                          textOutput('coefficient_of_variation', inline = TRUE)),
                  style = 'font-size: 14pt'
                )
            ),
           column(6)
         )
       )),
    
    # ---------------------------
    #  Daily blood sugar values - 
    # ---------------------------
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
          dateRangeInput('pattern_date_range', 'Select a range to start pattern analysis:'),
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
