library(shiny)
library(plotly)
library(lubridate)
library(dplyr)
library(tsmp)
library(shinyjs)

# activate autoreload
options(shiny.autoreload = TRUE)

# read data 
# TODO: externalize this
df <- read.csv('../data/clarity_export_20220730.csv', sep = ';')
colnames(df) <- c('index', 'timestamp_src', 'event_type', 'event_subtype', 
                  'patient_info', 'device_info', 'source_device', 'glucose_mgdl',
                  'insulin_u', 'carbohydrates_g', 'duration', 'glucose_change_rate',
                  'emitter_time', 'emitter_id')

# get only measures and parse timestamp
df$glucose_mgdl <- as.numeric(df$glucose_mgdl)
df <- df %>% filter(event_type == 'EGV' & !is.na(glucose_mgdl))
df$timestamp <- ymd_hms(df$timestamp_src, tz = 'Europe/Vienna')
df$time <- hms::as_hms(format(df$timestamp, format = "%H:%M:%S"))

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    # --------------------
    #  Reactive elements - 
    # --------------------
    
    # the sample on which to apply out pattern recognition algorithm
    pattern_rec_sample <- reactive({
      df %>% filter(date(df$timestamp) >= input$pattern_date_range[1] & 
                      date(df$timestamp) <= input$pattern_date_range[2])
    })
    
    # intermediate result of the tsmp analysis
    tsmp_result <- reactive({
      df_sample <- pattern_rec_sample()
      if(nrow(df_sample) > 0) {
        analyze(df_sample$glucose_mgdl, windows = input$pattern_window_size) 
      }
    })

    # motif 1 and neighbours
    tsmp_motif_1 <- reactive({
      validate(
        need(!is.null(tsmp_result()), "Result must not be null."),
      )
      
      motifs <- motifs(tsmp_result())
      pattern_locations <- c(motifs$motif$motif_idx[[1]], motifs$motif$motif_neighbor[[1]])
      
      validate(
        need(!is.null(motifs), "Could not compute motifs."),
        need(!is.null(pattern_locations), "No samples found for this motif.")
      )
      
      # populate select list
      select_choices <- 1:length(pattern_locations)
      updateSelectInput(session, 'motif1_select', 'Pattern 1: Examples', choices = select_choices)
      
      return(pattern_locations)
    })
    
    # motif 2 and neighbours
    tsmp_motif_2 <- reactive({
      validate(
        need(!is.null(tsmp_result()), "Result must not be null."),
      )
      
      motifs <- motifs(tsmp_result())
      pattern_locations <- c(motifs$motif$motif_idx[[2]], motifs$motif$motif_neighbor[[2]])
      
      validate(
        need(!is.null(motifs), "Could not compute motifs."),
        need(!is.null(pattern_locations), "No samples found for this motif.")
      )
      
      # populate select list
      select_choices <- 1:length(pattern_locations)
      updateSelectInput(session, 'motif2_select', 'Pattern 2: Examples', choices = select_choices)
      
      return(pattern_locations)
    })
    
    # motif 3 and neighbours
    tsmp_motif_3 <- reactive({
      validate(
        need(!is.null(tsmp_result()), "Result must not be null."),
      )
      
      motifs <- motifs(tsmp_result())
      pattern_locations <- c(motifs$motif$motif_idx[[3]], motifs$motif$motif_neighbor[[3]])
      
      validate(
        need(!is.null(motifs), "Could not compute motifs."),
        need(!is.null(pattern_locations), "No samples found for this motif.")
      )
      
      # populate select list
      select_choices <- 1:length(pattern_locations)
      updateSelectInput(session, 'motif3_select', 'Pattern 3: Examples', choices = select_choices)
      
      return(pattern_locations)
    })

    
    # --------------------------------------------------
    #  Display daily time series of blood sugar values - 
    # --------------------------------------------------
    output$overviewTimeSeries <- renderPlot({
      if(exists('df') && is.data.frame(get('df'))) {
        # plot a sample day
        df_sample <- df %>% filter(date(df$timestamp) == input$daywise_ts)
        if(nrow(df_sample) > 0) {
          plot(df_sample$timestamp, df_sample$glucose_mgdl) 
        }
      }
    })
    
    # --------------------------------------------------
    #  Display daily time series of blood sugar values - 
    # --------------------------------------------------
    output$patternRecResult <- renderPlot({
      if(exists('df') && is.data.frame(get('df'))) {
        res <- tsmp_result()
        
        validate(
          need(!is.null(res), "Please select another time-frame.")
        )
        
        # visualize the result
        visualize(motifs(res))
      }
    })
    
    # ----------------------------------------------
    #  Detailed plots of individual patterns found - 
    # ----------------------------------------------
    
    # show samples of the first pattern
    output$motif1 <- renderPlot({
      pattern_locations <- tsmp_motif_1()
      
      # compute start and end index in the original data
      df_sample <- pattern_rec_sample()
      start_index <- pattern_locations[as.numeric(input$motif1_select)]
      end_index <- start_index + input$pattern_window_size
      
      validate(
        need(!is.null(start_index), "Could not compute start index."),
        need(!is.na(end_index), "Could not compute end index.")
      )
      
      # aaand plot
      plot(df_sample$timestamp[start_index:end_index], df_sample$glucose_mgdl[start_index:end_index])
    })
    
    # show samples of the second pattern
    output$motif2 <- renderPlot({
      pattern_locations <- tsmp_motif_2()
      
      # compute start and end index in the original data
      df_sample <- pattern_rec_sample()
      start_index <- pattern_locations[as.numeric(input$motif2_select)]
      end_index <- start_index + input$pattern_window_size
      
      validate(
        need(!is.null(start_index), "Could not compute start index."),
        need(!is.na(end_index), "Could not compute end index.")
      )
      
      # aaand plot
      plot(df_sample$timestamp[start_index:end_index], df_sample$glucose_mgdl[start_index:end_index])
    })
    
    # show samples of the third pattern
    output$motif3 <- renderPlot({
      pattern_locations <- tsmp_motif_3()
      
      # compute start and end index in the original data
      df_sample <- pattern_rec_sample()
      start_index <- pattern_locations[as.numeric(input$motif3_select)]
      end_index <- start_index + input$pattern_window_size
      
      validate(
        need(!is.null(start_index), "Could not compute start index."),
        need(!is.na(end_index), "Could not compute end index.")
      )
      
      plot(df_sample$timestamp[start_index:end_index], df_sample$glucose_mgdl[start_index:end_index])
    })
})
