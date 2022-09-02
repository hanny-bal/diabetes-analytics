library(shiny)
library(plotly)
library(lubridate)
library(dplyr)
library(tsmp)
library(shinyjs)

# activate autoreload
options(shiny.autoreload = TRUE)

# threshold values
high_thr = 210
low_thr = 70

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

# grab last date
lastRow <- tail(df, n = 1)
lastDate <- date(lastRow[1,'timestamp'])

# ----------------------------------------------------
# - Define server logic required to draw a histogram -
# ----------------------------------------------------
shinyServer(function(input, output, session) {
    
    # init date picker for daily view as to last date
    updateDateInput(session, 'daywise_ts','Select a day to display:', value = date(lastRow[1,'timestamp'])) 
  
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
    output$dailyTimeSeries <- renderPlotly({
      
      validate(
        need(exists('df'), 'Could not load data.'),
        need(is.data.frame(get('df')), 'Could not load data.'),
      )
      
      df_sample <- df %>% filter(date(df$timestamp) == input$daywise_ts)
      
      validate(
        need(nrow(df_sample) > 0, 'No data available on the selected day.')
      )
      
      #plot(df_sample$timestamp, df_sample$glucose_mgdl) 
      
      plot_ly(df_sample, type = 'scatter', mode = 'lines') %>% 
        add_trace(x = ~timestamp, y = ~glucose_mgdl, 
                  name = 'Blood sugar',
                  line = list(color = 'rgb(0,0,0)', width = 4),
                  hoverinfo = 'text',
                  text = ~paste(glucose_mgdl, 'mg/dl at', time)) %>% 
        # lower threshold
        add_segments(x = ~min(timestamp), xend = ~max(timestamp), 
                     y = ~low_thr, yend = ~low_thr,
                     line = list(color = 'rgb(205, 12, 24)', dash = 'dash'),
                     name = 'High threshold') %>% 
        # upper threshold
        add_segments(x = ~min(timestamp), xend = ~max(timestamp), 
                     y = ~high_thr, yend = ~high_thr,
                     line = list(color = 'rgb(205, 12, 24)', dash = 'dash'),
                     name = 'Low threshold') %>% 
        layout(xaxis = list(title='Time'), 
               yaxis = list(title = 'Blood sugar (mg/dl)'),
               title = paste('Glucose values on', input$daywise_ts),
               showlegend = FALSE)
    })
    
    # --------------------------------------------------
    #  Display daily time series of blood sugar values - 
    # --------------------------------------------------
    output$patternRecResultSimple <- renderPlot({
      if(exists('df') && is.data.frame(get('df'))) {
        res <- tsmp_result()
        
        validate(
          need(!is.null(res), "Please select another time-frame."),
        )
        
        # visualize the result
        visualize(motifs(res))
      }
    })
    
    output$patternRecResult <- renderPlotly({
      res <- tsmp_result()
      df_sample <- pattern_rec_sample()
      
      validate(
        need(!is.null(res), "Please select another time-frame."),
        need(!is.na(df_sample), "Could not load data."),
        need(nrow(df_sample) > 0, "Not enough values in date range.")
      )
      
      # visualize the entire time series
      fig <- plot_ly(df_sample, type = 'scatter', mode = 'lines', height = 200) %>% 
        add_trace(x = ~timestamp, y = ~glucose_mgdl, 
                  name = 'Blood sugar',
                  line = list(color = 'rgb(0,0,0)', width = 1.5),
                  hoverinfo = 'text',
                  text = ~paste(glucose_mgdl, 'mg/dl on', date(timestamp), "at", time)) %>% 
        layout(xaxis = list(title='Day/Time'), 
               yaxis = list(title = 'Blood sugar (mg/dl)'),
               title = paste('Data'),
               showlegend = FALSE)
      
      # add locations of patterns
      pattern_locations1 <- tsmp_motif_1()
      pattern_locations2 <- tsmp_motif_2()
      pattern_locations3 <- tsmp_motif_3()
      
      # add main locations to the plot (thick lines)
      main_pattern1 <- list(y0 = min(df_sample$glucose_mgdl), y1 = max(df_sample$glucose_mgdl),
                  x0 = df_sample[pattern_locations1[1], 'timestamp'], 
                  x1 = df_sample[pattern_locations1[1], 'timestamp'],
                  line = list(color = "red", width = 2.5))
      main_pattern2 <- list(y0 = min(df_sample$glucose_mgdl), y1 = max(df_sample$glucose_mgdl),
                   x0 = df_sample[pattern_locations2[1], 'timestamp'], 
                   x1 = df_sample[pattern_locations2[1], 'timestamp'],
                   line = list(color = "blue", width = 2.5))
      main_pattern3 <- list(y0 = min(df_sample$glucose_mgdl), y1 = max(df_sample$glucose_mgdl),
                   x0 = df_sample[pattern_locations3[1], 'timestamp'], 
                   x1 = df_sample[pattern_locations3[1], 'timestamp'],
                   line = list(color = "green", width = 2.5))
      shapes <- list(main_pattern1, main_pattern2, main_pattern3)
      
      fig <- fig %>% layout(shapes = shapes)
      
      # add neighbours
      for(i in 2:length(pattern_locations1)) {
        neighbour_pattern <- list(y0 = min(df_sample$glucose_mgdl), y1 = max(df_sample$glucose_mgdl),
                              x0 = df_sample[pattern_locations1[i], 'timestamp'], 
                              x1 = df_sample[pattern_locations1[i], 'timestamp'],
                              line = list(color = "red", width = 1, dash = 'dash'))
        shapes <- append(shapes, neighbour_pattern)
      }
      
      fig
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
