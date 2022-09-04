library(shiny)
library(plotly)
library(lubridate)
library(dplyr)
library(tsmp)
library(shinyjs)

# activate autoreload
options(shiny.autoreload = TRUE)

# threshold values
extreme_high_thr <- 250
high_thr <- 180
low_thr <- 70
extreme_low_thr <- 50

# colors 
bloody_red <- 'rgb(205, 12, 24)'
slimy_green <- 'rgb(41, 191, 18)'
crayon_yellow <- 'rgb(255, 209, 102)'
yale_blue <- 'rgb(8, 72, 135)'

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
df$hour <- hour(df$timestamp)

# grab last date
lastRow <- tail(df, n = 1)
lastDate <- date(lastRow[1,'timestamp'])

# ----------------------------------------------------
# - Define server logic required to draw a histogram -
# ----------------------------------------------------
shinyServer(function(input, output, session) {
    
    # init date pickers based on the latest data
    updateDateInput(session, 'daywise_ts','Select a day to display:', value = date(lastRow[1,'timestamp'])) 
    updateDateRangeInput(session, 'pattern_date_range', 'Select a range to start pattern analysis:',
                 start = date(lastRow[1,'timestamp'])-14, end = date(lastRow[1,'timestamp']))
    updateDateRangeInput(session, 'overview_date_range', 'Select a date range to analyze:',
                         start = date(lastRow[1,'timestamp'])-14, end = date(lastRow[1,'timestamp']))
  
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
      select_names <- c()
      for(val in select_choices) {
        select_names <- append(select_names, paste('#', val))
      }
      updateSelectInput(session, 'motif1_select', 'Pattern 1: Examples', 
                        choices = setNames(select_choices, select_names))
      
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
      select_names <- c()
      for(val in select_choices) {
        select_names <- append(select_names, paste('#', val))
      }
      updateSelectInput(session, 'motif2_select', 'Pattern 2: Examples', 
                        choices = setNames(select_choices, select_names))
      
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
      select_names <- c()
      for(val in select_choices) {
        select_names <- append(select_names, paste('#', val))
      }
      updateSelectInput(session, 'motif3_select', 'Pattern 3: Examples', 
                        choices = setNames(select_choices, select_names))
      
      return(pattern_locations)
    })
    
    # the sample on which to apply our "overview" analysis
    overview_sample <- reactive({
      df_sample <- df %>% filter(date(df$timestamp) >= input$overview_date_range[1] & 
                      date(df$timestamp) <= input$overview_date_range[2])
      
      validate(
        need(nrow(df_sample) > 0, 'No data available in selected date range.'),
        need(!is.na(df_sample), 'Error loading data')
      )
      
      return(df_sample)
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
      
      plot_ly(df_sample, type = 'scatter', mode = 'lines') %>% 
        add_trace(x = ~timestamp, y = ~glucose_mgdl, 
                  name = 'Blood sugar',
                  line = list(color = 'black', width = 3),
                  hoverinfo = 'text',
                  text = ~paste(glucose_mgdl, 'mg/dl at', time)) %>% 
        # lower threshold
        add_segments(x = ~min(timestamp), xend = ~max(timestamp), 
                     y = ~low_thr, yend = ~low_thr,
                     line = list(color = bloody_red, dash = 'dash'),
                     name = 'High threshold',
                     hoverinfo = 'none') %>% 
        # upper threshold
        add_segments(x = ~min(timestamp), xend = ~max(timestamp), 
                     y = ~high_thr, yend = ~high_thr,
                     line = list(color = bloody_red, dash = 'dash'),
                     name = 'Low threshold',
                     hoverinfo = 'none') %>% 
        layout(xaxis = list(title='Time'), 
               yaxis = list(title = 'Blood sugar (mg/dl)'),
               title = paste('Glucose values on ', 
                             c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", 
                                     "Friday", "Saturday")[as.POSIXlt(date(df_sample$timestamp[1]))$wday + 1],
                             ', ',
                             input$daywise_ts,
                             sep = ''),
               showlegend = FALSE)
    })
    
    # --------------------------------------------------
    #  Display daily time series of blood sugar values - 
    # --------------------------------------------------

    # total time series data with marked motifs
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
                  line = list(color = 'black', width = 1.2),
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
                  line = list(color = bloody_red, width = 3))
      main_pattern2 <- list(y0 = min(df_sample$glucose_mgdl), y1 = max(df_sample$glucose_mgdl),
                   x0 = df_sample[pattern_locations2[1], 'timestamp'], 
                   x1 = df_sample[pattern_locations2[1], 'timestamp'],
                   line = list(color = yale_blue, width = 3))
      main_pattern3 <- list(y0 = min(df_sample$glucose_mgdl), y1 = max(df_sample$glucose_mgdl),
                   x0 = df_sample[pattern_locations3[1], 'timestamp'], 
                   x1 = df_sample[pattern_locations3[1], 'timestamp'],
                   line = list(color = slimy_green, width = 3))
      shapes <- list(main_pattern1, main_pattern2, main_pattern3)
      
      # add neighbours
      for(i in 2:length(pattern_locations1)) {
        shapes[[length(shapes) + 1]] <- list(y0 = min(df_sample$glucose_mgdl), y1 = max(df_sample$glucose_mgdl),
                                            x0 = df_sample[pattern_locations1[i], 'timestamp'], 
                                            x1 = df_sample[pattern_locations1[i], 'timestamp'],
                                            line = list(color = bloody_red, width = 0.8, dash = 5))
      }
      for(i in 2:length(pattern_locations2)) {
        shapes[[length(shapes) + 1]] <- list(y0 = min(df_sample$glucose_mgdl), y1 = max(df_sample$glucose_mgdl),
                                             x0 = df_sample[pattern_locations2[i], 'timestamp'], 
                                             x1 = df_sample[pattern_locations2[i], 'timestamp'],
                                             line = list(color = yale_blue, width = 0.8, dash = 5))
      }
      for(i in 2:length(pattern_locations3)) {
        shapes[[length(shapes) + 1]] <- list(y0 = min(df_sample$glucose_mgdl), y1 = max(df_sample$glucose_mgdl),
                                             x0 = df_sample[pattern_locations3[i], 'timestamp'], 
                                             x1 = df_sample[pattern_locations3[i], 'timestamp'],
                                             line = list(color = slimy_green, width = 0.8, dash = 5))
      }
      
      # build the final plot
      fig %>% layout(shapes = shapes)
    })
    
    
    # plots of the individual patterns found in an overview window
    output$patternOverview <- renderPlotly({
      res <- tsmp_result()
      df_sample <- pattern_rec_sample()
      
      validate(
        need(!is.null(res), "Please select another time-frame."),
        need(!is.na(df_sample), "Could not load data."),
        need(nrow(df_sample) > 0, "Not enough values in date range.")
      )
      
      # grab pattern locations
      pattern_locations1 <- tsmp_motif_1()
      pattern_locations2 <- tsmp_motif_2()
      pattern_locations3 <- tsmp_motif_3()
      
      # plot the FIRST PATTERN
      fig1 <- plot_ly(df_sample, type = 'scatter', mode = 'lines', height = 220) %>% 
        add_trace(x = ~0:input$pattern_window_size, 
                  y = df_sample[pattern_locations1[1]:(pattern_locations1[1] + input$pattern_window_size), 
                                'glucose_mgdl'],
                  line = list(color = bloody_red, width = 3),
                  hoverinfo = 'none')
      
      # ad neighboring patterns
      for(i in 2:length(pattern_locations1)) {
        fig1 <- fig1 %>% add_trace(x = ~0:input$pattern_window_size, 
                                   y = df_sample[pattern_locations1[i]:(pattern_locations1[i] + input$pattern_window_size), 
                                                 'glucose_mgdl'],
                                   line = list(color = 'gray', width = 1, dash = 'dash'),
                                   hoverinfo = 'none')
      }
      
      fig1 <- fig1 %>% # lower threshold
        add_segments(x = ~0, xend = ~input$pattern_window_size, 
                     y = ~low_thr, yend = ~low_thr,
                     line = list(color = 'black', width = 0.6),
                     name = 'High threshold',
                     hoverinfo = 'none') %>% 
        # upper threshold
        add_segments(x = ~0, xend = ~input$pattern_window_size,
                     y = ~high_thr, yend = ~high_thr,
                     line = list(color = 'black', width = 0.6),
                     name = 'Low threshold',
                     hoverinfo = 'none') %>% 
        layout(xaxis = list(title='Time-Index'), 
                              yaxis = list(title = 'Blood sugar (mg/dl)'),
                              showlegend = FALSE) 
      
      # plot the SECOND PATTERN
      fig2 <- plot_ly(df_sample, type = 'scatter', mode = 'lines', height = 220) %>% 
        add_trace(x = ~0:input$pattern_window_size, 
                  y = df_sample[pattern_locations2[2]:(pattern_locations2[2] + input$pattern_window_size), 
                                'glucose_mgdl'],
                  line = list(color = yale_blue, width = 3),
                  hoverinfo = 'none')
      
      for(i in 2:length(pattern_locations2)) {
        fig2 <- fig2 %>% add_trace(x = ~0:input$pattern_window_size, 
                                   y = df_sample[pattern_locations2[i]:(pattern_locations2[i] + input$pattern_window_size), 
                                                 'glucose_mgdl'],
                                   line = list(color = 'gray', width = 1, dash = 'dash'),
                                   hoverinfo = 'none')
      }
      
      fig2 <- fig2 %>% # lower threshold
        add_segments(x = ~0, xend = ~input$pattern_window_size, 
                     y = ~low_thr, yend = ~low_thr,
                     line = list(color = 'black', width = 0.6),
                     name = 'High threshold',
                     hoverinfo = 'none') %>% 
        # upper threshold
        add_segments(x = ~0, xend = ~input$pattern_window_size,
                     y = ~high_thr, yend = ~high_thr,
                     line = list(color = 'black', width = 0.6),
                     name = 'Low threshold',
                     hoverinfo = 'none') %>% 
        layout(xaxis = list(title='Time-Index'), 
               yaxis = list(title = 'Blood sugar (mg/dl)'),
               showlegend = FALSE)
      
      # plot the THIRD PATTERN
      fig3 <- plot_ly(df_sample, type = 'scatter', mode = 'lines', height = 220) %>% 
        add_trace(x = ~0:input$pattern_window_size, 
                  y = df_sample[pattern_locations3[2]:(pattern_locations3[2] + input$pattern_window_size), 
                                'glucose_mgdl'],
                  line = list(color = slimy_green, width = 3),
                  hoverinfo = 'none')
      
      for(i in 2:length(pattern_locations3)) {
        fig3 <- fig3 %>% add_trace(x = ~0:input$pattern_window_size, 
                                   y = df_sample[pattern_locations3[i]:(pattern_locations3[i] + input$pattern_window_size), 
                                                 'glucose_mgdl'],
                                   line = list(color = 'gray', width = 1, dash = 'dash'),
                                   hoverinfo = 'none')
      }
      
      fig3 <- fig3 %>% # lower threshold
        add_segments(x = ~0, xend = ~input$pattern_window_size, 
                     y = ~low_thr, yend = ~low_thr,
                     line = list(color = 'black', width = 0.6),
                     name = 'High threshold',
                     hoverinfo = 'none') %>% 
        # upper threshold
        add_segments(x = ~0, xend = ~input$pattern_window_size,
                     y = ~high_thr, yend = ~high_thr,
                     line = list(color = 'black', width = 0.6),
                     name = 'Low threshold',
                     hoverinfo = 'none') %>% 
        layout(xaxis = list(title='Time-Index'), 
               yaxis = list(title = 'Blood sugar (mg/dl)'),
               showlegend = FALSE)
      
      # plot all three and add titles
      subplot(fig1, fig2, fig3, titleY = TRUE, titleX = TRUE, margin = 0.035) %>% 
        layout(annotations = list(
          list(x = 0.13 , y = 1.1, text = "Pattern 1", showarrow = F, xref='paper', yref='paper'),
          list(x = 0.50 , y = 1.1, text = "Pattern 2", showarrow = F, xref='paper', yref='paper'),
          list(x = 0.87 , y = 1.1, text = "Pattern 3", showarrow = F, xref='paper', yref='paper'))
        )
    })
    
    
    # ----------------------------------------------
    #  Detailed plots of individual patterns found - 
    # ----------------------------------------------
    
    # show samples of the first pattern
    output$motif1 <- renderPlotly({
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
      df_sample_region <- df_sample[start_index:end_index,]
      
      plot_ly(df_sample_region, type = 'scatter', mode = 'lines') %>% 
        add_trace(x = ~timestamp, y = ~glucose_mgdl, 
                  name = 'Blood sugar',
                  line = list(color = bloody_red, width = 3),
                  hoverinfo = 'text',
                  text = ~paste(glucose_mgdl, 'mg/dl at', time)) %>% 
        # lower threshold
        add_segments(x = ~min(timestamp), xend = ~max(timestamp), 
                     y = ~low_thr, yend = ~low_thr,
                     line = list(color = 'black', width = 0.7),
                     name = 'High threshold',
                     hoverinfo = 'none') %>% 
        # upper threshold
        add_segments(x = ~min(timestamp), xend = ~max(timestamp), 
                     y = ~high_thr, yend = ~high_thr,
                     line = list(color = 'black', width = 0.7),
                     name = 'Low threshold',
                     hoverinfo = 'none') %>% 
        layout(xaxis = list(title='Time'), 
               yaxis = list(title = 'Blood sugar (mg/dl)'),
               title = paste(c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", 
                               "Friday", "Saturday")[as.POSIXlt(date(df_sample_region$timestamp[1]))$wday + 1],
                             date(df_sample_region$timestamp[1]), sep = ', '),
               showlegend = FALSE)
    })
    
    # show samples of the second pattern
    output$motif2 <- renderPlotly({
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
      df_sample_region <- df_sample[start_index:end_index,]
      
      plot_ly(df_sample_region, type = 'scatter', mode = 'lines') %>% 
        add_trace(x = ~timestamp, y = ~glucose_mgdl, 
                  name = 'Blood sugar',
                  line = list(color = yale_blue, width = 3),
                  hoverinfo = 'text',
                  text = ~paste(glucose_mgdl, 'mg/dl at', time)) %>% 
        # lower threshold
        add_segments(x = ~min(timestamp), xend = ~max(timestamp), 
                     y = ~low_thr, yend = ~low_thr,
                     line = list(color = 'black', width = 0.7),
                     name = 'High threshold',
                     hoverinfo = 'none') %>% 
        # upper threshold
        add_segments(x = ~min(timestamp), xend = ~max(timestamp), 
                     y = ~high_thr, yend = ~high_thr,
                     line = list(color = 'black', width = 0.7),
                     name = 'Low threshold',
                     hoverinfo = 'none') %>% 
        layout(xaxis = list(title='Time'), 
               yaxis = list(title = 'Blood sugar (mg/dl)'),
               title = paste(c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", 
                                     "Friday", "Saturday")[as.POSIXlt(date(df_sample_region$timestamp[1]))$wday + 1], 
                             date(df_sample_region$timestamp[1]), sep = ', '),
               showlegend = FALSE)
    })
    
    # show samples of the third pattern
    output$motif3 <- renderPlotly({
      pattern_locations <- tsmp_motif_3()
      
      # compute start and end index in the original data
      df_sample <- pattern_rec_sample()
      start_index <- pattern_locations[as.numeric(input$motif3_select)]
      end_index <- start_index + input$pattern_window_size
      
      validate(
        need(!is.null(start_index), "Could not compute start index."),
        need(!is.na(end_index), "Could not compute end index.")
      )
      
      df_sample_region <- df_sample[start_index:end_index,]
      
      plot_ly(df_sample_region, type = 'scatter', mode = 'lines') %>% 
        add_trace(x = ~timestamp, y = ~glucose_mgdl, 
                  name = 'Blood sugar',
                  line = list(color = slimy_green, width = 3),
                  hoverinfo = 'text',
                  text = ~paste(glucose_mgdl, 'mg/dl at', time)) %>% 
        # lower threshold
        add_segments(x = ~min(timestamp), xend = ~max(timestamp), 
                     y = ~low_thr, yend = ~low_thr,
                     line = list(color = 'black', width = 0.7),
                     name = 'High threshold',
                     hoverinfo = 'none') %>% 
        # upper threshold
        add_segments(x = ~min(timestamp), xend = ~max(timestamp), 
                     y = ~high_thr, yend = ~high_thr,
                     line = list(color = 'black', width = 0.7),
                     name = 'Low threshold',
                     hoverinfo = 'none') %>% 
        layout(xaxis = list(title='Time'), 
               yaxis = list(title = 'Blood sugar (mg/dl)'),
               title = paste(c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", 
                                     "Friday", "Saturday")[as.POSIXlt(date(df_sample_region$timestamp[1]))$wday + 1],
                             date(df_sample_region$timestamp[1]), sep = ', '),
               showlegend = FALSE)
    })
    
    # ---------------------------------------------------------
    #  Overview page: avg, sd, distribution and median graph  - 
    # ---------------------------------------------------------
    
    # mean glucose
    output$average_glucose <- renderText({
      df_sample <- overview_sample()
      paste(round(mean(df_sample$glucose_mgdl)), 'mg/dl')
    })
    
    # standard deviation
    output$standard_deviation <- renderText({
      df_sample <- overview_sample()
      paste(round(sd(df_sample$glucose_mgdl)), 'mg/dl')
    })
    
    # estimation of a1c
    output$estimated_a1c <- renderText({
      df_sample <- overview_sample()
      avg_glucose <- mean(df_sample$glucose_mgdl)
      
      # using the formula from https://ebmcalc.com/GlycemicAssessment.htm
      paste(round((avg_glucose + 46.7) / 28.7, digits = 1), '%')
    })
    
    # coefficient of variation or glucose variability 
    output$coefficient_of_variation <- renderText({
      df_sample <- overview_sample()
      sd <- sd(df_sample$glucose_mgdl)
      mean <- mean(df_sample$glucose_mgdl)
      
      paste(round(sd/mean * 100, digits = 1), '%')
    })
    
    # overall trends, i.e. an overview graph with median, 25/75 and 10/90 percentiles
    output$overall_daily_trend <- renderPlotly({
      df_sample <- overview_sample()
      
      # compute quantiles and median
      glucose_summary <- df_sample %>% group_by(hour) %>% 
        summarise(median = median(glucose_mgdl),
                  quantile_10 = quantile(glucose_mgdl, 0.1),
                  quantile_25 = quantile(glucose_mgdl, 0.25),
                  quantile_75 = quantile(glucose_mgdl, 0.75),
                  quantile_90 = quantile(glucose_mgdl, 0.90))
      
      # extract a somewhat nice time format
      glucose_summary$time <- ordered(format(
        strptime(parse_date_time(glucose_summary$hour, 'H'), 
                 "%Y-%m-%d %H:%M:%S"),
        '%H:%M'))
      
      # and plot
      glucose_summary %>% 
        plot_ly(type = 'scatter', mode = 'lines') %>% 
        add_trace(x = ~time, y = ~quantile_25, 
                  line = list(width = 0),
                  showlegend = FALSE,
                  hoverinfo = 'none') %>% 
        add_trace(x = ~time, y = ~quantile_10, 
                  line = list(width = 0),
                  opacity = 0.2,
                  name = '10th percentile',
                  fill = 'tonexty',
                  fillcolor = 'rgba(0,0,0, 0.05)',
                  hoverinfo = 'none') %>%
        add_trace(x = ~time, y = ~median, 
                  line = list(width = 0), # width 0 to enable filling
                  showlegend = FALSE,
                  hoverinfo = 'none') %>% 
        add_trace(x = ~time, y = ~quantile_25, 
                  line = list(width = 0),
                  opacity = 0.5,
                  name = '25th percentile',
                  fill = 'tonexty',
                  fillcolor = 'rgba(0,0,0, 0.15)',
                  hoverinfo = 'none') %>% 
        add_trace(x = ~time, y = ~median, 
                  line = list(shape = "linear", color = 'black', width = 3),
                  hoverinfo = 'text',
                  text = ~median,
                  name = 'Median') %>% 
        add_trace(x = ~time, y = ~quantile_75, 
                  line = list(width = 0),
                  opacity = 0.5,
                  name = '75th percentile',
                  fill = 'tonexty',
                  fillcolor = 'rgba(0,0,0, 0.15)',
                  hoverinfo = 'none') %>% 
        add_trace(x = ~time, y = ~quantile_90, 
                  line = list(width = 0),
                  opacity = 0.2,
                  name = '90th percentile',
                  fill = 'tonexty',
                  fillcolor = 'rgba(0,0,0, 0.05)',
                  hoverinfo = 'none') %>% 
        # lower threshold
        add_segments(x = ~min(time), xend = ~max(time), 
                     y = ~low_thr, yend = ~low_thr,
                     line = list(color = bloody_red, dash = 'dash'),
                     name = 'High threshold',
                     showlegend = FALSE,
                     hoverinfo = 'none') %>% 
        # upper threshold
        add_segments(x = ~min(time), xend = ~max(time), 
                     y = ~high_thr, yend = ~high_thr,
                     line = list(color = bloody_red, dash = 'dash'),
                     name = 'Low threshold',
                     showlegend = FALSE,
                     hoverinfo = 'none') %>% 
        layout(xaxis = list(title='Time'), 
               yaxis = list(title = 'Blood sugar (mg/dl)'),
               title = "Overall Daily Trend")
    })
    
    # stacked bar chart for time in range
    output$time_in_range <- renderPlotly({
      df_sample <- overview_sample()
      
      range_percentages <- df_sample %>% 
        summarise(extreme_low_pct = sum(glucose_mgdl <= extreme_low_thr) / n() * 100,
                  low_pct = sum(glucose_mgdl <= low_thr & glucose_mgdl > extreme_low_thr) / n() * 100, 
                  in_range_pct = sum(glucose_mgdl < high_thr & glucose_mgdl > low_thr) / n() * 100, 
                  high_pct = sum(glucose_mgdl >= high_thr & glucose_mgdl < extreme_high_thr) / n() * 100, 
                  extreme_high_pct = sum(glucose_mgdl >= extreme_high_thr) / n() * 100)
      
      range_percentages %>% plot_ly() %>% 
        add_trace(x = 0, y = ~extreme_low_pct, type = 'bar',
                  marker = list(color = bloody_red),
                  width = 0.5,
                  name = paste('<=', extreme_low_thr),
                  hoverinfo = 'text',
                  text = ~paste(round(extreme_low_pct, digits = 2), '%')) %>% 
        add_trace(x = 0, y = ~low_pct, type = 'bar',
                  marker = list(color = crayon_yellow),
                  width = 0.5,
                  name = paste('>', extreme_low_thr, 'and <=', low_thr),
                  hoverinfo = 'text',
                  text = ~paste(round(low_pct, digits = 2), '%')) %>% 
        add_trace(x = 0, y = ~in_range_pct, type = 'bar',
                  marker = list(color = slimy_green),
                  width = 0.5,
                  opacity = 0.9,
                  name = paste('>', low_thr, 'and <', high_thr),
                  hoverinfo = 'text',
                  text = ~paste(round(in_range_pct, digits = 2), '%')) %>% 
        add_trace(x = 0, y = ~high_pct, type = 'bar',
                  marker = list(color = crayon_yellow),
                  width = 0.5,
                  name = paste('>=', high_thr, 'and <', extreme_high_thr),
                  hoverinfo = 'text',
                  text = ~paste(round(high_pct, digits = 2), '%')) %>% 
        add_trace(x = 0, y = ~extreme_high_pct, type = 'bar',
                  marker = list(color = bloody_red),
                  width = 0.5,
                  name = paste('>=', extreme_high_thr),
                  hoverinfo = 'text',
                  text = ~paste(round(extreme_high_pct, digits = 2), '%')) %>% 
        layout(barmode = 'stack',
               title = 'Time in Range',
               xaxis = list(title = '', showgrid = FALSE, visible = FALSE),
               yaxis = list(title = '', showgrid = FALSE, visible = TRUE))
    })
})


# idea for main overview page
#   use intervals, e.g. of 1 hour and sum the values 
#   otherwise: a1c from average glucose https://ebmcalc.com/GlycemicAssessment.htm
#   compute coefficient of variation (cv): https://diatribe.org/understanding-average-glucose-standard-deviation-cv-and-blood-sugar-variability
#   https://www.researchgate.net/figure/AGP-CGM-Report-Ambulatory-Glucose-Profile-Continuous-Glucose-Monitoring-Report_fig1_317158676
