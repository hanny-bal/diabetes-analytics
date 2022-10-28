# DiAnalytics (Diabetes Analytics)
An R shiny app for analyzing blood sugar data, specifically targeted towards type 1 diabetics. All plots are made with [Plotly for R](https://plotly.com/r/), making the dashboard interactive. 

The app consists of three primary views:

## Overview
Computes basic statistics like the mean glucose value, estimated a1c, standard deviation or glucose variability for a specified time-frame and plots the time-in-range as well as daily patterns occuring during that time based on on hourly quantiles. Basically, the overview is an interactive [AGP Report](http://www.agpreport.org/agp/agpreports).

<center>
  <img src="/screenshots/dianalytics_overview.png" width="75%"/>
</center>


## Daily Graph
Plots the glucose graph for a specified day.

<center>
  <img src="/screenshots/dianalytics_daily.png" width="75%"/>
</center>


## Pattern Recognition
An experimental page for automatized pattern recognition with algorithms based on the matrix profile. The matrix profile of a time-series is a vector that stores the normalized Euclidean distance between any subsequence within a time series and its nearest neighbor. An efficient implementation of matrix profile algorithms is available in the [tsmp](https://github.com/matrix-profile-foundation/tsmp) package - which is also used in this app.

<center>
  <img src="/screenshots/dianalytics_patterns.png" width="75%"/>
</center>
