#Convert date into vector
getDateAsVector <- function(date_1) {
  return(as.numeric(unlist(strsplit((as.character(date_1)), "-"
  ))))
}

#convert xts object into ts
convertXtstoTs <- function(tsr) {
  return (as.ts(tsr, start = head(index(tsr), 1), end = tail(index(tsr), 1)))
}

#convert percentage to value to divide data into train and test
getDatasetDivideValue <- function(value, size) {
  return(ceiling(((100 - value) / 100) * size))
}

#plot time series
plot_time_series <-
  function(ts_object,
           input_currency,
           input_model,
           input_column) {
    startYear <- start(ts_object) # Grabs start date
    endYear <- end(ts_object) # Grabs end date
    tsPlot <- autoplot(
      ts_object,
      ts.colour = 'turquoise4',
      size = 1,
      main = sprintf(
        "Plot of %s's  %s Time Series (%s - %s)",
        input_currency,
        input_model,
        startYear[1],
        endYear[1]
      )
    ) +
      theme(
        axis.text.x = element_text(angle = 35, hjust = 1),
        panel.background = element_rect(fill = "gray98"),
        axis.line.x = element_line(colour = "gray"),
        axis.line.y = element_line(colour = "gray")
      ) +
      labs(x = "Year", y = sprintf("%s Values", input_column))
    return(tsPlot)
  }



# FUNCTION FOR ACF AND PACF PLOTS
plot_acf_pacf <- function(ts, input_currency, input_column) {
  a <- autoplot(
    acf(ts, plot = FALSE),
    colour = 'turquoise4',
    conf.int.fill = '#4C4CFF',
    conf.int.value = 0.95,
    conf.int.type = 'ma'
  ) +
    theme(
      panel.background = element_rect(fill = "gray98"),
      axis.line.y   = element_line(colour = "gray"),
      axis.line.x = element_line(colour = "gray")
    ) +
    ggtitle(sprintf("ACF plot of %s (%s)", input_currency, input_column))
  
  b <- autoplot(
    pacf(ts, plot = FALSE),
    colour = 'turquoise4',
    conf.int.fill = '#4C4CFF',
    conf.int.value = 0.95,
    conf.int.type = 'ma'
  ) +
    theme(
      panel.background = element_rect(fill = "gray98"),
      axis.line.y   = element_line(colour = "gray"),
      axis.line.x = element_line(colour = "gray")
    ) + labs(y = "PACF") +
    ggtitle(sprintf("PACF plot of %s (%s)", input_currency, input_column))
  
  grid.arrange(a, b)
}

# plot Decomposed Plots
plot_decomp <-
  function(timeSeries_obj,
           input_currency,
           input_column) {
    autoplot(
      stl(timeSeries_obj, s.window = "periodic"),
      main = sprintf("Decomposition Plot of %s (%s)", input_currency, input_column),
      ts.colour = "turquoise4"
    ) +
      theme(
        panel.background = element_rect(fill = "gray98"),
        axis.line.y   = element_line(colour = "gray"),
        axis.line.x = element_line(colour = "gray")
      )
  }

# Plot Seasonal Plots
plot_seasonal <-
  function(timeSeries_obj,
           input_currency,
           input_column) {
    ggseasonplot(
      timeSeries_obj,
      xlab = "Year",
      main = sprintf("Seasonal Plot of %s (%s)", input_currency, input_column),
      year.labels = TRUE,
      year.labels.left = TRUE,
      col = 1:20,
      pch = 19
    ) +
      theme(
        panel.background = element_rect(fill = "gray98"),
        axis.line.y = element_line(colour = "gray"),
        axis.line.x = element_line(colour = "gray")
      )
  }

#Create forecast by plotly graph
createPlotlyForecastGraph <-
  function(ts_obj, test_data, forecast_model) {
    p <- plot_ly() %>%
      add_lines(
        x = index(ts_obj),
        y = coredata(ts_obj),
        color = I("black"),
        name = "observed"
      ) %>%
      add_ribbons(
        x = test_data,
        ymin = forecast_model$lower[, 2],
        ymax = forecast_model$upper[, 2],
        color = I("gray95"),
        name = "95% confidence"
      ) %>%
      add_ribbons(
        x = test_data,
        ymin = forecast_model$lower[, 1],
        ymax = forecast_model$upper[, 1],
        color = I("gray80"),
        name = "80% confidence"
      ) %>%
      add_lines(
        x = test_data,
        y = forecast_model$mean,
        color = I("blue"),
        name = "prediction"
      )
    return(p)
  }

#create test vs prediction graph
createComparisonGraph <-
  function(test_data,
           stepsize,
           forecast_model,
           input_column,
           model_selected) {
    if (model_selected == "SVM") {
      y_axis = forecast_model[(length(forecast_model) - (stepsize - 1)):(length(forecast_model))]
    } else if (model_selected == "Prophet") {
      y_axis = exp(forecast_model$yhat[(nrow(forecast_model) - (stepsize - 1)):(nrow(forecast_model))])
    } else{
      y_axis = forecast_model$mean
    }
    p <- plot_ly() %>%
      add_lines(
        x = test_data$date[1:stepsize],
        y = y_axis,
        color = I("blue"),
        name = "Prediction"
      ) %>%
      add_lines(
        x = test_data$date[1:stepsize],
        y = test_data[[input_column]][1:stepsize],
        color = I("black"),
        name = "Observed"
      )
    return(p)
  }

# create prediction graph for plotly
prophetPlotly <-
  function(ts_obj,
           test_data,
           forecast_model,
           stepsize) {
    p <- plot_ly() %>%
      add_lines(
        x = index(ts_obj),
        y = coredata(ts_obj),
        color = I("black"),
        name = "observed"
      ) %>%
      add_ribbons(
        x = test_data,
        ymin = exp(forecast_model$yhat_lower[(nrow(forecast_model) - (stepsize -
                                                                        1)):(nrow(forecast_model))]),
        ymax = exp(forecast_model$yhat_upper[(nrow(forecast_model) - (stepsize -
                                                                        1)):(nrow(forecast_model))]),
        color = I("gray95"),
        name = "95% confidence"
      ) %>%
      add_lines(
        x = test_data,
        y = exp(forecast_model$yhat[(nrow(forecast_model) - (stepsize - 1)):(nrow(forecast_model))]),
        color = I("blue"),
        name = "prediction"
      )
    return(p)
  }

#create prediction graph for SVM
SVMPlotly <- function(ts_obj,
                      test_data,
                      forecast_model,
                      stepsize) {
  y_axis = forecast_model[(length(forecast_model) - (stepsize - 1)):(length(forecast_model))]
  
  p <- plot_ly() %>%
    add_lines(
      x = index(ts_obj),
      y = coredata(ts_obj),
      color = I("black"),
      name = "observed"
    ) %>%
    add_lines(
      x = test_data,
      y = y_axis,
      color = I("blue"),
      name = "prediction"
    )
  return(p)
}

#create prediction graph for Feed forward Neural Network
neuralPlotly <- function(ts_obj,
                         test_data,
                         forecast_model,
                         stepsize) {
  p <- plot_ly() %>%
    add_lines(
      x = index(ts_obj),
      y = coredata(ts_obj),
      color = I("black"),
      name = "observed"
    ) %>%
    add_lines(
      x = test_data,
      y = forecast_model$mean,
      color = I("blue"),
      name = "prediction"
    )
  return(p)
}

#Create summary for neural network
summary.neural <- function(applied_model) {
  # print("method: %s     series : %s       nodes in hidden layers:%s        lambda: %s  ", applied_model$method,
  #         applied_model$series, applied_model$size, applied_model$lambda)
  cat("Forecast method : ", applied_model$method, "\n")
  cat("Forecast series : ", applied_model$series, "\n \n")
  cat("nodes in hidden layer", "       ", "lambda", "      ", "P", "       ", "p", "\n")
  cat(
    applied_model$size,
    "                          ",
    applied_model$lambda,
    "           ",
    applied_model$P,
    "       ",
    applied_model$p,
    "\n"
  )
}

# create prophet summary
summary.prophet <- function(applied_model) {
  cat("growth : ", applied_model$growth, "\n")
  cat("Mode of seasonality : ",
      applied_model$seasonality.mode,
      "\n \n")
  cat(
    "yearly seasonality",
    "       ",
    "weekly seasonality",
    "      ",
    "daily seasonality",
    "       ",
    "holidays specified",
    "\n"
  )
  cat(
    applied_model$yearly.seasonality,
    "                      ",
    applied_model$weekly.seasonality,
    "                 ",
    applied_model$daily.seasonality,
    "       ",
    applied_model$holidays,
    "\n"
  )
  
}

# generate accuracy table for SVM
SVMAccuracy <- function(testset, forecast_model, stepsize) {
  prediction = forecast_model[(length(forecast_model) - (stepsize - 1)):(length(forecast_model))]
  a <- accuracy(prediction, testset[1:stepsize])
  return(a)
}

# Generate prophet Accuracy table
prophetAccuracy <- function(testset, forecast_model, stepsize) {
  prediction = forecast_model$yhat[(nrow(forecast_model) - (stepsize - 1)):(nrow(forecast_model))]
  a <- accuracy(exp(prediction), (testset[1:stepsize]))
  return(a)
}