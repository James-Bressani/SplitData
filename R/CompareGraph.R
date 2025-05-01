#CompareGraph Function
#Makes graphs to compare the compareData results
#Returns a ggplot2 graph

compareGraph <- function(compareDataResults, plot = "metrics"){
  if(!is.data.frame(compareDataResults)){
    stop("The results must be from compareData")
  }
  if(!plot %in% c("metrics", "residuals", "actualVsPredicted")){
    stop("Please use the following plots (metrics, residuals, actualVsPredicted)")
  }
  if(!requireNamespace("ggplot2")){
    stop("GGplot2 is required")
  }
  if(!requireNamespace("tidyr")){
    stop("tidyr is required")
  }

  if(plot == "metrics"){
    plotData <- tidyr::pivot_longer(compareDataResults,
                                    cols = -model,
                                    names_to = "metric",
                                    values_to = "value")

    metricsPlot <- ggplot2::ggplot(plotData, ggplot2::aes(x = metric, y = value, fill = model)) +
      ggplot2::geom_bar(stat = "identity", position = "dodge") +
      ggplot2::ggtitle("Model Comparison") +
      ggplot2::xlab("Metric") +
      ggplot2::ylab("Value")

    return(metricsPlot)
  }

  if(plot == "residuals"){
    residuals <- attr(compareDataResults, "residuals")

    if(is.null(residuals)){
      stop("No residuals found - did you use compareData()?")
    }

    plotData <- tidyr::pivot_longer(
      residuals,
      cols = everything(),
      names_to = "model",
      values_to = "residual"
    )

    residualsPlot <- ggplot2::ggplot(plotData, ggplot2::aes(x = model, y = residual, fill = model)) +
      ggplot2::geom_boxplot() +
      ggplot2::ggtitle("Residual Comparison") +
      ggplot2::xlab("Model") +
      ggplot2::ylab("Residual")

    return(residualsPlot)
  }

  if(plot == "actualVsPredicted"){
    predictions <- attr(compareDataResults, "predictions")

    if(is.null(predictions)){
      stop("No predictions found - did you use compareData()?")
    }

    actual <- predictions$actual

    model1Data <- data.frame(
      actual = actual,
      predicted = predictions$model1,
      model = "model1"
    )

    model2Data <- data.frame(
      actual = actual,
      predicted = predictions$model2,
      model = "model2"
    )

    plotData <- rbind(model1Data, model2Data)

    actualVsPredPlot <- ggplot2::ggplot(plotData, ggplot2::aes(x = actual, y = predicted, color = model)) +
      ggplot2::geom_point() +
      ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
      ggplot2::ggtitle("Actual vs Predicted Values") +
      ggplot2::xlab("Actual") +
      ggplot2::ylab("Predicted") +
      ggplot2::facet_wrap(~model)

    return(actualVsPredPlot)
  }
}
