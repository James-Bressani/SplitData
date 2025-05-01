#CompareData Function
#Uses a dataframe, a model one, a model two, and a variable you want to use to compare the models
#Returns a data frame with percentages

#' @title Comparing Two Prediction Models
#' @name compareData
#'
#' @description Compares Two fitted models with Root Mean Squared Error, Mean Absoulte Error, and R^2.
#'
#' @param df A data frame to use for prediction and comparison.
#' @param model1 First Model.
#' @param model2 Second Model.
#' @param x Variable you want to compare the models predictions.
#' @param metrics Metrics to compare.
#'
#' @return A data frame comparing metrics
#'
#' @examples
#'
#' # Example using iris dataset
#' flowers <- iris
#' split <- splitData(flowers, trainingRatio = 0.8, seed = NULL)
#' trainingOne <- split$training
#' testingOne <- split$testing
#'
#'model1 <- lm(Sepal.Length ~ Petal.Length + Petal.Width, data = trainingOne)
#' model2 <- lm(Sepal.Length ~ Petal.Length + Petal.Width + Species, data = trainingOne)
#'
#' results <- compareData(testingOne, model1, model2, x = "Sepal.Length")
#' results
#' @export

compareData <- function(df, model1, model2, x, metrics = c("rmse", "mae", "r2")){
  if (!is.data.frame(df)){
    stop("The data must be a dataframe")
  }
  if (!x %in% colnames(df)){
    stop("The variable you would like to compare is not in the dataframe")
  }

  variable <- df[[x]]

  prediction1 <- predict(model1, newdata = df)
  prediction2 <- predict(model2, newdata = df)

  results<- data.frame(model = c("model1", "model2"), row.names = c("model1","model2"))

  #The Metrics (one for each of the metrics)

  if("rmse" %in% metrics){
    rmse1 <- sqrt(mean((variable - prediction1)^2))
    rmse2 <- sqrt(mean((variable - prediction2)^2))
    results$rmse <- c(rmse1, rmse2)
  }

  if("mae" %in% metrics){
    mae1 <- mean(abs(variable - prediction1))
    mae2 <- mean(abs(variable - prediction2))
    results$mae <- c(mae1, mae2)
  }

  if("r2" %in% metrics){
    sst <- sum((variable - mean(variable))^2)
    ssr1 <- sum((variable - prediction1)^2)
    ssr2 <- sum((variable - prediction2)^2)

    r21 <- 1 - (ssr1/sst)
    r22 <- 1 - (ssr2/sst)
    results$r2 <- c(r21, r22)
  }

  attr(results, "predictions") <- data.frame(
    actual = variable,
    model1 = prediction1,
    model2 = prediction2
  )

  attr(results, "residuals") <- data.frame(
    model1 = variable - prediction1,
    model2 = variable - prediction2
  )

  return(results)
}

