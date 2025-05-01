#CompareData Function
#Uses a dataframe, a model one, a model two, and a variable you want to use to compare the models
#Returns a data frame with percentages

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
    ssr <- sum((variable - mean(variable))^2)
    ssr1 <- sum((variable - mean(variable))^2)
    ssr2 <- sum((variable - mean(variable))^2)

    r21 <- 1 - (ssr1/ssr)
    r22 <- 1 - (ssr2/ssr)
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

