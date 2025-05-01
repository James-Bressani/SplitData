#SplitData Function

#Takes in the data frame, a training ratio (default is 80%), and a seed to replicate results (default is null)
#Returns a training and testing dataset in a list

splitData <- function(df, trainingRatio = 0.8, seed = NULL){
  if (!is.data.frame(df)){
    stop("The data must be a dataframe")
  }
  if (!is.numeric(trainingRatio) || trainingRatio <= 0 || trainingRatio >= 1){
    stop("The training ratio must be between 0 and 1")
  }
  if (!is.null(seed)){
    set.seed(seed)
  }
  rows <- nrow(df)

  trainingSize <- floor(trainingRatio * rows)

  #Create a random sample of rows

  trainingSample <- sample(1:rows, trainingSize)

  #The training and testing set

  trainingDataset <- df[trainingSample, ,drop = FALSE]
  testingDataset <- df[-trainingSample, ,drop = FALSE]

  return(list(training = trainingDataset, testing = testingDataset))
}


cars <- mtcars
head(cars)

cars <- splitData(cars)

trainingOne <- cars$training
testingOne <- cars$testing

str(trainingOne)
str(testingOne)
