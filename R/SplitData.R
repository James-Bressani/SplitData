#SplitData Function

#Takes in the data frame, a training ratio (default is 80%), and a seed to replicate results (default is null)
#Returns a training and testing dataset in a list

#' @title Splits Data into Training and Testing Sets
#' @name splitData
#'
#' @description Takes in a dataframe, a training ratio, and a seed and returns a training and testing dataset in a list.
#'
#' @param df A data frame
#' @param trainingRatio A ratio between 0 and 1 indicating the proportion of the dataset used for training (Default is 0.8).
#' @param seed Seed for reproducible results (null by default).
#'
#' @return A list with a training and testing dataframe:
#' \describe{
#'   \item{training}{The training subset of the dataframe.}
#'   \item{testing}{The testing subset of the dataframe.}
#' }
#'
#' @examples
#' # How to use splitData() function
#' flowers <- iris
#' head(flowers)
#'
#' flowers <- splitData(flowers, trainingRatio = 0.8, seed = NULL)
#'
#' # Use variables to extract the new dataframes
#' trainingOne <- flowers$training
#' testingOne <- flowers$testing
#'
#' # Checking structure of the new data
#' str(trainingOne)
#' str(testingOne)
#'
#' @export
#'
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


