#' Random forest k-fold cross-validation
#'
#' @description Performs k-fold cross-validation using random forest as a model, and outputs a model from each fold and the
#' prediction probabilities. \strong{Important:} input matrix/dataframe should include the response vector as a column.
#'
#' @param df A dataframe of observations as rows and features as columns. \strong{Important:} input matrix/dataframe should
#' include the response vector as a column.
#' @param colname.response The name of a column containing the response classes.
#' @param k Number of CV folds.
#' @param stratify Stratification ensures that all classes are present in every train/test set when splitting the
#' data into k-folds (k).
#' @param ... Other arguments that can be passed to \pkg{randomForest::tuneRF()}, \pkg{randomForest::randomForest()} and
#' \pkg{mltoolkit::randomForestTrainAndTest()}.
#'
#' @return Returns a list of length(k), with sublists containing the random forest model trained on the train dataset and the
#' prediction probabilities on the test set.
#' @export
#'

randomForestCV <- function(df, colname.response, k = 10, stratify = T, ...)
{
   if(stratify == T){
      l_TrainTestSets <- createCvTrainTestSets(df, k = k, stratify.by.col = colname.response)
   } else {
      l_TrainTestSets <- createCvTrainTestSets(df, k = k)
   }
   
   ## Cross validation loop
   lapply(1:k, function(i){
      message(paste0('<< CV fold: ', i,' >>'))
      train <- l_TrainTestSets[[i]]$train
      test <- l_TrainTestSets[[i]]$test

      randomForestTrainAndTest(train, test, colname.response, ...)
   })
}
