#' Random forest k-fold cross-validation
#'
#' @description Helper function for randomForestCV(). This function determines the best mtry on a training set, and
#' subsequently creates a randomforest model with this mtry value. This function is the core of each randomForestCV()
#' iteration. \strong{Important:} input matrix/dataframe should include the response vector as a column.
#'
#' @param df A dataframe of observations as rows and features as columns. \strong{Important:} input matrix/dataframe should
#' include the response vector as a column.
#' @param k Number of CV folds.
#' @param colname.response The name of a column containing the response classes.
#'
#' @param balance Balancing of classes by simple up/down sampling.
#' @param incl.expected.response Whether to include the response vector as a column in the dataframe containing the prediction
#' probabilities. This must be TRUE for the downstream statistics plots to work.
#'
#' @param ntreeTry From \pkg{randomForest::tuneRF()}. Number of decision trees to create when determining the optimal mtry value.
#' @param stepFactor From \pkg{randomForest::tuneRF()}. Inflation rate of mtry for each iteration.
#' @param improve From \pkg{randomForest::tuneRF()}. Relative improvement in out of bag error (OOBE) must be by this much for
#' the search to continue.
#' @param plot From \pkg{randomForest::tuneRF()}. Whether to plot the OOB error as function of mtry.
#' @param trace From \pkg{randomForest::tuneRF()}. Whether to print the progress of the search
#'
#' @param randomForest.ntree From \pkg{randomForest::randomForest()}. Number of decision trees to create when building the
#' random forest model from the training set
#' @param importance From \pkg{randomForest::randomForest()}. Whether to return within the object created by
#' randomForest::randomForest() the MeanDecreaseGini and MeanDecreaseAccuracy. This must be TRUE for the downstream importance
#' plots to work.
#'
#' @param ... Other arguments that can be passed to \pkg{randomForest::tuneRF()} and \pkg{randomForest::randomForest()}.
#'
#' @return Returns a list of length(k), with sublists containing the random forest model trained on the train dataset and the
#' prediction probabilities on the test set.
#' @export
#'

randomForestCV <- function(df, colname.response, k = 10, ...)
{
   l_TrainTestSets <- createCvTrainTestSets(df, k = k)

   rfCV <- lapply(1:k, function(i){
      message(paste0('< CV: round ', i,' >'))
      df.train <- l_TrainTestSets[[i]]$train
      df.test <- l_TrainTestSets[[i]]$test

      randomForestTrainAndTest(df.train, df.test, colname.response,...)
   })

   return(rfCV)
}
