#' Logistic regression nested k-fold cross-validation
#'
#' @description Performs nested k-fold cross-validation to assess the performance of a logistic regression model.
#' \strong{Important:} input matrix/dataframe should include the response vector as a column. Inner folds: uses
#' \pkg{glmnet::cv.glmnet()} fit a logistic regression to the outer fold training set (i.e. determine the optimal lambda and
#' subsequently the regression coefficients). Outer folds: uses the model created from the inner fold to predict on the outer
#' fold test set.
#'
#' @param df A dataframe of observations as rows and features as columns. \strong{Important:} input matrix/dataframe should
#' include the response vector as a column.
#' @param colname.response The name of a column containing the response classes.
#' @param k The number of outer cross-validation folds. Also the number of inner cross-validation folds (i.e. fitting the 
#' regression with \pkg{glmnet::cv.glmnet()} ).
#' @param stratify Stratification ensures that all classes are present in every train/test set when splitting the
#' data into k-folds.
#' @param ... Other arguments that can be passed to \pkg{glmnet::cv.glmnet()} and \pkg{logRegTrainAndTest}.
#'
#' @examples 
#' logRegCV(df, colname.response = 'response', positive.response = 'BRCA', 
#'          k = 10, stratify= T, standardize = T, balance = 'up')
#'
#' @return Returns a list of length(k), with sublists containing (1) a cv.glmnet object from the training on the outer
#' fold train set, and (2) a dataframe of probabilities from the prediction on the outer fold test set.
#'
#' @export

logRegCV <- function(df, colname.response, positive.response, k = 10, stratify = T, ...)
{
   ## Get outer fold train and test sets
   if(stratify == T){
      l_TrainTestSets <- createCvTrainTestSets(df, k = k, stratify.by.col = colname.response)
   } else {
      l_TrainTestSets <- createCvTrainTestSets(df, k = k)
   }

   ## Outer fold cross validation loop
   lapply(1:k, function(outerFold)
   {
      message(paste0('<< Outer fold CV: ', outerFold, ' >>'))

      ## Get train and test for current outer fold
      train <- l_TrainTestSets[[outerFold]]$train
      test <- l_TrainTestSets[[outerFold]]$test
      
      ## Inner fold cross validation loop and prediction 
      logRegTrainAndTest(train, test, colname.response = 'response', positive.response = positive.response, ...)
   })
}