#' Random forest train and test workflow
#'
#' @description This function determines the best mtry on a training set, and subsequently creates a randomforest model with
#' this mtry value. This function is also the core of each randomForestCV() iteration. \strong{Important:} input
#' matrix/dataframe should include the response vector as a column.
#'
#' @param train Train dataset. A dataframe of observations as rows and features as columns. \strong{Important:} input
#' matrix/dataframe should include the response vector as a column.
#' @param test Test dataset. If test is NULL, no prediction will be performed. A dataframe of observations as rows and features 
#' as columns. \strong{Important:} input matrix/dataframe should include the response vector as a column.
#' @param colname.response The name of a column containing the response classes.
#' @param balance Balancing of classes by simple up/down sampling.
#' @param incl.expected.response Whether to include the response vector as a column in the dataframe containing the
#' prediction probabilities. This must be TRUE for the downstream statistics plots to work.
#'
#' @param ntreeTry From \pkg{randomForest::tuneRF()}. Number of decision trees to create when determining the optimal mtry
#' value.
#' @param stepFactor From \pkg{randomForest::tuneRF()}. Inflation rate of mtry for each iteration.
#' @param improve From \pkg{randomForest::tuneRF()}. Relative improvement in out of bag error (OOBE) must be by this much for
#' the search to continue.
#' @param plot From \pkg{randomForest::tuneRF()}. Whether to plot the OOB error as function of mtry.
#' @param trace From \pkg{randomForest::tuneRF()}. Whether to print the progress of the search.
#' @param equal.oobe.decision If there are multiple mtry with min(OOB error), choose the highest or lowest mtry value ('max' or 'min' respectively)?
#'
#' @param mtry \pkg{randomForest::randomForest()}. Number of features to use for building each decision tree
#' @param randomForest.ntree From \pkg{randomForest::randomForest()}. Number of decision trees to create when building the
#' random forest model from the training set.
#' @param importance From \pkg{randomForest::randomForest()}. Whether to return within the object created by
#' randomForest::randomForest() the MeanDecreaseGini and MeanDecreaseAccuracy. This must be TRUE for the downstream importance
#' plots to work.
#'
#' @param ... Other arguments that can be passed to \pkg{randomForest::tuneRF()} and \pkg{randomForest::randomForest()}.
#'
#' @return Returns a list containing the random forest model trained on the train dataset and the prediction probabilities
#' on the test set.
#' @export
#'

randomForestTrainAndTest <- function(train, test = NULL, colname.response, balance = F, incl.expected.response = T,

                                     ## randomForest::tuneRF() args
                                     ntreeTry = 500, stepFactor = 1.2, improve = 0.001, plot = F, trace = F, equal.oobe.decision = 'max',

                                     ## randomForest::randomForest() args
                                     mtry = NULL, randomForest.ntree = 500, importance = T,

                                     ## Other
                                     ...)
{
   ## Up/down balance classes
   if(balance == T | balance == 'up'){
      train <- balanceClasses(train, colname.response, scaling = 'up')
   } else if (balance == 'down'){
      train <- balanceClasses(train, colname.response, scaling = 'down')
   }

   ## Get mtry where OOBE is min
   if( is.null(mtry) ){
      mtryTune <- tuneRF(x = train %>% .[,colnames(.) != colname.response], #df of features/observations
                         y = train %>% .[,colname.response], ## vector of expected response
                         ntreeTry=ntreeTry,
                         stepFactor=stepFactor,
                         improve=improve,
                         plot=plot,
                         trace=trace,
                         ...)

      mtry <-
         mtryTune %>%
         .[.[,2] == min(.[,2]),1]

      if(equal.oobe.decision == 'max'){
         mtry <- mtry[length(mtry)]
      } else if(equal.oobe.decision == 'min'){
         mtry <- mtry[1]
      }
   }

   ## Fit RF model
   RF <- randomForest(x = train %>% .[,colnames(.) != colname.response],
                      y = train %>% .[,colname.response],
                      ntree = randomForest.ntree,
                      importance = importance,
                      mtry = mtry,
                      ...)

   ## Return RF and prediction object
   rfTrainTestOut <- list()
   rfTrainTestOut$RF <-  RF
   
   if(!is.null(test)){
      ## Predict on test set
      pred <- predict(object = RF, newdata = test[,colnames(test) != 'response'], type = "prob")
      pred <- pred %>% as.data.frame()
      
      if(incl.expected.response == T){
         pred$response <- test[,colname.response]
      }
      
      rfTrainTestOut$pred <- pred
   } else {
      rfTrainTestOut$pred <- NA
   }
   
   return(rfTrainTestOut)
}
