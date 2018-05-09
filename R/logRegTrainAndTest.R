#' Logistic regression train and test workflow
#'
#' @param train Train dataset. A dataframe of observations as rows and features as columns. \strong{Important:} input
#' matrix/dataframe should include the response vector as a column.
#' @param test Test dataset. A dataframe of observations as rows and features as columns. \strong{Important:} input
#' matrix/dataframe should include the response vector as a column.
#' @param colname.response The name of a column containing the response classes.
#' @param positive.response The name of the positive class (i.e. the class one wants to predict).
#' 
#' @param pre.transform.formula A character string describing the mathematical operation, e.g. 'log(x + 1)', where x is the 
#' variable to transform.
#' @param standardize Transform each feature column by (x - mean(x))/sd(x). Standardization scales each feature so that the 
#' values are comparable across features. In stochastic gradient descent, feature scaling can improve the convergence speed 
#' of the algorithm.
#' @param na.replace During feature standardization, imputation of NA feature values by 'mean' or 'median'.
#' @param sd.zero.bypass During feature standardization, if standard deviation of a feature is 0, standardization will fail
#' due to divide-by-zero error. If sd.zero.bypass = TRUE, a vector of zeros will be returned instead.
#' @param balance Balancing of classes by up/down sampling.
#' 
#' @param k Number of folds to split the data in when fitting the regression with glmnet::cv.glmnet().
#' @param alpha From \pkg{glmnet::cv.glmnet()}. Elastic net regularization parameter. 0: ridge, 1: lasso.
#' @param type.measure From \pkg{glmnet::cv.glmnet()}. Type of measure to determine optimal lambda value.
#' @param nlambda From \pkg{glmnet::cv.glmnet()}. Number of lambda values to try to determine optimal lambda value.
#' @param lower.limits From \pkg{glmnet::cv.glmnet()}. Lower limit of coefficients. Default: 0 forces coefficients to be 
#' non-negative.
#' 
#' @param predictLambda {From \pkg{glmnet::cv.glmnet()}. For prediction on test set use the lambda value with the lowest
#' error ('lambda.min') or the lambda value one standard error higher than lambda.min ('lambda.1se')}.
#' 
#' @param ... Other arguments that can be passed to \pkg{glmnet::cv.glmnet()}.
#'
#' @return 
#' Omitting a test set only returns a glmnet object, while  Including a test set returns a list containing the glmnet 
#' object and prediction probabilities
#' 
#' @examples 
#' ## Omitting a test set only returns a glmnet object
#' logRegTrainAndTest(train, colname.response = 'response', positive.response = 'BRCA', 
#'                    standardize = T, balance = 'up')
#' 
#' ## Including a test set returns a list containing the glmnet object and prediction probabilities
#' logRegTrainAndTest(train, test, colname.response = 'response', positive.response = 'BRCA', 
#'                    standardize = T, balance = 'up')
#' 
#' @export

logRegTrainAndTest <- function(train, test = NULL, colname.response, positive.response = NULL,
                   
                               ## Data processing args
                               pre.transform.formula = NULL,
                               standardize = F, na.replace = 'median', sd.zero.bypass = T,
                               balance = F,
                               
                               ## Regression fitting args
                               k = 10, alpha = 1, type.measure = 'class', nlambda = 100, lower.limits = 0,
                               
                               ## Prediction args
                               predictLambda = 'lambda.min',
                               
                               ## Other glmnet::cvglmnet() args
                               ...)
{
   if( is.null(test) ){
      dataset <- list(train = train)
   } else {
      dataset <- list(train = train, 
                      test = test)
   }
   
   ## Transform data by indicated formula
   if( !is.null(pre.transform.formula) ){
      dataset <- lapply(dataset, function(i){
         transformFeatures(i, formula = pre.transform.formula, colname.response = colname.response) 
      })
   }

   ## Standardize data
   if(standardize == T){
      dataset <- standardizeTrainTestSets(dataset$train, dataset$test, colname.response,
                                          na.replace = na.replace, sd.zero.bypass = sd.zero.bypass)
   }

   if( is.null(test) ){
      dataset <- list(train = dataset)
   }
      
   ## Balance classes
   if(balance == T | balance == 'up'){
      dataset$train <- balanceClasses(dataset$train, colname.response, scaling = 'up')
   } else if (balance == 'down'){
      dataset$train <- balanceClasses(dataset$train, colname.response, scaling = 'down')
   }
   
   ## Set positive response as 2nd factor level.
   if( is.null(positive.response) ){
      message('Please specify positive.response')
   } else {
      dataset <- lapply(dataset, function(i){
         responseVector <- i[, colname.response ]
         classes <- responseVector %>% factor() %>% levels()

         if(length(classes) != 2){
            stop('The response vector in either the train or test set does not have exactly 2 classes')
         }

         negative.response <- classes[classes != positive.response]

         i[, colnames(i) == colname.response ] <- relevel(responseVector, negative.response)

         return(i)
      })
   }
   
   ## Determining best lambda
   logReg <-
      cv.glmnet(x = as.matrix(dataset$train %>% .[, colnames(.) != colname.response ]), ## signature matrix
                y = dataset$train[,colname.response], ## response vector
                nfolds = k,
                family = 'binomial',
                alpha = alpha,
                type.measure = type.measure,
                nlambda = nlambda,
                lower.limits = lower.limits,
                standardize = F, ## own standardization has been performed above
                ...)
   
   ## Output
   if( is.null(test) ){
      return(logReg)
   
   } else {
      ## Predict on test set with best lambda
      pred <-
         predict.cv.glmnet(logReg,
                           as.matrix(dataset$test %>% .[, colnames(.) != colname.response ]),
                           s = predictLambda,
                           type = 'response'
         )
      
      colnames(pred) <- 'prediction'
      pred <- pred %>% as.data.frame()
      pred$response <- dataset$test$response
      
      ## Store CV object and prediction probabilities
      logRegTrainTestOut <- list(logReg = logReg, pred = pred)
      return(logRegTrainTestOut)
   }
}

