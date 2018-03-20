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
#' @param stratify Stratification ensures that all classes are present in every train/test set when splitting the
#' data into k-folds (k.outer).
#'
#' @param k.outer Number of folds to split the data in the outer CV loop.
#' @param standardize Transform each feature column by (x - mean(x))/sd(x). Standardization is done before class
#' balancing. Standardization scales each feature so that the values are comparable across features. In stochastic gradient
#' descent, feature scaling can improve the convergence speed of the algorithm.
#' @param na.replace During feature standardization, imputation of NA feature values by 'mean' or 'median'.
#' @param sd.zero.replace During feature standardization, if standard deviation of a feature is 0, standardization will fail
#' due to divide-by-zero error. If sd.zero.replace = TRUE, a vector of zeros will be returned instead.
#' @param balance Balancing of classes by up/down sampling.
#'
#' @param k.inner Number of folds to split the data in the inner CV loop.
#' @param alpha From \pkg{glmnet::cv.glmnet()}. Elastic net regularization parameter. 0: ridge, 1: lasso.
#' @param type.measure From \pkg{glmnet::cv.glmnet()}. Type of measure to determine optimal lambda value.
#' @param nlambda From \pkg{glmnet::cv.glmnet()}. Number of lambda values to try to determine optimal lambda value.
#' @param lower.limits From \pkg{glmnet::cv.glmnet()}. . Lower limit of
#' coefficients. Default: 0 forces coefficients to be non-negative.
#'

#' @param predictLambda {From \pkg{glmnet::cv.glmnet()}. For prediction on test set use the lambda value with the lowest
#' error ('lambda.min') or the lambda value one standard error higher than lambda.min ('lambda.1se')}.
#'
#' @param ... Other arguments that can be passed to \pkg{glmnet::cv.glmnet()}.
#'
#' @return Returns a list of length(k.outer), with sublists containing (1) a cv.glmnet object from the training on the outer
#' fold train set, and (2) a dataframe of probabilities from the prediction on the outer fold test set.
#'
#' @export

logRegCV <- function(df, colname.response, stratify= T,

                     ## Outer CV loop args
                     k.outer = 10,
                     standardize = T, na.replace = 'median', sd.zero.replace = T,
                     balance = F,

                     ## Inner CV loop args
                     k.inner = 10, alpha = 1, type.measure = 'class', nlambda = 100, lower.limits = 0,

                     ## Prediction args
                     predictLambda = 'lambda.min',

                     ## Other glmnet::cvglmnet() args
                     ...)
{
   ## Get outer fold train and test sets
   if(stratify == T){
      l_TrainTestSets <- createCvTrainTestSets(df, k=k.outer, stratify.by.col = colname.response)
   } else {
      l_TrainTestSets <- createCvTrainTestSets(df, k=k.outer)
   }

   ## Nested cross validation
   LR_CV_nested <- lapply(1:length(l_TrainTestSets), function(outerFold)
   {
      message(paste0('<< Outer fold CV: ', outerFold, ' >>'))

      ## Get train and test for current outer fold
      foldSet <- list(
         l_TrainTestSets[[outerFold]]$train,
         l_TrainTestSets[[outerFold]]$test
      )

      set_names <- c('train','test')
      names(foldSet) <- set_names

      ## Standardize data
      if(standardize == T){
         ## Isolate feature martrix and ln(x+1) transform
         features <- lapply(foldSet,function(i){
            i[, colnames(i) != colname.response ] %>% apply(., 2, function(col){ ln(col+1) }) %>% as.data.frame()
         })

         ## Isolate response vectors
         responses <- lapply(foldSet, function(i){ i[,colname.response] })

         ## Get mean and sd of train set
         train_stats <- list(
            mean = apply(features$train, 2, function(col){ mean(col, na.rm = T) }),
            sd = apply(features$train, 2, function(col){ sd(col, na.rm = T) }),
            median = apply(features$train, 2, function(col){ median(col, na.rm = T) })
         )

         ## Standardize training set; use train set mean and sd to standardize test set
         ## Overwrite foldSet object
         foldSet <- lapply(set_names,function(setName){
            setFeatures <- features[[setName]]
            setResponses <- responses[[setName]]

            df <- lapply(1:ncol(setFeatures),function(i){
               standardizeVector(setFeatures[,i],
                                 Mean = train_stats$mean[i],
                                 SD = train_stats$sd[i],
                                 Median = train_stats$median[i],
                                 na.replace = na.replace,
                                 sd.zero.replace = sd.zero.replace) }) %>% do.call(cbind,.) %>% as.data.frame()
            colnames(df) <- colnames(setFeatures)
            rownames(df) <- rownames(setFeatures)

            df$response <- setResponses

            return(df)
         })
         names(foldSet) <- set_names
      }

      ## Balance classes
      if(balance == T | balance == 'up'){
         foldSet$train <- balanceClasses(foldSet$train, colname.response, scaling = 'up')
      } else if (balance == 'down'){
         foldSet$train <- balanceClasses(foldSet$train, colname_response, scaling = 'down')
      }

      ## Inner fold CV: Determining best lambda
      LR_CV_inner <-
         cv.glmnet(x = as.matrix(foldSet$train %>% .[, colnames(.) != colname.response ]), ## signature matrix
                   y = foldSet$train[,colname.response], ## response vector
                   family = 'binomial',
                   nfolds = k.inner,
                   alpha = alpha,
                   type.measure = type.measure,
                   nlambda = nlambda,
                   lower.limits = lower.limits,
                   ...)

      ## Predict on test set with best lambda
      pred <-
         predict.cv.glmnet(LR_CV_inner,
                           as.matrix(foldSet$test %>% .[, colnames(.) != colname.response ]),
                           s = predictLambda,
                           type = 'response'
         )

      colnames(pred) <- 'prediction'
      pred <- pred %>% as.data.frame()
      pred$response <- foldSet$test$response

      ## Store CV object and prediction probabilities
      LR_summary <- list(cv.inner = LR_CV_inner, pred = pred)
      return(LR_summary)
   })

   return(LR_CV_nested)
}
