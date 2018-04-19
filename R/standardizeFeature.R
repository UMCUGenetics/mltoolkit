#' Feature standardization
#'
#' @description Standardize a vector with the following transformation: (x - mean)/sd. Standardization makes the values of 
#' each feature in the data have zero-mean.
#'
#' @param x A numeric/integer vector.
#' @param na.replace Imputation by median (TRUE or 'median') or mean ('mean').
#' @param sd.zero.bypass If standard deviation is zero, return a vector of zeros.
#' @param Mean User specified mean value.
#' @param SD User specified standard deviation value.
#' @param Median User specified median value. Only used for imputation (i.e. if na.replace = T).
#'
#' @details Feature scaling is done so that feature values are comparable across features. 
#' 
#' For regularization when fitting a generalized linear model (e.g. with \pkg{glmnet}), feature scaling must be done to 
#' ensure that regularization penalties are applied with the same magnitude across features. 
#' 
#' In stochastic gradient descent, feature scaling can improve the convergence speed of the algorithm. 
#' 
#' User specified mean and SD can be useful when standardizing train/test sets, where the \emph{test} set is 
#' standardized using the mean and sd from the \emph{training} set. This is also a helper function for 
#' standardizeTrainTestSets().
#'
#' @return Reurns a vector of standardized values
#' @export
#'
#' @examples standardizeFeature(c(1,1,3,5,7,11))
#' standardizeFeature(c(1,1,0,2,3,2,1,NA), na.replace = 'median')
#' standardizeFeature(c(1,1,1,1,1), sd.zero.replace = T)

standardizeFeature <- function(x, na.replace = F, sd.zero.bypass = F, 
                               Mean = NULL, SD = NULL, Median = NULL)
{
   ## Allow user specified mean and SD, and median (only for imputation)
   if( is.null(Mean) ){
      Mean <- mean(x, na.rm = T)
   }
   if( is.null(SD) ){
      SD <- sd(x, na.rm = T)
   }
   if( is.null(Median) ){
      Median <- median(x, na.rm = T)
   }

   ## Imputation of NAs
   if(na.replace == 'median' | na.replace == T){
      x[is.na(x)] <- Median
   } else if(na.replace == 'mean') {
      x[is.na(x)] <- Mean
   } else if(na.replace == F & sum(is.na(x)) >= 1){
      stop('Feature contains NAs. Impute with na.replace = \'mean\' or \'median\'.')
   }

   ## Return vector of zeros if SD = 0
   if(SD == 0 & sd.zero.bypass == F){
      stop('Feature standard deviation is 0. Bypass this error by using sd.zero.bypass = T (returns vector of zeros).')
   } else if(SD == 0 & sd.zero.bypass == T){
      norm_x <- rep(0, length(x))
   } else {
      norm_x <- (x - Mean)/SD
   }

   return(norm_x)
}
