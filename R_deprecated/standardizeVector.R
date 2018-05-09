#' Feature standardization
#'
#' @description Standardize a vector with the following transformation: (x - mean)/sd. Standardization makes the values of each
#' feature in the data have zero-mean.
#'
#' @param x A numeric/integer vector
#' @param Mean User specified mean value
#' @param SD User specified standard deviation value
#' @param Median User specified median value. Only used for imputation (i.e. if na.replace = T)
#' @param na.replace Imputation by median (TRUE or 'median') or mean ('mean').
#' @param sd.zero.replace If standard deviation is zero, return a vector of zeros
#'
#' @details This is used for feature scaling so that values are comparable across features. In stochastic gradient descent,
#' feature scaling can improve the convergence speed of the algorithm.
#'
#' User specified mean and sd can be useful when standardizing train/test sets, where the \emph{test} set is standardized using
#' the mean and sd from the \emph{training} set
#'
#' @return Reurns a vector of standardized values
#' @export
#'
#' @examples standardizeVector(c(1,1,3,5,7,11))
#' standardizeVector(c(1,1,0,2,3,2,1,NA), na.replace = 'median')
#' standardizeVector(c(1,1,1,1,1), sd.zero.replace = T)

standardizeVector <- function(x, Mean = NULL, SD = NULL, Median = NULL, na.replace = F, sd.zero.replace = F)
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
      stop('Vector contains NAs. Impute with na.replace = \'mean\' or \'median\'')
   }

   ## Return vector of zeros if SD = 0
   if(SD == 0 & sd.zero.replace == F){
      stop('Standard deviation = 0. Use sd.zero.replace =T to return vector of zeros of length(input vector)')
   } else if(SD == 0 & sd.zero.replace == T){
      norm_x <- rep(0, length(x))
   } else {
      norm_x <- (x - Mean)/SD
   }

   return(norm_x)
}
