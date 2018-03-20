#' Logistic regression equation
#'
#' @description Predict the probability of a binary outcome for an observation given the feature values, intercept and coefficients
#'
#' @param featureValues A numeric/integer vector containing values for each feature
#' @param intercept Intercept
#' @param coefs Coefficients for each feature
#'
#' @return Returns a value between 0 and 1 giving the probability of the positive outcome

logisticRegression <- function(featureValues, intercept, coefs)
{
   P <- 1 / (1 + exp(1) ^ -(intercept + sum(featureValues * coefs) ) )
   return( P %>% unname() )
}
