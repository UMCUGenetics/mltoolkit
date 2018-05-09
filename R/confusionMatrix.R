#' Calculate confusion matrix
#'
#' @param probs.predicted A numeric vector of prediction probabilities.
#' @param logicals.expected A character vector of response labels. If provided, dots will be coloured by these labels.
#' @param cutoff If provided, a horizontal line will be drawn at the specified value to indicate the chosen cutoff.
#'
#' @return A vector or matrix
#' @export
confusionMatrix <- function(probs.predicted, logicals.expected, cutoff = 0.5, as.vector = F){
   # probs.predicted = rf_agg_pred$BRCA
   # logicals.expected = rf_agg_pred$response
   # cutoff = 0.558
   
   tp <- sum(probs.predicted >= cutoff & logicals.expected == 1)
   fp <- sum(probs.predicted >= cutoff & logicals.expected == 0)
   
   tn <- sum(probs.predicted < cutoff & logicals.expected == 0)
   fn <- sum(probs.predicted < cutoff & logicals.expected == 1)
   
   if(as.vector == T){
      v <- 
         c(tp = tp,
           tn = tn,
           fp = fp,
           fn = fn
         )
      return(v)
   }
   
   else if(as.vector == F){
      m <- rbind(
         c(tp, fn),
         c(fp, tn)
      )
      rownames(m) <- c('p\'', 'n\'')
      colnames(m) <- c('p', 'n')
      return(m)
   }
}