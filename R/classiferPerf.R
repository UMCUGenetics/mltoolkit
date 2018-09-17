#' Calculate classifier performance metrics
#'
#' @param confusion A numeric vector or matrix or data frame. These should contain a confusion matrix for one cutoff or 
#' confusion matrices for multiple cutoffs.
#' @param metrics A character vector of the desired statistics. Multiple metrics can be specified.
#' @param melt If melt = FALSE, output in wide format. If melt = TRUE, output in long format
#' 
#' @return A numeric vector; or matrix or data frame
#' @export
#' 
#' @examples classifierPerf(confusion, c('tpr','tnr))

classifierPerf <- function(confusion, metrics, melt = F){
   
   metrics_sorted <- classifierPerf.env$internalNamesOrder[classifierPerf.env$internalNamesOrder %in% metrics]
   
   metrics_internal_names <- 
      names(classifierPerf.env$main)[
         sapply(classifierPerf.env$main, function(i){
            if( any(i$labels %in% metrics_sorted) ){ TRUE }
            else { FALSE }
         })
      ]

   if(length(metrics) != length(metrics_internal_names)){
      stop('Multiple different names for the same performance metric were provided')
   }
   
   if(is.vector(confusion)) {
      perfs <- lapply(metrics_internal_names, function(label){
         classifierPerf.env$main[[label]]$f( confusion['tp'], confusion['tn'], confusion['fp'], confusion['fn'] )
      })
      
      if(length(perfs) >= 2){
         perfs <- unlist(perfs)
         names(perfs) <- metrics_sorted
      } else {
         names(perfs) <- NULL
      }
      
   } else if (is.matrix(confusion) || is.data.frame(confusion)){
      perfs <- lapply(metrics_internal_names, function(label){
         apply(confusion,1,function(row){
            classifierPerf.env$main[[label]]$f( row['tp'], row['tn'], row['fp'], row['fn'] )
         })
      })
      names(perfs) <- metrics_sorted
      
      if(melt == T){
         perfs <- lapply(names(perfs), function(i){
            data.frame(
               cutoff = confusion[,'cutoff'],
               statistic = perfs[[i]],
               metric = i
            )
         })
         perfs <- do.call(rbind, perfs)

      } else {
         perfs <- do.call(cbind, perfs)
         perfs <- cbind(cutoff = confusion[,'cutoff'], perfs)
      }
   }

   return(perfs)
}
