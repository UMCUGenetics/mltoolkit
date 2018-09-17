#' Calculate classifier performance metrics (multi-class)
#'
#' @param confusion A matrix, data frame; or a list of matrices or data frames. These should contain a confusion matrix for
#' one cutoff or confusion matrices for multiple cutoffs.
#' @param metrics A character vector of the desired statistics. Multiple metrics can be specified; however, usually only two are used for
#' binary classification statistics plots.
#' @param avg.method 'macro': Simple average. Performance metrics are calculated individually for each class, and then averaged.
#' 'weighted': Weighted average. Similar to 'macro', except that the performance metrics for each class are weighted by the 
#' relative contribution of each class (i.e. classes with more samples have more weighting)
#' @param melt If melt = FALSE, output in wide format. If melt = TRUE, output in long format
#' @return A matrix or data frame
#' @export
#' 
#' @examples classifierPerf(confusion, c('tpr','tnr), avg.method = 'macro')

classifierPerfMC <- function(confusion, metrics, avg.method = NULL, melt = F){

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
   
   if(is.matrix(confusion) || is.data.frame(confusion)){
      perfs <- apply(confusion, 1, function(row){
         classifierPerf(row, metrics = metrics)
      })
      perfs <- t(perfs)
      
      if(is.null(avg.method)){ return(perfs) } 
      else if(avg.method == 'macro'){ return(colMeans(perfs)) }
      else if(avg.method == 'weighted'){
         response_contribs <- apply(confusion, 1, function(row){
            row['tp'] + row['fn']
         })
         response_contribs <- response_contribs/sum(response_contribs)
         response_contribs <- response_contribs[rownames(confusion)]
         
         return( apply(perfs, 2, function(col){ col * response_contribs }) )
      }
   }
   
   if(is.list(confusion)){
      
      if(is.null(avg.method)){
         
         perfs <- lapply(confusion, function(i){ 
            perf_mat <- classifierPerf(i, metrics = metrics) 
            
            if(melt){
               perf_mat <- melt(as.data.frame(perf_mat), id.vars = 'cutoff')
               colnames(perf_mat) <- c('cutoff', 'metric', 'statistic')
            }
            
            return(perf_mat)
         
         })
         
         return(perfs)
      } 
      
      else {
         cutoffs <- confusion[[1]][,'cutoff']
         
         perfs <- lapply(confusion, function(i){ 
            perf_mat <- classifierPerf(i, metrics = metrics)
            perf_mat[, colnames(perf_mat) != 'cutoff']
         })
         
         if(avg.method == 'macro'){
            out <- Reduce('+', perfs)/length(perfs)
         } 
         
         else if(avg.method == 'weighted'){
            if(is.null(responses.expected)){
               stop('For avg.method = \'weighted\', responses.expected is required')
            }
            response_contribs <- sapply(confusion, function(i){
               i[i[,'cutoff'] == 0, 'tp']
            })
            response_contribs <- response_contribs/sum(response_contribs)
            response_contribs <- response_contribs[names(confusion)]
            
            out <- lapply(names(perfs), function(i){ 
               perfs[[i]] * response_contribs[i]
            })
            
            out <- Reduce('+', out)
         }
         
         out <- cbind(cutoff = cutoffs, out)

         if(melt){
            out <- melt(as.data.frame(out), id.vars = 'cutoff')
            colnames(out) <- c('cutoff', 'metric', 'statistic')
            
            if(length(metrics) == 1){ out$metric <- metrics }
         }
         
         return(out)
      }
   }
   
}
