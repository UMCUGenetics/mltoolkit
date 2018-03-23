#' Calculate binary classification statistics
#'
#' @description Uses \pkg{ROCR::prediction()} and \pkg{ROCR::performance()} to calculate classification statistics and outputs
#' a long or wide format dataframe suitable for plotting with \pkg{ggplot2}.
#'
#' @param probs.predicted A numeric vector of prediction probabilities.
#' @param logicals.expected A logical vector/factor stating the expected outcome of the observation.
#' @param metrics Default: c('tpr','tnr'). A character vector of the desired classification statistics. See documentation for
#' \pkg{ROCR:performance()} for more information of the available statistics.
#' @param melt If melt = FALSE, output in wide format. If melt = TRUE, output in long format
#'
#' @return Returns a long format dataframe of the desired statistics.
#' @export
#'
performanceAsDf <- function(probs.predicted, logicals.expected, metrics=c('tpr','tnr'), melt = F)
{
   if(length(probs.predicted) != length(logicals.expected)){
      stop('probs.predicted and logicals.expected must be of the same length')
   }
   
   if( !all(logicals.expected %in% c(0,1,TRUE,FALSE)) ) {
      stop('probs.predicted must be a vector of logicals (TRUE/FALSE or 0/1)')
   }

   ROCRPred <- ROCR::prediction(probs.predicted, logicals.expected)

   perf_x_values <- ROCRPred@cutoffs %>% unlist()
   perf_y_values <- lapply(metrics, function(i){ performance(ROCRPred, measure = i)@y.values %>% unlist() })
   names(perf_y_values) <- metrics

   if(melt == F){

      df_perfRates <- do.call(cbind, perf_y_values) %>% cbind(perf_x_values,.) %>% as.data.frame()
      colnames(df_perfRates)[1] <- 'cutoff'

   } else if(melt == T){

      df_perfRates <- lapply(metrics,function(i){
         df_metric <- cbind(
            performance(ROCRPred, measure = i)@x.values %>% unlist(),
            performance(ROCRPred, measure = i)@y.values %>% unlist()
         ) %>% as.data.frame()

         colnames(df_metric) <- c('cutoff','statistic')
         df_metric$metric <- i %>% as.factor()

         return(df_metric)
      }) %>% do.call(rbind,.)

   }

   return(df_perfRates)
}
