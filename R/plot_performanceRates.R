#' Line plot of binary classification statistics
#'
#' @description Plots the rates of binary classification statistics (e.g. true positive rate, true negative rate) as a line plot.
#'
#' @param probs.predicted A numeric vector of prediction probabilities.
#' @param logicals.expected A logical vector/factor stating the expected outcome of each prediction.
#' @param metrics A character vector of the desired statistics. See documentation for \pkg{ROCR:performance()} for more
#' information of the available statistics. Multiple metrics can be specified; however, usually only two are used for
#' binary classification statistics plots.
#' @param title User specified plot title. If unspecified, the plot title defaults to the names of the statistics chosen.
#' @param show.intersection Show the intersection of 2 performance metrics.
#' @param intersection.only Only return the intersection value (cutoff value)
#'
#' @return Returns a ggplot2 line plot
#' @export
#'
plot_performanceRates <- function(probs.predicted, logicals.expected, metrics=c('tpr','tnr'), title = NULL, show.intersection = F, intersection.only = F)
{
   df_melt <- performanceAsDf(probs.predicted, logicals.expected, metrics, melt = T)
   
   diff_perfRate <-
      abs(
         df_melt[df_melt$metric == metrics[1],'statistic'] - df_melt[df_melt$metric == metrics[2],'statistic']
      ) %>% .[!is.na(.)]
   
   intersection <-
      which(diff_perfRate == min(diff_perfRate)) %>%
      df_melt[.,'cutoff'] %>%
      signif(.,3)
   
   if(intersection.only == T){
      return(intersection)
   
   } else {
      plot <- ggplot(data=df_melt, aes(x=cutoff, y=statistic, colour=metric)) +
         geom_line() +
         
         xlab('Probability cutoff') +
         ylab('Fractional value') +
         scale_color_discrete(name='',labels = levels(df_melt$metric)) +
         
         theme(plot.title = element_text(hjust = 0.5))
      
      if( !is.null(title) ){
         plot <- plot + ggtitle(title)
      }
      
      if(show.intersection == T){
         plot <- plot +
            geom_vline(xintercept = intersection,linetype = 3,colour = 'black') +
            annotate('text', x=intersection*0.95, y=0.07, hjust = 1, vjust = 0.5, label=paste0('P = ',intersection))
      }
      
      return(plot) 
   }
}

