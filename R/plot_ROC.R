#' Plot ROC curve
#'
#' @param probs_predicted A numeric vector of prediction probabilities.
#' @param logicals.expected A logical vector/factor stating the expected outcome of each prediction.
#' @param title User specified plot title. If unspecified, the plot title defaults to 'ROC'.
#' @param show.auc Default: TRUE. Shows the AUC on the plot.
#' @param auc.only If TRUE, only returns the AUC value
#'
#' @return Returns a ggplot2 line plot
#' @export
#'
plot_ROC <- function(probs_predicted, logicals_expected, title=NULL, show.auc=T, auc.only = F)
{
   df <- performanceAsDf(probs_predicted, logicals_expected, metrics=c('tpr','fpr'), melt = F)
   
   auc <-
      prediction(probs_predicted, logicals_expected) %>%
      performance(.,'auc') %>%
      .@y.values %>%
      unlist()
   
   if(auc.only == T){
      return(auc)
   
   } else {
      plot <- ggplot(data=df, aes(x=fpr, y=tpr)) +
         geom_line() +
         geom_abline(intercept = 0, slope = 1, linetype=3) +
         
         xlab('False positive rate') +
         ylab('True positive rate') +
         theme(plot.title = element_text(hjust = 0.5))
      
      if( !is.null(title) ){
         plot <- plot + ggtitle(title)
      }
      
      if(show.auc == T){
         plot <- plot + annotate('text', x=0.5, y=0.05,
                                       hjust = 0.5, vjust = 0.5,
                                       label=paste0('AUC-ROC = ', auc %>% round(.,3)))
      }
      
      return(plot)
   }
}
