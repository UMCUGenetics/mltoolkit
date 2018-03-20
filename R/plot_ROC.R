#' Plot ROC curve
#'
#' @param probs_predicted A numeric vector of prediction probabilities.
#' @param logicals.expected A logical vector/factor stating the expected outcome of each prediction.
#' @param title User specified plot title. If unspecified, the plot title defaults to 'ROC'.
#' @param showAUC Default: TRUE. Shows the AUC on the plot.
#'
#' @return Returns a ggplot2 line plot
#' @export
#'
plot_ROC <- function(probs_predicted, logicals_expected, title=NULL, showAUC=T)
{
   df <- performanceAsDf(probs_predicted, logicals_expected, metrics=c('tpr','fpr'), melt = F)

   ROCPlot <- ggplot(data=df, aes(x=fpr, y=tpr)) +
      geom_line() +
      geom_abline(intercept = 0, slope = 1, linetype=3) +

      xlab('False positive rate') +
      ylab('True positive rate') +
      theme(plot.title = element_text(hjust = 0.5))

   if(is.null(title)){
      ROCPlot <- ROCPlot + ggtitle('ROC')
   } else {
      ROCPlot <- ROCPlot + ggtitle(title)
   }

   if(showAUC == T){
      auc <-
         prediction(probs_predicted, logicals_expected) %>%
         performance(.,'auc') %>%
         .@y.values %>%
         unlist()

      ROCPlot <- ROCPlot + annotate('text', x=0.5, y=min(df$tpr),
                                    hjust = 0.5, vjust = 0.5,
                                    label=paste0('AUC-ROC = ', auc %>% round(.,4)))
   }

   return(ROCPlot)
}
