#' Plot ROC curve
#'
#' @param m A matrix or data frame with each row being a confusion matrix. The output of 
#' confusionMatrix(probs.predicted, logicals.expected, cutoff = 'all')
#' @param title User specified plot title.
#' @param show.auc Default: TRUE. Shows the AUC on the plot.
#' @param auc.only If TRUE, only returns the AUC value
#'
#' @return Returns a ggplot2 line plot
#' @export
#'

plotROC <- function(confusion, title=NULL, show.auc=T, auc.only = F, avg.method = NULL)
{
   if( all(c('confusion.multiclass','list') %in% class(confusion)) ){
      if(is.null(avg.method)){
         stop('Please provide avg.method for aggregating multiclass performance')
      }
      df <- classifierPerfMC(confusion, metrics = c('fpr','tpr'), avg.method = avg.method)
   } else {
      df <- classifierPerf(confusion = confusion, metrics = c('fpr','tpr'))
   }
   
   df <- as.data.frame(df)
   df <- df[rev(order(df$cutoff)),]
   
   auc <- calcAuc(
      c(0, df$fpr), 
      c(0, df$tpr)
   )
   
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
                                       label=paste0('AUC-ROC = ', auc %>% round(.,4)))
      }
      
      return(plot)
   }
}

