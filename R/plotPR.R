#' Plot precision recall
#'
#' @param m A matrix or data frame with each row being a confusion matrix. The output of 
#' confusionMatrix(probs.predicted, logicals.expected, cutoff = 'all')
#' @param title User specified plot title.
#' @param show.auc Shows the area under the under value.
#' @param auc.only If TRUE, only returns the AUC value
#'
#' @return Returns a ggplot2 line plot
#' @export

plotPR <- function(confusion, title = NULL, show.auc = T, auc.only = F, avg.method = NULL)
{
   if( all(c('confusion.multiclass','list') %in% class(confusion)) ){
      if(is.null(avg.method)){
         stop('Please provide avg.method for aggregating multiclass performance')
      }
      df <- classifierPerfMC(confusion, metrics = c('ppv','tpr'), avg.method = avg.method)
   } else {
      df <- classifierPerf(confusion = confusion, metrics = c('ppv','tpr'))
   }
   
   df <- as.data.frame(df)
   df <- df[rev(order(df$cutoff)),]
   df$ppv[is.na(df$ppv)] <- 1
   
   ## Add starting/ending coordinates
   df <- rbind(c(1,0), df, c(0,1))
   
   auc <- calcAuc(df$tpr, df$ppv)
   
   if(auc.only == T){
      return(auc)
   
   } else {
      plot <- ggplot(data=df, aes(x=tpr, y=ppv)) +
         geom_line() +
         geom_hline(yintercept = 0.5, linetype=3) +
         
         xlim(0,1) + ylim(0,1) +
         
         ylab('Positive predictive value') +
         xlab('True positive rate') +
         
         theme(plot.title = element_text(hjust = 0.5))
      
      if( !is.null(title) ){
         plot <- plot + ggtitle(title)
      }
      
      ## Calculate AUC
      if(show.auc == T){
         plot <- plot + annotate('text', x=0.5, y=0.05,
                                 hjust = 0.5, vjust = 0.5, label=paste0('AUC-PR = ', auc %>% round(.,4)))
      }
      
      return(plot)
   }
}
