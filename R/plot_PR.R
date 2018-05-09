#' Plot precision recall
#'
#' @param probs.predicted A numeric vector of prediction probabilities.
#' @param logicals.expected A logical vector/factor stating the expected outcome of each prediction.
#' @param title User specified plot title. If unspecified, the plot title defaults to 'PR'.
#' @param show.auc Shows the area under the under value.
#'
#' @return Returns a ggplot2 line plot
#' @export

plot_PR <- function(probs.predicted, logicals.expected, title = NULL, show.auc = T, auc.only = F)
{
   df <- performanceAsDf(probs.predicted, logicals.expected, metrics=c('tpr','ppv'))

   ## Assign point (0,1) as first point
   df[df$tpr==0,]$ppv <- 1
   
   ## Calculate AUC
   df_aucCalc <-
      df[df$tpr != 1,c('tpr','ppv')] %>%
      rbind(.,c(1,0)) ## Set (1,0) as last coordinate (set last y value to 0)
   
   auc <- integrateDiscrete(df_aucCalc$tpr, df_aucCalc$ppv) ## No need to divide by max(x)*max(y); lims are already (1,1)
   
   if(auc.only == T){
      return(auc)
   
   } else {
      plot <- ggplot(data=df, aes(x=tpr, y=ppv)) +
         geom_line() +
         geom_hline(yintercept = 0.5, linetype=3) +
         
         ylim(0,1) +
         
         ylab('Positive predictive value') +
         xlab('True positive rate') +
         
         theme(plot.title = element_text(hjust = 0.5))
      
      if( !is.null(title) ){
         plot <- plot + ggtitle(title)
      }
      
      ## Calculate AUC
      if(show.auc == T){
         plot <- plot + annotate('text', x=0.5, y=0.05,
                                     hjust = 0.5, vjust = 0.5, label=paste0('AUC-PR = ', auc %>% round(.,3)))
      }
      
      return(plot)
   }
}



