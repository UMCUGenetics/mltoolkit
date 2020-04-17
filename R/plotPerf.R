#' Line plot of classification statistics
#'
#' @description Plots the classification statistics (e.g. true positive rate, true negative rate) as a line plot.
#'
#' @param m A matrix or data frame with each row being a confusion matrix. The output of 
#' confusionMatrix(probs.predicted, logicals.expected, cutoff = 'all')
#' @param metrics A character vector of the desired statistics.  Multiple metrics can be specified.
#' @param title User specified plot title. If unspecified, the plot title defaults to the names of the statistics chosen.
#' @param show.intersection Show the intersection of 2 performance metrics.
#' @param intersection.only Only return the intersection value (cutoff value)
#' @param legend.in.plot Put legend within plot area?
#'
#' @return Returns a ggplot2 line plot
#' @export
plotPerf <- function(
   confusion, metrics=c('tpr','tnr'), avg.method=NULL, title=NULL, 
   show.intersection=F, intersection.only=F, legend.in.plot=T
){
   #confusion=confusion_mc$BRCA1
   if( all(c('confusion.multiclass','list') %in% class(confusion)) ){
      if(is.null(avg.method)){
         stop('Please provide avg.method for aggregating multiclass performance')
      }
      df_melt <- calcPerfMC(confusion, metrics, avg.method=avg.method, melt=T)
   } else {
      df_melt <- calcPerf(confusion, metrics, melt=T)
   }
   
   df_melt <- as.data.frame(df_melt)
   
   ## Calculate intersection line
   if(show.intersection || intersection.only){
      if(length(metrics)==2){
         diff_perfRate <- abs(
            df_melt[df_melt$metric == metrics[1],'statistic'] - df_melt[df_melt$metric == metrics[2],'statistic']
         )
         diff_perfRate <- diff_perfRate[!is.na(diff_perfRate)]
         
         intersection <- df_melt[ which(diff_perfRate == min(diff_perfRate)) ,'cutoff']
         intersection <- signif(intersection, 4)
      }
      
      if(intersection.only){
         return(intersection)
      }
   }
   
   ## Main
   plot <- ggplot(data=df_melt, aes(x=cutoff, y=value, color=metric)) +
      geom_line() +
      
      xlab('Cutoff') + xlim(0,1) +
      ylab('Metric value') + ylim(0,1) +
      scale_color_discrete(name='Metrics',labels = levels(df_melt$metric)) + 
      theme_bw()
   
   if(!is.null(title)){
      plot <- plot + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
   }
   
   if(legend.in.plot){
      plot <- plot + theme(
         legend.position=c(0.5,0.065),
         legend.justification=c(0.5,0),
         legend.background=element_rect(color='black',size=0.2,fill=alpha('white',0.8)),
         legend.key = element_rect(size=0.2,fill=NA)
      )
   }
   
   if(length(metrics)==2 && show.intersection){
      plot <- plot +
         geom_vline(xintercept = intersection,linetype = 3,colour = 'black') +
         annotate('text', x=intersection*0.95, y=0.07, hjust = 1, vjust = 0.5, label=paste0('P = ',intersection))
   }
   
   return(plot) 
   
}