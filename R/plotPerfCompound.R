#' Plot compound performance metrics
#'
#' @param confusion A matrix or data frame with each row being a confusion matrix. The output of 
#' confusionMatrix(probs.predicted, logicals.expected, cutoff = 'all')
#' 'weighted' or 'macro'
#' @param jagged.rough.fix Fixed the jagged line when tpr=0 and ppv=0
#' @param metric.names.as.x.y If TRUE, will convert colnames: 'tpr' and 'ppv', to: 'x' and 'y'
#' @param ... Other arguments that can be passed to calcPerfCompound()
#' 
#' @return Returns a precision-recall matrix compatible for plotting and calculating AUC
#' @export
plotPerfCompound <- function(
   confusion, compound.metric='roc', title=NULL, 
   show.auc=T, auc.rounding=3, show.cutoff.colors=T, legend.in.plot=T, ...
){
   df <- calcPerfCompound(confusion,compound.metric, metric.names.as.x.y=T, ...)
   #confusion=confusion_mc$BRCA1
   #df <- calcPerfCompound(confusion,'pr', metric.names.as.x.y=T)
   
   metric_internal_name <- class(df)[2]
   df <- as.data.frame(df)
   
   ## Base plot
   if(show.cutoff.colors){
      plot <- ggplot(df, aes(x, y, color=cutoff)) +
         geom_path(size=1.2) +
         scale_colour_distiller(palette='Spectral', name='Cutoff')
   } else {
      plot <- ggplot(data=df, aes(x, y)) + geom_line()
   }
   
   ## Variable plot settings
   plot <- calcPerfCompound.env[[metric_internal_name]]$plot.additions(plot)
   
   ## Constant plot settings
   plot <- plot + 
      xlim(0,1) + ylim(0,1) +
      theme_bw() +
      theme(plot.title = element_text(hjust=0.5))
   
   
   if( !is.null(title) ){
      plot <- plot + ggtitle(title)
   }
   
   if(legend.in.plot){
      plot <- plot + theme(
         legend.title=element_text(hjust=0.5),
         legend.position=c(0.8, 0.065),
         legend.justification=c(0.5,0),
         legend.background=element_rect(color='black',size=0.2,fill=alpha('white',0.8))
      )
   }
   
   if(show.auc){
      auc <- calcAUC(df$x, df$y)
      if(!is.null(auc.rounding)){
         auc <- format(round(auc,auc.rounding),nsmall=auc.rounding)
      }
      
      plot <- plot + 
         annotate(
            'text', x=0.5, y=0.025, hjust=0.5, vjust=0, 
            label=paste0('AUC = ', auc)
         )
   }
   
   return(plot)
}
#plotPerfCompound(confusion, 'npv_tnr')
