calcPerfCompound.env <- new.env()

calcPerfCompound.env$roc <- list(
   labels = c('receiver operating characteristic', 'roc'),
   metrics = c('fpr','tpr'),
   #coords.start=c(cutoff=1,fpr=0,tpr=0),
   #coords.end=c(cutoff=0,fpr=1,tpr=1),
   m.adj.func=NULL,
   rev.order=T,
   plot.additions=function(plot){ 
      plot + 
         ggtitle('ROC') +
         xlab('False positive rate') + ylab('True positive rate') +  
         geom_abline(intercept=0, slope=1, linetype=3)
   }
)

calcPerfCompound.env$pr <- list(
   labels = c('precision recall', 'pr'),
   metrics = c('tpr','ppv'),
   #coords.start=c(cutoff=0,tpr=1,ppv=0),
   #coords.end=c(cutoff=1,tpr=0,ppv=1),
   m.adj.func=function(m){
      m[m[,'tpr']==0 & m[,'ppv']==0,'ppv'] <- 1 
      return(m)
   },
   rev.order=T,
   plot.additions=function(plot){ 
      plot + 
         ggtitle('Precision-recall') +
         xlab('Recall (TPR)') + ylab('Precision (PPV)') + 
         geom_hline(yintercept=0.5, linetype=3)
   }
)

calcPerfCompound.env$npv_tnr <- list(
   labels = c('npv_tnr'),
   metrics = c('tnr','npv'),
   #coords.start=c(cutoff=0,tnr=0,npv=1),
   #coords.end=c(cutoff=1,tnr=1,npv=0),
   m.adj.func=NULL,
   rev.order=F,
   plot.additions=function(plot){ 
      plot + 
         ggtitle('NPV-TNR') +
         xlab('True positive rate') + ylab('Negative predictive value') + 
         geom_hline(yintercept=0.5, linetype=3)
   }
)

####################################################################################################
#' Calculate compound performance metrics (e.g. roc)
#'
#' @param confusion A matrix or data frame with each row being a confusion matrix. The output of 
#' confusionMatrix(probs.predicted, logicals.expected, cutoff = 'all')
#' @param compound.metric The name of the compound metric
#' @param avg.method How to average performance metrics in multiclass classification. Can be NULL, 
#' 'weighted' or 'macro'
#' @param metric.names.as.x.y If TRUE, will convert colnames: 'tpr' and 'ppv', to: 'x' and 'y'
#' 
#' @return A matrix of the two performance metrics that the compound metric is composed of
#' @export
calcPerfCompound <- function(confusion, compound.metric, avg.method=NULL, metric.names.as.x.y=F){
   
   #compound.metric='pr'
   
   if(length(compound.metric)>1){ stop('Please provide only one compound.metric') }
   
   ## Find compount metric recipe
   metric_names_lookup <- lapply(calcPerfCompound.env,`[[`,'labels')
   metric_internal_name <- names(metric_names_lookup)[ 
      sapply(metric_names_lookup, function(j){ compound.metric %in% j }) 
   ]
   
   recipe <- calcPerfCompound.env[[metric_internal_name]]
   
   ## Calculate the 2 relevant performance metrics
   if( is.list(confusion) & !is.data.frame(confusion) ){
      if(is.null(avg.method)){
         stop('Please provide avg.method for aggregating multiclass performance')
      }
      m <- calcPerfMC(confusion, metrics=c(recipe$metrics[1],recipe$metrics[2]), avg.method=avg.method)
   } else {
      m <- calcPerf(confusion, metrics=c(recipe$metrics[1],recipe$metrics[2]))
   }
   
   ## Custom adjustment to the confusion matrix based on metric
   if(!is.null(recipe$m.adj.func)){
      m <- recipe$m.adj.func(m)
   }
   
   ## Add start/end coords so that the line plot starts/ends at the corners
   #m <- rbind(recipe$coords.start, m, recipe$coords.end)
   m <- unique(m) ## start/end coords already exist, remove duplicates
   
   ## Reverse order for certain metrics so that AUC calculation is not negative
   #m <- m[order(m[,'cutoff'], decreasing=recipe$rev.order),]
   
   ## Export
   if(metric.names.as.x.y){
      colnames(m)[2:3] <- c('x','y')
   }
   
   class(m) <- c(class(m), metric_internal_name)
   
   return(m)
}
#calcPerfCompound(confusion, 'pr')

