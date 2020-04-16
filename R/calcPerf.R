#' Calculate classifier performance metrics
#'
#' @param confusion A numeric vector or matrix or data frame. These should contain a confusion matrix for one cutoff or 
#' confusion matrices for multiple cutoffs.
#' @param metrics A character vector of the desired statistics. Multiple metrics can be specified.
#' @param melt If melt = FALSE, output in wide format. If melt = TRUE, output in long format
#' 
#' @return A numeric vector or matrix of the selected performance metrics
#' @export
#' 
#' @examples calcPerf(confusion, c('tpr','tnr'))

calcPerf.env <- new.env()

calcPerf.env$acc <- list(
   labels = c('accuracy', 'acc'),
   f = function(tp,tn,fp,fn){ (tp + tn)/(tp + tn + fp + fn) },
   start.end.values=c(0,NA),
   na.value=NULL
)

calcPerf.env$tpr <- list(
   labels = c('true positive rate','tpr',
              'sens', 'sensivity',
              'rec', 'recall'),
   f = function(tp,tn,fp,fn){ tp / (tp + fn) },
   start.end.values=c(1,0),
   na.value=NULL
)

calcPerf.env$tnr <- list(
   labels = c('true negative rate', 'tnr',
              'spec', 'specificity'),
   f = function(tp,tn,fp,fn){ tn / (tn + fp) },
   start.end.values=c(0,1),
   na.value=NULL
)

calcPerf.env$fpr <- list(
   labels = c('false positive rate','fpr'),
   f = function(tp,tn,fp,fn){ fp / (fp + tn) },
   start.end.values=c(1,0),
   na.value=NULL
)

calcPerf.env$fnr <- list(
   labels = c('false negative rate','fnr'),
   f = function(tp,tn,fp,fn){ fn / (fn + tp) },
   start.end.values=c(0,1),
   na.value=NULL
)

calcPerf.env$ppv <- list(
   labels = c('positive predictive value','ppv',
              'precision','prec'),
   f = function(tp,tn,fp,fn){ tp / (tp + fp) },
   start.end.values=c(0,1),
   na.value=1
)

calcPerf.env$npv <- list(
   labels = c('negative predictive value','npv'),
   f = function(tp,tn,fp,fn){ tn / (tn + fn) },
   start.end.values=c(1,0),
   na.value=1
)

calcPerf.env$f1 <- list(
   labels = c('f1 score','f1'),
   f = function(tp,tn,fp,fn){       
      (tp*tn - fp*fn) / sqrt( (tp+fp) * (tp+fn) * (tn+fp) * (tn+fn) )
   },
   start.end.values=c(0,0),
   na.value=0
)

calcPerf.env$mcc <- list(
   labels = c('matthews correlation coefficient','mcc'),
   f = function(tp,tn,fp,fn){       
      ppv <- tp / (tp + fp)
      tpr <- tp / (tp + fn)
      (2 * ppv * tpr) / (ppv + tpr) 
   },
   start.end.values=c(0,0),
   na.value=0
)

#calcPerf(confusion_mc$BRCA1, c('tpr','fpr','prec'))
calcPerf <- function(confusion, metrics, melt=F, add.start.end.values=T){
   
   #metrics <- c("fpr", "rec", "tpr")
   #confusion <- confusion$chord
   metric_names_lookup <- lapply(calcPerf.env, function(i){ i$labels })
   
   metric_internal_names <- sapply(metrics, function(i){
      names(metric_names_lookup)[ sapply(metric_names_lookup, function(j){ i %in% j }) ]
   })
   
   if(length(metrics) != length(metric_internal_names)){
      warning('Different names for the same performance metric were provided. Taking only the first instance of duplicate metrics')
      metric_internal_names <- metric_internal_names[!duplicated(metric_internal_names)]
   }
   
   if(is.vector(confusion)) {
      perfs <- lapply(metric_internal_names, function(label){
         recipe <- calcPerf.env[[label]]
         
         v <- recipe$f( confusion['tp'], confusion['tn'], confusion['fp'], confusion['fn'] )
         if(!is.null(recipe$na.value)){ v[is.na(v)] <- recipe$na.value }
         
         return(v)
      })
      
      if(length(perfs) >= 2){
         perfs <- unlist(perfs)
         names(perfs) <- names(metric_internal_names)
      } else {
         names(perfs) <- NULL
      }
   
   } else if(is.matrix(confusion) || is.data.frame(confusion)){
      if (is.data.frame(confusion)){
         confusion <- as.matrix(confusion)
      }
      
      perfs <- lapply(metric_internal_names, function(label){
         #label='tpr'
         recipe <- calcPerf.env[[label]]
         v <- apply(confusion,1,function(row){
            #row=confusion[1,]
            value <- recipe$f( row['tp'], row['tn'], row['fp'], row['fn'] )
            if(!is.null(recipe$na.value)){ value[is.na(value)] <- recipe$na.value }
            return(value)
         })
         
         if(!is.null(recipe$start.end.values) & add.start.end.values){
            
            if(!is.na(recipe$start.end.values[1])){
               start_value <- recipe$start.end.values[1]
            } else {
               start_value <- v[1]
            }
            
            if(!is.na(recipe$start.end.values[2])){
               end_value <- recipe$start.end.values[2]
            } else {
               end_value <- v[length(v)]
            }
            
            v <- c(start_value,v,end_value)
         }
         
         return(v)
      })
      names(perfs) <- names(metric_internal_names)
      
      m <- do.call(cbind, perfs)
      cutoffs <- confusion[,'cutoff']
      
      if(add.start.end.values){ cutoffs <- c(0,cutoffs,1) }
      
      m <- cbind(cutoff=cutoffs, m)
      
      if(melt==T){
         m <- do.call(rbind,lapply(colnames(m)[-1], function(i){
            data.frame(cutoff=m[,1], metric=i,value=m[,i])
         }))
         m$metric <- factor(m$metric, levels=metrics)
      }
      
      return(m)
   }

   return(perfs)
}
