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
   labels = c('positive predictive value','ppv', 'precision','prec'),
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
      prec <- tp / (tp + fp)
      rec <- tp / (tp + fn)
      2 * (prec*rec) / (prec+rec)
   },
   start.end.values=c(0,0),
   na.value=0
)

calcPerf.env$mcc <- list(
   labels = c('matthews correlation coefficient','mcc'),
   f = function(tp,tn,fp,fn){       
      (tp*tn - fp*fn) / sqrt( (tp+fp) * (tp+fn) * (tn+fp) * (tn+fn) )
   },
   start.end.values=c(0,0),
   na.value=0
)

####################################################################################################
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
calcPerf <- function(confusion, metrics, melt=F, add.start.end.values=T){
   
   #metrics <- c("fpr", "rec", "tpr")
   #confusion <- confusion$chord
   metric_names_lookup <- lapply(calcPerf.env, `[[`, 'labels')
   
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
      
      return(perfs)
   
   } else if(is.matrix(confusion) | is.data.frame(confusion)){
      # if (is.data.frame(confusion)){
      #    confusion <- as.matrix(confusion)
      # }
      
      if('class' %in% colnames(confusion)){
         add.start.end.values <- FALSE
      }
      
      perfs <- lapply(metric_internal_names, function(label){
         #label='tpr'
         recipe <- calcPerf.env[[label]]
         v <- apply(confusion[,c('tp','tn','fp','fn')],1,function(row){
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
      
      df <- as.data.frame(perfs)
      
      if('class' %in% colnames(confusion)){
         cutoffs <- NA
      } else {
         cutoffs <- confusion[,'cutoff']
      }
      
      if(add.start.end.values){ 
         cutoffs <- c(0,cutoffs,1) 
      }
      df <- cbind(cutoff=cutoffs, df)
      
      if('class' %in% colnames(confusion)){
         df <- cbind(class=confusion[,'class'], df)
         df$cutoff <- NULL
      }
      
      if(melt==T){
         df <- do.call(rbind,lapply(colnames(df)[-1], function(i){
            data.frame(cutoff=df[,1], metric=i,value=df[,i])
         }))
         df$metric <- factor(df$metric, levels=metrics)
      }
      
      return(df)
   }
}

####################################################################################################
#' Calculate classifier performance metrics (multi-class)
#'
#' @param confusion A matrix, data frame; or a list of matrices or data frames. These should contain a confusion matrix for
#' one cutoff or confusion matrices for multiple cutoffs.
#' @param metrics A character vector of the desired statistics. Multiple metrics can be specified; however, usually only two are used for
#' binary classification statistics plots.
#' @param avg.method 'macro': Simple average. Performance metrics are calculated individually for each class, and then averaged.
#' 'weighted': Weighted average. Similar to 'macro', except that the performance metrics for each class are weighted by the 
#' relative contribution of each class (i.e. classes with more samples have more weighting)
#' @param melt If melt = FALSE, output in wide format. If melt = TRUE, output in long format
#' @return A numeric vector or matrix of the selected performance metrics
#' @export
#' 
#' @examples calcPerf(confusion, c('tpr','tnr'), avg.method = 'macro')
#'
calcPerfMC <- function(confusion, metrics, avg.method = NULL, melt = F){
   
   #metrics = c('tpr','tnr','prec')
   
   if(is.matrix(confusion) || is.data.frame(confusion)){
      perfs <- apply(confusion[,c('tp','tn','fp','fn')], 1, function(row){
         calcPerf(row, metrics = metrics)
      })
      
      if(length(metrics) == 1){ ## hack to force pre-output into matrix if metrics only has one value
         perfs <- do.call(rbind,perfs)
         perfs <- apply(perfs,2,as.numeric)
         
         rownames(perfs) <- rownames(confusion)
         colnames(perfs) <- metrics
      } else {
         perfs <- t(perfs)
      }
      
      if(is.null(avg.method)){ return(perfs) } 
      else if(avg.method == 'macro'){ return(colMeans(perfs)) }
      else if(avg.method == 'weighted'){
         response_contribs <- apply(confusion, 1, function(row){
            row['tp'] + row['fn']
         })
         response_contribs <- response_contribs/sum(response_contribs)
         response_contribs <- response_contribs[rownames(confusion)]
         
         return( apply(perfs, 2, function(col){ col * response_contribs }) )
      }
   }
   
   if(is.list(confusion)){
      
      #confusion=confusion_mc
      
      if(is.null(avg.method)){
         
         perfs <- lapply(confusion, function(i){ 
            perf_mat <- calcPerf(i, metrics = metrics) 
            
            if(melt){
               perf_mat <- melt(as.data.frame(perf_mat), id.vars = 'cutoff')
               colnames(perf_mat) <- c('cutoff', 'metric', 'value')
            }
            
            return(perf_mat)
            
         })
         
         return(perfs)
      } 
      
      else {
         
         perfs <- lapply(confusion, function(i){ calcPerf(i, metrics = metrics) })
         
         ## Store cutoffs generated by calcPerf (added 0 and 1 to the ends)
         cutoffs <- perfs[[1]][,'cutoff']
         
         perfs <- lapply(perfs, function(i){ i[, colnames(i) != 'cutoff'] })
         
         if(avg.method == 'macro'){
            out <- Reduce('+', perfs)/length(perfs)
         } 
         
         else if(avg.method == 'weighted'){
            response_contribs <- sapply(confusion, function(i){
               i[i[,'cutoff'] == 0, 'tp']
            })
            response_contribs <- response_contribs/sum(response_contribs)
            response_contribs <- response_contribs[names(confusion)]
            
            out <- lapply(names(perfs), function(i){ 
               perfs[[i]] * response_contribs[i]
            })
            
            out <- Reduce('+', out)
         }
         
         out <- cbind(cutoff = cutoffs, out)
         
         if(melt){
            out <- melt(as.data.frame(out), id.vars = 'cutoff')
            colnames(out) <- c('cutoff', 'metric', 'value')
            
            if(length(metrics) == 1){ out$metric <- metrics }
         }
         
         return(out)
      }
   }
   
}


