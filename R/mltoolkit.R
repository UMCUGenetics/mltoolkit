classifierPerf.env <- new.env()

classifierPerf.env$main$acc <- list(
   labels = c('acc', 'accuracy'),
   f = function(tp,tn,fp,fn){ (tp + tn)/(tp + tn + fp + fn) }
)

classifierPerf.env$main$tpr <- list(
   labels = c('tpr', 'true positive rate',
              'sens', 'sensivity',
              'rec', 'recall'),
   f = function(tp,tn,fp,fn){ tp / (tp + fn) }
)

classifierPerf.env$main$tnr <- list(
   labels = c('tnr', 'true negative rate',
              'spec', 'specificity'),
   f = function(tp,tn,fp,fn){ tn / (tn + fp) }
)

classifierPerf.env$main$fpr <- list(
   labels = c('fpr', 'false positive rate'),
   f = function(tp,tn,fp,fn){ fp / (fp + tn) }
)

classifierPerf.env$main$fnr <- list(
   labels = c('fnr', 'false negative rate'),
   f = function(tp,tn,fp,fn){ fn / (fn + tp) }
)

classifierPerf.env$main$ppv <- list(
   labels = c('ppv', 'positive predictive value',
              'prec', 'precision'),
   f = function(tp,tn,fp,fn){ tp / (tp + fp) }
)

classifierPerf.env$main$npv <- list(
   labels = c('npv', 'negative predictive value'),
   f = function(tp,tn,fp,fn){ tn / (tn + fp) }
)

classifierPerf.env$main$f1 <- list(
   labels = c('f1', 'f1 score'),
   f = function(tp,tn,fp,fn){       
      (tp*tn - fp*fn) / sqrt( (tp+fp) * (tp+fn) * (tn+fp) * (tn+fn) )
   }
)

classifierPerf.env$main$mcc <- list(
   labels = c('mcc', 'matthews correlation coefficient'),
   f = function(tp,tn,fp,fn){       
      ppv <- tp / (tp + fp)
      tpr <- tp / (tp + fn)
      (2 * ppv * tpr) / (ppv + tpr) 
   }
)

classifierPerf.env$internalNamesOrder <- unlist( lapply(classifierPerf.env$main, function(i){ i$labels }), use.names = F )

