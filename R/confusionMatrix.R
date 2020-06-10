#' Calculate true/false positive/negative counts
#'
#' @param predicted A numeric vector of prediction probabilities, or a logical
#' vector
#' @param actual A logical vector of the actual classes
#' @param cutoff A single classification cutoff from 0 to 1. Only required when
#' `predicted` are probabilities
#'
#' @return A integer vector of the true/false positive/negative counts
#' @export
#'
calcTfpn <- function(
   predicted=NULL, actual=NULL, cutoff=0.5
){
   # cutoff=0.5
   # predicted=as.logical(pred_ohe[,'Lung'])
   # predicted=prob[,'Lung']
   # actual=as.logical(test_data$y_ohe[,'Lung'])
   
   if(!is.logical(actual)){
      stop('`actual` must be a logical vector')
   }
   
   if(!is.logical(predicted)){
      predicted <- predicted >= cutoff
   }
   
   out <- structure(rep(0,4), names=c('tp','tn','fp','fn'))
   
   tfpn <- rep('tn',length(predicted))
   
   tfpn[ predicted==TRUE & actual==TRUE ] <- 'tp'
   tfpn[ predicted==TRUE & actual==FALSE ] <- 'fp'
   
   tfpn[ predicted==FALSE & actual==TRUE ] <- 'fn'
   
   tab <- table(tfpn)
   out[names(tab)] <- tab
   return(out)
}

####################################################################################################
#' Create confusion matrix(-ces)
#'
#' @param predicted A numeric vector of prediction probabilities, or a logical
#' vector
#' @param actual A logical vector of the actual classes
#' @param cutoff A single classification cutoff from 0 to 1. Only required when
#' `predicted` are probabilities
#'
#' @return A integer vector of the true/false positive/negative counts
#' @export
#'
confusionMatrix <- function(
   predicted, actual, 
   cutoff='all', cutoff.interval=0.02, simplify=F
){
   
   ## Test args
   # ## Matrix input
   # predicted=prob
   # predicted=pred_ohe
   # 
   # actual=test_data$y
   # actual=test_data$y_ohe
   # 
   # ## Vector input
   # predicted=prob[,'Lung']
   # predicted=as.logical(pred_ohe[,'Lung'])
   # actual=as.logical(test_data$y_ohe[,'Lung'])
   # 
   # cutoff.interval=0.02
   
   #--------- Make cutoffs ---------#
   if(cutoff=='all'){
      if(!is.null(cutoff.interval)){
         cutoffs <- seq(0,1,cutoff.interval)
      } else {
         cutoffs <- sort(unique(
            c(0,as.vector(as.matrix(predicted)),1)
         ))
      }
      
   } else {
      cutoffs <- cutoff
   }
   
   #--------- Vector input ---------#
   if(is.vector(predicted) & is.vector(actual)){
      
      if(length(predicted) != length(actual)){
         stop('`predicted` and `actual` are different lengths')
      }
      
      if(is.logical(predicted)){
         out <- t(calcTfpn(predicted, actual))
         out <- cbind(cutoff=NA, out)
         return(out)
      } else {
         out <- do.call(rbind, lapply(cutoffs, function(i){
            calcTfpn(predicted, actual, cutoff=i)
         }))
         out <- cbind(cutoff=cutoffs, out)
         return(out)
      }
   }
   
   #--------- Matrix input ---------#
   if(is.matrix(predicted) | is.data.frame(predicted)){
      
      ## Convert actual factor to matrix
      if(!is.factor(actual) & !is.matrix(actual)){
         stop('`actual` must a factor or matrix')
      }
      if(is.factor(actual)){ actual <- oneHotEncode(actual) }
      
      ## Check matrix dimensions
      #print(predicted)
      #print(dim(predicted))
      #print(dim(actual))
      if(nrow(predicted) != nrow(actual)){
         stop('`predicted` and `actual` have differing number of rows')
      }
      if(ncol(predicted) != ncol(actual)){
         print(colnames(predicted))
         print(colnames(actual))
         stop('`predicted` and `actual` have differing number of columns')
      }
      
      ## Main
      if(is.logical(predicted)){
         out <- lapply(1:ncol(predicted),function(i){
            #i=1
            cbind(
               cutoff=NA,
               t(calcTfpn(predicted[,i], actual[,i]))
            )
         })
         names(out) <- colnames(actual)
         
         if(simplify){
            out <- as.data.frame(do.call(rbind,out))
            out <- cbind(class=colnames(predicted), out)
            out$cutoff <- NULL
         }
         
         return(out)
         
      } else {
         out <- do.call(rbind, lapply(cutoffs, function(i){
            #i=0.00
            df <- do.call(rbind, lapply(1:ncol(predicted),function(j){
               #j=1
               calcTfpn(predicted[,j], actual[,j], cutoff=i)
            }))
            
            df <- as.data.frame(df)
            df <- cbind(class=colnames(actual), cutoff=i, df)
            return(df)
         }))
         
         out$class <- factor(out$class, colnames(actual))
         
         if(!simplify){
            out <- lapply(split(out, out$class), function(i){
               i$class <- NULL
               return(i)
            })
         }
         
         return(out)
      }
   }
   
   stop()
}

####################################################################################################
#' Create confusion matrix(-ces) multi-class
#' 
#' @description DEPRECATED; DO NOT USE
#'
#' @param predicted A matrix or data frame containing the prediction probabilities of each class in each column.
#' @param expected A character vector of response labels.
#' @param cutoff A single or vector of classification cutoffs ranging from 0 to 1
#' @param neg.response The names of the negative response(s). These classes will be excluded from the calculations.
#'
#' @return A vector or matrix
#' @export
#' 
confusionMatrixMC <- function(predicted, expected, cutoff='all', neg.response=NULL)
{
   
   # df=preds_ss$HMF_CV
   # predicted=df[,c('BRCA1','BRCA2','none')]
   # expected=df$response
   
   if(nrow(predicted) != length(expected)){
      stop('predicted and expected are of different lengths')
   }
   
   responses <- unique(expected)
   
   if(is.data.frame(predicted)){
      predicted <- as.matrix(predicted)
   }
   
   if(!is.null(neg.response)){
      predicted <- predicted[,colnames(predicted) != neg.response]
      responses <- responses[responses != neg.response]
   }
   
   if(is.numeric(cutoff) && length(cutoff) == 1){
      out <- lapply(responses, function(i){
         #i='BRCA1'
         response <- ifelse(expected==i,1,0)
         probs <- predicted[,i]
         
         calcTfpn(probs, response, cutoff)
      })
      out <- do.call(rbind, out)
      rownames(out) <- responses
   }
   
   else if(cutoff == 'all' | length(cutoff)>1){
      
      cutoffs <- unique(unlist(predicted,use.names=F))
      cutoffs <- sort(c(0,cutoffs,1))
      
      out <- lapply(responses, function(i){
         #i='BRCA1'
         response <- ifelse(expected==i,1,0)
         probs <- predicted[,i]
         
         m <- do.call(rbind, lapply(cutoffs, function(j){ calcTfpn(probs, response, j) }))
         m <- cbind(cutoff=cutoffs, m)
      })
      names(out) <- responses
   }
   
   else {
      stop("Please specificity a cutoff from 0 to 1, or 'all'")
   }
   
   class(out) <- c(class(out), 'confusion.multiclass')
   return(out)
}
