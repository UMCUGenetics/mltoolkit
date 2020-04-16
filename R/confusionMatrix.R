#' Calculate true/false positive/negative counts
#'
#' @param probs.predicted A numeric vector of prediction probabilities. 
#' @param logicals.expected A character vector of response labels.
#' @param cutoff A single classification cutoff from 0 to 1
#'
#' @return A integer vector of the true/false positive/negative counts
#' @export
#'
getTfpn <- function(probs.predicted, logicals.expected, cutoff){
   #cutoff=0.5
   out <- structure(rep(0,4), names=c('tp','tn','fp','fn'))
   
   tfpn <- rep('tn',length(probs.predicted))
   
   tfpn[ probs.predicted>=cutoff & logicals.expected==1 ] <- 'tp'
   tfpn[ probs.predicted>=cutoff & logicals.expected==0 ] <- 'fp'
   
   tfpn[ probs.predicted<cutoff & logicals.expected==1 ] <- 'fn'
   
   tab <- table(tfpn)
   out[names(tab)] <- tab
   return(out)
}

####################################################################################################
#' Create confusion matrix(-ces)
#'
#' @param probs.predicted A numeric vector of prediction probabilities.
#' @param logicals.expected A character vector of response labels.
#' @param cutoff A single or vector of classification cutoffs ranging from 0 to 1
#'
#' @return A vector or matrix
#' @export

confusionMatrix <- function(probs.predicted, logicals.expected, cutoff='all'){
   
   #df=preds_ss$HMF_CV
   #probs.predicted=df$hrd
   #logicals.expected=toBinaryResponse(df$response,c('BRCA1','BRCA2'),1,'none',0)
   
   if(length(probs.predicted) != length(logicals.expected)){
      stop('probs.predicted and logicals.expected are of different lengths')
   }
   
   if(is.factor(logicals.expected)){
      #message('logicals.expected is a factor Automatically converting to logical.')
      if( !all(levels(logicals.expected) %in% c('0','1','TRUE','FALSE')) ){
         stop("logicals.expected must consist of '0','1','TRUE', or 'FALSE'")
      }
      logicals.expected <- as.logical(as.integer(as.character(logicals.expected)))
   }
   
   if(is.numeric(cutoff) && length(cutoff) == 1){
      m <- getTfpn(probs.predicted, logicals.expected, cutoff)
      names(m) <- c('tp','tn','fp','fn')
      
   } else if(cutoff == 'all'){
      cutoffs <- sort(unique(
         c(0,probs.predicted,1)
      ))
      
      m <- do.call(rbind, lapply(cutoffs, function(i){
         c(i, getTfpn(probs.predicted, logicals.expected, i) )
      }))
      colnames(m) <- c('cutoff','tp','tn','fp','fn')
      m <- unique(m)
   } else {
      stop("Please specificity a cutoff from 0 to 1, or 'all'")
   }
   
   return(m)
}

####################################################################################################
#' Create confusion matrix(-ces) (multi-class)
#'
#' @param probs.predicted A matrix or data frame containing the prediction probabilities of each class in each column.
#' @param responses.expected A character vector of response labels.
#' @param cutoff A single or vector of classification cutoffs ranging from 0 to 1
#' @param neg.response The names of the negative response(s). These classes will be excluded from the calculations.
#'
#' @return A vector or matrix
#' @export

confusionMatrixMC <- function(probs.predicted, responses.expected, cutoff='all', neg.response=NULL)
{
   
   # df=preds_ss$HMF_CV
   # probs.predicted=df[,c('BRCA1','BRCA2','none')]
   # responses.expected=df$response
   
   if(nrow(probs.predicted) != length(responses.expected)){
      stop('probs.predicted and responses.expected are of different lengths')
   }
   
   responses <- unique(responses.expected)
   
   if(is.data.frame(probs.predicted)){
      probs.predicted <- as.matrix(probs.predicted)
   }
   
   if(!is.null(neg.response)){
      probs.predicted <- probs.predicted[,colnames(probs.predicted) != neg.response]
      responses <- responses[responses != neg.response]
   }
   
   if(is.numeric(cutoff) && length(cutoff) == 1){
      out <- lapply(responses, function(i){
         #i='BRCA1'
         response <- ifelse(responses.expected==i,1,0)
         probs <- probs.predicted[,i]
         
         getTfpn(probs, response, cutoff)
      })
      out <- do.call(rbind, out)
      rownames(out) <- responses
   }
   
   else if(cutoff == 'all' | length(cutoff)>1){
      
      cutoffs <- unique(unlist(probs.predicted,use.names=F))
      cutoffs <- sort(c(0,cutoffs,1))
      
      out <- lapply(responses, function(i){
         #i='BRCA1'
         response <- ifelse(responses.expected==i,1,0)
         probs <- probs.predicted[,i]
         
         m <- do.call(rbind, lapply(cutoffs, function(j){ getTfpn(probs, response, j) }))
         m <- cbind(cutoffs, m)
      })
      names(out) <- responses
   }
   
   else {
      stop("Please specificity a cutoff from 0 to 1, or 'all'")
   }
   
   class(out) <- c(class(out), 'confusion.multiclass')
   return(out)
}