#' Create confusion matrix(-ces) (multi-class)
#'
#' @param probs.predicted A matrix or data frame containing the prediction probabilities of each class in each column.
#' @param logicals.expected A character vector of response labels. If provided, dots will be coloured by these labels.
#' @param cutoff If provided, a horizontal line will be drawn at the specified value to indicate the chosen cutoff.
#' @param neg.response The names of the negative response(s). These classes will be excluded from the calculations.
#'
#' @return A vector or matrix
#' @export

confusionMatrixMC <- function(probs.predicted, responses.expected, cutoff = 0.5, neg.response = NULL)
{

   # probs.predicted <- rf_agg_pred[,c('BRCA1','BRCA2','none')]
   # responses.expected <- rf_agg_pred$response
   
   if(nrow(probs.predicted) != length(responses.expected)){
      stop('probs.predicted and responses.expected are of different lengths')
   }
   
   responses <- unique(responses.expected)
   
   if( !all(colnames(probs.predicted) %in% responses) ){
      stop('colnames of prediction columns must be the same as the response names')
   }
   
   if(!is.null(neg.response)){
      probs.predicted <- probs.predicted[,colnames(probs.predicted) != neg.response]
      responses <- responses[responses != neg.response]
   }

   getTfpn <- function(selected.response, thres){
      
      tfpn <- lapply(1:length(responses.expected),function(i){

         pred <- probs.predicted[i, selected.response]
         response <- responses.expected[i]
         
         if( response == selected.response ){
            if( pred >= thres ){ 1 } ## tp
            else{ 2 } ## fn
         } 
         
         else if( response != selected.response ){
            if( pred >= thres ){ 3 } ## fp
            else{ 4 } ## tn
         }
      })
      
      c(
         sum(tfpn==1),
         sum(tfpn==4),
         sum(tfpn==3),
         sum(tfpn==2)
      )
   }
   
   if(is.numeric(cutoff) && length(cutoff) == 1){
      out <- lapply(responses, function(i){ getTfpn(i, cutoff) })
      out <- do.call(rbind, out)
      rownames(out) <- responses
      colnames(out) <- c('tp','tn','fp','fn')
      
   } 
   
   else if(cutoff == 'all'){
      
      cutoffs <- c( sort(unique(unlist(probs.predicted,use.names = F))), 1 )
      
      out <- lapply(responses, function (i){ 
         m <- lapply(cutoffs, function(j){ getTfpn(i, j) })
         m <- as.data.frame(do.call(rbind, m))
         m <- cbind(cutoffs, m)
         
         colnames(m) <- c('cutoff','tp','tn','fp','fn')
         return(m)
      })
      names(out) <- responses
      
      
   } 
   
   else {
      stop('Please specificity a cutoff from 0 to 1, or \'all\'')
   }
   
   class(out) <- c(class(out), 'confusion.multiclass')
   return(out)
}
