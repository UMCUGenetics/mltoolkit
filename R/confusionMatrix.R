#' Create confusion matrix(-ces)
#'
#' @param probs.predicted A numeric vector of prediction probabilities.
#' @param logicals.expected A character vector of response labels. If provided, dots will be coloured by these labels.
#' @param cutoff If provided, a horizontal line will be drawn at the specified value to indicate the chosen cutoff.
#'
#' @return A vector or matrix
#' @export

confusionMatrix <- function(probs.predicted, logicals.expected, cutoff = 'all'){

   if(length(probs.predicted) != length(logicals.expected)){
      stop('probs.predicted and logicals.expected are of different lengths')
   }
   
   if(is.factor(logicals.expected)){
      #message('logicals.expected is a factor Automatically converting to logical.')
      if( !all(levels(logicals.expected) %in% c('0','1','TRUE','FALSE')) ){
         stop('logicals.expected must consist of \'0\',\'1\',\'TRUE\', or \'FALSE\'')
      }
      logicals.expected <- as.logical(as.integer(as.character(logicals.expected)))
   }
   
   getTfpn <- function(probs.predicted, logicals.expected, thres){
      tfpn <- apply(cbind(probs.predicted, logicals.expected),1,function(i){
         pred <- i[1]
         response <- i[2]
         
         if( pred >= thres ){
            if( response == 1 ){ 1 } ## tp
            else{ 2 } ## fp
            
         } else if( pred < thres ){
            if( response != 1 ){ 3 } ## tn
            else{ 4 } ## fn
         }
      })
      
      ## did not assign vector names for speed
      c(
         sum(tfpn==1),
         sum(tfpn==3),
         sum(tfpn==2),
         sum(tfpn==4)
      )
   }

   if(is.numeric(cutoff) && length(cutoff) == 1){
      m <- getTfpn(probs.predicted, logicals.expected, cutoff)
      names(m) <- c('tp','tn','fp','fn') 

   } else if(cutoff == 'all'){
      cutoffs <- c( sort(unique(probs.predicted)), 1 )
      
      m <- 
         do.call(rbind,
                 lapply(cutoffs, function(i){
                    c(i,
                      getTfpn(probs.predicted, logicals.expected, thres = i) 
                    )
                 })
         )
      colnames(m) <- c('cutoff','tp','tn','fp','fn')
   } else {
      stop('Please specificity a cutoff from 0 to 1, or \'all\'')
   }
   
   return(m)
}
