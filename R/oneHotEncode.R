#' Converts a vector to a matrix of labels
#'
#' @param labels A vector of labels
#' @param sample.names Optional sample names
#'
#' @return A logical matrix
#' @export
#'
oneHotEncode <- function(labels, sample.names=NULL, as.integer=F){
   #labels=test_data$y
   
   if(!is.factor(labels)){ stop('`labels` must be a factor') }
   
   if(is.null(sample.names)){
      sample.names <- 1:length(labels)
   }
   
   labels_split <- split(sample.names, labels)
   
   m <- do.call(cbind, lapply(labels_split, function(i){
      sample.names %in% i
   }))
   rownames(m) <- sample.names
   
   if(as.integer){
      m <- m + 0
   }
   
   return(m)
}
