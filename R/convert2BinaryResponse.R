#' Convert multiclass response to binary response
#'
#' @param chr A character vector or factor
#' @param p.classes Name of positive classes to convert
#' @param p.return Name of positive class to return
#' @param n.classes Name of negative classes to convert
#' @param n.return Name of negative class to return
#' @param as.factor Option to return a factor instead of character vector
#'
#' @details Classes here refer to the type of responses; not to be confused with R classes.
#'
#' If only one class is specified for Pclasses and no name is specified for Preturn, Pclasses is returned. Preturn can be
#' specified in case one would like to change the name of the class. This is also the same for Nclasses/Nreturn.
#'
#' @return Returns a character vector or factor with only two classes
#' @export
#'
#' @examples ## Convert c('BRCA1','BRCA2') to 'BRCA'. Leave 'none'  as is.
#' convert2BinaryResponse(c('BRCA1', 'BRCA2', 'none'), c('BRCA1','BRCA2'), 'BRCA', 'none', 'none')

convert2BinaryResponse <- function(chr, p.classes=NULL, p.return=NULL, n.classes=NULL, n.return=NULL, as.factor = TRUE)
{
   if(is.null(p.classes) | is.null(n.classes)){
      stop('Please specify positive and/or negative classes')
   }

   chr <- as.character(chr)

   if( !is.null(p.return) ){
      chr[chr %in% p.classes] <- p.return
   }

   if( !is.null(n.return) ){
      chr[chr %in% n.classes] <- n.return
   }

   if( length(unique(chr)) > 2 ){
      warning('Output response vector contains >2 classes')
   }

   if(as.factor == TRUE){
      return(as.factor(chr))
   } else {
      return(chr)
   }
}
