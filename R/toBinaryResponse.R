#' Convert multiclass response to binary response
#'
#' @param chr A character vector or factor
#' @param p.responses Name of positive classes to convert
#' @param p.return Name of positive class to return
#' @param n.responses Name of negative classes to convert
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
#' @examples
#' responses <- c('BRCA1', 'BRCA2', 'none','BRCA1','BRCA1', 'BRCA2', 'none')
#' toBinaryResponse(responses)

toBinaryResponse <- function(chr, 
                             p.responses = NULL, p.return = NULL, 
                             n.responses = NULL, n.return = NULL, 
                             as.factor = TRUE)
{
   # chr = rf_agg_pred$response
   # p.responses = c('BRCA1','BRCA2')
   # p.return = 1
   # n.responses = 'none'
   # n.return = 0
   
   if(is.null(p.responses) & is.null(n.responses)){
      stop('Please specify positive and/or negative classes')
   }

   chr <- as.character(chr)
   
   if( !is.null(p.return) ){ chr[chr %in% p.responses] <- p.return }
   if( !is.null(n.return) ){ chr[chr %in% n.responses] <- n.return }
   
   if( length(unique(chr)) > 2 ){ warning('Output response vector contains >2 classes') }

   if(as.factor == TRUE){
      
      out <- as.factor(chr)
      out <- relevel(out, as.character(p.return))
      
      return(out)
   } 
   
   else {
      return(chr)
   }
}
