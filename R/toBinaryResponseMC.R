#' Convert multiclass response to all combinations of binary responses
#'
#' @param chr A character vector or factor
#' @param as.factor Option to return a factor instead of character vector
#'
#' @return A data frame of binary responses in relation to each response
#' @export
#'
#' @examples 
#' responses <- c('BRCA1', 'BRCA2', 'none','BRCA1','BRCA1', 'BRCA2', 'none')
#' toBinaryResponseMC(responses)

toBinaryResponseMC <- function(chr, as.factor = T){

   responses <- unique(chr)
   
   out <- lapply(responses, function(p.response){
      n.responses <- responses[responses != p.response]
      toBinaryResponse(chr, 
                       p.response = p.response, p.return = 1,
                       n.response = n.responses, n.return = 0,
                       as.factor = as.factor)
   })
   names(out) <- responses
   
   to_df_string <- unlist( 
         lapply(responses, function(i){ paste0(i,'=out[\'',i,'\']') }), use.names = F
      )
   to_df_string <- paste(to_df_string,collapse = ',')
   to_df_string <- paste0('data.frame(',to_df_string,')')
   
   out <- eval(parse(text = to_df_string))
   
   return(out)
}
