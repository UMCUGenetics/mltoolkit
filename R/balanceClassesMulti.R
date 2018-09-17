#' Up/down sample a dataframe containing multiple classes
#'
#' @param df A dataframe of observations as rows and features as columns
#' @param colname.response The name of a column containing the response classes
#' @param balance.options A list of vectors stating the options of the base function, balanceClasses(). Vectors should be in the 
#' format: c(target.class, scale.ratio, method)
#'
#' @return
#' @export
#'
#' @examples 
#' balance.options = list(
#'    c('none', 1/3, 'simple'),
#'    c('BRCA1', 2, 'SMOTE')
#' )
#' 
#' balanceClassesMulti(df, 'response', balance.options)

balanceClassesMulti <- function(df, colname.response, balance.options){
   
   pipeline_string <- sapply(balance.options, function(i){
      
      options_string <- paste0('\'', paste(i ,collapse = '\',\''), '\'' )
      
      function_string <- paste0(
         'balanceClasses(','.', ',', 'colname.response', ',', options_string,')' )
   })
   
   pipeline_string <- paste0('df %>% ',  paste(pipeline_string, collapse = ' %>% ') )
   
   eval(parse(text=pipeline_string))
}
