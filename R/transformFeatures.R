#' Transform feature columns
#' 
#' @description Transform the feature columns in a dataframe while excluding the response column (if is exists)
#' 
#' @param df A dataframe of features (with or without a response column)
#' @param formula A character string describing the mathematical operation, e.g. 'log(x + 1)', where x is the variable to 
#' transform.
#' @param colname.response (Optional) The name of a column containing the response classes. This can be omitted if the 
#' dataframe only contains feature columns. This is useful if the response vector is a separate object.
#'
#' @return Returns a dataframe of transformed features
#' @export
#'
#' @examples
#' transformFeatures(df, formula = 'log(x + 1)', colname.response = 'response')

transformFeatures <- function(df, formula, colname.response = NULL){
   if( is.null(colname.response) ){
      featureCols <- df
   } else {
      featureCols <- df[,colnames(df) != colname.response]
      responseCol <- df[,colname.response]
   }

   featureCols_transformed <- apply(featureCols, 2, function(x){
      parse(text = formula) %>% eval()
   }) %>% as.data.frame()
   
   featureCols_transformed[,colname.response] <- responseCol
   
   return(featureCols_transformed)
   
   # if( is.null(colname.response) ){
   #    featureCols <- colnames(df)
   # } else {
   #    featureCols <- colnames(df) %>% .[. != colname.response]
   # }
   # 
   # tagValues <- lapply(featureCols, function(i){
   #    Xsubstituted <- gsub('\\<x\\>', paste0('`',i,'`'), formula)
   #    paste0('`',i,'`','=', Xsubstituted)
   #    return(Xsubstituted)
   # })
   # 
   # transformFunctionString <- paste0(
   #    'transform(df, ',
   #    paste(tagValues, collapse = ', '),
   #    ')'
   # )
   # 
   # transformFunctionString %>% parse(text = .) %>% eval()
}

