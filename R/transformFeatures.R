#' Transform feature columns
#' 
#' @description Transform the feature columns in a dataframe while excluding the response column (if is exists)
#' 
#' @param df A dataframe of features (with or without a response column)
#' @param FUN A function describing the transformation of each column
#' @param colname.response (Optional) The name of a column containing the response classes. This can be omitted if the 
#' dataframe only contains feature columns. This is useful if the response vector is a separate object.
#'
#' @return Returns a dataframe of transformed features
#' @export
#'
#' @examples
#' transformFeatures(df, function(x){ log10(x) }, colname.response = 'response')

transformFeatures <- function(df, FUN, colname.response = NULL){
   if( is.null(colname.response) ){
      featureCols <- df
   } else {
      featureCols <- df[,colnames(df) != colname.response]
      responseCol <- df[,colname.response]
   }

   featureCols_transformed <- as.data.frame(apply(featureCols, 2, FUN))
   featureCols_transformed[,colname.response] <- responseCol
   
   return(featureCols_transformed)
}

