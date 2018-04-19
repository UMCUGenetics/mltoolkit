#' Standardize train and test datasets together
#'
#' @description Standardize the train set with the following transformation: (x - mean)/sd. If test set is unspecified, 
#' returns the standardized train set. If test set is specified, it is standardized using the mean and sd of the 
#' train set, and a list of the standardized train and test sets are returned.
#' 
#' @param train Train dataset. A dataframe of observations as rows and features as columns. \strong{Important:} input
#' matrix/dataframe should include the response vector as a column.
#' @param test (Optional) Test dataset. A dataframe of observations as rows and features as columns. \strong{Important:} 
#' input matrix/dataframe should include the response vector as a column.
#' @param colname.response The name of a column containing the response classes.
#' @param na.replace Imputation by median (TRUE or 'median') or mean ('mean').
#' @param sd.zero.bypass If standard deviation is zero, return a vector of zeros.
#'
#' @details For more details. See documentation for standardizeFeature().
#'
#' @return Only train set specified: returns the standardized train set. Both train and test sets specified: returns a list 
#' containing the standardized train and test sets.
#' @export
#' standardizeTrainTestSets(train, test, colname.response = 'response', na.replace = T, sd.zero.bypass = T)

standardizeTrainTestSets <- function(train, test = NULL, colname.response, na.replace = F, sd.zero.bypass = F){
   if( is.null(test) ){
      dataset <- list(train = train)
   } else {
      dataset <- list(train = train, test = test)
   }

   dataset_names <- names(dataset)
   
   ## Isolate features and response vector
   features <- lapply(dataset,function(i){ i[, colnames(i) != colname.response ] })
   responses <- lapply(dataset, function(i){ i[,colname.response] })
   
   ## Get mean and sd of train set
   train_stats <- list(
      mean = apply(features$train, 2, function(col){ mean(col, na.rm = T) }),
      sd = apply(features$train, 2, function(col){ sd(col, na.rm = T) }),
      median = apply(features$train, 2, function(col){ median(col, na.rm = T) })
   )

   ## Standardize training set; use train set mean and sd to standardize test set
   ## Overwrite set object
   dataset <- lapply(dataset_names,function(setName){
      setFeatures <- features[[setName]]
      setResponses <- responses[[setName]]

      df <- lapply(1:ncol(setFeatures),function(i){
         standardizeFeature(setFeatures[,i],
                            Mean = train_stats$mean[i],
                            SD = train_stats$sd[i],
                            Median = train_stats$median[i],
                            na.replace = na.replace,
                            sd.zero.bypass = sd.zero.bypass) }) %>% do.call(cbind,.) %>% as.data.frame()

      colnames(df) <- colnames(setFeatures)
      rownames(df) <- rownames(setFeatures)

      df$response <- setResponses

      return(df)
   })
   names(dataset) <- dataset_names
   
   if( is.null(test) ){
      return( dataset$train )
   } else {
      return(dataset)
   }
   
   return(dataset)
}
