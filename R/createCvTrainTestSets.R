#' Create train and test sets for k-fold cross validation
#'
#' @description Shuffles observations and splits them into k-fold train/test sets.
#'
#' @param df A dataframe of observations as rows and features as columns
#' @param k Number of folds to split the data into
#' @param stratify.by.col (Optional) The name of a column containing the response classes by which to stratify the data.
#' Stratification ensures that all classes are present in every train/test set.
#'
#' @return A list of length(k), containing a sublist of train and test sets
#' @export
#'
#' @examples createCvTrainTestSets(df, k=10, stratify.by.col = 'response')

createCvTrainTestSets <- function(df, k=10, stratify.by.col = NULL, seed = NULL)
{  
   ## Shuffle data
   df_shuffled <- df[sample(1:nrow(df)),]

   ## No stratification
   if( is.null(stratify.by.col) ){
      return( splitDataIntoKfolds(df_shuffled) )
   }

   ## Stratification
   else {
      ## Split data by column name provided by stratify.by.col
      responseLvls <-  levels(as.factor(df[,stratify.by.col]))
      l_df_strat <- lapply(responseLvls,function(lvl){
         df_shuffled[df_shuffled[,stratify.by.col] == lvl ,]
      })

      ## Get train/test set per response group
      ## List structure: l_TrainTestStrat[[responseGroup]][[kfold]][[train/test]]
      l_TrainTestStrat <- lapply(l_df_strat, function(df_responseGroup){
         splitDataIntoKfolds(df_responseGroup, k=k)
      })

      ## Merge response groups per k-fold
      nResponses <- 1:length(responseLvls)

      l_TrainTest <- lapply(1:k,function(k){
         ## Avoided inner lapply loop here for for readability
         trainSet <-
            eval(parse(text = 
                        paste0('rbind(',
                               paste0('l_TrainTestStrat[[',nResponses,']][[k]][[\'train\']]', collapse=',')
                               ,')')
            ))
         
         testSet <-
            eval(parse(text = 
                          paste0('rbind(',
                                 paste0('l_TrainTestStrat[[',nResponses,']][[k]][[\'test\']]', collapse=',')
                                 ,')')
            ))
         list(train = trainSet, test = testSet)
      })

      return(l_TrainTest)
   }
}
