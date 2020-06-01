#' Split data into k-fold train and test sets
#'
#' @description Helper function for createCvTrainTestSets()
#'
#' @param df A dataframe of observations as rows and features as columns
#' @param k Number of folds to split the data into
#' @param return.data Return the split dataframe or only indexes
#' @param show.warnings Show warnings?
#'
#' @return A list of length(k), containing a sublists of train/test data or 
#' indexes
splitDataIntoKfolds <- function(df=NULL, k=10, return.data=T, show.warnings=T){
   #df=responses_split$Testis
   #df=training_data
   
   total_samples <- nrow(df)
   
   if(total_samples < k){
      k_n_samples <- rep(0,k)
      k_n_samples[1:total_samples] <- 1
      if(show.warnings){
         warning('Number of samples is less than k. Some folds will have missing data')
      }
   } else {
      ## Calculate number of desired samples per fold (=number of test samples)
      n_samples <- total_samples %/% k
      n_remainder <- total_samples %% k
      
      ## Create vector of n_samples per fold. 
      k_n_samples <- rep(n_samples,k)
      
      ## Evenly distribute remainder sames to take into account
      if(n_remainder != 0){
         k_n_samples[1:n_remainder] <- k_n_samples[1:n_remainder] + 1
      }
   }
   
   ## Get start and end indexes of test sets
   starts <- c(1, cumsum(k_n_samples)[-k]+1)
   ends <- cumsum(k_n_samples)
   
   if(total_samples < k){
      starts[-(1:total_samples)] <- 0
      ends[-(1:total_samples)] <- 0
   }
   
   starts <- rev(starts)
   ends <- rev(ends)
   
   indexes <- 1:total_samples
   l_indexes <- lapply(1:k, function(i){
      start <- starts[i]
      end <- ends[i]
      
      list(
         train=indexes[-(start:end)],
         test=indexes[start:end]
      )
   })
   
   
   ## Output
   if(!return.data){ return(l_indexes) }
   
   lapply(l_indexes, function(i){
      lapply(i, function(j){ df[j,] })
   })
}

####################################################################################################
#' Create train and test sets for k-fold cross validation
#'
#' @description Shuffles observations and splits them into k-fold train/test sets.
#'
#' @param df A dataframe of observations as rows and features as columns
#' @param k Number of folds to split the data into
#' @param stratify.by.col (Optional) The name of a column containing the 
#' response classes by which to stratify the data. Stratification ensures that 
#' all classes are present in every train/test set.
#' @param return.data Return the split dataframe or only indexes
#' @param show.warnings Show warnings?
#'
#' @return A list of length(k), containing a sublist of train and test data or 
#' indexes
#' @export
#'
#' @examples createCvTrainTestSets(df, k=10, stratify.by.col = 'response')
createCvTrainTestSets <- function(df, k=10, stratify.by.col=NULL, return.data=T, show.warnings=T){
   
   #df=training_data
   #stratify.by.col='response'
   
   indexes <- 1:nrow(df)
   
   ## Shuffle data
   indexes_shuffled <- sample(indexes)
   #indexes_shuffled <- indexes
   
   ## No stratification
   if( is.null(stratify.by.col) ){
      return(splitDataIntoKfolds(
         df[indexes_shuffled,], 
         k=k, return.data=return.data, show.warnings=show.warnings
      ))
   }
   
   ## Stratification
   responses <- data.frame(
      index=indexes_shuffled,
      response=df[indexes_shuffled, stratify.by.col]
   )
   responses_split <- split(responses, responses$response)
   
   warning_responses <- c()
   response_k_folds <- lapply(responses_split, function(i){
      if(show.warnings){
         if(nrow(i) < k){
            warning_responses <<- c(warning_responses, i$response[1])
         }
      }
      splitDataIntoKfolds(i, return.data=T, show.warnings=F)
   })
   
   if(show.warnings & length(warning_responses)!=0){
      warning(
         'Some folds will have missing data as these classes have fewer samples than k:\n',
         paste(warning_responses, collapse=', ')
      )
   }
   
   ## Combine stratified folds
   l_indexes <- lapply(1:k, function(i){
      #k=1
      
      train <- do.call(rbind, lapply(1:length(responses_split), function(j){
         #j=1
         response_k_folds[[j]][[i]]$train
      }))
      
      test <- do.call(rbind, lapply(1:length(responses_split), function(j){
         #j=1
         response_k_folds[[j]][[i]]$test
      }))
      
      list(
         train = sort(train$index),
         test = sort(test$index)
      )
   })
   
   ## Output
   if(!return.data){ 
      return(l_indexes) 
   }
   
   lapply(l_indexes, function(i){
      lapply(i, function(j){ df[j,] })
   })
   
}
