#' Split data into k-fold train and test sets
#'
#' @description Helper function for createCvTrainTestSets()
#'
#' @param df A dataframe of observations as rows and features as columns
#' @param k Number of folds to split the data into
#'
#' @return A list of length(k), containing a sublist of train and test sets

splitDataIntoKfolds <- function(df, k=10)
{
   ## Total samples
   totalSamples <- nrow(df)

   ## Calculate number of desired samples per fold (=number of test samples)
   nSamples <- totalSamples %/% k
   nRemainder <- totalSamples %% k

   ## Create vector of nSamples per fold. Evenly distribute remainder sames to take into account if totalSamples is not
   ## a multiple of k)
   v_nSamples <- rep(nSamples,k)
   v_nSamples[1:nRemainder] <- v_nSamples[1:nRemainder] + 1

   ## Create list with first start/end row number for each fold
   l_StartEnd <- list( c(1,v_nSamples[1]) ) ## initialize start/end row numbers list with first fold
   for(i in 2:length(v_nSamples))
   {
      prevStartEnd <- l_StartEnd[[(i-1)]]
      l_StartEnd[[i]] <- c(prevStartEnd[2]+1, prevStartEnd[2]+v_nSamples[i])
   }

   ## Output list of train and test sets
   l_TrainTest <- lapply(l_StartEnd, function(i)
   {
      Start <- i[1]
      End <- i[2]

      testSet <- df[Start:End,]
      trainSet <- df[-(Start:End),]

      list(train = trainSet, test = testSet)
   })

   return(l_TrainTest)
}
