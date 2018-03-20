#' Balancing of classes by up/down sampling
#'
#' @description Data has class imbalance if e.g. there are 100 BRCA deficient donors and 900 BRCA proficient donors.
#' If scaling = 'up' the 100 BRCA deficient donors are up-sampled to 900. If scaling = 'down' the 900 BRCA deficient donors
#' are down-sampled to 100. This function also works if there are multiple classes (i.e. response is not binary)
#'
#' @param df A dataframe of observations as rows and features as columns
#' @param colnameResponse The name of a column containing the response classes
#' @param scaling If scaling = 'up', less represented classes are sampled up (with replacement) to the highest represented class.
#' If scaling = 'down', higher represented classes are sampled down to the lowest represented class.
#'
#' @return Returns a dataframe with balanced classes
#' @export
#'
#' @examples balanceClasses(df, 'response', scaling = 'up')

balanceClasses <- function(df, colnameResponse, scaling = 'up')
{
   classFreq <- df[,colnameResponse] %>% table() %>% as.data.frame()

   if(scaling == 'up'){
      targetFreq <- classFreq$Freq %>% max()
   } else if(scaling == 'down') {
      targetFreq <- classFreq$Freq %>% min()
   }

   targetFreqClass <- classFreq[classFreq$Freq == targetFreq,'.']

   df_BalancedClasses <- lapply( as.character(classFreq$.), function(i){
      df_ss <- df[df[,colnameResponse] == i,]

      if(scaling == 'up'){
         if(i != targetFreqClass){
            sample(1:nrow(df_ss), targetFreq, replace = T) %>% df_ss[.,]
         } else {
            df_ss
         }

      } else if(scaling == 'down'){
         sample(1:nrow(df_ss), targetFreq, replace = F) %>% df_ss[.,]
      }

   }) %>% do.call(rbind,.) %>% .[order(rownames(.)),]

   return(df_BalancedClasses)
}
