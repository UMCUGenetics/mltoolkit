#' Balancing of classes by up/down sampling
#'
#' @description Data has class imbalance if e.g. there are 100 BRCA deficient donors and 900 BRCA proficient donors. This 
#' function up/down-samples a desired class.
#'
#' @param df A dataframe of samples as rows and features as columns
#' @param colname.response The name of a column containing the response classes
#' @param target.class The target class for which the sample size will be adjusted
#' @param scale.ratio The amount of up/down-sampling (e.g. 2 is 2x up, 0.5 is 2x down)
#' @param method 'simple': random samples are replicated when upsampling or random samples are removed when downsampling.
#' 'SMOTE': synthesizes samples based on the feature values of a class (can only be used for upsampling)
#'
#' @return Returns a dataframe with balanced classes
#' @export
#'
#' @examples balanceClasses(df, 'response', 'BRCA1', 1.5, 'SMOTE')

balanceClasses <- function(df, colname.response, target.class, scale.ratio, method = 'simple'){

   scale.ratio <- as.numeric(scale.ratio)
   
   df_split <- list(
      target = df[ df[, colname.response] == target.class,],
      remainder = df[ df[, colname.response] != target.class,]
   )
   
   if(scale.ratio == 1){
      stop('scale.ratio must not == 1')
   }

   targetClassFreq <- nrow(df_split$target)
   newTargetClassFreq <- round(targetClassFreq * scale.ratio)
      
   if(method == 'simple'){

      if(scale.ratio > 1){
         sample_with_replace <- T
      } else {
         sample_with_replace <- F
      }
      
      df_split$target <- 
         sample(1:targetClassFreq, 
                newTargetClassFreq, 
                replace = sample_with_replace) %>% df_split$target[.,]
   }
   
   if(method == 'SMOTE'){
      if(scale.ratio <= 1){
         stop('For method = \'SMOTE\', scale.ratio must be > 1')
      } else {
         scale.ratio <- (scale.ratio - 1) * 100
      }
      
      df_dummy <- df_split$remainder[1,]
      df_dummy <- df_dummy[rep(1,targetClassFreq + 1),]
      
      df_SMOTE <- rbind(df_split$target,df_dummy)
      df_SMOTE[,colname.response] <- as.factor(df_SMOTE[,colname.response])
      
      df_split$target <- SMOTE(response ~ ., df_SMOTE, perc.over = 100, perc.under = 0)
   }
   
   return(
      rbind(df_split$target, df_split$remainder)
   )
}