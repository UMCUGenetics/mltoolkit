#' Plot MeanDecreaseAccuracy
#'
#' @param df.importance A data.frame containing a column of feature names and a column of MeanDecreaseAccuracy values
#' @param title User specified plot title. If unspecified, title defaults to 'Feature importance'
#' @param sort Whether to sort the features by decreasing importance
#' @param topn Whether to only show the top n ranked features. Depends on sort.
#'
#' @return Returns a ggplot2 scatter plot
#' @export
#'

plot_MeanDecreaseAccuracy <- function(df.importance, title = NULL, sort = T, topn = F)
{
   ## Sort features by MeanDecreaseAccuracy
   if(sort == T){
      df.importance <- df.importance %>% .[order(.$MeanDecreaseAccuracy, decreasing = T),]
   }

   ## Take top n ranked features
   if(topn == T){
      df.importance <- df.importance[1:topn,]
   }

   ## Reverse MeanDecreaseAccuracy order again. ggplot2 coord_flip orders the MeanDecreaseAccuracy in the reverse order
   df.importance <- df.importance %>% .[order(.$MeanDecreaseAccuracy, decreasing = F),]

   ## Plot id as x to ensure proper sorting order
   df.importance$id <- 1:nrow(df.importance)

   MeanDecAccPlot <- ggplot(df.importance,aes(id,MeanDecreaseAccuracy)) +
      geom_point(shape=1,size=2) +
      coord_flip() +
      scale_x_discrete(labels=df.importance$feature,limits=1:length(df.importance$feature)) +

      theme(axis.title.y = element_blank(),
            axis.text = element_text(size=10),
            plot.title = element_text(hjust=0.5))

   if( is.null(title) ){
      MeanDecAccPlot <- MeanDecAccPlot + ggtitle('Feature importance')
   } else {
      MeanDecAccPlot <- MeanDecAccPlot + ggtitle(title)
   }

   return(MeanDecAccPlot)
}
