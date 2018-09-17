#' Plot feature importance
#'
#' @param df.importance A data.frame containing a column of feature names and a column of MeanDecreaseAccuracy values
#' @param title User specified plot title. If unspecified, title defaults to 'Feature importance'
#' @param sort Whether to sort the features by decreasing importance
#' @param topn Whether to only show the top n ranked features. Depends on sort.
#' @param rm.features.all.zero Remove feature columns which contain all zeroes
#' @param final.model.imp If a vector is provided, red dashes will be added to the plot according to vector values. Used
#' for overlaying the feature importance of the final model over feature importance as determined by cross-validation
#' @param export.features If TRUE, only returns a vector containing the feature names
#'
#' @return Returns a ggplot2 scatter plot
#' @export

plotFeatureImportance <- function(df, title = NULL, sort = T, topn = F, rm.features.all.zero = F, 
                                  final.model.imp = NULL, export.features = F)
{

   nFolds <- ncol(df)
   
   if(rm.features.all.zero == T){
      df <- df %>% .[apply(.,1,sum) != 0,]
   }
   
   ## Sort features by MeanDecreaseAccuracy
   if(sort == T){
      median_imp <- apply(df,1,median) %>% sort(.,decreasing = F)
      df <- df[names(median_imp),]
   }
   
   ## Stats for dot plot
   df$min <- apply(df,1,min)
   df$max <- apply(df,1,max)
   df$median <- apply(df,1,median)
   
   df$feature <- rownames(df)
   
   df$id <-
      1:nrow(df) %>%
      as.character() %>%
      str_pad(., width = nchar(.) %>% max, pad = '0',side = 'left')
   
   ## Plot
   plot <- ggplot(df, aes(x=id,y=median))
   
   # if(min(df[,1:nFolds]) < 0){
   #    plot <- plot + geom_hline(yintercept = 0, color = 'grey60')
   # } else {
   plot <- plot + geom_hline(yintercept = -0.0000001, color = 'gray90')
   # }
   
   plot <- plot +
      geom_errorbar(aes(ymin=min, ymax=max), width = 0, colour = 'grey50') +
      geom_point(shape=21, size=2, fill='white') +
      
      scale_x_discrete(labels = df$feature) +
      
      coord_flip() +
      
      theme(
         axis.ticks.y = element_blank(),
         panel.background = element_blank(),
         panel.border = element_rect(fill = NA),
         panel.grid.major.y = element_line(colour = 'grey90', linetype = 'dashed'),
         panel.grid.major.x = element_line(colour = 'grey90'),
         axis.title.y = element_blank(),
         axis.text.y = element_text(hjust = 0),
         plot.title = element_text(hjust = 0.5)
      )
   
   if('coef' %in% class(df)){
      plot <- plot + ylab('Coefficients')
   } else if('mdg' %in% class(df)){
      plot <- plot + ylab('Mean decrease in Gini')
   } else if('mda' %in% class(df)){
      plot <- plot + ylab('Mean decrease in Accuracy')
   }
   
   if( !is.null(final.model.imp) ){
      df$final.model.imp <- final.model.imp
      df$line.alpha <- as.factor(df$final.model.imp != 0) %>% relevel(.,'TRUE')
      plot <- plot + 
         geom_point(data = df, aes(x = id, y = final.model.imp, alpha = line.alpha), colour = 'red', shape = 108, size = 1, stroke = 4) +
         scale_alpha_discrete(range = c(1, 0), guide = F)
   }
   
   if( !is.null(title) ){
      plot <- plot + ggtitle(title)
   }
   
   if(export.features == T){
      return(df$feature)
   } else {
      return(plot)
   }
}
