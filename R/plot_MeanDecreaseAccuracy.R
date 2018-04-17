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

#plot_MeanDecreaseAccuracy(agg_coef, rm.features.all.zero = T, final.model.imp = fc) + ylab('Coefficients')

plot_MeanDecreaseAccuracy <- function(df, title = NULL, sort = T, topn = F, rm.features.all.zero = F, final.model.imp = NULL, export.features = F)
{
   # df = aggregateRandomForestCV(rfCV, 'importance')
   # df = aggregateLogRegCV(lrCV, 'coef')
   
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
   
   if(min(df[,1:nFolds]) < 0){
      plot <- plot + geom_hline(yintercept = 0, color = 'grey60')
   }
      
   plot <- plot +
      geom_errorbar(aes(ymin=min, ymax=max), width = 0, colour = 'grey50') +
      geom_point(shape=21, size=2, fill='white') +
      
      scale_x_discrete(labels = df$feature) +

      ylab('Mean Decrease in Accuracy') +
      
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

   if( !is.null(final.model.imp) ){
      #final.model.imp <- sample(15.5:16, size = 41, replace = T) %>% sort()
      # ###
      # df$final.model.coefs <- final.model.coefs
      # df$cross.alpha <- df$final.model.coefs != 0
      # 
      # plot <- plot + geom_point(data = df, aes(x = id, y = final.model.coefs, alpha = cross.alpha), colour = 'red', shape = 4, size = 2, stroke = 1) +
      #    scale_alpha_discrete(range = c(0, 1), guide = F)
      # ###
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
