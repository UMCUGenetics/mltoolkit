#' Plot feature importance
#'
#' @param df.importance A data.frame containing a column of feature names and a column of MeanDecreaseAccuracy values
#' @param title User specified plot title. If unspecified, title defaults to 'Feature importance'
#' @param sort Whether to sort the features by decreasing importance
#' @param final.model.imp If a vector is provided, red dashes will be added to the plot according to vector values. Used
#' for overlaying the feature importance of the final model over feature importance as determined by cross-validation
#' @param export.features If TRUE, only returns a vector containing the feature names
#'
#' @return Returns a ggplot2 scatter plot
#' @export

plotFeatureImportance <- function(df, final.model.imp = NULL, title = NULL, mid.function = 'mean',
                                  sort = T, export.features = F)
{
   #df=agg_imp

   if(!is.null(final.model.imp)){
      #final.model.imp <- final_imp
      colnames(final.model.imp)[1] <- 'final.model.imp'
      
      df <- merge(df, final.model.imp, all = T, by = 'row.names')
      rownames(df) <- df[,1]
      df[,1] <- NULL
   }
   
   ## Stats for dot plot
   supp_stats <- t(apply(df[,colnames(df) != 'final.model.imp'], 1, function(i){
      if(all(is.na(i))){
         out <- c(min = NA, max = NA, mid = NA, stable = 0)
      } else {
         out <- c(
            min = min(i, na.rm = T),
            max = max(i, na.rm = T),
            mid = if(mid.function == 'median'){ median(i, na.rm = T) } else if(mid.function == 'mean'){ mean(i, na.rm = T) },
            stable = sum(!is.na(i)) >= length(i)*0.5
         )
      }
      return(out)
   }))
   
   df <- cbind(df, supp_stats)
   
   ## Deal with features without CV values but with final model values
   df$mid[is.na(df$mid)] <- df$final.model.imp[is.na(df$mid)]
   
   if(sort){ df <- df[order(df$mid),] }
   
   df$feature <- rownames(df)
   df$id <- str_pad(1:nrow(df), width = max(nchar(1:nrow(df))), pad = '0',side = 'left')
   
   if(export.features){
      return(df$feature)
   
   } else {
      plot <- ggplot(df, aes(x=id,y=mid)) + 
         geom_hline(yintercept = -0.0000001, color = 'grey90') +
         geom_errorbar(aes(ymin=min, ymax=max), width = 0, color = 'grey50') +
         geom_point(shape=21, size=1.5, color = 'grey30', fill = ifelse(df$stable, 'grey30', 'white') ) +
         
         scale_x_discrete(labels = df$feature) +
         scale_color_discrete(guide = F) +
         
         coord_flip() +
         
         theme_bw() +
         theme(
            axis.ticks.y = element_blank(),
            panel.background = element_blank(),
            panel.border = element_rect(fill = NA),
            panel.grid.major.y = element_line(colour = 'grey90', linetype = 'longdash'),
            panel.grid.major.x = element_line(colour = 'grey90'),
            axis.title.y = element_blank(),
            axis.text.y = element_text(hjust = 1, color = ifelse(df$stable, 'black', 'grey80') ),
            plot.title = element_text(hjust = 0.5)
         )
      
      if(!is.null(final.model.imp)){
         plot <- plot +
            geom_point(
               data = df, aes(x = id, y = final.model.imp), 
               color = 'red', shape = 108, size = 1, stroke = 4) +
               #color = 'red', fill = 'red', shape = 23, size = 1.5) +
            #scale_alpha_discrete(range = c(1, 0), guide = F)
            scale_alpha(range = c(1, 0), guide = F)
      }
      
      if('coef' %in% class(df)){ 
         y_label <- 'Coefficients' 
      } else if('mdg' %in% class(df)){ 
         y_label <- 'Mean decrease in Gini' 
      } else if('mda' %in% class(df)){ 
         y_label <- 'Mean decrease in Accuracy' 
      } else {
         y_label <- 'Importance'
      }
      plot <- plot + ylab(y_label)
      
      if(!is.null(title)){ plot <- plot + ggtitle(title) }
      
      return(plot)
   }
}
