#' Plot logistic regression coefficients
#'
#' @param df A dataframe/matrix with rows as features and columns as CV runs. Number of cols can be 1, to facilitate plotting
#' of coefficients from a final model
#' @param title User specified plot title. If unspecified, the plot title defaults to 'Feature weights'.
#' @param final.model.coefs If a vector of length(features) is specified, red crosses will be added with the corresponding values
#'
#' @return Returns a ggplot2 boxplot
#' @export

plot_logRegCoefs <- function(df, title = NULL, final.model.coefs = NULL, export.features = F)
{
   #df = aggregateLogRegCV(lrCV,'coef')
   
   ## Remove rows where all coefs = 0
   df <- df %>% .[apply(.,1,sum) != 0,]

   ## Rename rows and cols
   rownames(df) <- rownames(df) %>% gsub('[()]','',.)

   ## Reorder
   median_coef <- apply(df,1,median) %>% sort(.,decreasing = F)
   df <- df[names(median_coef),]

   df$feature <- rownames(df)

   df$id <-
      1:nrow(df) %>%
      as.character() %>%
      str_pad(., width = nchar(.) %>% max, pad = '0',side = 'left')

   ## Plot
   df_melt <- melt(df, c('id','feature'))
   plot <- ggplot(df_melt, aes(x=id,y=value)) +
      geom_hline(yintercept = 0, color = 'dark gray') +
      geom_boxplot(outlier.shape=NA, fatten = 1, color = 'grey40') +
      geom_jitter(size = 0.5, width = 0.15) +
      
      scale_x_discrete(labels = df$feature) +
      #scale_y_continuous(expand = c(0, 0), limits = c(ylim_min - value_range*0.01, ylim_max) ) +
      
      xlab('Feature') +
      ylab('Coefficient') +
      coord_flip() +

      theme(
         axis.ticks.y = element_blank(),
         panel.background = element_blank(),
         panel.border = element_rect(fill = NA),
         axis.title.y = element_blank(),
         axis.text.y = element_text(hjust = 0),
         plot.title = element_text(hjust = 0.5)
      )
   
   ## Add custom final coef values
   if( !is.null(final.model.coefs) ){
      
      #final.model.coefs <- c(2,0,2,2,1,2,2,0,1,0,0,0)
      
      df$final.model.coefs <- final.model.coefs
      df$cross.alpha <- df$final.model.coefs != 0
         
      plot <- plot + geom_point(data = df, aes(x = id, y = final.model.coefs, alpha = cross.alpha), colour = 'red', shape = 4, size = 2, stroke = 1) +
         scale_alpha_discrete(range = c(0, 1), guide = F)
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
