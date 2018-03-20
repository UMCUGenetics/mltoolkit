#' Plot logistic regression coefficients
#'
#' @param df A dataframe/matrix with rows as features and columns as CV runs. Number of cols can be 1, to facilitate plotting
#' of coefficients from a final model
#' @param title User specified plot title. If unspecified, the plot title defaults to 'Feature weights'.
#'
#' @return Returns a ggplot2 boxplot
#' @export

plot_logRegCoefs <- function(data, title = NULL)
{
   nFolds <- ncol(data)

   ## Convert to df
   df <- data %>% as.matrix() %>% as.data.frame()

   ## work-around to ensure data remains as dataframe and not converted to vector
   if(nFolds == 1){ df$placeholder <- 0 }

   ## Remove rows where all coefs = 0
   df <- df %>% .[apply(.,1,sum) != 0,]

   ## Rename rows and cols
   colnames(df) <- 1:ncol(df)
   rownames(df) <- rownames(df) %>% gsub('[()]','',.)

   ## Coef means
   if(nFolds == 1){
      mean_coef <- df[order(df$`1`, decreasing = F),]
      names_mean_coef <- rownames(mean_coef)
      mean_coef <- mean_coef[,1]
      names(mean_coef) <- names_mean_coef
   } else {
      mean_coef <- apply(df,1,mean) %>% .[. != 0] %>% sort(.,decreasing = F)
      mean_coef <- c(mean_coef %>% .[names(.) == 'Intercept'], mean_coef %>% .[names(.) != 'Intercept'])
   }

   ## Reorder
   df <- df %>% .[names(mean_coef),]

   ## Melt dataframe
   df$feature <- rownames(df)

   df$id <-
      1:nrow(df) %>%
      as.character() %>%
      str_pad(., width = nchar(.) %>% max, pad = '0',side = 'left')

   if(nFolds == 1){ df$`2` <- NULL }
   df_melt <- melt(df, c('id','feature'))

   ## Annotation of mean
   mean_label <- signif(mean_coef,3) %>% as.character()
   value_range <- max(df_melt$value) - min(df_melt$value)
   ylim_min <- min(df_melt$value) - value_range*0.18
   ylim_max <- max(df_melt$value) + value_range*0.1

   ## Plot
   coefsPlot <- ggplot(df_melt, aes(x=id,y=value))

   if(nFolds == 1){
      coefsPlot <- coefsPlot + geom_boxplot()
   } else {
      coefsPlot <- coefsPlot +
         geom_boxplot() +
         geom_jitter() +
         stat_summary(fun.y="mean", geom = "point", shape=4, size=5, color="red")
   }

   coefsPlot <- coefsPlot +
      geom_hline(yintercept = 0, color = 'dark gray') +

      annotate("text", x = 1:nrow(df), y = ylim_min, label = mean_label, size = 3.2, hjust = 0, color = "red4") +

      scale_x_discrete(labels = df$feature) +
      scale_y_continuous(expand = c(0, 0), limits = c(ylim_min - value_range*0.01, ylim_max) ) +

      xlab('Feature') +
      ylab('Coefficient') +

      theme(
         axis.ticks.y = element_blank(),
         panel.background = element_blank(),
         panel.border = element_rect(fill = NA),
         plot.title = element_text(hjust = 0.5)
      ) +

      coord_flip()

   if( is.null(title) ){
      coefsPlot <- coefsPlot + ggtitle('Feature weights')
   } else {
      coefsPlot <- coefsPlot + ggtitle(title)
   }

   return(coefsPlot)
}
