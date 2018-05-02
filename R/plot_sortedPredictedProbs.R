#' Plot sorted prediction probabilities
#'
#' @param probs.predicted A numeric vector of prediction probabilities.
#' @param logicals.expected A character vector of response labels. If provided, dots will be coloured by these labels.
#' @param cutoff If provided, a horizontal line will be drawn at the specified value to indicate the chosen cutoff.
#' @param show.confusion Show confusion matrix?
#' @param decreasing Sort by decreasing?
#' @param title User specified plot title. 
#'
#' @return A ggplot2 scatter plot
#' @export
#'
plot_sortedPredictedProbs <- function(probs.predicted = NULL, logicals.expected = NULL, 
                                      cutoff = NULL, show.confusion = T, decreasing = F, title = NULL)
{
   probs.predicted = rf_agg_pred$BRCA
   logicals.expected = toBinaryResponse(rf_agg_pred$response, '1', 'BRCA deficient', '0', 'BRCA proficient') %>% relevel(.,'BRCA deficient')
   
   if( is.null(logicals.expected) ){
      df <- data.frame(probs.predicted)
   } else {
      df <- data.frame(probs.predicted, logicals.expected)
   }
   
   df <- df$probs.predicted %>% order(., decreasing = decreasing) %>% df[.,]
   df$index <- 1:nrow(df)
   
   plot <- ggplot(data = df, aes(x = index, y = probs.predicted) ) +
      ggtitle(title) +
      xlim(NA, max(df$index)) +
      xlab('Rank') + 
      ylab('Probability') +
      theme(plot.title = element_text(hjust = 0.5),
            panel.background = element_blank(),
            panel.grid = element_blank(),
            axis.line = element_line(colour = "black"))
   
   if( !is.null(logicals.expected) ){
      plot <- plot + 
         geom_bar(stat = 'identity', width = 1, aes(fill = logicals.expected) ) +
         scale_fill_manual(name = 'logicals.expected', values = c("red", "grey"))
   } else {
      plot <- plot + 
         geom_bar(stat = 'identity', width = 1)
   }
   
   if( !is.null(title) ){
      plot <- plot + ggtitle(title)
   }
   
   if( !is.null(cutoff) ){
      plot <- plot +
         geom_hline(yintercept = cutoff, linetype = 2) #+
         # annotate('text', x=0, y=cutoff,
         #          hjust = 0, vjust = -1,
         #          label=paste0('P = ',cutoff))
   }
   
   if(show.confusion == T){
      m <- confusionMatrix(rf_agg_pred$BRCA, rf_agg_pred$response, cutoff = cutoff) %>% t()
      rownames(m) <- NULL
      colnames(m) <- NULL
      
      cols <- matrix(c('red','darkgray'), nrow(m), ncol(m), byrow = T)
      tt <- ttheme_minimal(core=list(fg_params = list(col = cols)))
      
      table_confusion = tableGrob(m, theme = tt)
      
      table_xpos <- nrow(df) * 0.15
      plot <- plot + annotation_custom(table_confusion, xmin=table_xpos, xmax=table_xpos, ymin=cutoff, ymax=cutoff)
   }
   
   return(plot)
}
