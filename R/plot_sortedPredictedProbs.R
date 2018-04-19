#' Plot sorted prediction probabilities
#'
#' @param probs.predicted A numeric vector of prediction probabilities.
#' @param annotation A character vector of response labels. If provided, dots will be coloured by these labels.
#' @param cutoff If provided, a horizontal line will be drawn at the specified value to indicate the chosen cutoff.
#' @param title User specified plot title. 
#' @param decreasing Sort by decreasing?
#'
#' @return A ggplot2 scatter plot
#' @export
#'
plot_sortedPredictedProbs <- function(probs.predicted = NULL, annotation = NULL, cutoff = NULL, title = NULL, decreasing = F)
{
   
   if( is.null(annotation) ){
      df <- data.frame(probs.predicted)
   } else {
      df <- data.frame(probs.predicted, annotation)
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
   
   if( !is.null(annotation) ){
      plot <- plot + 
         geom_bar(stat = 'identity', width = 1, aes(fill = annotation) ) +
         scale_fill_manual(name = 'Annotation', values = c("red", "grey"))
   } else {
      plot <- plot + 
         geom_bar(stat = 'identity', width = 1)
   }
   
   if( !is.null(cutoff) ){
      plot <- plot +
         geom_hline(yintercept = cutoff, linetype = 2) +
         annotate('text', x=0, y=cutoff,
                  hjust = 0, vjust = -1,
                  label=paste0('P = ',cutoff))
   }
   
   if( !is.null(title) ){
      plot <- plot + ggtitle(title)
   }
   
   return(plot)
}
