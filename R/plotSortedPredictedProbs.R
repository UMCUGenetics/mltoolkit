#' Plot sorted prediction probabilities
#'
#' @param probs.predicted A numeric vector of prediction probabilities.
#' @param logicals.expected A character vector of response labels. If provided, dots will be coloured by these labels.
#' @param cutoff If provided, a horizontal line will be drawn at the specified value to indicate the chosen cutoff.
#' @param show.confusion Show confusion matrix?
#' @param annotations A vector containining the desired legend names for the responses, e.g. c('BRCA proficient','BRCA deficient')
#' @param title User specified plot title. 
#'
#' @return A ggplot2 bar plot
#' @export
#'

plotSortedPredictedProbs <- function(probs.predicted, logicals.expected = NULL, cutoff = NULL, 
                                     show.confusion = T, show.hline = T, annotations = NULL, title = NULL)
{
   
   # probs.predicted = hmf_prediction_sum_brca12_ss_truth_set$BRCA
   # logicals.expected = hmf_prediction_sum_brca12_ss_truth_set$response
   
   if( is.null(logicals.expected) ){
      df <- data.frame(probs.predicted = sort(probs.predicted))
   } else {
      ## Making sure that logicals.expected is in the right format. Factor ordering affects ggplot bar fill color
      logicals.expected <- replace(logicals.expected, logicals.expected %in% c('TRUE',T), 1)
      logicals.expected <- replace(logicals.expected, logicals.expected %in% c('FALSE',F), 0)
      logicals.expected <- factor(logicals.expected, levels = c('0','1'))
      
      df <- data.frame(probs.predicted, logicals.expected)
      df <- df[order(df$probs.predicted),]
   }

   df$index <- 1:nrow(df)
   
   plot <- ggplot(data = df, aes(x = index, y = probs.predicted) ) +
      xlim(NA, max(df$index)) + xlab('Rank') + ylab('Probability') +
      theme(plot.title = element_text(hjust = 0.5),
            panel.background = element_blank(),
            panel.grid = element_blank(),
            axis.line = element_line(colour = "black"),
            legend.title=element_blank())
   
   if( !is.null(logicals.expected) ){
      plot <- plot + geom_bar(stat = 'identity', width = 1, aes(fill = logicals.expected ) )
         
      if(is.null(annotations)){
         plot <- plot + scale_fill_manual(values = c('grey','red'))
      } else {
         plot <- plot + scale_fill_manual(labels = annotations, values = c('grey','red'))
      } 

   } else {
      plot <- plot + geom_bar(stat = 'identity', width = 1, color = 'grey')
   }
   
   if( !is.null(title) ){ plot <- plot + ggtitle(title) }
   
   if(show.confusion == T){
      if(is.null(cutoff)){
         message('No cutoff specified. Defaulting to 0.5 for producing the confusion matrix')
         cutoff <- 0.5
      }
      
      if(!is.null(logicals.expected)){
         m <- confusionMatrix(probs.predicted, logicals.expected, cutoff = cutoff)
         m <- rbind(
            m[c('tp','fp')],
            m[c('fn','tn')]
         )
         cols <- matrix(c('red','grey50'), nrow(m), ncol(m), byrow = T)
      } else {
         m <- matrix( c(sum(probs.predicted >= cutoff), sum(probs.predicted < cutoff)), nrow = 2)
         cols <- 'grey50'
      }
      
      rownames(m) <- NULL
      colnames(m) <- NULL
      
      tt <- ttheme_minimal(core=list(fg_params = list(col = cols)))
      
      table_confusion <- tableGrob(m, theme = tt)
      table_xpos <- nrow(df) * 0.15
      
      #plot + annotation_custom(table_confusion, xmin=table_xpos, xmax=table_xpos, ymin=cutoff, ymax=cutoff)
      plot <- plot + annotation_custom(table_confusion, xmin=table_xpos, xmax=table_xpos, ymin=cutoff, ymax=cutoff)
      
   }
   
   if( show.hline ){ plot <- plot + geom_hline(yintercept = cutoff, linetype = 2) }
   
   return(plot)
}
