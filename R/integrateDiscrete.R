#' Calculate AUC
#'
#' @description Calculates the area under a curve (AUC) by integration via the trapezoidal rule, where discrete (x,y)
#' coordinates are given.
#'
#' @param x A numeric/integer vector of discrete x values
#' @param y A numeric/integer vector of discrete y values
#'
#' @return Returns the area under the curve
#' @export
#'
#' @examples integrateDiscrete(1:10, runif(10))

integrateDiscrete <- function(x, y)
{
   if(length(x) != length(y)){
      stop('x must have the same number of elements as y')
   }

   x <- df_aucCalc$tpr
   y <- df_aucCalc$ppv

   ## (x2 - x1) * (y1 + y2)/2

   m <- cbind(x,y)

   sapply(1:(nrow(m)-1),function(i){
      trapez <- m[i:(i+1),]

      x1 <- trapez[1,'x']
      x2 <- trapez[2,'x']
      y1 <- trapez[1,'y']
      y2 <- trapez[2,'y']

      (x2 - x1)*(y1 + y2)/2
   }) %>% sum()
}
