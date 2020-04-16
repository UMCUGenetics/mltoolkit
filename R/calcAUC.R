#' Calculate AUC
#'
#' @description Calculates the area under a curve (AUC) by integration via the trapezoidal rule, where discrete (x,y)
#' coordinates are given.
#'
#' @param x A numeric/integer vector of discrete x values
#' @param y A numeric/integer vector of discrete y values
#' @na.rm Remove NA's?
#'
#' @return Returns the area under the curve
#' @export
#'
#' @examples calcAUC(1:10, runif(10))

calcAUC <- function(x, y, na.rm = FALSE){
   
   # x=perf$tpr
   # x_store=x
   # y=perf$ppv
   # y_store=y
   
   if(length(x) != length(y)){ stop('x must have the same number of elements as y') }
   if(any(y<0)){ stop('Some values of y are < 0') }
   
   if((any(is.na(y)) | any(is.na(x))) & na.rm==F) 
   {
      ## warning("y or x contains NA with na.rm=F - returning NA\n")
      return(NA)	
   }
   
   ## Remove any missing values and issue warning if removing values
   if(na.rm) {
      ## Locate missing values in both vectors
      miss <- as.logical(is.na(x) + is.na(y))
      if(any(miss)) {
         x <- x[!miss]
         y <- y[!miss]
         ## warning(paste(sum(miss), "missing values removed\n"))
         if(length(x) < 2) {
            warning("No valid observations remaining after NA removal")
            return(NA)	
         }
      }
   }
   
   dx <- diff(x)
   yfirst <- y[-length(y)]
   ylast <- y[-1]
   
   out <- sum(dx*(yfirst+ylast)) / 2
   out <- abs(out) # prevent negative AUC's
   
   return(out)
}

# calcAUC <- function(x, y)
# {
#    if(length(x) != length(y)){ stop('x must have the same number of elements as y') }
#    if(any(y<0)){ stop('Some values of y are < 0') }
#    
#    m <- cbind(x,y)
#    m <- m[order(m[,'x']),]
#    
#    sum(
#       sapply(1:(nrow(m)-1),function(i){
#          trapez <- m[i:(i+1),]
#          
#          x1 <- trapez[1,'x']
#          x2 <- trapez[2,'x']
#          y1 <- trapez[1,'y']
#          y2 <- trapez[2,'y']
#          
#          (x2 - x1)*(y1 + y2)/2
#       }, USE.NAMES=F)
#    )
# }
