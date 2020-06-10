#' Calculate AUC
#'
#' @description Calculates the area under a curve (AUC) by integration via the trapezoidal rule, 
#' where discrete (x,y) coordinates are given.
#'
#' @param x A numeric/integer vector of discrete x axis values
#' @param y A numeric/integer vector of discrete y axis values
#' @param na.rm Remove NA's?
#'
#' @return Returns the area under the curve
#' @export
#'
#' @examples calcAUC(1:10, runif(10))
#'
calcAUC <- function(x, y, na.rm = FALSE){
   
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
   
   ## Ensure values are ordered by x
   df <- data.frame(x,y)
   df <- df[order(x),]
   x <- df$x
   y <- df$y
   rm(df)
   
   ## Main
   dx <- diff(x)
   yfirst <- y[-length(y)]
   ylast <- y[-1]
   
   out <- sum(dx*(yfirst+ylast)) / 2
   out <- abs(out) # prevent negative AUC's
   
   return(out)
}

