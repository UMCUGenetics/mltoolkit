#' Natural logarithm
#'
#' @param x A numeric/integer vector
#'
#' @return Returns an ln() transformed numeric/integer vector
#' @export
#'
#' @examples ln(2)

ln <- function(x)
{
   log(x, exp(1))
}
