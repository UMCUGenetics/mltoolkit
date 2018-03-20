#' Remove PCAWG donor name suffix
#'
#' @param chr A character vector of PCAWG donor names with the format: PD01004a2
#'
#' @return Removes the a/b/a2/b2 suffix to give PD01004
#'
#' @examples rmDonorSuffix(PD01004a2)

rmDonorSuffix <- function(chr)
{
   chr %>% str_extract_all(.,'PD\\d+') %>% unlist()
}
