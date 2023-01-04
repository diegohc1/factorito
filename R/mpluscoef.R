
#' Parametros estimados de mplus_hlm
#'
#' @param mplus_hlm_salida objeto de mplus_hlm
#'
#' @return Devuelve parametros estimados de mplus_hlm
#' @export
#'
#' @examples
#' \dontrun{
#' mpluscoef(m1)
#' }
#'
mpluscoef <- function(mplus_hlm_salida){
  return(mplus_salida$results$parameters$unstandardized)
}


#' ICC estimado de mplus_hlm
#'
#' @param mplus_hlm_salida objeto de mplus_hlm
#'
#' @return Devuelve ICC estimado de mplus_hlm
#' @export
#'
#' @examples
#' \dontrun{
#' mplusicc(m1)
#' }
#'
#'
#'
mplusicc <- function(mplus_hlm_salida){
  return(mplus_hlm_salida$data_summary$ICCs)
}


