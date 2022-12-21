#' Acomoda la sintaxis para el cfa
#'
#' Toma la informacion del MIAU y acomoda para a la sintaxis que aplica lavaan::cfa
#'
#' @param data_preg Tabla con informacion del modelo a estimar. Filtramos la escala en el MIAU y aplicamos la funcion.
#'
#' @return Sintaxis para modelos lavaan::cfa
#' @export
#'
#' @importFrom magrittr "%>%"
#' @import purrr
#'
#' @examples
#' \dontrun{
#' acomoda_text_lavaan(preg)
#'}
#'
#'
acomoda_text_lavaan <- function(data_preg){
  if(length(unique(data_preg$Cod_indice2)) == 1){
    mm <- paste(unique(data_preg$Cod_indice), paste(data_preg$cod_preg, collapse = '+'), sep = '=~')

  }else{
    mm <- split(data_preg, data_preg$Cod_indice2) %>%
      purrr::map(~paste(pull(.x, cod_preg), collapse = "+")) %>%
      purrr::imap(~paste(.y, .x, sep = '=~')) %>%
      paste(collapse = "\n")
  }
  return(mm)
}


