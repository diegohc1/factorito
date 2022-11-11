#' Tabla de frecuencias (Frequency table)
#'
#' Lo mismo que table() pero devuelve un data frame y acepta pesos (Same as table() but returns a dataframe and accepts weights)
#'
#' @param data data frame
#' @param x variable categorica (categorical variable)
#' @param peso en caso hayan pesos (if there are weights)
#' @param na en caso se consideren valores perdidos
#' @param compara util para comparar los porcentajes segun el grupo
#'
#' @return data frame con la cantidad de casos (n) y porcentaje (prop)
#' @export
#'
#' @importFrom magrittr "%>%"
#' @import dplyr tidyr
#'
#' @examples
#' tablaf(mtcars, "gear")

tablaf <- function(data, x, peso = NULL, na = NULL, compara = NULL){

  if(!require(dplyr)) stop("'dplyr' debe estar instalado")

  if(!is.null(peso)){peso_s <- dplyr::sym(peso)}
  if(!is.null(peso)){
    if(!sum(is.na(data[[peso]])) == 0) stop("La variable de pesos tiene missing")
  }

  x_s <- dplyr::sym(x)

  data2 <- data %>%
    {if(!is.null(peso)) dplyr::count(., {{x_s}}, wt = {{peso_s}}) else dplyr::count(., {{x_s}})} %>%
    {if(is.null(na)) dplyr::filter(., !is.na({{x_s}})) else .} %>%
    dplyr::mutate(prop = round(prop.table(n)*100, 1))

  names(data2)[names(data2) == x] <- "opcion"

  if(dplyr::is_grouped_df(data) & !is.null(compara)){ #util para comparar los %
    data2 <- tidyr::pivot_wider(data2, -n, names_from = names(group_keys(data)), values_from = prop)
  }

  return(data2)


}


