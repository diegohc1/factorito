#' Tabla de frecuencias con varias columnas (Frequency table with many columns)
#'
#' Aplica [tablaf()] a varias columnas
#'
#' @param data data frame
#' @param nomvar vector con el nombre de la variable categorica (categorical variable)
#' @param peso en caso hayan pesos (if there are weights)
#' @param starts TRUE si se quiere aplicar sobre columnas que comparten los mismos caracteres al inicio
#' @param na en caso se consideren valores perdidos ()
#'
#' @return data frame con la cantidad de casos (n) y porcentaje (prop)
#' @export
#'
#' @import dplyr
#'
#' @examples
#' tablaf_col(iris, "Sepal", starts = TRUE)
#'
tablaf_col <- function(data, nomvar, peso = NULL, starts = NULL, na = NULL){

  if(!is.null(starts)){
    nom <- names(data[, grep(paste0("^", nomvar), names(data), value = TRUE)])
    names(nom) <- nom
  }else{
    nom <- nomvar
    names(nom) <- nom
  }

  df2 <- lapply(nom, function(x) tablaf(data, x, peso = peso, na = na))
  dplyr::bind_rows(df2, .id = "var")

}
