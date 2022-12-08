
#' Promedio de una variable segun grupos
#'
#'
#' @param data data frame
#' @param m variable continua
#' @param w en caso hayan pesos (if there are weights)
#' @param vis_na en caso se consideren valores perdidos
#'
#' @return data frame con el promedio segun grupo, la cantidad de casos (n) y porcentaje (prop)
#' @export
#'
#' @import dplyr
#' @importFrom stats complete.cases weighted.mean
#'
#' @examples
#' dplyr::group_by(iris, Species) |> mean_prop_grupo(m = "Sepal.Width", w = "Petal.Width")

mean_prop_grupo <- function(data, m, w = NULL, vis_na = NULL){

  options(dplyr.summarise.inform = FALSE) #para que no aparezca el mensaje de agrupado
  if(!dplyr::is_grouped_df(data)) stop("Los datos deben estar agrupados, 'dplyr::group_by()'")

  if(!is.null(w)){
    w_s <- dplyr::sym(w)
    if(!sum(is.na(data[[w]])) == 0) stop("La variable de pesos tiene missing")
  }

  m_s <- dplyr::sym(m)

  if(is.null(w)){
    tab1 <- dplyr::summarise(data,  media = mean({{m_s}}, na.rm = TRUE), n = n())
  }else{
    tab1 <- dplyr::summarise(data, media = weighted.mean({{m_s}}, w = {{w_s}}, na.rm = TRUE), n = sum({{w_s}}))
  }

  if(is.null(vis_na)) tab1 <- tab1[complete.cases(tab1), ] # retiremos NA o lo contrario

  tab2 <- dplyr::mutate(tab1, prop = round((n/sum(n)*100), 1))
  return(tab2)

}
