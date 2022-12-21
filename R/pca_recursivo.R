#' Realiza analisis de componentes principales buscando una solucion razonable
#'
#' Si recursivo = TRUE, ejecuta un PCA hasta encontrar por lo menos una varianza explicada de .50
#'
#' @param data dataframe
#' @param recursivo TRUE si se quiere recursivo
#' @param puntajes TRUE en caso se requieran los puntajes
#'
#' @return lista con la varianza explicada, cargas y puntajes del primer componente
#' @export
#'
#' @import lavaan dplyr purrr
#'
#' @examples
#'\dontrun{
#' pca_recursivo(iris[1:4], recursivo = TRUE)
#'}
#'
pca_recursivo <- function(data, recursivo = TRUE, puntajes = TRUE){

  pca_uno <- pca_1(data)

  if(recursivo){

    indi <- pca_uno$varex
    cargafac <- pca_uno$cargas

    if(indi < .50){

      indi_nueva = indi
      cargafac_nueva = cargafac
      data2 = data


      repeat{
        if(nrow(cargafac_nueva) <= 4){break} # si son 4 o menos items, pará

        if(indi_nueva < .50){

          # identificamos items
          if(nrow(filter(cargafac_nueva, abs(Cargas) < 0.4)) == 0){ # si no hay items con cargas menores a 0.4, identificamos el menor
            eliminar = filter(cargafac_nueva, Cargas == min(abs(Cargas)))$Item
          }else{
            eliminar = filter(cargafac_nueva, abs(Cargas) < 0.4)$Item # identificamos items con cargas menores a 0.4
          }

          # retiramos las columnas y nuevo modelo
          data2 <- data2[, !(names(data2) %in% eliminar)]
          pca_dos <- pca_1(data2)
          indi_nueva <- pca_dos$varex
          cargafac_nueva <- pca_dos$cargas

        }else{break} # paramos
      }

      pca_inicial <- reporte_pca(data, corr = "poly", puntajes = FALSE)
      pca_sugerido <- reporte_pca(data2, corr = "poly", puntajes = puntajes)

      lista_pca <- list(pca_inicial = pca_inicial, pca_sugerido = pca_sugerido)

      cargas <- map(lista_pca, "cargas") %>%
        map(~select(.x, 1, 3)) %>%
        reduce(~left_join(.x, .y, by = c("Item"), suffix = c(".inicial", ".sugerido")))

      indicadores <- map_df(lista_pca, "indicadores")

      return(list(cargas = cargas, indicadores = indicadores))

    }else{

      pca_inicial <- reporte_pca(data, corr = "poly", puntajes = puntajes)
      return(pca_inicial)

    }

  }

  pca_inicial <- reporte_pca(data, corr = "poly", puntajes = puntajes)
  return(pca_inicial)


}

