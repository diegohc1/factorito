#' Realiza analisis factorial confirmatorio buscando una solucion razonable
#'
#' Si recursivo = TRUE, ejecuta un CFA hasta encontrar una solucion razonable. CFI y TLI > 0.95; SRMR y RMSEA < 0.10.
#'
#' @param data dataframe
#' @param model_lavaan sintaxis de lavaan (text)
#' @param recursivo TRUE si se quiere recursivo
#' @param puntajes TRUE en caso se requieran los puntajes
#'
#' @return lista con indicadores de ajuste, cargas factoriales y confiabilidad
#' @export
#'
#' @import lavaan dplyr purrr
#'
#' @examples
#' \dontrun{
#' HS.model <- ' visual  =~ x1 + x2 + x3
#' textual =~ x4 + x5 + x6
#' speed   =~ x7 + x8 + x9 '
#' cfa_recursivo_lavaan(lavaan::HolzingerSwineford1939, HS.model, recursivo = FALSE)
#'}
#'
cfa_recursivo_lavaan <- function(data, model_lavaan, recursivo = TRUE, puntajes = TRUE){

  mod1 <- cfa(model_lavaan, data = data, ordered = TRUE, mimic = "Mplus", estimator = "WLSMV")

  if(recursivo){

    indi <- lavaan::fitmeasures(mod1,  c("cfi", "tli", "srmr", "rmsea"))
    cargafac <- subset(lavaan::parameterEstimates(mod1), op == "=~")


    if(
      any(c(purrr::map_lgl(indi[c("cfi", "tli")], ~.x < 0.95),
            purrr::map_lgl(indi[c("srmr", "rmsea")], ~.x > 0.10)))
    ){

      indi_nueva = indi
      cargafac_nueva = cargafac

      repeat{
        if(length(lavaan::lavNames(mod1, type = "lv")) == 1){
          if(nrow(cargafac_nueva) <= 4){break} # 1 var latente: si son 4 o menos items, pará
        }else{
          # 2 var latente: si son 3 o menos items en alguno, pará
          if(any(purrr::map_lgl(lapply(split(cargafac_nueva, cargafac_nueva$lhs), nrow), ~.x <= 3))){break}
        }

        if(any(c(purrr::map_lgl(indi_nueva[c("cfi", "tli")], ~.x < 0.95),
                 purrr::map_lgl(indi_nueva[c("srmr", "rmsea")], ~.x > 0.10)))){

          # identificamos items
          if(nrow(filter(cargafac_nueva, est < 0.4)) == 0){ # si no hay items con cargas menores a 0.4, identificamos el menor
            eliminar = filter(cargafac_nueva, est == min(est))$rhs
          }else{
            eliminar = filter(cargafac_nueva, est < 0.4)$rhs # identificamos items con cargas menores a 0.4
          }

          cargafac_nueva = filter(cargafac_nueva, !rhs %in% all_of(eliminar)) # nuevo modelo

          modstring <- split(cargafac_nueva, cargafac_nueva$lhs) %>%
            purrr::map(~paste(pull(.x, rhs), collapse = "+")) %>%
            purrr::imap(~paste(.y, .x, sep = '=~')) %>%
            paste(collapse = "\n")

          mod2 <- lavaan::cfa(modstring, data = data, ordered = TRUE, mimic = "Mplus", estimator = "WLSMV")

        }else{break} # paramos
      }

      cfa_inicial <- reporte_cfa_lavaan(mod1, puntajes = FALSE)
      cfa_sugerido <- reporte_cfa_lavaan(mod2, puntajes = puntajes)

      lista_cfa <- list(cfa_inicial = cfa_inicial, cfa_sugerido = cfa_sugerido)

      cargas <- map(lista_cfa, "cargas") %>%
        map(~select(.x, -4, -5)) %>%
        reduce(~left_join(.x, .y, by = c("Escala", "Item"), suffix = c(".inicial", ".sugerido")))

      indicadores <- map(lista_cfa, "indicadores") %>%
        map(~select(.x, -3)) %>%
        reduce(~left_join(.x, .y, by = c("Indicadores"), suffix = c(".inicial", ".sugerido")))

      return(list(cargas = cargas, indicadores = indicadores))

    }else{

      cfa_inicial <- reporte_cfa_lavaan(mod1, puntajes = puntajes)
      return(cfa_inicial)

    }

  }

  cfa_inicial <- reporte_cfa_lavaan(mod1, puntajes = puntajes)
  return(cfa_inicial)


}
