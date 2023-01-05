#' Regresion multinivel en Mplus
#'
#' Regresion multinivel con pesos. Una version mas fluida para usar las bondades de MplusAutomation. Debe estar instalado Mplus.
#'
#' @param data dataframe
#' @param y variable dependiente
#' @param x1 variable independiente de nivel 1
#' @param x2 variable independiente de nivel 2
#' @param peso_est peso de estudiante
#' @param peso_ie peso de escuela
#' @param idie id de escuela
#' @param r ruta y nombre del archivo Mplus. La funcion crea un archivo .dat y .out de Mplus.
#'
#' @return objeto mplus
#' @export
#'
#' @import glue MplusAutomation
#'
#' @examples
#' \dontrun{
#' rr <- "D:/EM22/02-reportes-scripts/03-cruce-con-rendimiento/2-modelos-ffaa/mods/mod"
#' m00 <- reg_mplus(bd1,
#'                  y = "M500_L",
#'                  x1 = "ise"
#'                  peso_est = "Peso_mate",
#'                  peso_ie = "peso_ie",
#'                  idie = "cod_mod7",
#'                  r = rr)
#' }
#'

mplus_hlm <- function(data, y, x1 = NULL, x2 = NULL, peso_est, peso_ie, idie, r){

  if (is.null(x2)) {x2 = ""} else x2 <- paste(x2, collapse = " ")
  if (is.null(x1)) {x1 = ""} else x1 <- paste(x1, collapse = " ")

  # definimos las variables
  vars <-
    glue::glue("CLUSTER = {idie};
     WEIGHT = {peso_est};
     BWEIGHT  = {peso_ie};
     WTSCALE  = CLUSTER;
     BWTSCALE = SAMPLE;
     USEVARIABLES = {y} {x1} {x2};
     WITHIN = {x1};
     BETWEEN = {x2};")

  # definimos los modelos
  if (is.null(x1)){ # solo nivel2
    mod <- glue::glue("%BETWEEN% \n {y} on {x2};")
  } else if (is.null(x2)){ # solo nivel 1
    mod <- glue::glue("%WITHIN% \n {y} on {x1};")
  } else { #todos
    mod <- glue::glue("%WITHIN% \n {y} on {x1}; \n %BETWEEN% \n {y} on {x2};")
  }

  # objeto mplus
  text_mplus <- MplusAutomation::mplusObject(
    TITLE = "Modelo XXX;",
    VARIABLE = vars,
    ANALYSIS = "type = TWOLEVEL;" ,
    MODEL = mod,
    rdata = data,
  )

  # corremos modelo y lo regresamos
  mplus_out <- MplusAutomation::mplusModeler(
    text_mplus,
    dataout = paste0(r, ".dat"),
    modelout = paste0(r, ".inp"),
    check = TRUE,
    run = TRUE,
    hashfilename = FALSE)

}
