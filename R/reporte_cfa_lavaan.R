#' Acomoda informacion del CFA para el reporte
#'
#' Toma un objeto cfa del paquete lavaan y acomoda la informacion para el reporte psicometrico
#'
#' @param model_cfa_lavaan objeto cfa de lavaan
#' @param puntajes si quieres puntajes
#'
#' @return lista con indicadores de ajuste, cargas factoriales y confiabilidad
#' @export
#'
#' @import lavaan
#' @importFrom semTools compRelSEM
#'
#' @examples
#' \dontrun{
#' HS.model <- ' visual  =~ x1 + x2 + x3
#' textual =~ x4 + x5 + x6
#' speed   =~ x7 + x8 + x9 '
#' fit <- lavaan::cfa(HS.model, data = lavaan::HolzingerSwineford1939
#' reporte_cfa_lavaan(fit, puntajes = FALSE)
#'}
#'
reporte_cfa_lavaan <- function(model_cfa_lavaan, puntajes = TRUE){

  #cargas factoriales estandarizadas
  m <- lavaan::standardizedSolution(model_cfa_lavaan, ci = FALSE, cov.std = FALSE)
  m <- m[which(m$op == "=~"), ]
  m <- within(m, {stars =
    ifelse(pvalue < 0.001, "***",  ifelse(pvalue < 0.01, "**",  ifelse(pvalue < 0.05, "*", " ")))})
  m <- m[c("lhs", "rhs", "est.std", "se", "stars")]
  m <- setNames(m, c("Escala", "Item", "Est", "SE", "sig."))
  vredo <- c("Est", "SE") #para redondear
  # m[vredo] <- apply(m[vredo], 2, function(x) format(round(x, 3), decimal.mark = ","))

  #indicadores de ajuste
  fit1 <- lavaan::fitmeasures(model_cfa_lavaan,
                              c("cfi", "tli", "srmr", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))
  indicadores1 <- data.frame(Indicadores = names(fit1)[1:4], Valores = unclass(fit1)[1:4], row.names = NULL)
  indicadores1 <- within(indicadores1, {'IC al 90%' =
    ifelse(Indicadores == "rmsea",  paste0("[", round(fit1[5], 3), "-", round(fit1[6], 3), "]"), "")})

  # el paquete indica que "reliability" esta deprecated, indica usar "compRelSEM"
  omega1 <- semTools::compRelSEM(model_cfa_lavaan)

  if(puntajes == TRUE){
    puntajes1 <- as.data.frame(lavaan::lavPredict(model_cfa_lavaan))
  }

  if(puntajes == TRUE){
    return(list(cargas = m, indicadores = indicadores1, omega1 = omega1, puntajes = puntajes1))}
  else {return(list(cargas = m, indicadores = indicadores1, omega1 = omega1))}

}
