#' Calcula el ICC
#'
#' Calcula el coeficiente de correlacion intraclase de un modelo nulo (ICC)
#'
#' @param model_lmer Output de lme4::lmer en el modelo nulo
#'
#' @return icc del modelo nulo
#' @export
#'
#' @importFrom lme4 VarCorr
#'
#' @examples
#' \dontrun{
#'fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#'calc_icc(fm1)
#'}
#'
calc_icc <- function(model_lmer){
  var <- as.data.frame(lme4::VarCorr(model_lmer))
  icc <- (var$vcov[1]/(var$vcov[1]+var$vcov[2]))
  return(icc)
}
