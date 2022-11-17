#' Analisis de componentes principales (PCA)
#'
#' Realiza un analisis de componentes principales y devuelve la varianza explicada, las cargas y puntajes del primer componente
#'
#' @param x dataframe
#' @param corr con "poly" realiza correlación policorica
#' @param puntajes si es que se quiere puntajes
#'
#' @return lista con la varianza explicada, cargas, alpha de cronbach y puntajes del primer componente
#' @export
#'
#' @import psych
#'
#' @examples
#'
#' reporte_pca(iris[1:4])
#'
reporte_pca <- function(x, corr = NULL, puntajes = TRUE){

  if(sum(sapply(x, function(xx) sum(is.na(xx)))) > 0) stop("Es preferible que no hayan NAs en las columnas =)")

  if(is.null(corr)){
    ee <- eigen(cor(x), symmetric = FALSE) # symmetric=FALSE previene cargas negativas [espero]
  }else{
    cor_pol <- psych::polychoric(x)$rho
    ee <- eigen(cor_pol, symmetric = FALSE)
    alfa <- psych::alpha(cor_pol, warnings = FALSE)$feldt$alpha[[1]] #Confiabilidad
  }

  #calculamos varianza (val), cargas (l), pesos (w)
  val <- ee$values
  val_sq <- sqrt(val) #desviacion
  l <- ee$vectors %*% diag(val_sq)
  w <- ee$vectors %*% diag(1/val_sq)

  if(all(l[, 1] < 0)) {l[, 1] <- l[, 1]*-1; w[, 1] <- w[, 1]*-1} # ¿por que? U_U

  if(puntajes == TRUE){
    z <- as.matrix(scale(x)) # datos estandarizados y matrix
    s <- z %*% l # datos estandarizados por sus cargas
    s <- scale(s)
  }

  cargas <- data.frame(Item = names(x), Pesos = w[, 1], Cargas = l[, 1])
  vr <- c("Pesos", "Cargas")
  cargas[vr] <- apply(cargas[vr], 2, function(x) format(round(x, 3), decimal.mark = ","))
  varex <- format(round(val[1]/sum(val)*100, 2), decimal.mark = ",")

  if(puntajes == TRUE){
    return(list(puntajes = s[, 1], indicadores = varex, cargas = cargas, confiabilidad = alfa))}
  else {
    return(list(indicadores = varex, cargas = cargas, confiabilidad = alfa))}

}
