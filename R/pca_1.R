#' PCA
#'
#' @param x dataframe
#'
#' @return lista con la varianza explicada y cargas
#' @export
#'
#' @importFrom psych polychoric
#'
#' @examples
#'reporte_pca(iris[1:4])
#'
#'
pca_1 <- function(x){
  ee <- eigen(psych::polychoric(x)$rho, symmetric = FALSE)
  val <- ee$values #varianza
  l <- ee$vectors %*% diag(sqrt(val)) #cargas
  if(all(l[, 1] < 0)) {l[, 1] <- l[, 1]*-1} # ¿por que? U_U
  cargas <- data.frame(Item = names(x), Cargas = l[, 1])
  varex <- val[1]/sum(val)
  return(list(cargas = cargas, varex = varex))

}
