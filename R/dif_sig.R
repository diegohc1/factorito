#' Contraste de hipotesis
#'
#' Toma la estimacion puntual y los errores estandar asociados para establecer diferencias estadisticas
#'
#' @param data data frame
#' @param m estimacion
#' @param se error estandar de la estimacion
#' @param nom grupos
#' @param long en caso se quiera en long
#'
#' @return data frame indicando diferencias estadisticas
#' @export
#'
#' @import tidyr
#'
#' @examples
#' tabla1 <- data.frame(
#'  sex = c("Hombre", "Mujer"),
#'  mean1 = c(317.68, 335.34),
#'  se1 = c(7.22, 6.17)
#'  )
#' dif_sig(tabla1, m = "mean1", se = "se1", long = TRUE)
#'
#'
dif_sig <- function(data, m, se, nom = NULL, long = NULL){

  m <- data[[m]]
  if(length(m) != 2) stop("La prueba de hipotesis entre dos variables")

  if(!is.null(nom)){
    nm <- setNames(m, data[[nom]])
  }else{
    nm <- setNames(m, c("v1", "v2"))}

  dif <- diff(m)
  se <- data[[se]]
  zm <- dif/sqrt(sum(se^2))
  p_valor <- ifelse(zm < 0, 2*pnorm(zm), 2*(1-pnorm(zm)))
  sig <- ifelse(p_valor < 0.05, 1, 0)

  nomgrup <- data.frame(as.list(nm))
  tab1 <- cbind(nomgrup, data.frame(dif = dif, pval = round(p_valor, 4), sig = sig))

  if(!is.null(long)){
    cls <- c("dif", "pval", "sig")
    tab1 <- tidyr::pivot_longer(tab1, names_to = "grupo", values_to = "medida", cols = -all_of(cls))
    tab1 <- within(tab1, {se = se})
    tab1 <- tab1[c("grupo", "medida", "se", cls)]
  }

  return(tab1)

}
