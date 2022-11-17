#' Matriz de correlaciones
#'
#' Lo mismo que cor() pero con algunas acomodaciones. Si hay NA usa "pairwise.complete.obs".
#'
#' @param data data frame
#' @param r redondear valores
#' @param long en caso se requiera en formato dataframe
#' @param corr si colocas 'poly' aplica correlacion policorica
#'
#' @return matriz de correlaciones
#' @export
#'
#' @import psych
#'
#' @examples
#' cor2(iris[1:4])
#'
cor2 <- function(data, r = 2, long = NULL, corr = NULL){

  if(is.null(corr)){
    mc <- round(cor(data, use = "pairwise.complete.obs"), r)
  }else{
    mc <- round(psych::polychoric(data, na.rm = TRUE)$rho, r)
  }

  if(!is.null(long)){

    df_mc <- data.frame(
      v1 = rownames(mc)[row(mc)],
      v2 = colnames(mc)[col(mc)],
      corr = c(mc))

    mc <- subset(df_mc, v1 != v2) #quitamos repetidos

  }

  return(mc)
}
