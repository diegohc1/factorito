



tablaf <- function(data, x, peso = NULL, na = NULL, compara = NULL){

  if(!require(dplyr)) stop("'dplyr' debe estar instalado")

  if(!is.null(peso)){peso_s <- dplyr::sym(peso)}
  if(!is.null(peso)){
    if(!sum(is.na(data[[peso]])) == 0) stop("La variable de pesos tiene missing")
  }

  x_s <- dplyr::sym(x)

  data2 <- data %>%
    {if(!is.null(peso)) dplyr::count(., {{x_s}}, wt = {{peso_s}}) else dplyr::count(., {{x_s}})} %>%
    {if(is.null(na)) dplyr::filter(., !is.na({{x_s}})) else .} %>%
    dplyr::mutate(prop = round(prop.table(n)*100, 1))

  names(data2)[names(data2) == x] <- "opcion"

  if(dplyr::is_grouped_df(data) & !is.null(compara)){ #util para comparar los %
    data2 <- tidyr::pivot_wider(data2, -n, names_from = names(group_keys(data)), values_from = prop)
  }

  return(data2)


}


