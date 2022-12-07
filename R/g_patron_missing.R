#' Visualizar patron de valores perdidos
#'
#' Visualizar patron de valores perdidos de un dataframe.
#'
#' @param data data frame
#' @param cols para especificar algunas columnas
#' @param text.size cambiar tamaño del texto de las columnas
#'
#' @return grafico
#' @export
#'
#' @import dplyr ggplot2 tidyr
#' @importFrom ggplot2 alpha
#'
#'
#' @examples
#' g_patron_missing(data = iris)
#' g_patron_missing(data = iris, cols = c("Sepal.Length", "Sepal.Width"), text.size = 10)
#'
g_patron_missing <- function(data, cols = NULL, text.size = 5){

  if(!is.null(cols)){data <- data[cols]}

  data %>%
    dplyr::mutate(id = row_number()) %>%
    tidyr::gather(-id, key = "key", value = "val") %>%
    dplyr::mutate(isna = is.na(val)) %>%
    ggplot2::ggplot(aes(rev(key), id, fill = isna)) +
    ggplot2::geom_raster(alpha = 0.8) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.text.y = element_text(size = text.size, color = "black", hjust = 0), #
          axis.text.x = element_blank(),
          legend.position = "bottom",
          axis.title = element_text(size = 8.5),
          legend.text = element_text(size = 8, color = "#5A5D63"),
          plot.margin = unit(c(0, 0, 0.0, 0), "cm")) +
    scale_fill_manual(name = "", values = c('gray', 'black'), labels = c("Presente", "Missing")) +
    labs(x = 'Pregunta\n', y = "Observaciones", title = " ") +
    scale_x_discrete(labels = rev(names(data))) +
    coord_flip()

}

