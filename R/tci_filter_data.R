#' Filtrar datos de medicion por frecuencia
#'
#' La funcion permite filtrar por frecuencia los resultados de las mediciones
#' @param tci_data Dataframe con el contenido de las mediciones
#' @param fqrange Vector con el rango de frecuencias a filtrar
#' @export
#' @family TCI file management functions
tci.filter.data <- function(tci_data, fqrange) {
  fq_filter <- (tci_data$Frequency_MHz >= fqrange[1]) & (tci_data$Frequency_MHz <= fqrange[2])
  filt_data <- tci_data[fq_filter, ]
  return(filt_data)
}
