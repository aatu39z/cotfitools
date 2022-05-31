#' Redondear frecuencias en MHz
#'
#' Funcion que redondea una frecuencia en MHz al valor mas proximo segun el
#' ancho de banda de resolucion en kHz de las mediciones
#'
#' @export

fq.round <- function(fq, bw) {
  if (!is.na(fq) & !is.na(bw)) {
    fq <- fq*1000
    r <- fq %% bw
    if (r < (bw/2)) {
      fq <- (fq - r)/1000
    } else {
      fq <- (fq - r + bw)/1000
    }
  } else {
    fq <- NA
  }
  return(fq)
}
