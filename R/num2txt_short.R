#' Convertir numero en texto
#'
#' Funcion que convierte un numero en una cadena de caracteres
#'
#' @export

num2txt.sh <- function(num){
  x <- ""
  if (!is.na(num)) {
    if (abs(num) < 1) {
      x <- paste(num*1000, "kHz")
    } else {
      x <- paste(num, "MHz")
    }
  }
  return(x)
}
