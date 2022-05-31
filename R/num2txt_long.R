#' Convertir numero en texto
#'
#' Funcion que convierte un numero en una cadena de caracteres
#'
#' @export

num2txt.lg <- function(num){
  x <- ""
  if (!is.na(num)) {
    if (num < 3) {
      x <- paste(num*1000, "kHz")
    } else if (num <= 3000) {
      x <- paste(num, "MHz")
    } else if (num > 3000){
      x <- paste(num/1000, "GHz")
    }
  }
  return(x)
}
