#' Convertir texto en numero
#'
#' Funcion que convierte una cadena de caracteres en un numero.
#'
#' @export

txt2num <- function(txt){
  n <- nchar(txt)
  x <- NA
  if (n != 0){
    x <- gsub("[^0-9.]", "", txt)
    if (x != ""){
      x <- as.numeric(x)
      if (grepl("k", txt, ignore.case = TRUE)){
        x <- x/1000
      } else if (grepl("m", txt, ignore.case = TRUE)){
        x <- x*1
      } else if (grepl("g", txt, ignore.case = TRUE)){
        x <- x*1000
      }
    } else {
      x <- NA
    }
  }
  return(x)
}
