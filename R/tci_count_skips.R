#' Get number of rows until the measurement data
#'
#' The function tci.count.skips identify the number of rows that must
#' be skipped until measurement data of any TCI file is detected. This
#' information will permit reading TCI data when used read.csv function since
#' the number of rows to be skipped is already known.
#' @param tci_fdf Data frame with information of the path and TCI file name to
#' be read
#' @examples wd <- getwd()
#' setwd(system.file("extdata", package = "cotfitools"))
#' mon_info <- tci.find.dir()
#' n_skips <- tci.count.skips(mon_info)
#' setwd(wd)
#' @usage tci.count.skips(tci_fdf)
#' @family TCI file management functions
#' @export
tci.count.skips <- function(tci_fdf) {
  n <- nrow(tci_fdf)
  #Se inicializa la variable de salida "tci_skip" como un dataframe vacio
  tci_skip <- rep(0, n)
  #Se configuran los parametros de la barra de carga
  bar <- winProgressBar(title = "CoTFi Tools: Preparing",
                        label = "Progress: ",
                        min = 0, max = 100, initial = 0, width = 350)
  #Se ejecuta un loop para cada uno de los archivos de la lista de entrada
  for (i in 1:n) {
    #Se forma la cadena de texto con la direccion de cada archivo a leer
    tci_fpath <- file.path(tci_fdf$Path[i], tci_fdf$File[i])
    #Se extraen las primeras dos filas del archivo .csv de tci
    row <- read.csv(file = tci_fpath, nrows = 2, skip = 5,
                    header = FALSE, sep = "^", blank.lines.skip = FALSE)
    #Se inicializa la variable "j" la cual irá contando el numero de espacios
    #hasta llegar a la fila de datos de medicion
    j <- 1
    while (!(all(is.na(row[2, ])))) {
      #Mientras la segunda fila extraida contenga datos que leer, entonces:
      #Leemos un nuevo conjunto de dos filas desplanzandonos una posicion
      #hacia abajo
      row <- read.csv(file = tci_fpath, nrows = 2, skip = 5+j,
                      header = FALSE, sep = "^", blank.lines.skip = FALSE)
      #Contamos el numero de veces que se ejecuto el loop
      j <- j+1
    }
    #Almacenamos la cantidad de espacios en la variable "tci_skip"
    tci_skip[i] <- j + 5
    #Se actualizan los parametros de la barra de carga del proceso
    setWinProgressBar(bar, value = i*100/n,
                      title = paste("CoTFi Tools: Preparing",
                                    i, "of", n, "files"),
                      label = paste("Progress: ",
                                    round(i*100/n), "%", sep = ""))
  }
  #Cerramos la barra de carga
  close(bar)
  #Se devuelven los resultados del calculo
  return(tci_skip)
}
