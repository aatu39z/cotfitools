#' Determinar el numero de filas a saltar para llegar a los datos de medicion
#'
#' La funcion "tci.count.skips" ayuda a identificar el numero de filas que deben
#' saltarse dentro de un archivo de medicion de TCI con el objetivo de extraer
#' los datos registrados en cada tarea de medicion.
#' @param tci_fdf Dataframe con la informacion de los archivos a leer. Sin valor
#' por defecto configurado.
#' @export
#' @examples wd <- getwd()
#' setwd(system.file("extdata", package = "cotfitools"))
#' start <- as.POSIXct("2021-01-01")
#' stop <- as.POSIXct("2021-12-31")
#' mon_info <- tci.find.dir(stt_date = start, stp_date = stop)
#' n_skips <- tci.count.skips(mon_info)
#' setwd(wd)
#' @usage tci.count.skips(tci_fdf)
#' @family TCI file management functions
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
    tci_fpath <- file.path(tci_fdf[i, 1], tci_fdf[i, 2])
    #Se extraen las primeras dos filas del archivo .csv de tci
    row <- read.csv(file = tci_fpath, nrows = 2, skip = 5,
                    header = FALSE, sep = "^", blank.lines.skip = FALSE)
    #Se incializa la variable "j" la cual irá contando el numero de espacios
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
