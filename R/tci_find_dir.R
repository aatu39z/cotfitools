#' Identify TCI files inside directory
#'
#' The function tci.find.dir explores the directory path supplied as input
#' argument and looks for TCI files of measurements data. In brief, the function
#' navigates inside the given directory path and checks if any file meet
#' the criteria of a TCI file. As an output, the function brings back a data frame
#' with the path and the detected TCI file names.
#' @param main_folder_path String of characters with the directory path to be
#' explored. Working directory is the default path.
#' @examples
#' wd <- getwd()
#' setwd(system.file("extdata", package = "cotfitools"))
#' mon_info <- tci.find.dir()
#' View(mon_info)
#' setwd(wd)
#' @usage tci.find.dir(__directory path__)
#' @export
#' @family TCI file management functions
tci.find.dir <- function(main_folder_path = getwd()) {
  dir_list <- list.dirs(path = main_folder_path) #Leyendo carpetas dentro del directorio ppal
  tci.flist <- data.frame() #Inicializando variable tci.flist
  #Declarar variable con los campos que contiene un archivo .csv de TCI en ingles
  tci_en <- c("Task ID", "Storage Interval", "Operator ID",
              "Message Length (secs)", "Start Time", "Stop Time",
              "Threshold Method", "Duration", "Station Name",
              "All Single Channels", "Location (lat)", "Location (lon)",
              "Antenna", "Polarization")
  #Declarar variable con los campos que contiene un archivo .csv de TCI en espanol
  tci_sp <- c("Identificación de la Tarea", "Almacenar Cada", "Operador ID",
              "Longitud del Mensaje (segs)", "Comenzar Tiempo", "Parar Tiempo",
              "El Método del Umbral", "Duración", "Estación",
              "Todos los Monocanales", "Lugar (lat)", "Lugar (lon)",
              "Antena", "Polarización")
  x <- 1
  for (i in dir_list) {
    #Extrayendo lista de archivos .csv del directorio i-esimo
    csv_list <- list.files(path = i, pattern = ".csv")
    #Determinando el numero de elementos en la lista de archivos .csv
    n <- length(csv_list)
    #Configurando parametros para la barra de carga
    bar <- winProgressBar(title = paste("CoTFi Tools: Reading Directory",
                                        x, "of", length(dir_list)),
                          label = paste("Progress: "),
                          min = 0, max = 100, initial = 0, width = 350)
    if (n != 0) {
      #Si la lista contiene archivos .csv que revisar, entonces:
      for (j in 1:n) {
        path <- file.path(i, csv_list[j])
        #Extraer las primeras dos filas de cada archivo .csv
        csv_fields <- read.csv(file = path, sep = "^", skip = 1, nrows = 2,
                               header = FALSE, encoding = "latin1")
        #Extraer las cabeceras de cada archivo .csv
        csv_headers <- as.character(csv_fields[1, ])
        if ((ncol(csv_fields) == 14) && (all(csv_headers == tci_en) || all(csv_headers == tci_sp))) {
          #Si se detecta que es un archivo TCI valido, entonces:
          #Se crea un dataframe con la informacion relacionada a ese archivo
          write_row <- data.frame(Path = i, File = csv_list[j])
          #Se almacena la informacion en la variable de salida
          tci.flist <- rbind(tci.flist, write_row)
        }
        #Actualizando los parametros de la barra de carga
        setWinProgressBar(bar, value = j*100/n,
                          label = paste("Progress: ", round(j*100/n), "%", sep = ""))
      }
    }
    x <- x + 1
    #Cerrando barra de carga
    close(bar)
  }
  if (length(tci.flist) == 0) {
    warning("No valid TCI's files identified in directory")
    tci.flist <- data.frame(Path = main_folder_path, File = NA)
  }
  return(tci.flist)
}
