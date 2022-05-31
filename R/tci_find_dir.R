#' Cargar archivos ".csv" de TCI
#'
#' La funcion "tci.find.dir()" explora los archivos que se encuentran dentro
#' de la carpeta principal otorgada como argumento de entrada, con el objetivo de
#' cargar la informacion de los archivos TCI identificados dentro de esta. La
#' funcion devuelve a su salida un dataframe con el directorio, el nombre de los
#' archivos identificados y el periodo de medicion asociado a cada archivo de
#' medicion correspodniente
#' @param main_folder_path String de caracteres con la direccion de la carpeta
#' a explorar. Por defecto se toma el directorio actual de trabajo.
#' @param stt_date Fecha inicial de la medicion del tipo as.POSIXct. Por defecto
#' se toman seis meses antes de la fecha actual del sistema
#' @param stp_date Fecha final de la medicion del tipo as.POSIXct. Por defecto
#' se toma la fecha actual del sistema.
#' @examples wd <- getwd()
#' setwd(system.file("extdata", package = "cotfitools"))
#' start <- as.POSIXct("2021-01-01")
#' stop <- as.POSIXct("2021-12-31")
#' mon_info <- tci.find.dir(stt_date = start, stp_date = stop)
#' View(mon_info)
#' setwd(wd)
#' @usage tci.find.dir()
#' @export
#' @family TCI file management functions
tci.find.dir <- function(main_folder_path = getwd(),
                            stt_date = as.POSIXct(Sys.time()) - 6*31*24*60*60,
                            stp_date = as.POSIXct(Sys.time())) {
  #Configurando parametros de hora local del sistema
  prev <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
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
    bar <- winProgressBar(title = "CoTFi Tools: Reading Directory",
                          label = paste("Progress: "),
                          min = 0, max = 100, initial = 0, width = 350)
    if (n != 0) {
      #Si la lista contiene archivos .csv que revisar, entonces:
      for (j in 1:n) {
        path <- file.path(i, csv_list[j])
        #Extraer las primeras dos filas de cada archivo .csv
        ex_row <- read.csv(file = path, sep = "^", skip = 1,
                           nrows = 2, header = FALSE)
        #Extraer primera fila con los nombres de campo del archivo a inspeccionar
        csv_fields <- as.character(ex_row[1, ])
        if (all(csv_fields == tci_en | csv_fields == tci_sp)) {
          #Si se detecta que es un archivo TCI valido, entonces:
          #Se extrae informacion de fecha inicial y fecha final de cada archivo
          stt_tcidate <- as.POSIXct(ex_row[2,5], format = "%m/%d/%Y %I:%M:%S %p")
          stp_tcidate <- as.POSIXct(ex_row[2,6], format = "%m/%d/%Y %I:%M:%S %p")
          if ((stt_tcidate < stp_date) & (stp_tcidate > stt_date)) {
            #Si la medicion del archivo se encuentra en el rango de fechas
            #establecido, entonces:
            #Se crea un dataframe con la informacion relacionada a ese archivo
            write_row <- data.frame(Path = i,
                                    File = csv_list[j],
                                    Start_Time = stt_tcidate,
                                    Stop_Time = stp_tcidate)
            #Se almacena la informacion en la variable de salida
            tci.flist <- rbind(tci.flist, write_row)
          }
        }
        #Actualizando los parametros de la barra de carga
        setWinProgressBar(bar, value = j*100/n,
                          title = paste("CoTFi Tools: Reading Directory",
                                        x, "of", length(dir_list)),
                          label = paste("Progress: ",
                                        round(j*100/n), "%", sep = ""))
      }
    }
    x <- x + 1
    #Cerrando barra de carga
    close(bar)
  }
  #Se regresan los parametros de hora local a su estado original
  Sys.setlocale("LC_TIME", prev)
  #Se devuelve el resultado de la busqueda
  return(tci.flist)
}
