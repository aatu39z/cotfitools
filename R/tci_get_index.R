#' Obtener indice con informacion de medicion de archivos TCI
#'
#' La funcion 'get.tci.findex' permite crear un indice con infomacion concerniente
#' al tipo de parametros que se utilizaron para configurar cada tarea de medicion.
#' @param tci_fdf Dataframe con informacion del directorio, nombre del archivo,
#' fecha inicial y fecha final de la medicion de los archivos TCI.
#' @param nskips Vector tipo numerico con el detalle del numero de filas que
#' deben saltarse en cada archivo TCI para extraer la informacion del indice.
#' @export
#' @usage get.tci.findex(tci_fdf, nskips)
#' @examples wd <- getwd()
#' setwd(system.file("extdata", package = "cotfitools"))
#' start <- as.POSIXct("2021-01-01")
#' stop <- as.POSIXct("2021-12-31")
#' mon_info <- tci.find.dir(stt_date = start, stp_date = stop)
#' n_skips <- tci.count.skips(mon_info)
#' index <- tci.get.index(mon_info, n_skips)
#' setwd(wd)
#' @family TCI file management functions
tci.get.index <- function(tci_fdf, nskips) {
  #Se crea variable "tci_findex" como un dataframe vacio
  tci_findex <- data.frame()
  #Se determina el numero de filas presentes en el dataframe "tci_fdf"
  n <- nrow(tci_fdf)
  #Se configuran los parametros de la barra de carga
  bar <- winProgressBar(title = "CoTFi Tools: Building Measurements Index",
                        label = "Progress: ",
                        min = 0, max = 100, initial = 0, width = 350)
  #Se ejecuta un loop a lo largo de todos los registros de "tci_fdf"
  for (i in 1:n) {
    #Se construye la direccion del archivo a leer
    tci_path <- file.path(tci_fdf[i, 1], tci_fdf[i, 2])
    #Se lee la informacion del archivo .csv de TCI para construir el indice
    index_data <- read.csv(file = tci_path, nrows = nskips[i], skip = 1,
                           header = FALSE, sep = "^", blank.lines.skip = FALSE)
    #Se ejecuta un loop para cada una de las sub bandas de frecuencias de tal
    #manera que cada sub banda vaya acompañada de informacion de referencia
    for (j in 1:(nskips[i] - 5)) {
      #Se contruye fila con informacion del indice
      row_to_insert <- cbind(tci_fdf[i, ],
                             index_data[(j+4), c(1, 2, 3, 4)],
                             index_data[2, c(-5, -6)],
                             nskips[i])
      #Se inserta la fila construida como un registro nuevo en "tci_findex"
      tci_findex <- rbind(tci_findex, row_to_insert)
    }
    #Se actualizan los parametros de la barra de carga del proceso
    setWinProgressBar(bar, value = i*100/n,
                      title = paste("CoTFi Tools: Building Measurements Index",
                                    i, "of", n, "files"
                                    ),
                      label = paste("Progress: ",
                                    round(i*100/n), "%", sep = "")
                      )
  }
  #Se genera un vector de texto con los nombres de las filas
  names_row <- seq(1, nrow(tci_findex))
  #Se nombran las filas como numeros consecutivos
  row.names(tci_findex) <- names_row
  #Se da nombre a las columnas del dataframe a entregar
  colnames(tci_findex) <- c("Path", "File", "Start_Time", "Stop_Time",
                            "Band_Number", "Start_Fq_MHz", "Stop_Fq_MHz",
                            "Bandwidth_kHz", "Task_ID", "Storage_Interval",
                            "Operator_ID", "Message_Length", "Threshold_Method",
                            "Duration", "Station_Name", "All_Single_Channels",
                            "Location_Lat", "Location_Long", "Antenna",
                            "Polarization", "Skips")
  #Se establece el tipo de clase para cada columna del dataframe a entregar
  tci_findex <- transform(tci_findex, Band_Number = as.integer(Band_Number),
                          Start_Fq_MHz = as.numeric(Start_Fq_MHz),
                          Stop_Fq_MHz = as.numeric(Stop_Fq_MHz),
                          Bandwidth_kHz= as.numeric(Bandwidth_kHz),
                          Task_ID = as.integer(Task_ID),
                          Location_Lat = as.numeric(Location_Lat),
                          Location_Long = as.numeric(Location_Long))
  #Cerramos la barra de carga
  close(bar)
  #Se devuelve el resultado de la funcion
  return(tci_findex)
}
