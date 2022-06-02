#' Build index with TCI measurement task info
#'
#' The 'get.tci.findex' function allows creating an index with information
#' concerning the type of parameters that were used to configure each
#' measurement task.
#' @param tci_fdf Dataframe with the path and the TCI file names to be included
#' in the index
#' @param nskips Numeric vector with details of the number of rows that must be
#' skipped in each TCI file to extract information.
#' @usage get.tci.findex(tci_fdf, nskips)
#' @examples
#' wd <- getwd()
#' setwd(system.file("extdata", package = "cotfitools"))
#' mon_info <- tci.find.dir()
#' n_skips <- tci.count.skips(mon_info)
#' index <- tci.get.index(mon_info, n_skips)
#' View(index)
#' setwd(wd)
#' @family TCI file management functions
#' @export
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
    #Se lee la informacion del archivo .csv de TCI para construir el indice
    index_data <- read.csv(file = file.path(tci_fdf$Path[i], tci_fdf$File[i]),
                           nrows = nskips[i],
                           skip = 1,
                           header = FALSE,
                           sep = "^",
                           blank.lines.skip = FALSE)
    #Se ejecuta un loop para cada una de las sub bandas de frecuencias de tal
    #manera que cada sub banda vaya acompañada de informacion de referencia

    num <- nskips[i] - 5
    blockA <- as.data.frame(lapply(tci_fdf[i, ], rep, num))
    blockB <- index_data[5:(num + 4), c(1, 2, 3, 4)]
    blockC <- as.data.frame(lapply(index_data[2, ], rep, num))
    blockD <- as.data.frame(lapply(data.frame(n = nskips[i]), rep, num))
    row_to_insert <- cbind(blockA, blockB, blockC, blockD)
    tci_findex <- rbind(tci_findex, row_to_insert)
    #Se actualizan los parametros de la barra de carga del proceso
    setWinProgressBar(bar, value = i*100/n,
                      title = paste("CoTFi Tools: Building Measurements Index",
                                    i, "of", n, "files"
                                    ),
                      label = paste("Progress: ",
                                    round(i*100/n), "%", sep = "")
                      )
  }
  prev <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
  #Se genera un vector de texto con los nombres de las filas
  names_row <- seq(1, nrow(tci_findex))
  #Se nombran las filas como numeros consecutivos
  row.names(tci_findex) <- names_row
  #Se da nombre a las columnas del dataframe a entregar
  colnames(tci_findex) <- c("Path", "File", "Band_Number", "Start_Fq_MHz",
                            "Stop_Fq_MHz", "Bandwidth_kHz", "Task_ID",
                            "Storage_Interval", "Operator_ID", "Message_Length",
                            "Start_Time", "Stop_Time", "Threshold_Method",
                            "Duration", "Station_Name", "All_Single_Channels",
                            "Location_Lat", "Location_Long", "Antenna",
                            "Polarization", "Skips")
  #Se establece el tipo de clase para cada columna del dataframe a entregar
  tci_findex <- transform(tci_findex,
                          Band_Number = as.integer(Band_Number),
                          Start_Fq_MHz = as.numeric(Start_Fq_MHz),
                          Stop_Fq_MHz = as.numeric(Stop_Fq_MHz),
                          Bandwidth_kHz = as.numeric(Bandwidth_kHz),
                          Task_ID = as.integer(Task_ID),
                          Start_Time = as.POSIXct(Start_Time, format = "%m/%d/%Y %I:%M:%S %p"),
                          Stop_Time = as.POSIXct(Stop_Time, format = "%m/%d/%Y %I:%M:%S %p"),
                          Location_Lat = as.numeric(Location_Lat),
                          Location_Long = as.numeric(Location_Long))
  #Cerramos la barra de carga
  close(bar)
  Sys.setlocale("LC_TIME", prev)
  #Se devuelve el resultado de la funcion
  return(tci_findex)
}
