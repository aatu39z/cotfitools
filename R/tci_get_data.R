#' Obtener datos de monitoreo con los resultados de la medicion
#'
#' La funcion permite obtener los datos de monitoreo de un archivo de mediciones
#' TCI. Para ejecutar la funcion se necesita proporcionar el indice con la
#' informacion del archivo a importar.
#' @param index_row Dataframe de una fila con informacion tipo indice del archivo
#' a importar. Las columnas de este dataframe de entrada deben estar compuestas
#' segun lo que resulta de ejecutar la funcion "tci.get.index"
#' @examples wd <- getwd()
#' setwd(system.file("extdata", package = "cotfitools"))
#' start <- as.POSIXct("2021-01-01")
#' stop <- as.POSIXct("2021-12-31")
#' mon_info <- tci.find.dir(stt_date = start, stp_date = stop)
#' n_skips <- tci.count.skips(mon_info)
#' index <- tci.get.index(mon_info, n_skips)
#' View(index)
#' tci_data <- tci.get.data(index[1, ])
#' View(tci_Data)
#' setwd(wd)
#' @export
#' @family TCI file management functions
tci.get.data <- function(index_row, impute_data = FALSE) {
  #Se extraen los datos de medicion a partir de la informacion proporcionada
  tci_data <- read.csv(file = file.path(index_row$Path, index_row$File),
                       sep = "^", skip = index_row$Skips + 2,
                       header = FALSE, na.strings = "---")

  #Se crea variable con los tipos de archivos TCI soportados en ingles
  type_en <- c("Field Strength vs. Channel", "Field Strength vs. Time of Day",
               "Occupancy vs. Channel", "Occupancy vs. Time of Day")
  #Se crea variable con los tipos de archivos TCI soportados en espanol
  type_sp <- c("Int. de Campo vs. Canal", "Int. de Campo vs. Hora del Día",
               "Ocupación vs. Canal", "Ocupación vs. Hora del Día")
  #Se crea variable con el detalle de las cabeceras a utilizar segun tipo de archivo
  head_list <- data.frame(HeaderA = c("Channel_Number", "Channel_Number",
                                      "Channel_Number", "Channel_Number"),
                          HeaderB = c("Frequency_MHz", "Frequency_MHz",
                                      "Frequency_MHz", "Frequency_MHz"),
                          HeaderC = c("Max_FS_dBu", "Time",
                                      "Max_Occ_Percent", "Time"),
                          HeaderD = c("Avg_FS_dBu", "FS_dBu",
                                      "Avg_Occ_Percent", "Occ_Percent"),
                          HeaderE = c(NA, NA, "Max_FS_dBu", "FS_dBu"),
                          HeaderF = c(NA, NA, "Avg_FS_dBu", NA))
  #Se lee informacion del tipo de umbral utilizado para la medicion
  #Lo anterior para determinar el metodo para imputar datos de medicion perdidos
  th_meth <- index_row$Threshold_Method
  #Se ejecuta un loop para identificar el tipo de archivo del que se trata
  for (i in 1:4) {
    #Se evalua si el archivo coincide con alguno de los tipos soportados
    if (grepl(type_en[i], index_row$File) | grepl(type_sp[i], index_row$File)) {
      #Se coloca el nombre de las cabeceras de acuerdo con el tipo de archivo
      colnames(tci_data) <- as.character(head_list[i, !is.na(head_list[i, ])])
      #Se determina aquellas mediciones dentro del rango de frecuencias establecido
      fq_filter <- (tci_data$Frequency_MHz >= index_row$Start_Fq_MHz) & (tci_data$Frequency_MHz <= index_row$Stop_Fq_MHz)
      #Se filtran las mediciones dentro de dicho rango
      tci_data <- tci_data[fq_filter, ]
      #En caso de que el archivo identificado sea tipo 1 o 3
      if ((i == 1)|(i == 3)) {
        #Se eliminan los datos de mediciones en frecuencias repetidas
        tci_data <- tci_data[!duplicated(tci_data$Frequency_MHz), ]
        if (impute_data == TRUE) {
          #Y considerando además el metodo de umbral de la medicion
          if ((grepl("Fixed", th_meth))|(grepl("Fijo", th_meth))) {
            #Se remplazan los valores perdidos por el nivel umbral de la medicion
            replace_data <- as.numeric(gsub(".*?([0-9]+).*", "\\1", th_meth))
            tci_data$Max_FS_dBu[is.na(tci_data$Max_FS_dBu)] <- replace_data
            tci_data$Avg_FS_dBu[is.na(tci_data$Avg_FS_dBu)] <- replace_data
          } else {
            #Se reemplazan los valores perdidos por el nivel minimo de la medicion
            rep_data_max <- tryCatch(min(tci_data$Max_FS_dBu, na.rm = TRUE),
                                     warning = function(w){NA})
            rep_data_avg <- tryCatch(min(tci_data$Avg_FS_dBu, na.rm = TRUE),
                                     warning = function(w){NA})
            tci_data$Max_FS_dBu[is.na(tci_data$Max_FS_dBu)] <- rep_data_max
            tci_data$Avg_FS_dBu[is.na(tci_data$Avg_FS_dBu)] <- rep_data_avg
          }
        }
      }
      break
    }
  }
  return(tci_data)
}
