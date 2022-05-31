#' Importar los datos de medicion de un conjunto de archivos de monitoreo
#'
#' La función "tci.import.data" permite importar los datos de medición de un
#' conjunto de archivos. Dichos datos son almacenados en un objeto tipo "tibble".
#' Los datos de cada uno de los archivos importados se almacenan en la variable
#' 'measurements' la cual es acompañada con los datos del indice que ofrecen más
#' información de las características propias de los datos asociados a estos.
#' @param index_list Dataframe con el indice de la informacion a cargar
#' @usage tci.import.data(index_list)
#' @export
#' @family TCI file management functions
tci.import.data <- function(index_list){
  #Se crea variable como una lista vacia
  tci_data_list <- list()
  #Se identifica el numero de registros presentes en la lista de archivos a importar
  n <- nrow(index_list)
  #Arranca la barra de carga del proceso
  bar <- winProgressBar(title = "Importing TCI data: ", label = "Progress: ",
                        min = 0, max = 100, initial = 0, width = 500)
  #Se ejecuta un loop a lo largo de todo el dataframe de entrada
  for (i in 1:n) {
    #Se crea variable con nombres temporales para almacenar en la lista
    temp_name <- paste("M", sep = "", i)
    #Se extraen y almacenan los datos de medicion para el archivo i-esimo
    tci_data_list[[temp_name]] <- tryCatch(expr = {tci.get.data(index_list[i, ])},
                                           error = function(e){"Corrupted File"})
    #Se actualiza el estado de la barra de carga
    setWinProgressBar(bar, value = i*100/n,
                      title = paste("Importing TCI data: ", index_list$File[i]),
                      label = paste("Progress: ", round(i*100/n), "%", sep = ""))
  }
  #Se incorporan los datos de medicion al indice
  full_tci_data <- tibble(index_list, measurements = tci_data_list)
  #Se cierra la barra de carga
  close(bar)
  #Se regresa el resultado de la funcion
  return(full_tci_data)
}
