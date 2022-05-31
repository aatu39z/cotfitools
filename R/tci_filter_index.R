#' Filtrar registros del indice de archivos TCI
#'
#' La funcion "tci.filter.index" provee de una herramienta agil para filtrar la
#' informacion contenida de un indice de archivos de mediciones de TCI. La funcion
#' devuelve a la salida un vector con datos TRUE en la posicion de aquellos
#' registros que cumplieron con todos los criterios de busqueda configurados.
#' @param tci_index Dataframe con informacion de los archivos de medicion TCI
#' organizados como un indice.
#' @param stt_date Fecha inicial del tipo "POSIXct" del rango de datos que se
#' pretende filtrar
#' @param stp_date Fecha final del tipo "POSIXct" del rango de datos
#' @param stt_fq Frecuencia inicial en MHz
#' @param stp_fq Frecuencia final en MHz
#' @param stn_name Nombre de la estacion
#' @param type Tipo de archivo a filtrar de acuerdo con el siguiente detalle:
#' 1 = Field Strength vs. Channel,
#' 2 = Field Strength vs. Time of Day,
#' 3 = Occupancy vs. Channel,
#' 4 = Occupancy vs. Time of Day.
#' @examples wd <- getwd()
#' setwd(system.file("extdata", package = "cotfitools"))
#' start <- as.POSIXct("2021-01-01")
#' stop <- as.POSIXct("2021-12-31")
#' mon_info <- tci.find.dir(stt_date = start, stp_date = stop)
#' n_skips <- tci.count.skips(mon_info)
#' index <- tci.get.index(mon_info, n_skips)
#' index_filter <- tci.filter.index(index, stp_date = as.POSIXct("2021-03-01"), type = 1)
#' View(index[index_filter, ])
#' index_filter <- tci.filter.index(index, stn_name = "Sonsonate")
#' View(index[index_filter, ])
#' index_filter <- tci.filter.index(index, stt_fq = 210, stp_fq = 300)
#' View(index[index_filter, ])
#' setwd(wd)
#' @usage tci.filter.index(tci_index, stt_date, stp_date, stt_fq, stp_fq, stn_name, type)
#' @export
#' @family TCI file management functions
tci.filter.index <- function(tci_index,
                             stt_date = NULL, stp_date = NULL,
                             stt_fq = NULL, stp_fq = NULL,
                             stn_name = NULL, type = NULL,
                             service = NULL) {
  #Se lee el numero de registros que contiene el dataframe de entrada "tci_index"
  n <- nrow(tci_index)
  #Se inicializan las siguientes variables como un vector con datos del tipo logico
  dt_filter <- fq_filter <- stn_filter <- type_filter <- rng_filter <- !logical(n)
  #Se determinan los registros cuya fecha es superior a la fecha inicial
  if(!is.null(stt_date)) {dt_filter <- (tci_index$Stop_Time > stt_date) & dt_filter}
  #Se determinan los registros cuya fecha es inferior a la fecha final
  if(!is.null(stp_date)) {dt_filter <- (tci_index$Start_Time < stp_date) & dt_filter}
  #Se detemrinan los registros cuya frecuencia es superior a la frecuencia inicial
  if(!is.null(stt_fq)) {fq_filter <- (tci_index$Stop_Fq_MHz > stt_fq) & fq_filter}
  #Se determinan los registros cuya frecuencia es inferior a la frecuencia final
  if(!is.null(stp_fq)) {fq_filter <- (tci_index$Start_Fq_MHz < stp_fq) & fq_filter}
  #Se determinan los registros que coinciden con los nombres de estacion a filtrar
  if(!is.null(stn_name)) {
    #Se contruye un vector lleno de FALSE
    stn_filter <- !stn_filter
    for (i in stn_name) {
      #Se identifican los registros que coinciden con el nombre de estacion
      stn_filter <- (tci_index$Station_Name == i) | stn_filter
    }
  }
  #Se determinan los registros que coinciden con el tipo de archivo a extraer
  if(!is.null(type)) {
    #Se crea un vector de opciones disponibles en idioma ingles
    type_options_en <- c("Field Strength vs. Channel",
                         "Field Strength vs. Time of Day",
                         "Occupancy vs. Channel",
                         "Occupancy vs. Time of Day")
    #Se crea un vector de opciones disponibles en idioma espanol
    type_options_sp <- c("Int. de Campo vs. Canal",
                         "Int. de Campo vs. Hora del Día",
                         "Ocupación vs. Canal",
                         "Ocupación vs. Hora del Día")
    #Se crea un vector con todas las opciones de idioma a evaluar
    type <- c(type_options_en[type], type_options_sp[type])
    #Se construye un vector lleno de FALSE
    type_filter <- !type_filter
    for (j in type) {
      #Se identifican aquellos registros que coinciden con el tipo de archivo a extraer
      type_filter <- grepl(j, tci_index$File, fixed = TRUE) | type_filter
    }
  }

  serv_options <- data.frame(id = c(1,2,2,3,4,5,6,7,8,8,9,10,9,
                                    11,12,13,14,15,16,17,18),
                             sttrg = c(0.5,54,76,88,108,140,174,
                                       210,300,335,400,430,440,470,
                                       851,869,939,1930,2110,2570,2620),
                             stprg = c(2,72,88,108,138,174,210,
                                       270,330,350,430,440,470,806,
                                       869,894,960,1990,2170,2620,2690))
  serv_names <- data.frame(id = 1:18,
                           name = c("Broadcasting AM",
                                    "Broadcasting TV VHF I",
                                    "Broadcasting FM",
                                    "Aeronautical",
                                    "Radiocom VHF 150",
                                    "Broadcasting TV VHF II",
                                    "Broadcasting Links 200",
                                    "Broadcasting Links 300",
                                    "Radiocom UHF 400",
                                    "Others",
                                    "Broadacsting TV UHF III",
                                    "Mobile DL Trunking 800",
                                    "Mobile DL 850",
                                    "Mobile DL 900",
                                    "Mobile DL 1900",
                                    "Mobile DL 2100",
                                    "Mobile DL 2500 TDD",
                                    "Mobile DL 2500 FDD"))
  cr <- !is.na(match(serv_options$id, service))
  if (any(cr)) {
    serv_options <- serv_options[cr, ]
    rng_filter <- !rng_filter
    for (k in seq(1, nrow(serv_options))) {
      sub_filter <- (tci_index$Stop_Fq_MHz > serv_options[k,2]) & (tci_index$Start_Fq_MHz < serv_options[k,3])
      rng_filter <- sub_filter | rng_filter
    }
  }
  #Se crea un vector que engloba todos los criterios de busqueda solicitados
  global_filter <- dt_filter & fq_filter & stn_filter & type_filter & rng_filter
  #Se imprimen los parametros de busqueda que se ejecutaron para referencia del ususario
  cat("********** CRITERIOS DE BÚSQUEDA EJECUTADOS **********\n")
  cat(paste("Fecha inicial:", stt_date, sep = "\t\t"), "\n")
  cat(paste("Fecha final:", stp_date, sep = "\t\t"), "\n")
  cat("Frec. inicial (MHz):", stt_fq, sep = "\t", "\n")
  cat("Frec. final: (MHz)", stp_fq, sep = "\t", "\n")
  cat("Nombre de la estación: ", stn_name, sep = "\n\t\t\t")
  cat("Tipos de archivos: ", type, sep = "\n\t\t\t")
  cat("Servicios: ", serv_names$name[service], sep = "\n\t\t\t")
  cat("******************************************************\n")
  #Se devuelve el resultado de la funcion
  return(global_filter)
}
