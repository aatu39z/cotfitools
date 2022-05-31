#' Graficar el porcentaje de ocupacion medido
#'
#' La funcion "spectrum.plot" prroporciona una alternativa para construir
#' graficas que permitan visualizar los niveles de campo electrico medidos en las
#' tareas de monitoreo
#' @param measDF Dataframe con la informacion de las mediciones. Dicho dataframe
#' puede obtenerse por medio de la funcion "tci.get.data".
#' @param refLev Un numero que representa el nivel de referencia utilizado para
#' construir el eje Y del grafico
#' @param DBdiv longitud de cada recuadro en el eje Y del grafico
#' @param Avg Si es TRUE se grafica el nivel promedio de campo electrico medido
#' @param Max Si es TRUE se grafica el nivel maximo de campo electrico medido
#' @param thr Nivel umbral utilizado
#' @usage plot.spectrum(measDF, refLev, DBdiv, Avg, Max)
#' @family TCI file management functions
#' @export
graph.occupancy <- function(measDF, Avg = TRUE, Max = TRUE,
                            barline = TRUE, threshold = 0, bw){
  offsetfq <- bw/2000
  x <- measDF$Frequency_MHz - offsetfq
  n <- length(x)
  xP <- rep(x, each = 2)
  xS <- x[2:n-1]
  sttfq <- x[1] + offsetfq
  stpfq <- x[n] + offsetfq
  span <- stpfq - sttfq
  xgrid <- seq(sttfq, stpfq, span/10)
  ygrid <- seq(0, 100, 10)

  par(mar = c(1,1,1,1))
  plot(1, type = "n", bty = "n",
       xlab = "", ylab = "",
       xaxt =  "n", yaxt = "n",
       xlim = c(sttfq, stpfq),
       ylim = c(0, 100))
  clip(sttfq, stpfq, 0, 100)
  if (Max){
    yMax <- measDF$Max_Occ_Percent
    yMP <- c(0, rep(yMax, length.out = 2*n-2, each = 2), 0)
    if (barline) {
      polygon(xP, yMP, col = "dodgerblue3")
      segments(xS, 0*xS, xS, yMax[2:n-1])
    } else {
      polygon(xP, yMP, col = "dodgerblue3", border = "dodgerblue3")
    }
  }
  if (Avg){
    yAvg <- measDF$Avg_Occ_Percent
    yAP <- c(0, rep(yAvg, length.out = 2*n-2, each = 2), 0)
    if (barline) {
      polygon(xP, yAP, col = "chartreuse3")
      segments(xS, 0*xS, xS, yAvg[2:n-1])
    } else {
      polygon(xP, yAP, col = "chartreuse3", border = "chartreuse3")
    }
  }
  abline(v = xgrid, lty = 2, col = "gray")
  abline(h = ygrid, lty = 2, col = "gray")

  axis(side = 2, at = ygrid, tick = FALSE,
       labels = ygrid, lty = 1, las = 2,
       col = "gray", pos = c(sttfq, 0), cex.axis = 0.8)


  mtext(text = "%", side = 3, line = 0, adj = 0.04, cex = 0.9)
}
