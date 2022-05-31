#' Graficar los niveles de campo electrico medidos
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
#' @usage spectrum.plot(measDF, refLev, DBdiv, Avg, Max)
#' @family TCI file management functions
#' @export
graph.spectrum <- function(measDF, refLev, DBdiv,
                           Avg = TRUE, Max = TRUE,
                           surpass = FALSE, threshold = 60,
                           MkrDF = data.frame(), MkrSel = NULL){
  x <- measDF$Frequency_MHz
  sttfq <- x[1]
  stpfq <- tail(x, 1)
  span <- stpfq - sttfq
  ystt <- refLev - 10*DBdiv
  xlabels <- seq(sttfq, stpfq, span/10)
  ylabels <- seq(ystt, refLev, DBdiv)
  par(mar = c(1,1,2,1))
  plot(1, type = "n", bty = "n",
       xlab = "", ylab = "",
       xaxt =  "n", yaxt = "n",
       xlim = c(sttfq, stpfq),
       ylim = c(ystt, refLev))
  clip(sttfq, stpfq, ystt, refLev)
  abline(v = xlabels, lty = 2, col = "gray")
  abline(h = ylabels, lty = 2, col = "gray")
  if (Avg){
    yAvg <- measDF$Avg_FS_dBu
    lines(x, yAvg, col = "chartreuse3")
  }
  if (Max) {
    yMax <- measDF$Max_FS_dBu
    lines(x, yMax, col = "dodgerblue3")
  }
  if (ncol(MkrDF) != 0) {
    n <- ncol(MkrDF)
    nmMkr <- colnames(MkrDF); nuMkr <- gsub("M", "", nmMkr)
    sel <- nmMkr == MkrSel
    Msel <- rep("gray60", n); Msel[sel] <- "orangered"
    Muns <- rep("gray20", n); Muns[sel] <- "orangered"
    xMkr <- MkrDF[1, ]
    yMkr <- MkrDF[2, ]
    xDel <- MkrDF[3, ] + xMkr
    yDel <- MkrDF[4, ] + yMkr
    Dch <- rep(25, n); Dch[MkrDF[3,] == 0] <- 23
    Dpos <- rep(1, n); Dpos[MkrDF[3,] < 0] <- 2; Dpos[MkrDF[3,] > 0] <- 4
    points(xMkr, yMkr, pch = 23, cex = 1.4, bg = Msel, col = Muns)
    text(xMkr, yMkr, labels = paste("M", nuMkr, sep = ""), pos = 3, cex = 0.9)
    points(xDel, yDel, pch = Dch, cex = 1.4, bg = Msel, col = Muns)
    text(xDel, yDel, labels = paste("D", nuMkr, sep = ""), pos = Dpos, cex = 0.9)
    mtext(text = paste(nmMkr[sel],": ", num2txt.lg(xMkr[sel]), ", ", yMkr[sel], " dBu", sep = ""),
          side = 3, line = 1, adj = 1, cex = 0.9)
    if (!is.na(MkrDF[3,sel])) {
      mtext(text = paste("D", nuMkr[sel], ": ", num2txt.sh(round(MkrDF[3,sel], 3)), ", ", MkrDF[4, sel], " dB", sep = ""),
            side = 3, line = 0, adj = 1, cex = 0.9)
    }
  }
  if (surpass & threshold >= ystt & threshold <= refLev) {
    clip(sttfq, stpfq, threshold, refLev)
    if (Avg == TRUE) {
      lines(x, yAvg, col = "firebrick1")
    }
    if (Max == TRUE) {
      lines(x, yMax, col = "firebrick1")
    }
    abline(h = threshold, col = "firebrick1")
  }
  axis(side = 1, at = xlabels, tick = FALSE,
       labels = FALSE, lty = 1, las = 0,
       col = "gray", pos = c(ystt, sttfq))
  axis(side = 2, at = ylabels, tick = FALSE,
       labels = ylabels, lty = 1, las = 2,
       col = "gray", pos = c(sttfq, ystt), cex.axis = 0.8)

  mtext(text = paste("Ref", refLev, "dBu"), side = 3, line = 1, adj = 0.04, cex = 0.9)
  mtext(text = paste(DBdiv, "dB/div"), side = 3, line = 0, adj = 0.04, cex = 0.9)
}
