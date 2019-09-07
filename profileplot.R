makeProfilePlot <- function(mylist,names)
{
  require(RColorBrewer)
  numvariables <- length(mylist)
  # Se asigna un color por cara numero de variables
  colours <- brewer.pal(numvariables,"Set1")
  # Fijar valores minimos y maximos para variables, estos van a ser reemplazados con los minimos
  #reales y maximos reales de la muestra
  mymin <- 1e+20
  mymax <- 1e-20
  for (i in 1:numvariables)
  {
    vectori <- mylist[[i]]
    mini <- min(vectori)
    maxi <- max(vectori)
    if (mini < mymin) { mymin <- mini }
    if (maxi > mymax) { mymax <- maxi }
  }
  # Graficar las variables
  for (i in 1:numvariables)
  {
    vectori <- mylist[[i]]
    namei <- names[i]
    colouri <- colours[i]
    if (i == 1) { plot(vectori,col=colouri,type="l",ylim=c(mymin,mymax)) }
    else         { points(vectori, col=colouri,type="l")                                     }
    lastxval <- length(vectori)
    lastyval <- vectori[length(vectori)]
    text((lastxval-10),(lastyval),namei,col="black",cex=0.6)
  }
}
