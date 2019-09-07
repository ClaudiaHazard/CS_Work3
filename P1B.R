f<-read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data",sep=",")
f[8:14]<-NULL # Se borran las columnas que no se utilizaran.
#a)
colnames(f)[1] <- "Cultivo"
colnames(f)[2] <- "C_Alcohol"
colnames(f)[3] <- "C_AcidoM"
colnames(f)[4] <- "C_Ceniza"
colnames(f)[5] <- "Alcalinidad"
colnames(f)[6] <- "C_Mg"
colnames(f)[7] <- "C_Fenoles"

#b)
scatterplotMatrix(f[2:7])

#c)
#Concentracion de Ceniza y Concentracion de Magnesio 

#Concentracion de Ceniza y Alcalinidad.
cor(f$C_Ceniza,f$Alcalinidad)  #0.4433672
cov(f$C_Ceniza,f$Alcalinidad)  #0.4062083

#Concentracion de Fenoles y Alcohol.
cor(f$C_Alcohol,f$C_Fenoles)  # 0.2891011
cov(f$C_Alcohol,f$C_Fenoles)  # 0.1468872


#Concentracion de Alcalinidad y Alcohol.
cor(f$C_Alcohol,f$Alcalinidad)  # -0.3102351
cov(f$C_Alcohol,f$Alcalinidad)  # -0.8410929


#Concentracion de Acido Melico y Fenoles.
cor(f$C_AcidoM,f$C_Fenoles)  # -0.335167
cov(f$C_AcidoM,f$C_Fenoles)  # -0.2343377


par(mfrow=c(2,2))
plot(f$C_Ceniza,f$Alcalinidad,pch=16,col= ifelse(f$Cultivo == 1,'red',ifelse(f$Cultivo==2,"blue",'green')),xlab="C_Ceniza",ylab = "Alcalinidad")
plot(f$C_Alcohol,f$C_Fenoles,pch=16,col= ifelse(f$Cultivo == 1,'red',ifelse(f$Cultivo==2,"blue",'green')),xlab="C_Alcohol",ylab = "C_Fenoles")
plot(f$C_Alcohol,f$Alcalinidad,pch=16,col= ifelse(f$Cultivo == 1,'red',ifelse(f$Cultivo==2,"blue",'green')),xlab="C_Alcohol",ylab = "Alcalinidad")
plot(f$C_AcidoM,f$C_Fenoles,pch=16,col=ifelse(f$Cultivo == 1,'red',ifelse(f$Cultivo==2,"blue",'green')),xlab="C_AcidoM",ylab = "C_Fenoles")

#d)
par(mfrow=c(1,1))

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
    if (i == 1) { plot(vectori,col=colouri,type="l",ylim=c(mymin,mymax),ylab="Cantidad",xlab="Datos") }
    else         { points(vectori, col=colouri,type="l")                                     }
    lastxval <- length(vectori)
    lastyval <- vectori[length(vectori)]
    text((lastxval-10),(lastyval),namei,col="black",cex=0.6)
  }
}

c1=f[(f$Cultivo==1),][2:7]
c2=f[(f$Cultivo==2),][2:7]
c3=f[(f$Cultivo==3),][2:7]

par(mfrow=c(1,1))
makeProfilePlot(c1,c("C_Alcohol","C_AcidoM","C_Ceniza","Alcalinidad","C_Mg","C_Fenoles"))
title("Cultivo 1")
makeProfilePlot(c2,c("C_Alcohol","C_AcidoM","C_Ceniza","Alcalinidad","C_Mg","C_Fenoles"))
title("Cultivo 2")
makeProfilePlot(c3,c("C_Alcohol","C_AcidoM","C_Ceniza","Alcalinidad","C_Mg","C_Fenoles"))
title("Cultivo 3")


