#a)

#Nombrar las columnas
colnames(iris)[1] <- "Largo_Sepalo"
colnames(iris)[2] <- "Ancho_Sepalo"
colnames(iris)[3] <- "Largo_Hoja"
colnames(iris)[4] <- "Ancho_Hoja"
n=c()
for (i in 1:150){
  if (i<=50){
    n=c(n,"Setosa")}
  if((50<i & i<100) | i==50){n=c(n,"Versicolor")}
  if(100<i){n=c(n,"Virginica")}
}

iris[5:7]<- list(NULL)   #Borrar las columnas v5,v6 y v7.

#Unir las variables booleanas dependientes en una sola.

iris["Cepa"]<-n


#b)
install.packages("car")
library(car)

scatterplotMatrix(iris[1:4],var.labels=c("Largo Sepalo","Ancho Sepalo", "Largo Hoja", "Ancho Hoja"))
#pairs(iris[1:4],col=ifelse(iris$Cepa == "Setosa",'red',ifelse(iris$Cepa=="Versicolor","blue","green")),pch=16)

#c)
#Largo_hoja y Ancho_hoja
cor(iris$Largo_Hoja,iris$Ancho_Hoja)  #0.9627571
cov(iris$Largo_Hoja,iris$Ancho_Hoja)  #1.296387

#Ancho_sepalo y Largo_hoja.
cor(iris$Largo_Sepalo,iris$Ancho_Hoja)  #0.8179411
cov(iris$Largo_Sepalo,iris$Ancho_Hoja)  #0.5162707

#Largo_sepalo y Largo_hoja.
cor(iris$Largo_Sepalo,iris$Largo_Hoja)  # 0.8717542
cov(iris$Largo_Sepalo,iris$Largo_Hoja)  # 1.273682


par(mfrow=c(1,3))
plot(iris$Largo_Hoja,iris$Ancho_Hoja,pch=16,col= ifelse(iris$Cepa == "Setosa",'red',ifelse(iris$Cepa=="Versicolor","blue",'green')),xlab="Largo Hoja",ylab="Ancho Hoja")
plot(iris$Largo_Sepalo,iris$Ancho_Hoja,pch=16,col= ifelse(iris$Cepa == "Setosa",'red',ifelse(iris$Cepa=="Versicolor","blue",'green')),xlab="Largo Sepalo",ylab="Ancho Hoja")
plot(iris$Largo_Hoja,iris$Largo_Sepalo,pch=16,col= ifelse(iris$Cepa == "Setosa",'red',ifelse(iris$Cepa=="Versicolor","blue",'green')),xlab="Largo Hoja",ylab="Largo Sepalo")


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

seto=iris[(iris$Cepa=="Setosa"),][1:4]
versi=iris[(iris$Cepa=="Versicolor"),][1:4]
virgi=iris[(iris$Cepa=="Virginica"),][1:4]

par(mfrow=c(3,1))
makeProfilePlot(seto,c("Ancho_Sepalo","Ancho_Hoja","Largo_Hoja","Largo_Sepalo"))
title("Setosa")
makeProfilePlot(versi,c("Ancho_Sepalo","Ancho_Hoja","Largo_Hoja","Largo_Sepalo"))
title("Versicolor")
makeProfilePlot(virgi,c("Ancho_Sepalo","Ancho_Hoja","Largo_Hoja","Largo_Sepalo"))
title("Virginica")
