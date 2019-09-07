#1)

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

#-----------------------------------------------------------
#2)
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

#------------------------------------------------------
#3)
#Demostracion transformada inversa.
#Si X tiene distribucion de probabilidad acumulada F=exp(2), se escogio exponencial pues
#es facil de sacar su inversa.

efe<- function(x){
  return (2 * exp(- 2 * x ))
}

efeinv<- function(u){
  return(log(u)/-2)
}
#histograma de datos de exp(2)
set.seed(321)
u=runif(100000)
X<-efeinv(u)
hist(X,freq=F)

#Funcion inversa de la distribucion de Weibull

efeinv2<- function(x,a,b){
  return((-(1/b^a)*log(x))^(1/a))
}
#Datos aleatorios con distribucion uniforme.
u=runif(2000)

#Modelo A
a<-efeinv2(u,2.5,200)
#Modelo B
b<-efeinv2(u,0.7,200)
#Modelo C
c<-efeinv2(u,6,15)
#Modelo D
d<-efeinv2(u,1.2,150)


par(mfrow=c(2,2),oma = c( 0, 0, 2, 0 ))
qqplot(a,dielectrico_A,col="blue")
title("Modelo A")
qqplot(b,dielectrico_A)
title("Modelo B")
qqplot(c,dielectrico_A)
title("Modelo C")
qqplot(d,dielectrico_A,col="blue")
title("Modelo D")
title("Dielectrico 1",outer = TRUE)


qqplot(a,dielectrico_B,col="blue")
title("Modelo A")
qqplot(b,dielectrico_B)
title("Modelo B")
qqplot(c,dielectrico_B)
title("Modelo C")
qqplot(d,dielectrico_B,col="blue")
title("Modelo D")
title("Dielectrico 2", outer=TRUE)


qqplot(a,dielectrico_C)
title("Modelo A")
qqplot(b,dielectrico_C)
title("Modelo B")
qqplot(c,dielectrico_C,col="blue")
title("Modelo C")
qqplot(d,dielectrico_C)
title("Modelo D")
title("Dielectrico 3", outer=TRUE)
