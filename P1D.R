#Modelo A
a=rweibull(3400,2.5,200)
#Modelo B
b=rweibull(3400,0.7,200)
#Modelo C
c=rweibull(3400,6,15)
#Modelo D
d=rweibull(3400,1.2,150)

par(mfrow=c(2,2))
hist(a)
hist(b)
hist(c)
hist(d)
par(mfrow=c(1,1))
#Un grafico Cuantil-Cuantil permite observar cuan cerca esta la
#distribucion de un conjunto de datos a alguna distribucion ideal
#o comparar la distribucion de dos conjuntos de datos. 
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


#Parametro gamma es el lapso en el cual la probabilidad de falla es nula, eta fija la vida util del producto,
#beta refleja la dispersion de los datos y la forma que toma la distribucion.

#Alfa es vida caracteristica, el tiempo para el que la variable tenga probabilidad de fallo acumulada,
#63.2, a mayor alfa mayor el intervalo en el que se produciran fallos.
#wiebull muestra probabilidad de fallo despues de un tiempo. 