#Generar numeros que correspondan a otra distribucion(no uniforme).
# X variable aleatoria con distribucion de distribucion de probabilidad acumulada F,
#generar valores de x distribuidos segun F.

#Si X tiene distribucion de probabilidad acumulada F=exp(2), se escogio exponencial pues
#es facil de sacar su inversa.

efe<- function(x){
  return (2 * exp(- 2 * x ))
}

efeinv<- function(u){
  return(log(u)/-2)
}
set.seed(321)
u=runif(100000)
X<-efeinv(u)
hist(X,freq=F)


#Ahora para distribucion F=geo(2) de fracasos. :c

efe2<- function(x){
  return (2 * (-1)**(x-1))
}

#Weibull
efeinv2<- function(x,b,a){
  return((-(1/b^a)*log(x))^(1/a))
}
Xa<-efeinv2(u,2.5,200)
Xb<-efeinv2(u,0.7,200)
Xc<-efeinv2(u,6,15)
Xd<-efeinv2(u,1.2,150)
hist(Xa)
hist(Xb)
hist(Xc)
hist(Xd)
