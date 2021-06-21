# setwd("/media/alex/Data/informatica/Fisica/FisicaLab/Informe6p2")
# Nombre: Alexander James Alvarez Rojas 
# Fecha: 14/06/2021
# Objetivos: Cinematica 
# Aclaracion: fjkdashfkjhdsk

######################## MRU ###########################
t <- seq(1,10, length.out = 200)
x0 <- runif(1,1,4)
v = runif(1,1, 4)
WGN <- rnorm(length(t), 0, 4)

x = x0 + v*t + WGN

plot(t,x,xlab ="t[s]", ylab = "x[m]", main = "x = v*t")
plot(v, xlab = "t[s]", ylab = "v[m/s]", main = "v = f(t)")
abline(h=v)
# linear model
fit1 <- lm(x~t)

plot(t, predict(fit1), t="l")
points(t,x)

#################### MRUV ############################

#t <- seq(1,10, lenght.out = 15)
WGN = rnorm(length(t), 0, 10)
a = runif(1,1,4) 

x = x0 + v*t + (1/2)*a*t^2 + WGN
vf = v  + a*t + WGN

plot(t,x, xlab = "t[s]", ylab = "x[m]", main= "x = v*t+0.5*a*t^2")
plot(t,vf)
# linear model
fit2 <- lm(x~poly(t,2))

plot(t,predict(fit2), t="l", xlab = "t[s]", ylab = "x[m]", main= "x = v*t+0.5*a*t^2")
points(t,x)
fitv <- lm(vf~t)

plot(t,predict(fitv), t="l", xlab = "t[s]", ylab = "v[m/s]", main= "vf = v0 + a*t")
points(t,vf)
plot(a, xlab = "t[s]", ylab = "a[m/s^2]", main = "a = f(t)")
abline(h=a)

########################## MU#################################
t = seq(1,10, length.out = 200)
WGN <- rnorm(length(t), 0, 20)
x = 14*t^5 + 16*t^3 - 12*t +8 + WGN
v = 14*5*t^4 + 16*3*t^2 - 12 +WGN
a = 14*20*t^3 + 16*6*t + WGN
astart <- 1
datos = data.frame(t,x)

plot(t,x)
#linear model
fit4 <- nls(x~(14*t^5 + 16*t^3 - 12*t +8), data = datos,start = list(t=1))
fit5 <- lm(x~poly(t,5))
plot(t,predict(fit5),t="l", main = "x = 14*t^5 + 16*t^3 - 12*t +8", xlab = "t[s]", ylab = "x[m]")
points(t,x)
predict(fit4)
length(x) == length(t)

datosMU <- data.frame("t"=t, "posicion inicial" = x, "posicion arreglada" = predict(fit5))


fit6 <- lm(v~poly(t,4))
plot(t,predict(fit6), t="l", main = "v = 14*5*t^4 + 16*3*t^2 - 12", xlab = "t[s]", ylab = "v[m/s]")
points(t,v)
datosMU <- data.frame("t"=t, "velocidad inicial" = x, "velocidad arreglada" = predict(fit6))

fit7 <- lm(a~poly(t,3))
plot(t,predict(fit7), t="l", main ="a = 14*20*t^3 + 16*6*t", xlab = "t[s]", ylab = "a[m/s^2]")
points(t,a)
datosMU <- data.frame("t" = t, "aceleracion inicial" = a, "aceleracion arreglada" = predict(fit7))

############################### mv ######################
# mu mariposa
a <- runif(n = 1,min = 1,max = 4)
b <- runif(n = 1,min = 1,max = 4)
WGN <- rnorm(n = length(t), mean = 0, sd = 1)

x = (a)*t/((b+t)) + WGN
v = a*b/(b+t)^2
a = -2*a*b/(b+t)^3

plot(t,x,ylab = "x[m]", xlab = "t[s]", main ="x = (a*t)/(b+t)")
plot((t),(v), xlab = "t[s]", ylab = "v[m/s]", main = "v = a*b/(b+t)^2")
points((t),(v), t="l",xlab = "t[s]", ylab = "v[m/s]", main = "v = a*b/(b+t)^2")
plot(t,a,t="l",xlab = "t[s]", ylab = "a[m/s^2]", main = "a = -2ab/(b+t)^3")
points(t,a)

# corrigiendo el movimiento
fit3 <- nls(x~(a*t)/((b+t)),start = list(a = 1, b=1))
summary(fit3)
fit3$coefficients

# graficando
plot(t,predict(fit3), t="l", col= 34, ylab = "x[m]", xlab = "t[s]", main ="x = (a*t)/(b+t)")
points(t,x, col=20)
datosMV <- data.frame("Tiempo"=t,"Posicion inicial" = x,"Posicion arreglada"=predict(fit3))



plot(t,v, xlab = "t[s]", ylab = "v[m/s]", main = "v = a*b/(b+t)^2")
points(t, predict(fit21))



# library(xtable)
# resultados <- xtable(datosMU)
# print(resultados,type ="latex")
