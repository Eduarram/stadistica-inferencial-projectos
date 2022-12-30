
################# Ejercicio 1 ###################
suma_dados <- c("2-4","5-6", "7", "8-9", "10-12")
frecuencia <- c(74, 120, 83, 135, 88)
frec_obser <- frecuencia/sum(frecuencia)
frec_teorica <- c((1/6), (9/36), (6/36), (9/36), (1/6))*sum(frecuencia)
p_teor <-  c((1/6), (9/36), (6/36), (9/36), (1/6))
df <- data.frame(suma_dados, frecuencia, frec_teorica)

library(tidyverse)

df <- df %>% mutate(vlor_contras=(((frecuencia-frec_teorica)^2)/
                                    frec_teorica))

pchisq(sum(df$vlor_contras), df=4, lower.tail = F)


chisq.test(frecuencia, p=p_teor)



############### Ejercicio 2 #####################################


no_dias <- c("1", "2", "3", "4-5", "6-9", "10-14", "15-30", "31-inf")
frecuencia2 <- c(89, 152, 105, 165, 221, 124, 106, 38)

extre_izquierdos <- c(0, 1.5, 2.5, 3.5,5.5, 9.5, 14.5, 30.5)
extre_derechos <- c(extre_izquierdos[-1], Inf)
prob_teoricas <- pchisq(extre_derechos, 4)-pchisq(extre_izquierdos, 4)

df2 <- data.frame(no_dias, frecuencia2, extre_izquierdos, 
                  extre_derechos, prob_teoricas)
chisq.test(df2$frecuencia2, p=df2$prob_teoricas)

mu  <- mean(frecuencia2)
sigma <- sd(frecuencia2)

library(car)


qqPlot(frecuencia2, distribution="norm", 
       mean=mean(frecuencia2), sd=sd(frecuencia2))

 
###########################ejercicio 3##############################


frecuencia <- c(8, 38, 45, 9)
calificacion <- c("70 < x <= 90", "90 < x <= 110", "110 < x <= 130", "130 < x <= 150")
df3 <- data.frame(calificacion, frecuencia) 



### replicamos la muestra por que no es lo suficientemente grande 

replicaciones <- rep(c(80, 100, 120, 140), frecuencia)

mu <- mean(replicaciones)
sigma <- sd(replicaciones)

extremos.izquierda <- c(-Inf, 90, 110, 130)
extremos.derecha  <- c(extremos.izquierda[-1], Inf)

probabilidades.teoricas <- pnorm(extremos.derecha, mu, sigma)- pnorm(extremos.izquierda, mu, sigma)

df3 <- data.frame(df3, extremos.derecha, extremos.izquierda, probabilidades.teoricas)


chisq.test(df3$frecuencia, p=df3$probabilidades.teoricas)

valor.estadistico <- chisq.test(frecuencia, p=probabilidades.teoricas)$statistic

pvalor.cor <- pchisq(valor.estadistico[[1]], 1, lower.tail = F)



########################## Ejercicio 4 ##########################


muesta4 <- c(0.479, 0.889, 0.216, 0.596, 0.359, 0.347, 0.646, 0.359, 0.991, 0.227, 0.774, 0.760, 0.448, 0.992, 0.742, 0.402, 0.049, 0.213, 0.296, 0.711)

# codigo de agrupacion por intervalos 

intervalos <- table(cut(muesta4, 4))

intervalos <- c("0 - 0.25", "0.25 - 0.5", "0.5 - 0.75", "0.75 - 1")
frecuencia <- c(4, 7, 4, 3)
extremos.izquierdos <- c(0, 0.25, 0.5, 0.75)
extremos.derechos <- c(extremos.izquierdos[-1], 1)


df4 <- data.frame(intervalos, frecuencia, extremos.izquierdos, extremos.derechos)

library(tidyverse)


df4 <- df4 %>% mutate(pfrec = frecuencia/sum(frecuencia)) %>% 
  mutate(teo_frec = c(0.25, 0.5, 0.75, 1)) %>% 
  mutate(prob_vect=punif(extremos.derechos)-punif(extremos.izquierdos))

df4 <- mutate(df4, prop.p = (((pfrec-teo_frec)^2)/teo_frec))
pchisq(sum(df4$prop.p), df=4) 

chisq.test(df4$frecuencia, p=df4$prob_vect)  



##### Ejercicio 5 ##############









#############ejercicio de los lobos #############


XY <- c("X1", "X2", "X3", "nj.")

Y1 <- c(5 , 20, 15, 40)
Y2 <- c(8, 26, 10, 44)
Y3 <- c(15, 46, 15, 76)
Y4 <- c(22, 8, 10, 40)
nj.<- c(50, 100, 50, 200)




df <- data.frame(XY,Y1, Y2,Y3,Y4,nj.)


namesdf <- c((paste(colnames(df), c(rep("t", 4)))))

dtf <- data.frame(XY, ((Y1*nj.)/200),((Y2*nj.)/200), ((Y3*nj.)/200),((Y4*nj.)/200), nj.)

colnames(dtf) <- namesdf  


dtf2 <- (((df[1:3,2:5]-dtf[1:3,2:5])^2)/ dtf[1:3,2:5]) 

pchisq(sum(dtf2$Y1, dtf2$Y2, dtf2$Y3, dtf2$Y4), df=198)


#################### contraste de independencia con una amatriz########

chisq.test(as.matrix(df[1:3,2:5]))



######## ejericio del contraste de homogeneidad 


df1 <- table(mtcars$am, mtcars$vs)

chisq.test(as.matrix(df1), correct = TRUE)

 










