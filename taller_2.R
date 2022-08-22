########################## taller de la amplitud ##############################

###############################################################################
########################## ejercicio no 1 #####################################
###############################################################################


#calculamos Z
alphus <- 0.1
amp <- alphus*2
z<- 1-(alphus/2)

#calculamos el cuantil
znew <- qnorm(z)

muestra_tam <- (znew^2/amp^2)

################################################################################
################################# ejercicio 2 ##################################
################################################################################

##################### intervalo de confainaza para la media ####################

televis <- c(521, 742, 593, 635, 788, 717, 606, 639, 666, 624)
ICZ(length(televis), med = mean(televis), sigma = var(televis), alpha = 0.1)

########################## intervalo de confianza para la desviacion estandar ##

vari_televis <- var(televis)

tamaño <- length(televis)


alpha_t <- 0.1

#calculamos cuantiles 

(cuantil_izquierda <- qchisq(1-alpha_t/2, tamaño-1))
(cuantil_derecha <- qchisq(alpha_t/2, tamaño-1))

#calculamos los intervalos

(valor_izquierdo =(tamaño-1)*vari_televis/cuantil_izquierda)
(valor_derecho = (tamaño-1)*vari_televis/cuantil_derecha)

# sacamos raiz 

c(sqrt(valor_izquierdo), sqrt(valor_derecho))

################################################################################
######################    ejercicio 3 ##########################################
################################################################################

sample_size=function(error,p=0.5,alpha=1-0.95){
  A0=2*error
  n=ceiling(qnorm(1-alpha/2)^2/A0^2)
  n
}


sample_size(c(0.05,0.03,0.02,0.01))


tabla_n=data.frame(error=c(0.05,0.03,0.02,0.01),
                   n=sample_size(c(0.05,0.03,0.02,0.01),alpha=1-0.95), 
                   conf.level=0.95)
tabla_n

################################################################################
#######################################  ejercio 4 #############################
################################################################################

#### parte 1 calcular los intervalos de confianza para la muestra poblacional


ICZ(n=81, med = 112, sigma = 36, alpha = 0.05)

#### parte 2 calcular los intervalos de confianza de la sigma de las reservas.


n=81
stilde=36
alpha=1-0.95
IC=c((n-1)*stilde^2/qchisq(1-alpha/2,df=n-1),(n-1)*stilde^2/qchisq(alpha/2,df=n-1))
IC

#### parte 3 calcular la proporcion  de la poblacion.

library(epitools)

binom.exact(30, 81)


######### parte 4 calcular la poblacion optima

A_o <- 0.1*0.05
alpha <- 1-0.95
n <- ceiling(qnorm(1-alpha/2)^2/A_o^2)






