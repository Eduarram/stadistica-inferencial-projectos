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








