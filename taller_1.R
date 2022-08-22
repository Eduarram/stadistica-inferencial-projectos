################################################################################
############################ ejercicio 1 #######################################

mu1 <- 1012
sigma <- 25
alpha <- 0.05
n <- 64

ICZ=function(n, med, sigma, alpha){
  c(med-qnorm(1-alpha/2)*sigma/sqrt(n),
    med+qnorm(1-alpha/2)*sigma/sqrt(n))
}
ICZ(n=n, med = mu1, sigma = sigma, alpha = alpha)


################################################################################
########################## ejercicio 2 #########################################

ICZ(n=400, med = 86, sigma = 10.2, alpha = 0.02)

################################################################################
#########################  ejercicio 3 #########################################
################################################################################
kilometros <- c(19600, 20300, 20500, 19800)

ICZ2=function(x, sigma, alpha){
  c(mean(x)-qnorm(1-alpha/2)*sigma/sqrt(length(x)),
    mean(x)+qnorm(1-alpha/2)*sigma/sqrt(length(x)))}
ICZ2(kilometros, sigma = var(kilometros), alpha = 0.05)



################################################################################
######################## ejercicio 4 ###########################################

###calcular los intervalos de confianza para la desviacion satandar

sx <- 50/(60*60)
alpha2 <- 0.01
n <- 10*100
#### cuantiless
(cuantil_izquierdoz <- qchisq(1-alpha2, n-1))
(cuantil_derecho <- qchisq(alpha2/2, n-1))

##calculo de los intervalos

(valor_izquierdo <- (n-1)*sx/cuantil_izquierdoz)
(valor_derecho <- (n-1)*sx/cuantil_derecho)

##valores al cuadrado 

c(sqrt(valor_izquierdo), sqrt(valor_derecho))


################################################################################
############################### ejercicio 5 ####################################

library(epitools)

binom.approx(30, 120)

2*qnorm(p=0.99, mean = 3)




