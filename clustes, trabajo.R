####kmeans
library(dplyr)
mtcat <- select(mtcars,mpg , hp, wt, qsec)

kmeans(mtcat, centers = 4, algorithm = "Lloyd")

kmeans(dist(mtcat, method = "manhattan"), centers = 4, algorithm = "Lloyd")


###scalado de variables 
apply(mtcat, 2, sd)

mtcat_scal <- scale(mtcat)

apply(mtcat_scal, 2, sd)

clust <- hclust(dist(mtcat_scal), method = "complete")

plot(clust,hang=1,xlab="mt_scal",
     sub="", ylab="distancias")
rect.hclust(clust,k=5)



#### Clusters de tarea 

library(faraway)

set.seed(2020)
golpekar <- sample(1:595, 25, replace = F)
golworld <- worldcup[golpekar,]

##separamos las variables cuantitavas

gol_k <- select(golworld, Time, Shots, Passes, Tackles)

##escalado de variables 

apply(gol_k, 2, sd)

gol_k_sca <- scale(gol_k)

apply(gol_k_sca, 2, sd)

###funcion de prueba de clusters para el mtodo de codo 

kmeans_test <- function(data, min_centers, max_centers) {
  
  # Crea una lista vacía para almacenar los resultados
  results <- list()
  
  #metodo que deseamos de la funcion
  #method1 <- c("Lloyd", "Hartigan-Wong", "MacQueen")
  # Recorre todos los valores posibles de centros en el cluster
  for (k in min_centers:max_centers) {
    
    # Aplica k-means con el número de centros actual
    km <- kmeans(data, centers = k, algorithm = "MacQueen")
    
    # Almacena los resultados en la lista
    results[[k]] <- km$tot.withinss
  }
  
  # Devuelve la lista de resultados
  return(results)
}

###prueba de la funcion

lilt <-kmeans_test(gol_k_sca,2, 10)

####como la diferncia entre el cluster 4 y 3 es la mayor el numero optimo es 4

### calculamos el K_means

algritm_def <- kmeans(gol_k_sca, centers = 4, algorithm = "MacQueen")

plot(algritm_def)

###algoritmo de los clusters para distacias binarias 

bin_dist <- select(golworld, Team, Position)

bin_dist$Position <- factor(bin_dist$Position,  levels =  unique(bin_dist$Position), 
                            labels =c(0,1,2,3))

bin_dist$Team <- factor(bin_dist$Team, levels = unique(bin_dist$Team),
                        labels = c(0:18))



#####algoritmo aglomerativo 

clust2 <- hclust(dist(gol_k_sca), method = "median")

plot(clust2,hang=-1,xlab="play_scal",
     sub="", ylab="distancias")
rect.hclust(clust2,k=4)

