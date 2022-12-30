




library(tidyverse)

mt1 <- mtcars %>% select(cyl, mpg) %>% filter(cyl==4)
mt2 <- mtcars %>% select(cyl, mpg) %>% filter(cyl==6)
mt3 <- mtcars %>% select(cyl, mpg) %>% filter(cyl==8)

mt4 <- rbind(mt1[1:7,], mt2[1:7,], mt3[1:7,])


summary(aov(cyl~as.factor(mpg), data=mt4))

boxplot(mpg ~ cyl, data=mtcars)



### test de  Holm
library(agricolae)
pairwise.t.test(mt4$mpg, as.factor(mt4$cyl), p.adjust.method="holm")


###test de homocedasticidad de barlet 


bartlett.test(mt4$mpg, mt4$cyl)



library(nortest)

lillie.test(mt4$mpg)


functionX <- function(x){
  lillie.test(x)
}

tapply(mt4$mpg, mt4$cyl, FUN = functionX)


####prblema n01 


factores <- rep(c(0.5, 1.0, 1.5, 2.0), each=3)
x <-c(1,3,5,4,6,2,3,5,7,8,10,6) 

df <- data.frame(factores, x)

summary(aov(x~as.factor(factores), data = df))

pairwise.t.test(df$x, as.factor(df$factores), p.adjust.method = "bonferroni")

pairwise.t.test(df$x, as.factor(df$factores), p.adjust.method = "holm")


### problema 2 

bloques <- rep(c(01, 02, 03, 04), each=6)
tratamiento <- rep(c(9, 10, 11, 12, 13, 14), 4)
media_tr <-c(22, 21, 17, 20, 16, 21, 25, 19, 23, 31, 15, 35, 24, 18, 26, 25, 23, 23, 11, 16, 17, 24, 24, 20)

df <- data.frame(bloques, tratamiento, media_tr)

summary(aov(media_tr~ bloques+tratamiento, data=df))




####problema 3 



bloqueA <- rep(rep(1:2, each=6), 3)
bloqueB <- rep(1:3, each=12)
vector1 <- c(69, 71.3, 73.2, 75.1, 74.4, 75, 71.1, 69.2, 70.4, 73.2, 71.2,70.9,
             96.1, 102.3, 107.5, 103.6, 100.7, 101.8, 81, 85.8, 86, 87.5, 88.1, 87.6,
             121, 122.9, 123.1, 125.7, 125.2, 120.1, 101.1, 103.2, 106.2, 109.7, 109, 106.9)


df2 <- data.frame(bloqueA, bloqueB, vector1)

summary(aov(vector1~bloqueA*bloqueB, data = df2))


### ejercicio 4 

X_fac <-rep(1:4, each=7) 
x_var <- c(20, 26, 26, 24, 23, 
           26, 21, 24, 22, 20, 21, 21, 22, 20, 
           16, 18, 20, 21, 24, 15, 17, 19, 15, 
           13, 16, 12, 11, 14)
  
  
df3 <- data.frame(X_fac, x_var)  


bartlett.test(x_var, X_fac)



summary(aov(x_var~as.factor(X_fac), data = df3))

