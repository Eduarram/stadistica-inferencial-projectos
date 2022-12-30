library(tidyverse)

x <- filter(mtcars, am == 0)

y <- filter(mtcars, am == 1)

t.test(x=x$mpg, y=y$mpg, alternative = "two.sided", var.equal = TRUE)


f <- table(mtcars$am, mtcars$vs)

a <- f[1,]
b <- f[2,]


proppor <- cbind(a, b)


fisher.test(x=proppor, alternative = "less")


var.test(x$mpg, x$mpg, alternative = "less")

Test de kolmogorov-smirnov.

p1 <- 1-exp(-0.2*0.1)
p2 <- 1-exp(-0.1* 2.6)
p3 <- 1-exp(-0.1* 4.4)
p4 <- 1-exp(-0.1* 4.9)
p5 <- 1-exp(-0.1* 10.6)
p6 <- 1-exp(-0.1* 11.3)
p7 <- 1-exp(-0.1* 11.8)
p8 <- 1-exp(-0.1* 12.6)
p9 <- 1-exp(-0.1* 23)
p10 <- 1-exp(-0.1* 40.8)



p <- c(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)

x <- c(0.2, 2.6,4.4,4.9,10.6,11.3,11.8,12.6,23,40.8)

probabilitys <- seq(from=0.1, to=1, by=0.1)


length(probabilitys)

df <- cbind(p, probabilitys) 

library(tidyverse)

df <-  mutate(as.data.frame(df), prob1 = abs(p-probabilitys))
df <- mutate(df, prob2 =abs(p-(probabilitys + 0.1)))
df <- cbind(df, x)

ggplot(df, mapping=aes(x=p, y=probabilitys)) + geom_point(color="red")+
  geom_line(color="blue")

ggplot(df, mapping=aes(x=x, y=probabilitys)) + geom_point(color="red")+
  geom_line(color="blue")


k <- table(mtcars$gear)

lambda <- 3.7

n <- length(mtcars$gear)

i <- c(3, 4, 5)


for(j in i){
    print(n*((lambda^j)/factorial(j))*exp(-lambda))
}

vprob <- c(6.679044, 6.178116, (length(mtcars$gear) -6.178116 - 6.679044))
  

frec <- c(15, 12, 5)

tab <- cbind(frec, vprob)


#library(tidyverse)

v_critico <- sum((frec-vprob)/vprob)

p_value <- pchisq(v_critico, n=32, df=2)



