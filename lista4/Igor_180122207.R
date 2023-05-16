setwd("C:/Users/Igor/Desktop/topicos_george_2023/lista4")
########################################################
## Exercício 14 do Capítulo 3 de James et al. (2021). ##
########################################################

#a,b e c
set.seed(180122207)
x1 = runif(100)
x2 = 0.5*x1+rnorm(100)/10
y = 2+2*x1+0.3*x2+rnorm(100)


#$Y = 2 + 2x_1 + 0.3x_2 + \epsilon$, where $\epsilon$ is a N(0,1) variable.
#$\beta_0=2$, $\beta_1=2$, $\beta_2=0.3$ 
  

cor(x1,x2) 
plot(x2~x1)



lm.fit9 = lm(y~x1+x2)
summary(lm.fit9)

# d
lm.fit10 = lm(y~x1)
summary(lm.fit10)

# e

lm.fit11 = lm(y~x2)
summary(lm.fit11)

# g
x1=c(x1 , 0.1)
x2=c(x2 , 0.8)
y=c(y,6)

lm.fit9_g = lm(y~x1+x2)
lm.fit10_g = lm(y~x1)
lm.fit11_g = lm(y~x2)

plot(x1,x2)
text(x=0.1, y=0.8, labels="new-obs", pos=4, cex=.7, col="blue")

par(mfrow=c(2,2))
plot(lm.fit9_g)

par(mfrow=c(2,2))
plot(lm.fit10_g)

par(mfrow=c(2,2))
plot(lm.fit11_g)