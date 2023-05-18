setwd("C:/Users/Igor/Desktop/topicos_george_2023/git_repo/lista4")
########################################################
## Exercício 14 do Capítulo 3 de James et al. (2021). ##
########################################################

#a,b e c
set.seed(180122207)
x1 = runif(100)
x2 = 0.5*x1+rnorm(100)/10
y = 2+2*x1+0.3*x2+rnorm(100)

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
x2=c(x2 , 1)
y=c(y,6)

lm.fit9_g = lm(y~x1+x2)
lm.fit10_g = lm(y~x1)
lm.fit11_g = lm(y~x2)

plot(x1,x2)
text(x=0.1, y=1, labels="observação_nova", pos=4, cex=.7, col="red")

par(mfrow=c(2,2))
plot(lm.fit9_g)

par(mfrow=c(2,2))
plot(lm.fit10_g)

par(mfrow=c(2,2))
plot(lm.fit11_g)
