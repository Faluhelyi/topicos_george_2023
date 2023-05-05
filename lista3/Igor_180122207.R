setwd(
  "C:/Users/Igor/Desktop/topicos_george_2023/git_repo/lista3"
  )
library(readxl)
library(ggplot2)

#########
# dados #
#########
df = read_xlsx('sample_whr.xlsx')
nomes <- c("Score", "LogGDP_per_capita", 
           "Social_support", "Healthy_life_exp",
           "Freedom_choices", "Generosity",
           "Percep_corruption", "Country_name")
setnames(df,nomes)

var(df[,2:7])
cor(df[,2:7])

###########
## PCA 1 ##
###########
cprecor <- prcomp(~ Score + LogGDP_per_capita +
                    Social_support + Healthy_life_exp
                  + Freedom_choices
                  + Generosity + Percep_corruption,
                  data=df, scale = T)

var_explained = cprecor$sdev^2 / sum(cprecor$sdev^2)
qplot(c(1:7), var_explained) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

plot(cumsum(cprecor$sdev**2)/sum(cprecor$sdev**2), 
     type="b", ylim = c(0.5,1),
     xlab="CPs", ylab="% da Variancia",xaxt = 'n')
axis(side=1, at=c(1:7),labels=c(1:7))

summary(cprecor)
cprecor


ggplot_pca(cprecor, labels = df$Country_name) +
  labs(title = "PCA World Happiness Report")

########################################################
## Retirar aleatoriamente 25% da informação dos dados ##
########################################################



###########
## PCA 2 ##
###########