#setwd("C:/Users/Igor/Desktop/Topicos_2_george")

#### Exercicio 1 ----


## Calculo (a)
data = data.frame(
  x1 = c(0,2,0,0,-1,1),
  x2 = c(3,0,1,1,0,1),
  x3 = c(0,0,3,2,1,1),
  y = c("red",'red','red',"green","green",'red')
)
test = c(0,0,0)

paste('dist(test, Observacao',1:6,') =',
      round(dist(rbind(test,data[-4]))[1:6],2 ))

## Calculo (c)
class::knn(train = data[-4],
           test,
           cl = data[,4],
           k = 3)


#### exercicio 24 da lista 2 ----
str <- "32 1.59 2738.86 4.2 24.1 61 0.49 824.26 3.9 29.8 51 1.14 1307.03 4.1 20.0 53 0.74 925.47 4.2 25.0 24 1.99 2787.46 3.8 21.5 65 1.00 1222.51 4.2 25.0 35 2.32 2038.28 4.1 18.7 45 0.93 1061.53 4.2 22.0 57 0.81 1657.73 4.2 31.2 32 1.23 1652.76 3.9 24.3 66 0.99 1636.25 4.1 27.7 27 1.40 1845.07 4.0 21.8 54 1.08 1542.30 3.9 29.0 55 1.22 1214.53 4.0 21.1 50 0.57 1451.17 4.0 27.1 48 0.83 1786.95 4.1 24.7 28 1.55 1975.26 3.5 18.8 66 1.10 1248.64 4.0 18.9 66 0.44 987.86 4.0 27.6 48 0.58 1067.10 4.3 26.4 60 0.43 968.62 4.0 35.9 59 0.66 836.94 3.9 25.3 50 1.81 1197.99 3.9 19.5 29 1.21 1818.31 4.2 21.8 40 0.98 1238.91 3.5 21.9 47 1.48 2153.47 3.5 17.3 52 0.98 1720.60 3.6 29.7 54 1.02 1906.30 4.5 31.9 53 0.82 981.85 3.9 26.2 47 0.46 1020.95 4.4 31.2 42 1.34 1028.10 3.6 18.1 79 1.48 1465.91 3.9 18.3 61 1.39 1456.12 3.9 24.9"

values <- gsub(" ", ",", str)
values <- strsplit(values, ",")[[1]]

df <- data.frame(matrix(as.numeric(values), ncol = 5, byrow = T))
colnames(df)<- c("Idade", "Proteina", "Energia", "Albumina", "IMC")

summary(df)

## R plots
# Load the necessary packages
library(ggplot2)
library(tidyr)
library(gridExtra)
library(corrplot)
# Convert data to long format
df_long <- pivot_longer(df, cols = everything(),
                        names_to = "Variable", values_to = "Value")
# Create a boxplot for each variable using facet_wrap
boxplot_list <- lapply(unique(df_long$Variable), function(var) {
  ggplot(data = subset(df_long, Variable == var),
         aes(x = Variable, y = Value)) +
    geom_boxplot(color = "black", fill = "lightblue", width = 0.5) +
    labs(title = paste0("Boxplot of ", var),
         x = "Variable",
         y = "Value") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank())
})
# Arrange the boxplots in a grid using grid.arrange
grid.arrange(grobs = boxplot_list, ncol = 2)
# Calculate the correlation matrix
cor_matrix <- cor(df)
# Create a correlation plot using corrplot
corrplot(cor_matrix, method = "circle", type = "full", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.7, 
         diag = FALSE, addCoef.col = "black",
         col = colorRampPalette(c("blue", "white", "red"))(200))


## DECOMPOSICAO SPECTRAL

decomp = svd(df)
plot(cumsum(decomp$d)/sum(decomp$d), type='l',
     ylab = 'Prop. acumulada',
     main = 'Valores singulares',
     xlab = 'Componentes'
)
abline(h = 0.99, col = "red")


