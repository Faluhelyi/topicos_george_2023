######################
### DATA WRANGLING ###
######################
library(readxl)
library(purrr)
library(dplyr)
library(ggplot2)
library(zoo)
library(forecast)
library(tibble)

setwd("C:/Users/u00378/Desktop/topicos_george_2023/lista6")
caminho_base <- "C:/Users/u00378/Desktop/topicos_george_2023/lista6/BaseDados.xlsx"
## Obter os nomes das abas
abas <- excel_sheets(caminho_base)
## Ler as abas e armazenar em uma lista
dados_abas <- map(abas, ~read_excel(caminho_base, sheet = .x))
## Realizar o join com base no índice (primeira coluna)
dados_combinados <- reduce(dados_abas, left_join, by = '...1')
## data wrangling
dados_combinados <- data.frame(dados_combinados, row.names = dados_combinados$...1)
dados_combinados <- dados_combinados[, -1]
linhas_especificas <- row.names(filter(dados_combinados, abs(y1)>10))

r1 = dados_combinados[,1]



caminho_base <- "C:/Users/u00378/Desktop/topicos_george_2023/lista6/BaseDados_ajustada.xlsx"
dados_combinados <- read_excel(caminho_base)
dados_combinados <- dados_combinados %>% column_to_rownames('date')
r2 = dados_combinados[,1]

# Criar um data frame de exemplo com a série temporal da variável resposta
dados <- data.frame(Data = seq(as.Date("2013-03-01"), by = "month", length.out = 120),
                    Resposta1 = r1, Resposta2 = r2)


# Criar o primeiro gráfico da série temporal da variável "Resposta"
grafico1 <- ggplot(dados, aes(x = Data, y = Resposta1)) +
  geom_line() +
  labs(title = "Série Temporal da Variável Resposta antes", y = "Resposta") +
  theme_minimal()

# Criar o segundo gráfico da série temporal da variável "OutraColuna"
grafico2 <- ggplot(dados, aes(x = Data, y = Resposta2)) +
  geom_line() +
  labs(title = "Série Temporal da Variável Resposta depois", y = "Resposta") +
  theme_minimal()

# Criar a imagem com dois gráficos lado a lado
imagem <- ggplot() +
  annotation_custom(ggplotGrob(grafico1), xmin = 0, xmax = 0.5, ymin = 0, ymax = 1) +
  annotation_custom(ggplotGrob(grafico2), xmin = 0.5, xmax = 1, ymin = 0, ymax = 1) +
  theme_void()

# Salvar a imagem como arquivo .png
ggsave("C:/Users/u00378/Desktop/topicos_george_2023/lista6/imagem.png",
       imagem, width = 10, height = 5, dpi = 300)


###############
### PARTE 1 ###
###############
#setwd("C:/Users/u00378/Desktop/topicos_george_2023/lista5")
library(readxl)
library(purrr)
library(dplyr)
library(ggplot2)
library(zoo)
library(forecast)

## Item 1, 2 e 3
start <- Sys.time()

RMSEP_1 <- c()
c = 1
predicao = matrix(0, 120, 4)
n_PC = matrix(0, 120, 4)
for (j in c(0.7, 0.8, 0.9, 0.95)) {
  predicao_e = vector(length = 120)
  for (i in 1:120) {
    caminho_base <- "C:/Users/u00378/Desktop/topicos_george_2023/lista6/BaseDados_ajustada.xlsx"
    dados_combinados <- read_excel(caminho_base)
    dados_combinados <- dados_combinados %>% column_to_rownames('date')
    # Selecionar todas as colunas da linha retirada (LOOCV)
    true_y1 <- dados_combinados[i, 1]
    true_colunas <- dados_combinados[i, -1]
    # Selecionar todas as colunas com todas as linhas exceto a LINHA retirada (LOOCV)
    dados_combinados <- dados_combinados[-i,]
    
    resp <- dados_combinados %>% select(y1)
    colunas <- dados_combinados %>% select(-y1)
    # Executar prcomp para todas as colunas selecionadas
    resultado <- prcomp(colunas)
    var_explained = resultado$sdev^2 / sum(resultado$sdev^2)
    
    # max. de var explicada pelas CPs
    s = sum(cumsum(var_explained) <= j)
    n_PC[i, c] = s
    
    covariaveis <- as.matrix(colunas) %*%
      as.matrix(resultado$rotation[,c(1:s)])
    covariaveis <- data.frame(covariaveis)
    newdata = data.frame(cbind(resp, covariaveis))
    
    modelo <- lm(y1 ~ ., data = newdata)
    
    novo_caso <- as.matrix(true_colunas) %*% as.matrix(resultado$rotation[,c(1:s)])
    #Fazer a predicao da linha retirada usando o modelo ajustado
    predicao[i, c] <- predict(modelo, newdata = data.frame(novo_caso))
    predicao_e[i] <- (predict(modelo, newdata = data.frame(novo_caso)) - true_y1)**2
  }
  RMSEP_1 <- c(RMSEP_1, sqrt(mean(predicao_e)))
  c =c +1
}

Sys.time() - start # 2.737376 mins na BRB SEGUROS
                   # Processador: Intel(R) Core(TM) i5-10500 CPU @ 3.10GHz 3.10 GHz
                   # RAM instalada: 8,00 GB (utl: 7,72 GB)

# > RMSEP_1
#[1] 1.1386270 0.9768882 0.9811538 0.9760095

# media_n_PC_por_faixa_retencao_em_X = c(3.991667, 8, 16.95, 29.05)

## Item 4
predicao = vector(length = 24)
predicao_e = vector(length = 24)
t_y = vector(length = 24)
for (i in c(24:1)) {
  caminho_base <- "C:/Users/u00378/Desktop/topicos_george_2023/lista6/BaseDados_ajustada.xlsx"
  dados_combinados <- read_excel(caminho_base)
  dados_combinados <- dados_combinados %>% column_to_rownames('date')
  # Selecionar todas as colunas da linha retirada (LOOCV)
  true_y1 <- dados_combinados[120-i+1, 1]
  true_colunas <- dados_combinados[120-i+1, -1]
  # Selecionar todas as colunas com todas as linhas exceto a LINHA retirada (LOOCV)
  dados_combinados <- dados_combinados[c(1:(120-i)),]
  resp <- dados_combinados %>% select(y1)
  colunas <- dados_combinados %>% select(-y1)
  # Executar prcomp para todas as colunas selecionadas
  resultado <- prcomp(colunas)
  
  var_explained = resultado$sdev^2 / sum(resultado$sdev^2)
  s = sum(cumsum(var_explained) <= 0.80)
  
  covariaveis <- as.matrix(colunas) %*%
    as.matrix(resultado$rotation[,c(1:s)]) # usando 80% retencao
  covariaveis <- data.frame(covariaveis)
  newdata = data.frame(cbind(resp, covariaveis))
  
  modelo <- lm(y1 ~ ., data = newdata)
  
  novo_caso <- as.matrix(true_colunas) %*% as.matrix(resultado$rotation[,c(1:s)])
  #Fazer a predicao da linha retirada usando o modelo ajustado
  predicao[i] <- predict(modelo, newdata = data.frame(novo_caso))
  predicao_e[i] <- (predict(modelo, newdata = data.frame(novo_caso)) - true_y1)**2
  t_y[i] <- true_y1
}
RMSEP_1_4 <- sqrt(mean(predicao_e)) # 1.331318 com 0.95 de retenção


###############
### PARTE 2 ###
###############
#install.packages('caret')
library(caret)

#install.packages('pls')
library(pls)

start <- Sys.time()

caminho_base <- "C:/Users/u00378/Desktop/topicos_george_2023/lista6/BaseDados_ajustada.xlsx"
dados_combinados <- read_excel(caminho_base)
dados_combinados <- dados_combinados %>% column_to_rownames('date')

Y <- dados_combinados %>% select(y1)

pcr = pcr(y1~., ncomp = 29, data = dados_combinados, scale = F)

result = crossval(pcr, segments = 120, segment.type = 'consecutive',
                  length.seg = 1)

Sys.time() - start

# 10.67219 secs na BBR SEGUROS

RMSEP_2 <- c()
for (i in c(4, 8, 17, 29)){
  RMSEP_2 <- c(RMSEP_2, sqrt(mean((as.matrix(result$validation$pred[,,i]) - as.matrix(Y))**2)))
}

# RMSEP_1 E RMSEP_2 NÃO BATEM NA VIRGULA, POIS PARA O RMSE_1 O NÚMERO DE CPs QUE ENTRAM COMO
# VAR. EXP. NO MODELO LINEAR VARIA PARA CADA ITERAÇÃO DENTRO DO LOOCV, JÁ NO RMSE_2 O NÚMERO
# DE CPs QUE ENTRAM É FIXO.


###############
### PARTE 3 ###
###############
#install.packages('caret')
library(caret)

#install.packages('pls')
library(pls)

## Item 1, 2 e 3
start <- Sys.time()

caminho_base <- "C:/Users/u00378/Desktop/topicos_george_2023/lista6/BaseDados_ajustada.xlsx"
dados_combinados <- read_excel(caminho_base)
dados_combinados <- dados_combinados %>% column_to_rownames('date')

Y <- dados_combinados %>% select(y1)

plsr = plsr(y1~., data = dados_combinados, ncomp = 29)

result = crossval(plsr, segments = 120, segment.type = 'consecutive',
                  length.seg = 1)

Sys.time() - start

# 9.659629  secs na BBR SEGUROS

RMSEP_3 <- c()
for (i in c(4, 8, 17, 29)){
  RMSEP_3 <- c(RMSEP_3, sqrt(mean((as.matrix(result$validation$pred[,,i]) - as.matrix(Y))**2)))
}

# n_comp = 17 SE DESTACOU COMO MELHOR

## Item 4
predicao = vector(length = 24)
predicao_e = vector(length = 24)
t_y = vector(length = 24)
for (i in c(24:1)) {
  caminho_base <- "C:/Users/u00378/Desktop/topicos_george_2023/lista6/BaseDados_ajustada.xlsx"
  dados_combinados <- read_excel(caminho_base)
  dados_combinados <- dados_combinados %>% column_to_rownames('date')
  
  # Selecionar todas as colunas da linha retirada (LOOCV)
  true_y1 <- dados_combinados[120-i+1, 1]
  # Selecionar todas as colunas com todas as linhas exceto a LINHA retirada (LOOCV)
  dados_combinados <- dados_combinados[c(1:(120-i+1)),]
  
  plsr = plsr(y1~., data = dados_combinados, ncomp = 17)
  
  result = crossval(plsr, segments = 120, segment.type = 'consecutive',
                    length.seg = 1)
  predicao[i] <- tail(as.matrix(result$validation$pred[,,17]), 1)[1]
  predicao_e[i] <- (predicao[i] -true_y1)**2
  t_y[i] <- true_y1
}
RMSEP_3_4 <- sqrt(mean(predicao_e)) # 1.261655 com 0.95 de retenção


library(openxlsx)

write.xlsx(as.data.frame(rev(predicao)), file = "tt.xlsx")
write.xlsx(as.data.frame(t_y), file = "ttt.xlsx")




