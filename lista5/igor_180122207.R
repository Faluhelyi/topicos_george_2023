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

start <- Sys.time()

RMSEP <- c()
c = 1
predicao = matrix(0, 120, 4)
for (j in c(0.7, 0.8, 0.9, 0.95)) {
  predicao_e = vector(length = 120)
  for (i in 1:120) {
    caminho_base <- "C:/Users/u00378/Desktop/topicos_george_2023/lista5/BaseDados.xlsx"
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
    # Substitua os valores por NA nas linhas especificadas
    dados_combinados[linhas_especificas, ] <- replace(dados_combinados[linhas_especificas, ],
                                                      TRUE, NA)
    # Substituir NA pelo valor observado no mês anterior
    dados_combinados <- na.locf(dados_combinados, na.rm = FALSE)
    
    
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
  RMSEP <- c(RMSEP, sqrt(mean(predicao_e)))
  c =c +1
}

Sys.time() - start # 2.769196 mins na BRB SEGUROS
                   # Processador: Intel(R) Core(TM) i5-10500 CPU @ 3.10GHz 3.10 GHz
                   # RAM instalada: 8,00 GB (utl: 7,72 GB)

# > RMSEP
#[1] 1.1386270 0.9768882 0.9811538 0.9760095

###############
### PARTE 2 ###
###############






