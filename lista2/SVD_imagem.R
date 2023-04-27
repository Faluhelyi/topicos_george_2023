# Topicos em Aprendizado Estatístico
# Decomposicao em valores singulares
# Exemplo - Redução de Dimensionalidade
library(psych)
library(OpenImageR)

foto = "C:/Users/u00378/Desktop/topicos_george_2023/lista2/imag1.jpg"
#foto = "C:/Users/u00378/Desktop/topicos_george_2023/lista2/imag2.jpg"
img <- readImage(foto)
dim(img)
imageShow(img)
img_gray <- rgb_2gray(img) # ERROR!!!
img3 <- img[,,1:3] 
imageShow(img3)
dim(img3)
img_gray <- rgb_2gray(img3)
dim(img_gray)
imageShow(img_gray)

### Aplicando SVD na Imagem
imgg.svd <- svd(img_gray)
imgg.svd$d
D <- diag(imgg.svd$d)
dim(D)
U <- imgg.svd$u
V <- imgg.svd$v
plot(1:length(imgg.svd$d), imgg.svd$d)
abline(v=c(5,10,20,40))
plot(1:length(imgg.svd$d), imgg.svd$d,
     xlim = c(1,60))
abline(v=c(5,10,20,40))
plot(1:length(imgg.svd$d), imgg.svd$d,
     xlim = c(10,80),ylim = c(0,30))
abline(v=c(20,40,60))

### Aproximacoes
U5 <- as.matrix(U[,1:5])
V5 <- as.matrix(V[,1:5])
D5 <- diag(imgg.svd$d[1:5])
img_gray5 <- U5 %*% D5 %*% t(V5)
imageShow(img_gray5)
((tr(D5)/tr(D))-1) * 100 

U10 <- as.matrix(U[,1:10])
V10 <- as.matrix(V[,1:10])
D10 <- diag(imgg.svd$d[1:10])
img_gray10 <- U10 %*% D10 %*% t(V10)
imageShow(img_gray10)
((tr(D10)/tr(D))-1) * 100

U20 <- as.matrix(U[,1:20])
V20 <- as.matrix(V[,1:20])
D20 <- diag(imgg.svd$d[1:20])
img_gray20 <- U20 %*% D20 %*% t(V20)
imageShow(img_gray20)
((tr(D20)/tr(D))-1) * 100

U30 <- as.matrix(U[,1:30])
V30 <- as.matrix(V[,1:30])
D30 <- diag(imgg.svd$d[1:30])
img_gray30 <- U30 %*% D30 %*% t(V30)
imageShow(img_gray30)
((tr(D30)/tr(D))-1) * 100

U40 <- as.matrix(U[,1:40])
V40 <- as.matrix(V[,1:40])
D40 <- diag(imgg.svd$d[1:40])
img_gray40 <- U40 %*% D40 %*% t(V40)
imageShow(img_gray40)
((tr(D40)/tr(D))-1) * 100

U60 <- as.matrix(U[,1:60])
V60 <- as.matrix(V[,1:60])
D60 <- diag(imgg.svd$d[1:60])
img_gray60 <- U60 %*% D60 %*% t(V60)
imageShow(img_gray60)
tr(D60)/tr(D) * 100 

U99 <- as.matrix(U[,1:99])
V99 <- as.matrix(V[,1:99])
D99 <- diag(imgg.svd$d[1:99])
img_gray99 <- U99 %*% D99 %*% t(V99)
imageShow(img_gray99)
tr(D99)/tr(D) * 100

rankMatrix(img_gray)[1]
rankMatrix(img_gray99)[1]
