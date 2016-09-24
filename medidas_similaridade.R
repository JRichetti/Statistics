dados <- read.table("ex_binario.txt", header = T)   ## lendo um conjunto de dados em txt, header = T tem nome das colunas na 1ª linha ##

dados
dados[1,]
dados[2,]

require(fossil)
jaccard(dados[1,], dados[2,])

ecol.dist(t(dados), method = jaccard, type = "sim")

sorenson(dados[1,], dados[2,])

ecol.dist(t(dados), method = sorenson, type = "sim")

ecol.dist(t(dados), method = ochiai, type = "sim")


## DADOS DO ALFACE ##

dados1 <- read.table("acp_alface.txt", header = T)   

dados1 <- dados1[,2:7]

as.matrix(dados1)
x <- t(dados1)

disteuc <- dist(x)
disteuc

disteuc2 <- dist(x, method= "euclidean")
disteuc2

dist_manhatan <- dist(x, method= "manhattan")
dist_manhatan

dist_min2 <- dist(x, method= "minkowski", p=2)  ## coincide com a Euclidiana ##
dist_min2

dist_min1 <- dist(x, method= "minkowski", p=1)  ## coincide com a Manhattan ##
dist_min1

dist_minp <- dist(x, method= "minkowski", p=0.5)  ## coincide com a Manhattan ##
dist_minp

## correlação de Pearson e de Spearman ##

Rpe <- cor(dados1)
Rpe

Rspe <- cor(dados1, method = "spearman")
Rspe

r <- as.dist((1-(Rpe^2)))  ## MEDIDA DE DISSIMILARIDADE SUGERIDA POR RENCHER (2002) ##
r


## mahalanobis ##

s <- cov(dados1)                     ## matriz de covariância ##
s

x <- dados1
maha <- matrix(rep(NA,(dim(x)[1])^2), ncol = dim(x)[1],nrow = dim(x)[1])
for(i in 1:dim(x)[1]){
 xi <- as.vector(as.numeric(x[i,]))
 for(j in 1:dim(x)[1]){
  xj <- as.vector(as.numeric(x[j,]))
  maha[i,j] <- t(xi-xj) %*% (solve(s)) %*% (xi-xj)  ## distância de Mahalanobis ##

  }
 }

maha

i <- 1
j <- 3
xi <- as.vector(as.numeric(x[i,]))
xj <- as.vector(as.numeric(x[j,]))
mahalanobis(xi,xj, s)                ## distância de Mahalanobis ##


