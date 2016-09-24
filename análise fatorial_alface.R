## DADOS ALFACE ##

dados <- read.table("acp_alface.txt", header = T)   
dados

dados1 <- dados[,2:7]

dados1


row.names(dados1) = dados[,1]

dados1
attach(dados1)

R <- cor(dados1,dados1)
R

auto <- eigen(R)
auto

prop_acu <- sum(auto$values[1:2]) / sum(auto$values)
prop_acu

## cargas fatoriais pelo método de CP ##

carga1 <- sqrt(auto$values[1]) * auto$vectors[,1]
carga2 <- sqrt(auto$values[2]) * auto$vectors[,2]

carga1
carga2

L <- cbind(carga1,carga2)
L

com <- carga1^2 + carga2^2
com


plot(carga1,carga2)


########################################################################
PESQUISA: TESTES EM UMA ESCOLA PREPARATÓRIA, FORAM REALIZADOS,
COM 220 MENINAS

VARIÁVEIS: X1 = NOTA EM GAÉLICO            X2 =  NOTA EM INGLÊS
           X3 = NOTA EM HISTÓRIA           X4 = NOTA EM ARITMÉTICA
           X5 = NOTA EM ÁLGEBRA            X6 = NOTA EM GEOMETRIA
########################################################################

dados_cor <- c(1, 0.439, 0.410, 0.288, 0.329, 0.248, 0.439, 1, 0.351, 0.354, 0.32,
0.329, 0.41, 0.351, 1, 0.164, 0.19, 0.181, 0.288, 0.354, 0.164, 1, 0.595, 0.47,
0.329, 0.32, 0.19, 0.595, 1, 0.464, 0.248, 0.329, 0.181, 0.47, 0.464, 1)

R <- matrix(dados_cor, ncol = 6, nrow = 6)
R

auto <- eigen(R)
auto

prop_acu <- sum(auto$values[1:2]) / sum(auto$values)
prop_acu

## cargas fatoriais pelo método de CP ##

carga1 <- sqrt(auto$values[1]) * auto$vectors[,1]
carga2 <- sqrt(auto$values[2]) * auto$vectors[,2]

L <- cbind(carga1,carga2)
L

com <- carga1^2 + carga2^2
com

par(mfrow = c(1,2))
plot(carga1,carga2)


## rotação de 45º sentido anti-horário ##

T <- matrix(c(cos(pi/4), sin(pi/4), -sin(pi/4), cos(pi/4)), ncol = 2, nrow = 2)
T

LT <- L %*% T
LT

plot(LT[,1],LT[,2])

## rotação varimax ##

varimax(L, normalize = F)