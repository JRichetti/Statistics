#############################################
EXEMPLO CAFE: SEM USO DE GRUPO TREINAMENTO
#############################################

dados <- read.table("exdiscri_cafe.txt", header = T)   
dados

attach(dados)

plot(x1,x2, col = grupo, xlab = "Área da fazenda (ha)", 
ylab = "% da renda familiar quanto a atividade agrícola")
legend("bottomright", legend=c("Café", "Pecuária"), text.col = c("black", "red"), bty="n")

xbarca <- colMeans(dados[dados$grupo == 1,1:2])
xbarpe <- colMeans(dados[dados$grupo == 2,1:2])

xbarca
xbarpe

Sca <- cov(dados[dados$grupo == 1,1:2],dados[dados$grupo == 1,1:2])
Spe <- cov(dados[dados$grupo == 2,1:2],dados[dados$grupo == 2,1:2])

Sca
Spe

## discriminar cafe  ## pecuária
## método de Fisher, supondo SIGMAC = SIGMAV ##

n1 <- 12
n2 <- 12
n <- 24        ## tamanho amostral geral ##
p <- 2         ## nº de variáveis pesquisadas em cada grupo ##

n1
n2
n
p


S <- (((n1-1)*Sca) + ((n2-1)*Spe))/(n-2)
S

L <- t(xbarca - xbarpe) %*% solve(S)
L

num <- as.numeric(sqrt(L %*% t(L)))
num

Lpadro <- L * (1/(num))
Lpadro

pre <- L %*% t(dados[,1:2])   ## scores está errado ##
pre <- as.vector(pre)
pre

length(pre)

m <- 0.5 * (t(xbarca-xbarpe) %*% solve(S) %*% (xbarca+xbarpe))
as.numeric(m)

classe_pre <- rep(NA, n)
classe_pre

for(i in 1:length(classe_pre)){
a <- as.numeric(ifelse(pre[i] < m, 2, 1))
classe_pre[i] <- a
}
classe_pre

table(grupo)
table(classe_pre)

real <- as.vector(grupo)
real

cbind(real,classe_pre)

## QUALIDADE DA REGRA ##

## matriz de confusão ##

table(real, classe_pre)

## proporção de má classificação ##

table(real, classe_pre)[1,2]/sum(table(real, classe_pre)[1,])

table(real, classe_pre)[2,1]/sum(table(real, classe_pre)[2,])

## taxa de erro aparente: TEA ##

(table(real, classe_pre)[1,2]+table(real, classe_pre)[2,1])/n

## exatidão global: EG = 1-TEA ##

(table(real, classe_pre)[1,1]+table(real, classe_pre)[2,2])/n



require(MASS)

z <- lda(grupo ~ ., dados, prior = c(1,1)/2)

fit <- predict(z,dados )
fit$class

ct <- table(grupo, fit$class)
ct

sum(diag(prop.table(ct)))  ## % de classificação correta EG ##


#######################################################
EXEMPLO BANCO DE DADOS IRIS DO R
#######################################################

data(iris)

require(MASS)

help(lda)

iris3[,,1]     ## selecionar da espécie 1 ##

#########################################
## SEM O USO DE AMOSTRA DE TREINAMENTO ##
#########################################

Iris <- data.frame(rbind(iris3[,,1], iris3[,,2], iris3[,,3]),
                   Sp = rep(c("s","c","v"), rep(50,3)))       ## codificada o nome das espécies ##
Iris

dim(Iris)

xbars <- colMeans(Iris[Iris$Sp == "s",1:4])
xbarc <- colMeans(Iris[Iris$Sp == "c",1:4])
xbarv <- colMeans(Iris[Iris$Sp == "v",1:4])

xbars
xbarc
xbarv

Ss <- cov(Iris[Iris$Sp == "s",1:4],Iris[Iris$Sp == "s",1:4])
Sc <- cov(Iris[Iris$Sp == "c",1:4],Iris[Iris$Sp == "c",1:4])
Sv <- cov(Iris[Iris$Sp == "v",1:4],Iris[Iris$Sp == "v",1:4])

Ss
Sc
Sv

## discriminar versicolor com virginica ##
## método de Fisher, supondo SIGMAC = SIGMAV ##

n1 <- dim(Iris[Iris$Sp == "c",1:4])[1]    ## tamanho amostral do grupo 1: versicolor ##
n2 <- dim(Iris[Iris$Sp == "v",1:4])[1]    ## tamanho amostral do grupo 2: virginica ##
n <- n1+n2     ## tamanho amostral geral ##
p <- 4         ## nº de variáveis pesquisadas em cada grupo ##

n1
n2
n
p


S <- (((n1-1)*Sc) + ((n2-1)*Sv))/(n-2)
S

L <- t(xbarc - xbarv) %*% solve(S)
L

num <- as.numeric(sqrt(L %*% t(L)))
num
Lpadro <- L * (1/(num))
Lpadro

pre <- L %*% t(Iris[51:150,1:4])   ## scores está errado ##
pre <- as.vector(pre)
pre

length(pre)

m <- 0.5 * (t(xbarc-xbarv) %*% solve(S) %*% (xbarc+xbarv))
as.numeric(m)

classe_pre <- rep(NA, n)
classe_pre

for(i in 1:length(classe_pre)){
a <- as.numeric(ifelse(pre[i] < m, 2, 1))
classe_pre[i] <- a
}
classe_pre

table(Iris$Sp)
table(classe_pre)

real <- as.vector(Iris$Sp[51:150])
real

cbind(real,classe_pre)

ca <- as.numeric((sqrt(L%*%t(L))))
Lpa <- (1/ca)*t(L)
Lpa

## QUALIDADE DA REGRA ##

## matriz de confusão ##

table(real, classe_pre)

## proporção de má classificação ##

table(real, classe_pre)[1,2]/sum(table(real, classe_pre)[1,])

table(real, classe_pre)[2,1]/sum(table(real, classe_pre)[2,])

## taxa de erro aparente: TEA ##

(table(real, classe_pre)[1,2]+table(real, classe_pre)[2,1])/n

## exatidão global: EG = 1-TEA ##

(table(real, classe_pre)[1,1]+table(real, classe_pre)[2,2])/n



## USO DE AMOSTRA DE TREINAMENTO ##

Iris <- data.frame(rbind(iris3[,,1], iris3[,,2], iris3[,,3]),
                   Sp = rep(c("s","c","v"), rep(50,3)))       ## codificada o nome das espécies ##
Iris

dim(Iris)

train <- sort(sample(1:150, 75))     ## sorteio da posição das amostras para formar a regra de classificação ##
train

Iris[train,]                   ## amostras de treinamento ## 
dim(Iris[train,])      

Iris$Sp[train]
table(Iris$Sp[train])


## COMANDO lda: para mais de um grupo ##

require(MASS)

z <- lda(Sp ~ ., Iris, prior = c(1,1,1)/3, subset = train)

fit <- predict(z, Iris[-train, ])
fit$class

ct <- table(Iris[train,5], fit$class)
ct

sum(diag(prop.table(ct)))  ## % de classificação correta EG ##

plot(z)

## Estatística Press's Q ##
N <- sum(ct)
N
n <- sum(diag(ct))
n
k <- length(a)
PQ <- ((N-n*k)^2)/(N*(k-1))
PQ
qui_tab <- qchisq(0.95, df=1)    ##  colocar o complementar do nível de significância ##
qui_tab
1 - pchisq(PQ,1)