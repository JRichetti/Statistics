#############################################
#EXEMPLO CAFE: SEM USO DE GRUPO TREINAMENTO
#############################################

dados <- read.table("C:/Users/j_ric/Documents/P�s-Gradua��o/Disciplinas/An�lise Multivariada/R/exdiscri_cafe.txt", header = T)   
dados

attach(dados)

plot(x1,x2, col = grupo, xlab = "�rea da fazenda (ha)", 
ylab = "% da renda familiar quanto a atividade agr�cola")
legend("bottomright", legend=c("Caf�", "Pecu�ria"), text.col = c("black", "red"), bty="n")

xbarca <- colMeans(dados[dados$grupo == 1,1:2])
xbarpe <- colMeans(dados[dados$grupo == 2,1:2])

xbarca
xbarpe

Sca <- cov(dados[dados$grupo == 1,1:2],dados[dados$grupo == 1,1:2])
Spe <- cov(dados[dados$grupo == 2,1:2],dados[dados$grupo == 2,1:2])

Sca
Spe


## TESTE DE BARTTLET ##

dados11 <- dados[dados$grupo == 1,1:2 ]
dados12 <- dados[dados$grupo == 2,1:2 ]

dim(dados11)
dim(dados12)

n1 <- dim(dados11)[1]       ## tamanho amostral do grupo 1 ##
n2 <- dim(dados12)[1]       ## tamanho amostral do grupo 2 ##
n <- n1+n2     ## tamanho amostral geral ##
p <- length(1:dim(dados11)[2])         ## n� de vari�veis pesquisadas em cada grupo ##
n1
n2
p

s1 <- Sca
s1
s2 <- Spe
s2
sp <- (((n1-1)*s1) + ((n2-1)*s2))/(n-2)
sp
a <- ((1/(n1-1)) + (1/(n2-1)) - (1/(n-2)))
b <- ((n1-1)*log(det(s1))) + ((n2-1)*log(det(s2))) - ((n-2)*log(det(sp)))
qui_cal <- -(1-a)*b
qui_cal
v <- (p*(p+1))/2
v
qui_tab <- qchisq(0.95, df=v)    ##  colocar o complementar do n�vel de signific�ncia ##
qui_tab

## discriminar cafe  ## pecu�ria
## m�todo de Fisher, supondo SIGMAC = SIGMAV ##

n1 <- 12
n2 <- 12
n <- 24        ## tamanho amostral geral ##
p <- 2         ## n� de vari�veis pesquisadas em cada grupo ##

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

pre <- L %*% t(dados[,1:2])   ## scores est� errado ##
pre <- as.vector(pre)
pre

length(pre)

m <- 0.5 * (t(xbarca-xbarpe) %*% solve(S) %*% (xbarca+xbarpe))
as.numeric(m) #esse � o valor discriminante

classe_pre <- rep(NA, n)
classe_pre

for(i in 1:length(classe_pre)){
a <- as.numeric(ifelse(pre[i] < m, 2, 1))
classe_pre[i] <- a
}
classe_pre #fez a compara��o entre o Dx e o pre, separando entre 1 e 2, sendo 2 = errado e 1 = certo

#Taxa de erro aparente, 1 erro em 12 na popula��o 1; e 2 em 12 na popula��o 2

table(grupo) #quantos indiv�duso em cada grupo - correto
table(classe_pre) #quantos indiv�duos em cada grupo - resultado

real = as.vector(grupo)
real

cbind(real,classe_pre)

## QUALIDADE DA REGRA ##

## matriz de confus�o ##

table(real, classe_pre)

## propor��o de m� classifica��o ##

table(real, classe_pre)[1,2]/sum(table(real, classe_pre)[1,])

table(real, classe_pre)[2,1]/sum(table(real, classe_pre)[2,])

## taxa de erro aparente: TEA ##

(table(real, classe_pre)[1,2]+table(real, classe_pre)[2,1])/n

## exatid�o global: EG = 1-TEA ##

(table(real, classe_pre)[1,1]+table(real, classe_pre)[2,2])/n



require(MASS)

z <- lda(grupo ~ ., dados, prior = c(1,1)/2)

fit <- predict(z,dados )
fit$class

ct <- table(grupo, fit$class)
ct

sum(diag(prop.table(ct)))  ## % de classifica��o correta EG ##

z <- qda(grupo ~ ., dados, prior = c(1,1)/2)
z

fit <- predict(z,dados )
fit$class

ct <- table(grupo, fit$class)
ct

sum(diag(prop.table(ct)))  ## % de classifica��o correta EG ##

## Estat�stica Press's Q ##
N <- sum(ct)
N
n <- sum(diag(ct))
n
k <- length(a)
PQ <- ((N-n*k)^2)/(N*(k-1))
PQ
qui_tab <- qchisq(0.95, df=1)    ##  colocar o complementar do n�vel de signific�ncia ##
qui_tab
1 - pchisq(PQ,1) #esse � o p-value

#######################################################
#EXEMPLO BANCO DE DADOS IRIS DO R
#######################################################

data(iris)

require(MASS)

help(lda)

iris3[,,1]     ## selecionar da esp�cie 1 ##

#########################################
## SEM O USO DE AMOSTRA DE TREINAMENTO ##
#########################################

Iris <- data.frame(rbind(iris3[,,1], iris3[,,2], iris3[,,3]),
                   Sp = rep(c("s","c","v"), rep(50,3)))       ## codificada o nome das esp�cies ##
Iris

dim(Iris)

## teste M Box ##
require(biotools)

dados1 <- dados[,1:2]

data1 <- as.matrix(dados1)
grouping1 <- grupo
boxM(data1, grouping1) #caso o as matrizes sejam iguais, pode-se utilizar uma discriminante linear

## An�lise de discriminante ##
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
## m�todo de Fisher, supondo SIGMAC = SIGMAV ##

n1 <- dim(Iris[Iris$Sp == "c",1:4])[1]    ## tamanho amostral do grupo 1: versicolor ##
n2 <- dim(Iris[Iris$Sp == "v",1:4])[1]    ## tamanho amostral do grupo 2: virginica ##
n <- n1+n2     ## tamanho amostral geral ##
p <- 4         ## n� de vari�veis pesquisadas em cada grupo ##

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
Lpadro #valor de L padronizado, aqui quem � mais pr�ximo de -1 ou 1 s�o mais importantes

pre <- L %*% t(Iris[51:150,1:4])   ## scores est� errado ##
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

## matriz de confus�o ##

table(real, classe_pre)

## propor��o de m� classifica��o ##

table(real, classe_pre)[1,2]/sum(table(real, classe_pre)[1,])

table(real, classe_pre)[2,1]/sum(table(real, classe_pre)[2,])

## taxa de erro aparente: TEA ##

(table(real, classe_pre)[1,2]+table(real, classe_pre)[2,1])/n

## exatid�o global: EG = 1-TEA ##

(table(real, classe_pre)[1,1]+table(real, classe_pre)[2,2])/n



## USO DE AMOSTRA DE TREINAMENTO ##

Iris <- data.frame(rbind(iris3[,,1], iris3[,,2], iris3[,,3]),
                   Sp = rep(c("s","c","v"), rep(50,3)))       ## codificada o nome das esp�cies ##
Iris

dim(Iris)

train <- sort(sample(1:150, 75))     ## sorteio da posi��o das amostras para formar a regra de classifica��o ##
train

Iris[train,]                   ## amostras de treinamento ## 
dim(Iris[train,])      

Iris$Sp[train]
table(Iris$Sp[train])


## COMANDO lda: para mais de um grupo ## lda = linear qda = multipla

require(MASS)

z <- lda(Sp ~ ., Iris, prior = c(1,1,1)/3, subset = train)
z #dentro encontra-se os vetores L

#Padroniza��o do LD1
L = t(z$scaling[,1])
L
num <- as.numeric(sqrt(L %*% t(L)))
num
Lpadro <- L * (1/(num))
Lpadro 

fit <- predict(z, Iris[-train, ])
fit$class
table(fit$class)

ct <- table(Iris[-train,5], fit$class)
ct

sum(diag(prop.table(ct)))  ## % de classifica��o correta EG ##

plot(z)

