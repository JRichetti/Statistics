a <- 4+5
a

a+8
a^2

## SEQUENCIAS DE VALORES: VETORES ##

a <- 1:10   ## sequência de valores inteiros, do 1 ao 10, variando de 1 em 1 ##
a
b <- -5:5
b
c <- 5:-5
c

## dimensão (comprimento) de um vetor ##

length(b)

4 x 6
## podemos operacionalizar vetores com mesma dimensão ##

b + (2*c)   

d <- seq(0,20, by = 2)        ## comando sequencia com espaçamento 2 ##
d

e <- seq(0,20, by = 0.5)
e

f <- seq(0,20, length.out = 8)   ## sequencia com 8 elementos ##
f

## vetor com elementos pré-definidos ##

x <- c(1,0,-2,3,4,6,10)       ## que podem ser nº's ##  
x

d <- c("Ana", "Carlos", "Juliana")    ## ou palavras ##
d

## seleção de elementos de um vetor, com uma determinada posição ##

x[2]                   ## qual é o 2º elemento do vetor x? ##
x[1:4]                 ## qual é o vetor de elementos do vetor x, do 1º ao 4º? ##
x[c(1,4)]

## REPETIÇÕES ##

## repetição do nº 2, 20 vezes ##

a <- rep(2,20) 
a

## repetição de 26 células, sem nenhum elemento ##

b <- rep(NA, 26)
b

## repetição da sequência de nº's 1,2,3,4, cada nº sendo repetido 2 vezes ##

rep(1:4, each = 2)

## repetição da sequência de nº's 1,2,3,4, duas vezes ##

rep(1:4, 2)

## repetição da sequência de nº's 1,2,3,4, cada nº sendo repetido 2 vezes, ##
## e essa repetição, realizada 3 vezes ##

rep(1:4, each = 2, 3)


## MATRIZES ##

k <- 1:16
k

## matriz com os elementos de k, com 4 colunas e linhas ##

z <- matrix(k, ncol = 4, nrow = 4)
z


d <- c(4,0,1,2)
d

A <- matrix(c(4,0,1,2), ncol = 2, nrow = 2)
A

## matriz com os elementos de k, com 4 colunas e linhas ##
## sendo que os elementos são distribuídos por linhas ##

w <- matrix(k, ncol = 4, nrow = 4, byrow = T)
w

z+w
z-w
z * w
z %*% w
2*z

## seleção de um elemento na matrix w ##

w[1,3]        ## na posição: 1ª linha, 3ª coluna ##
w[2,]         ## elementos da 2ª linha ##
w[,4]         ## elementos da 4ª coluna ##

## dimensão de uma matriz ##

dim(w)

## matriz diagonal (e identidade) ##

B <- diag(1, 3)
B

C <- diag(2,4) 
C

A <- diag(c(1,2,4,7), 4)
A


## determinante de uma matriz ##

c <- matrix(c(1,2,2,1), ncol = 2, nrow = 2)
c

det(c)

## matriz inversa ##

solve(c)

## autovalores e autovetores normalizados ##

help(eigen)
?eigen

eigen(c)

## produto de kronecker ##

X <- matrix(c(1,0,0,2), ncol = 2, nrow = 2)

Y <- matrix(c(5,4,0,3), ncol = 2, nrow = 2)

X
Y

kronecker(X,Y)
kronecker(Y,X)

## matriz bloco diagonal com Y ##

kronecker(diag(1,3), Y)

## LER UM BANCO DE DADOS CRIADO EM BLOCO DE NOTAS ##
## header = T indica que na 1ª linha o arquivo contém os nomes das colunas ##

dados <- read.table("ex_solo.txt", header = T)   ## lendo um conjunto de dados em txt ##
dados

names(dados)
is.matrix(dados)
is.vector(dados)
is.data.frame(dados)

c <- as.matrix(dados)
c

dados$ca     ## variável ca ##

attach(dados)

ca
mg
sb

plot(ca, mg)

plot(ca, mg, xlab = "Ca", ylab = "Mg")
plot(ca, sb)
plot(mg, sb)

plot(dados,dados)

## gráficos de dispersão simultâneos ##

pairs(dados, c("Ca", "Mg", "SB"))

## histogramas individuais ##

hist(ca)
hist(ca, main = "", xlab = "Ca", ylab = "Frequência")

hist(mg, main = "", xlab = "Mg", ylab = "Frequência")
hist(sb, main = "", xlab = "SB", ylab = "Frequência")

hist(sb, col = "blue")

## gráficos de dispersão simultâneos com diagonal = histograma das variáveis ##
##  função que cria um histograma ##

panel.hist <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}

pairs(dados, c("Ca", "Mg", "SB"), diag.panel=panel.hist )


## gráfico de dispersão em 3D ##
## necessário instalar o pacote "scatterplot3d" ##

require(scatterplot3d)
scatterplot3d(ca, mg, sb)


## boxplot individuais ##

boxplot(ca)
boxplot(mg)
boxplot(sb)

## Estatísticas descritivas ##
summary(dados)
sd(dados)

mean(dados)

colMeans(dados)

cov(dados,dados)

cor(dados,dados)
cor(dados)

cor.test(ca,mg)
cor.test(ca,sb)
cor.test(mg,sb)



