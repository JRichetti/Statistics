a <- 4+5
a

a+8
a^2

## SEQUENCIAS DE VALORES: VETORES ##

a <- 1:10   ## sequ�ncia de valores inteiros, do 1 ao 10, variando de 1 em 1 ##
a
b <- -5:5
b
c <- 5:-5
c

## dimens�o (comprimento) de um vetor ##

length(b)

4 x 6
## podemos operacionalizar vetores com mesma dimens�o ##

b + (2*c)   

d <- seq(0,20, by = 2)        ## comando sequencia com espa�amento 2 ##
d

e <- seq(0,20, by = 0.5)
e

f <- seq(0,20, length.out = 8)   ## sequencia com 8 elementos ##
f

## vetor com elementos pr�-definidos ##

x <- c(1,0,-2,3,4,6,10)       ## que podem ser n�'s ##  
x

d <- c("Ana", "Carlos", "Juliana")    ## ou palavras ##
d

## sele��o de elementos de um vetor, com uma determinada posi��o ##

x[2]                   ## qual � o 2� elemento do vetor x? ##
x[1:4]                 ## qual � o vetor de elementos do vetor x, do 1� ao 4�? ##
x[c(1,4)]

## REPETI��ES ##

## repeti��o do n� 2, 20 vezes ##

a <- rep(2,20) 
a

## repeti��o de 26 c�lulas, sem nenhum elemento ##

b <- rep(NA, 26)
b

## repeti��o da sequ�ncia de n�'s 1,2,3,4, cada n� sendo repetido 2 vezes ##

rep(1:4, each = 2)

## repeti��o da sequ�ncia de n�'s 1,2,3,4, duas vezes ##

rep(1:4, 2)

## repeti��o da sequ�ncia de n�'s 1,2,3,4, cada n� sendo repetido 2 vezes, ##
## e essa repeti��o, realizada 3 vezes ##

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
## sendo que os elementos s�o distribu�dos por linhas ##

w <- matrix(k, ncol = 4, nrow = 4, byrow = T)
w

z+w
z-w
z * w
z %*% w
2*z

## sele��o de um elemento na matrix w ##

w[1,3]        ## na posi��o: 1� linha, 3� coluna ##
w[2,]         ## elementos da 2� linha ##
w[,4]         ## elementos da 4� coluna ##

## dimens�o de uma matriz ##

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
## header = T indica que na 1� linha o arquivo cont�m os nomes das colunas ##

dados <- read.table("ex_solo.txt", header = T)   ## lendo um conjunto de dados em txt ##
dados

names(dados)
is.matrix(dados)
is.vector(dados)
is.data.frame(dados)

c <- as.matrix(dados)
c

dados$ca     ## vari�vel ca ##

attach(dados)

ca
mg
sb

plot(ca, mg)

plot(ca, mg, xlab = "Ca", ylab = "Mg")
plot(ca, sb)
plot(mg, sb)

plot(dados,dados)

## gr�ficos de dispers�o simult�neos ##

pairs(dados, c("Ca", "Mg", "SB"))

## histogramas individuais ##

hist(ca)
hist(ca, main = "", xlab = "Ca", ylab = "Frequ�ncia")

hist(mg, main = "", xlab = "Mg", ylab = "Frequ�ncia")
hist(sb, main = "", xlab = "SB", ylab = "Frequ�ncia")

hist(sb, col = "blue")

## gr�ficos de dispers�o simult�neos com diagonal = histograma das vari�veis ##
##  fun��o que cria um histograma ##

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


## gr�fico de dispers�o em 3D ##
## necess�rio instalar o pacote "scatterplot3d" ##

require(scatterplot3d)
scatterplot3d(ca, mg, sb)


## boxplot individuais ##

boxplot(ca)
boxplot(mg)
boxplot(sb)

## Estat�sticas descritivas ##
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



