## INFERÊNCIA SOBRE DUAS POPULAÇÕES ##

data(iris)
iris

names(iris)

par(mfrow = c(2,2))
hist(iris$Sepal.Length)
hist(iris$Sepal.W)
hist(iris$Petal.Length)
hist(iris$Petal.W)

par(mfrow = c(2,2))
boxplot(iris$Sepal.Length)
boxplot(iris$Sepal.W)
boxplot(iris$Petal.Length)
boxplot(iris$Petal.W)

## ANÁLISE DA NORMALIDADE UNIVARIADA ##

shapiro.test(iris$Sepal.Length)
shapiro.test(iris$Sepal.W)
shapiro.test(iris$Petal.Length)
shapiro.test(iris$Petal.W)

par(mfrow = c(2,2))     ## abre uma janela gráfica com espaço para 2 gráficos ##
qqnorm(iris$Sepal.Length, main = "QQ-plot para Comprimento da Sépala")
qqline(iris$Sepal.Length, col = "blue")           ## qq-plot
qqnorm(iris$Sepal.W, main = "QQ-plot para Largura da Sépala")
qqline(iris$Sepal.W, col = "blue")           ## qq-plot
qqnorm(iris$Petal.Length, main = "QQ-plot para Comprimento da Pétala")
qqline(iris$Petal.Length, col = "blue")           ## qq-plot
qqnorm(iris$Petal.W, main = "QQ-plot para Largura da Pétala")
qqline(iris$Petal.W, col = "blue")           ## qq-plot

require(mvsf)

ad.test(iris$Sepal.Length)       ## teste de Anderson-Darling ##
ad.test(iris$Sepal.W)
ad.test(iris$Petal.Length)       
ad.test(iris$Petal.W)

lillie.test(iris$Sepal.Length)       ## teste de Kolmogorov-Smirnov ##
lillie.test(iris$Sepal.W)
lillie.test(iris$Petal.Length)       
lillie.test(iris$Petal.W)


## ANÁLISE DA NORMALIDADE MULTIVARIADA ##

require(mvnormtest)
require(mvsf)

c <- as.matrix(cbind(iris$Sepal.L, iris$Sepal.W,iris$Petal.L, iris$Petal.W))

mshapiro.test(t(c))

mvsf(t(c))  


## INFERÊNCIA PARA COMPARAÇÃO DE DOIS GRUPOS ##

seto <- iris[iris$Species == "setosa",1:4]
versi <- iris[iris$Species == "versicolor",1:4]
virgi <- iris[iris$Species == "virginica",1:4]

seto
versi
virgi

## comparando as matrizes de variâncias e covariâncias populacionais
## dos dois grupos

n1 <- 50       ## tamanho amostral do grupo 1 ##
n2 <- 50       ## tamanho amostral do grupo 2 ##
n <- n1+n2     ## tamanho amostral geral ##
p <- 4         ## nº de variáveis pesquisadas em cada grupo ##

s1 <- cov(seto)
s1
s2 <- cov(versi)
s2

sp <- (((n1-1)*s1) + ((n2-1)*s2))/(n-2)
sp

a <- ((1/(n1-1)) + (1/(n2-1)) - (1/(n-2)))

b <- ((n1-1)*log(det(s1))) + ((n2-1)*log(det(s2))) - ((n-2)*log(det(sp)))

qui_cal <- -(1-a)*b
qui_cal

v <- (p*(p+1))/2
v

qui_tab <- qchisq(0.95, df=v)    ##  colocar o complementar do nível de significância ##

## comparando vetor de médias setosa x versicolor ##

## tarefa: fazer a rotina para a comparação do vetor de médias setosa x versicolor ##