## instalar pacotes mvnormtest, mvsf e nortest, a partir da vers�o 2.13

require(mvnormtest)
require(mvsf)

## exemplo trabalhado em multivariada com voc�s ##
dados <- read.table("ex_solo.txt", header = T)   ## lendo um conjunto de dados em txt ##
dados


attach(dados)

par(mfrow = c(3,2))
hist(ca)
hist(mg)
hist(sb)

boxplot(ca)
boxplot(mg)
boxplot(sb)

## TESTE DE SHAPIRO-WILKS UNIVARIADO ##

shapiro.test(ca)
shapiro.test(mg)
shapiro.test(sb)

## GR�FICO QQ-PLOT CASO UNIVARIADO ##

qqnorm(ca, main = "QQ-plot para Ca")
qqline(ca, col = "blue")           ## qq-plot

qqnorm(mg, main = "QQ-plot para Mg")
qqline(mg, col = "blue")           ## qq-plot

qqnorm(sb, main = "QQ-plot para SB")
qqline(sb, col = "blue")           ## qq-plot

## TESTE DE ANDERSON DARLING E KOLMOGOROV SMIRNOV ##

require(mvsf)

ad.test(ca)       ## teste de Anderson-Darling ##
ad.test(mg)
ad.test(sb)

lillie.test(ca)   ## teste de Kolmogorov-Smirnov ##
lillie.test(mg)
lillie.test(sb)


## TESTE DE SHAPIRO WILKS MULTIVARIDO ##
## cada elemento amostral deve ser uma coluna ##

c <- as.matrix(dados)

mshapiro.test(t(c))

## TESTE DE NORMALIDADE MULTIVARIADO DE SHAPIRO-FRANCIA ##

mvsf(t(c))                    ## teste de Shapiro-Francia

## GR�FICO QQ-PLOT MULTIVARIADO ##

x <- as.matrix(dados)
x

S=var(x)
S

m=apply(x,2,mean)   ## indica que � p/ fazer os c�lculos para cada coluna ##
m

invS=solve(S)
invS

d1_2 <- t(x[5,]-m) %*% invS %*% (x[5,]-m)
d1_2

(dim(x)[1]-1+0.5)/dim(x)[1]
1-((1-0.5)/dim(x)[1])

d = q = NULL

n= nrow(x)    ## n� de indiv�duos ##
p= ncol(x)    ## n� de vari�veis ##

for (i in 1:n){
  d = c(d, t(x[i,]-m) %*% invS %*% (x[i,]-m))
  prob = (i-0.5)/n
  q = c(q, qchisq(prob, df=p))
}

d=sort(d)

d
q

plot(d,q, xlab = "Dist�ncias Ordenadas - di^2", ylab = "Quantil Qui-Quadrado")
