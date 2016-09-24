dados <- read.table("acp_alface.txt", header = T)   
dados

dados1 <- dados[,2:7]

dados1


row.names(dados1) = dados[,1]

dados1
attach(dados1)
names(dados1)

is.data.frame(dados1)

dim(dados1)

colMeans(dados1)

S <- cov(dados1,dados1)
S


R <- cor(dados1,dados1)
R

cor.test(HCO3,SO4)


## TESTE DE ESFERICIDADE DE BARTLETT
n <- nrow(dados1)
p <- ncol(dados1)
chi2 <- -(n-1-((2*p+5)/6))*log(det(R))
ddl <- p*(p-1)/2
print(chi2)
print(ddl)
print(pchisq(chi2,ddl,lower.tail=F))


require(psych)
cortest.bartlett(R,n)  ## teste de esfericidade de bartlett


require(psych)
KMO(R)  ## índice KMO pelo pacote psych ##

## COMANDO ESPECÍFICO DE COMPONENTES PRINCIPAIS ##

## USANDO A MATRIZ S ##

eigen(S)
cp <- prcomp(dados1)       ## cria os componentes principais usando S ##
cp

cp <- prcomp(dados1, scale = T)       ## cria os componentes principais usando R ##
cp


summary(cp)           ## desvio padrão, proporção e proporção acumulada ##

screeplot(cp)
screeplot(cp, type = "lines")     ## gráfico de cotovelo ##

names(cp)

cp$sdev               ## desvio padrão dos CP's: raíz quadrada autovalores ##
cp$rotation           ## coeficientes cada componente principal: autovetores ##
cp$center             ## coordenada central: média amostral ##

cp$rotation[,1]       ## coeficientes do 1º CP ##

score <- t(cp$rotation[,1]) %*% t(dados1)
score                  ## score para cada indivíduo no CP1 ##

cp$x                  ## scores com variáveis centradas ##

cbind(1:19,as.vector(score))

par(mfrow = c(1,2))
plot(1:19, as.vector(score))
plot(1:19, cp$x[,1])


## no comando biplot podemos adicionar o subcomando 
## choices = c(1,3), no qual escolhe quais cps quer plotar ##

biplot(cp)            ## gráfico biplot ##

cor(as.vector(score), dados1)


require(graphics)

## USANDO A MATRIZ R ##

cp2 <- princomp(dados1, cor = T)   ## usando a matriz de correlação ##
cp2

summary(cp2)

screeplot(cp2, type = "lines")

names(cp2)
cp2$loa                  ## coeficientes de cada componente principal ##
cp2$score                ## scores com variéveis centradas ##

cor(cp2$score[,1], HCO3)

par(mfrow = c(1,2))
plot(cp2$sco[,1], cp2$sco[,2])
biplot(cp2)  