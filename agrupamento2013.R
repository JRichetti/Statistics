dados <- read.table("C:/Users/j_ric/Documents/P�s-Gradua��o/Disciplinas/An�lise Multivariada/R/acp_alface.txt", header = T)   
dados

x <- dados[,2:7]
x

row.names(x) = dados[,1]
x

###################################
## AGRUPAMENTO DAS VARI�VEIS ##
###################################

R <- cor(x)
R                              ## matriz de correla��o Pearson ##

r <- as.dist((1-(R^2)))  ## transforma��o SUGERIDA POR RENCHER (2002) para que valores mais pr�ximos de 0 s�o mais similares##
r

#Agrupamento por liga��o simpes
###############################

aa_single <- hclust(r, "single")    ##agrupamento por liga��o simples ##
aa_single

aa_single$height       ## valores de jun��o no dendrograma ##

d2 <- cophenetic(aa_single)
cor(r,d2)              ## coeficiente de correla��o cofen�tica ##
cor.test(r,d2)

plot(aa_single, xlab = "Vari�veis", ylab = "Fun��o da Correla��o", main = "")

plot(aa_single, xlab = "Vari�veis", ylab = "Fun��o da Correla��o", main = "", 
hang = -1)

rect.hclust(aa_single, k = 3) #faz ret�ngulos no k grupos escolhidos

#M�todo com a Liga��o Completa
###############################

aa_com <- hclust(r, "complete")     ## liga��o completa ## 
aa_com

aa_com$height       ## valores de jun��o no dendograma ##

d2 <- cophenetic(aa_com)
cor(r,d2)              ## coeficiente de correla��o cofen�tica ##
cor.test(r,d2)

plot(aa_com, xlab = "Vari�veis", ylab = "Fun��o da Correla��o", main = "", 
hang = -1)
rect.hclust(aa_com, k = 3)

#Agrupamento por Ward
###############################

aa_ward <- hclust(r, "ward")       ## Ward ##
aa_ward


d2 <- cophenetic(aa_ward)
cor(r,d2)              ## coeficiente de correla��o cofen�tica ##
cor.test(r,d2)

plot(aa_ward, xlab = "Vari�veis", ylab = "Fun��o da Correla��o", main = "", 
hang = -1)

rect.hclust(aa_ward, k = 2)

################################
## AGRUPAMENTO DOS INDIV�DUOS ##
################################

disteuc <- dist(x)
disteuc

#Agrupamento por liga��o Simples com dist�ncia Euclidiana
#########################################################

aa_single <- hclust(disteuc, "single")    ## liga��o simples ##
aa_single

d2 <- cophenetic(aa_single)
cor(disteuc,d2)              ## coeficiente de correla��o cofen�tica ##
cor.test(disteuc,d2)


plot(aa_single, xlab = "Parcelas", ylab = "Dist�ncia Euclidiana", main = "", 
hang = -1)

#Agrupamento utilizando Centro�de
#################################

aa_cen <- hclust(disteuc, "centroid")        ## centr�ide ##
aa_cen

d2 <- cophenetic(aa_cen)
cor(disteuc,d2)              ## coeficiente de correla��o cofen�tica ##
cor.test(disteuc,d2)

plot(aa_cen, xlab = "Parcelas", ylab = "Dist�ncia Euclidiana", main = "", 
hang = -1)

#Agrupamentos utilizando o pacote Vegan
#######################################

require(vegan)

c <- cutree(aa_single, k = 3)     ## classifica os elementos em cada grupo ##
c  
plot(x, col = c)                  ## disgrama de dispers�o a cada 2 vari�veis, identificando os grupos ##             

#Agrupamento por k-mean
#######################

kmeans = kmeans(x,3)        ## m�todo kmeans ##

#####################
## DADOS BIN�RIOS ##
#####################

dados1 <- read.table("C:/Users/j_ric/Documents/P�s-Gradua��o/Disciplinas/An�lise Multivariada/R/ex_binario.txt", header = T)   ## lendo um conjunto de dados em txt, header = T tem nome das colunas na 1� linha ##
dados1

require(fossil)

jac <- ecol.dist(t(dados1), method = jaccard, type = "sim")
jac

jac <- 1-abs(jac)
jac

aa_single <- hclust(jac, "single")    ## liga��o simples ##
aa_single

d2 <- cophenetic(aa_single)
cor(jac,d2)              ## coeficiente de correla��o cofen�tica ##
cor.test(jac,d2)

plot(aa_single, xlab = "Fazendas", ylab = "Fun��o de Jaccard", main = "", 
hang = -1)
