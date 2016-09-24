dados <- read.table("C:/Users/j_ric/Documents/Pós-Graduação/Disciplinas/Análise Multivariada/R/acp_alface.txt", header = T)   
dados

x <- dados[,2:7]
x

row.names(x) = dados[,1]
x

###################################
## AGRUPAMENTO DAS VARIÁVEIS ##
###################################

R <- cor(x)
R                              ## matriz de correlação Pearson ##

r <- as.dist((1-(R^2)))  ## transformação SUGERIDA POR RENCHER (2002) para que valores mais próximos de 0 são mais similares##
r

#Agrupamento por ligação simpes
###############################

aa_single <- hclust(r, "single")    ##agrupamento por ligação simples ##
aa_single

aa_single$height       ## valores de junção no dendrograma ##

d2 <- cophenetic(aa_single)
cor(r,d2)              ## coeficiente de correlação cofenética ##
cor.test(r,d2)

plot(aa_single, xlab = "Variáveis", ylab = "Função da Correlação", main = "")

plot(aa_single, xlab = "Variáveis", ylab = "Função da Correlação", main = "", 
hang = -1)

rect.hclust(aa_single, k = 3) #faz retângulos no k grupos escolhidos

#Método com a Ligação Completa
###############################

aa_com <- hclust(r, "complete")     ## ligação completa ## 
aa_com

aa_com$height       ## valores de junção no dendograma ##

d2 <- cophenetic(aa_com)
cor(r,d2)              ## coeficiente de correlação cofenética ##
cor.test(r,d2)

plot(aa_com, xlab = "Variáveis", ylab = "Função da Correlação", main = "", 
hang = -1)
rect.hclust(aa_com, k = 3)

#Agrupamento por Ward
###############################

aa_ward <- hclust(r, "ward")       ## Ward ##
aa_ward


d2 <- cophenetic(aa_ward)
cor(r,d2)              ## coeficiente de correlação cofenética ##
cor.test(r,d2)

plot(aa_ward, xlab = "Variáveis", ylab = "Função da Correlação", main = "", 
hang = -1)

rect.hclust(aa_ward, k = 2)

################################
## AGRUPAMENTO DOS INDIVÍDUOS ##
################################

disteuc <- dist(x)
disteuc

#Agrupamento por ligação Simples com distância Euclidiana
#########################################################

aa_single <- hclust(disteuc, "single")    ## ligação simples ##
aa_single

d2 <- cophenetic(aa_single)
cor(disteuc,d2)              ## coeficiente de correlação cofenética ##
cor.test(disteuc,d2)


plot(aa_single, xlab = "Parcelas", ylab = "Distância Euclidiana", main = "", 
hang = -1)

#Agrupamento utilizando Centroíde
#################################

aa_cen <- hclust(disteuc, "centroid")        ## centróide ##
aa_cen

d2 <- cophenetic(aa_cen)
cor(disteuc,d2)              ## coeficiente de correlação cofenética ##
cor.test(disteuc,d2)

plot(aa_cen, xlab = "Parcelas", ylab = "Distância Euclidiana", main = "", 
hang = -1)

#Agrupamentos utilizando o pacote Vegan
#######################################

require(vegan)

c <- cutree(aa_single, k = 3)     ## classifica os elementos em cada grupo ##
c  
plot(x, col = c)                  ## disgrama de dispersão a cada 2 variáveis, identificando os grupos ##             

#Agrupamento por k-mean
#######################

kmeans = kmeans(x,3)        ## método kmeans ##

#####################
## DADOS BINÁRIOS ##
#####################

dados1 <- read.table("C:/Users/j_ric/Documents/Pós-Graduação/Disciplinas/Análise Multivariada/R/ex_binario.txt", header = T)   ## lendo um conjunto de dados em txt, header = T tem nome das colunas na 1ª linha ##
dados1

require(fossil)

jac <- ecol.dist(t(dados1), method = jaccard, type = "sim")
jac

jac <- 1-abs(jac)
jac

aa_single <- hclust(jac, "single")    ## ligação simples ##
aa_single

d2 <- cophenetic(aa_single)
cor(jac,d2)              ## coeficiente de correlação cofenética ##
cor.test(jac,d2)

plot(aa_single, xlab = "Fazendas", ylab = "Função de Jaccard", main = "", 
hang = -1)
