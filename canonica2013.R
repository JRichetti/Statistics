###############################################
## EXEMPLO LIVRO FERREIRA (2011), página 503 ##
###############################################

ca <- c(2.3,2.5,1.8,3.4,1.8,3.7,1.4,1.5,2.8,1.4,1.8,1.9,2.8)

mg <- c(1.7,2.5,2.1,2.5,1.1,1.4,0.7,0.6,2.2,0.8,0.6,1.7,0.8)

sb <- c(4.1,5.1,4.1,6.1,3,5.2,2.2,2.2,5.1,2.3,2.5,3.7,3.7)

t <- c(7.5,7.4,6.4,8.4,6.5,8.5,4.7,4,9,3.7,7.2,6,5.9)

dados <- cbind(ca,mg,sb,t)
dados

cor <- cor(dados)
cor

x <- as.matrix(cbind(ca,mg))
y <- as.matrix(cbind(sb,t))

corca <- cancor(x,y)

corca #u1 e v1 são as variáveis canônincas, desconsidera-se a 2ª que é de u2 e v2
#considerando o 1º valor, que é entre a u1 e v1.

#########################
## fazendo manualmente ##
#########################

r11 <- cor[1:2,1:2]
r11

r22 <- cor[3:4,3:4]
r22

r12 <- cor[1:2,3:4]
r12

A <- (solve(r11)) %*% r12 %*% (solve(r22)) %*% (t(r12))
A

B <- (solve(r22)) %*% (t(r12)) %*% (solve(r11)) %*% r12
B

eigen1 <- eigen(A)
eigen2 <- eigen(B)

eigen1
eigen2

sqrt(eigen1$values)      ## correlação canônica ##


#########################################
## estimativas das variáveis canônicas ##
#########################################

c1 <- (ca - mean(ca))/(sd(ca))
c1

p1pa <- (mg - mean(mg))/(sd(mg))
p1pa


z1 <- as.matrix(cbind(c1,p1pa))
z1

a1 <- as.matrix(eigen1$vectors[,1]) 
a1

u1 <- z1 %*% a1
u1

a2 <- as.matrix(eigen1$vectors[,2]) 
a2

u2 <- z1 %*% a2
u2


c2 <- (sb - mean(sb))/(sd(sb))
c2

p2pa <- (t - mean(t))/(sd(t))
p2pa


z2 <- as.matrix(cbind(c2,p2pa))
z2

b1 <- as.matrix(eigen2$vectors[,1]) 
b1

v1 <- z2 %*% b1
v1

b2 <- as.matrix(eigen2$vectors[,2]) 
b2

v2 <- z2 %*% b2
v2

cor(u1,v1)    ## correlação canônica ##
cor(u2,v2)    ## correlação canônica ##

#Gráficos das variáveis Canônicas

par(mfrow = c(1,2))
plot(u1,v1)
plot(u2,v2)

cor(u1,v2)    
cor(u2,v1)    

par(mfrow = c(1,2))
plot(u1,v2)
plot(u2,v1)

## correlação das variáveis canônicas U com as originais ##

cor(u1,c1)
cor(u1,p1)

cor(u1,c1)   ## iguais ao anterior ##
cor(u1,p1pa)

cor(u2,c1)
cor(u2,p1pa)

cor(u1,c2)
cor(u1,p2)

cor(u2,c2)
cor(u2,p2pa)

## correlação das variáveis canônicas V com as originais ##

cor(v1,c2)
cor(v1,p2)

cor(v2,c2)
cor(v2,p2pa)

cor(v1,c1)
cor(v1,p1)

cor(v2,c1)
cor(v2,p1pa)


## medida de qualidade do modelo ##
p <- 2  ## nº variáveis no 1º grupo ##
q <- 2  ## nº variáveis no 2º grupo ##


cor_u1 <- (cor(u1,c1)^2) + (cor(u1,p1pa)^2)
prop_u1 <- 100 * (cor_u1/p)
prop_u1 #quanto que reteve de informação a 1ª variável canônica do primeiro grupo

cor_v1 <- (cor(v1,c2)^2) + (cor(v1,p2pa)^2)
prop_v1 <- 100 * (cor_v1/p)
prop_v1#quanto que reteve de informação a 1ª variável canônica do segundo grupo



