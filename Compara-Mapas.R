#Mapa 1
# d1 será do mapa 1 (d.k1)
d1<-as.matrix(d.k1$predict)
#total de pixels
dim(d1)

#Os valores devem ser colocados de acordo com a escala
f1<-function(d1){
fx1<-matrix(0,dim(d1),1)
fx1[d1<9.5]<-1
fx1[d1>=9.5&d1<11.2]<-2
fx1[d1>=11.2&d1<12.9]<-3
fx1[d1>=12.9&d1<14.6]<-4
fx1[d1>=14.6]<-5
return(fx1)
}
a<-f1(d1)

##Mapa 2
# d2 será do mapa 2 (d.k2)
d2<-as.matrix(d.k2$predict)
#total de pixels
dim(d2)
f2<-function(d2){
fx2<-matrix(0,dim(d2),1)
fx2[d2<9.5]<-1
fx2[d2>=9.5&d2<11.2]<-2
fx2[d2>=11.2&d2<12.9]<-3
fx2[d2>=12.9&d2<14.6]<-4
fx2[d2>=14.6]<-5
return(fx2)
}
b<-f2(d2)

#Matriz de Erro
m<-table(as.vector(b),as.vector(a))
m

require(caret) 
confusionMatrix(m)

#Exatidão Global
EG=sum(diag(m))/sum(m)
EG

#Tau
Tau = (EG-1/5)/(1-1/5)
Tau
