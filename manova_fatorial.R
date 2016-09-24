x <- read.table("manova_expfatoria.txt", header = T)   ## lendo um conjunto de dados em txt ##
x

attach(x)

var <- as.matrix(x[,3:5])
var
cor(var)


## MANOVA ##
## test = c("Pillai" (defualt), "Wilks", "Hotelling-Lawley", "Roy") ##

var <- as.matrix(x[,2:3])
var
fit <- manova(var ~ razao + adi + razao:adi)

summary.aov(fit)

summary.manova(fit)
summary.manova(fit, test = "Wilks")
