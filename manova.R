x <- read.table("turfa.txt", header = T)   ## lendo um conjunto de dados em txt ##
x

attach(x)

par(mfrow = c(2,1))
boxplot(n ~ trat, names = c("Testemunha", "Fermentada", "Natural"), ylab = "Teor de N")
boxplot(p ~ trat, names = c("Testemunha", "Fermentada", "Natural"), ylab = "Teor de P")

## ANOVA ##

trat <- as.factor(trat)
anvn <- aov(n  ~ trat)
anvp <- aov(p ~ trat)
summary(anvn)
summary(anvp)

## MANOVA ##
## test = c("Pillai" (defualt), "Wilks", "Hotelling-Lawley", "Roy") ##

var <- as.matrix(x[,2:3])
var
fit <- manova(var ~ trat)

summary.aov(fit)

summary.manova(fit)
summary.manova(fit, test = "Wilks")
