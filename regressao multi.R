require(systemfit)

x <- c(24,11,5,12,17,14,11,18)
y1 <- c(40,17,9,15,6,12,5,9)
y2 <- c(53,19,10,29,13,27,19,30)

cor(y1,y2)

eq1 <- y1 ~ x
eq2 <- y2 ~ x

eqSystem <- list(semente = eq1, palha = eq2)

fit_ols <- systemfit(eqSystem)

fit_ols             ## estimativas dos coeficientes , Intercept = Beta 0##

model <- lm(cbind(y1,y2) ~ x)
model

summary(model) #resumo descritivo para cada coeficiente (Pr = p-valor se < aquele coeficiente é significativo e diferente de zero)
summary(fit_ols) #aqui tem OLS-R2  e McElroy-R2 são medidas da qualidade de ajuste geral da regressão multivariada

names(fit_ols)

fit_ols$coefCov       ## matriz de variâncias e covariâncias das estimativas dos coeficientes ##

fitted(fit_ols)       ## valores estimados ##

cbind(y1,y2,fitted(fit_ols))

e <- as.matrix(residuals(fit_ols))     ## resíduos ##
ee <- t(e) %*% e
ee/(length(y1)-1-1)                ## estimativa matriz covariâncias resíduos ##

fit_ols$residCov                   ## estimativa matriz covariâncias resíduos ##


X <- cbind(rep(1,length(x)),x)
X

invXX <- solve(t(X) %*% X)
invXX

fit_ols$residCov[1,1] * invXX
fit_ols$residCov[2,2] * invXX
fit_ols$coefCov               ## matriz covariâncias estimativas dos coeficientes ##



confint(fit_ols)   ## intervalos de confiança para as estimativas coeficientes, observa-se se o zero está dentro do intervalo,
## ou seja, o valor zero pode ser um valor, logo, o parâmetro não pe sifnificativo


## TESTE SE x É SIGNIFICATIVO NO MODELO ##

fit_ols             ## estimativas dos coeficientes com x no modelo##

eq12 <- y1 ~ 1
eq22 <- y2 ~ 1

eq12
eq22

eq2System <- list(semente = eq12, palha = eq22)

fit_ols2 <- systemfit(eq2System)

fit_ols2             ## estimativas dos coeficientes ##
mean(y1)
mean(y2)

lrtest(fit_ols, fit_ols2)
