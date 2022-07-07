#Bolivia
library(tidyverse)
library(lavaan)
library(semPlot)
library(psych)
library(GPArotation)
library(labelled)
library(haven)
library("mice")

Bolivia <- read_csv("novapasta/Bolivia.csv") # 
# primeiro listwise deletions
table(Bolivia$S002VS)

summary(Bolivia[,26:44])
b07 <- Bolivia[,26:44]
b07 <- subset(b07, select=-c(B006, F141))
b07m <- b07
b07 <- Bolivia
b07d <- subset(b07m)%>% na.omit()

#quantos?
KMO(b07d)# todos acima de 0.5
cortest.bartlett(b07d) # nao roda
nfactors(b07d, rotate = "varimax")
fit<-princomp(b07d,cor=TRUE)# AFE
summary(fit)# aqui olha o cumuçlative prop - razoavel 0.6 ou mais
plot(fit,type="lines")
pf1.2<- fa(b07d,nfactors=4,rotate = "varimax")
pf1.2$values
pf1.2
pf1.2$loadings
bol07d <- pf1.2$loadings
 # Bolivia dois fatores


#imputation

imp <- mice(b07m, seed=23109)# o nome da base e a seed sempre essa 23109
#stripplot(imp, pch=20, cex=1.2)# pra ver a imputação - se quiser - demora pra rodar
b07m <- complete(imp, 1)#sempre a m como  destino da mputação , empre escolher a 1

# aí repetir as fatoriais

KMO(b07m)# todos acima de 0.5
cortest.bartlett(b07m) # nao roda
nfactors(b07m, rotate = "varimax")
fit<-princomp(b07m,cor=TRUE)# AFE
summary(fit)# aqui olha o cumuçlative prop - razoavel 0.6 ou mais
plot(fit,type="lines")
pf1.2<- fa(b07m,nfactors=4,rotate = "varimax")
pf1.2$values
pf1.2
pf1.2$loadings
bol07m <- pf1.2$loadings
