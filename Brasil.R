# Brasil
#pacotes
library(tidyverse)
library(lavaan)
library(semPlot)
library(psych)
library(GPArotation)
library(labelled)
library(haven)
library("mice")
library(haven)

#baixar os bancos
#onda2
br2 <- read_sav("onda 2_89-93/moreno_clivagem_brasil1991.sav")
br2 <- remove_labels(br2)
br2m <- br2[,26:42]
summary(br2m)#verificar
br2d <- br2[,26:42]%>% na.omit()
summary(br2d)#verificar
#onda3
br3 <- read_sav("onda 3 _ 94-98/moreno_clivagem_brasil1997.sav")
br3 <- remove_labels(br3)
br3m <- br3[,26:41]
summary(br3m)#verificar
br3d <- br3[,26:41]%>% na.omit()
summary(br3d)#verificar

#onda5
br5 <- read_sav("onda 5 --- 2005-09/moreno_clivagem_brasil2006.sav")
br5 <- remove_labels(br5)
br5m <- br5[,26:40]
summary(br5m)#verificar
br5d <- br5[,26:40]%>% na.omit()
summary(br5d)#verificar

#onda6
br6 <- read_sav("onda 6 --- 2010 -14/moreno_clivagem_brasil2014.sav")
br6 <- remove_labels(br6)
br6m <- br6[,26:40]
summary(br6m)#verificar
br6d <- br6[,26:40]%>% na.omit()
summary(br6d)#verificar

#onda7
br7 <- read_sav("onda 7 - 2017-2022/moreno_clivagem_brasil2018.sav")
br7 <- remove_labels(br7)
br7m <- br7[,26:42]
summary(br7m)#verificar
br7d <- br7[,26:42]%>% na.omit()
summary(br7d)#verificar


# analysis 
# igual molde colombia(obs)
#onda 2
#listwise
KMO(br2d)# tudo ok - acima de 0.5
cortest.bartlett(br2d) #
nfactors(br2d, rotate = "varimax")
fit<-princomp(br2d,cor=TRUE)# AFE
summary(fit)# 
plot(fit,type="lines")
pfd<- fa(br2d,nfactors=4,rotate = "varimax")
pfd
pfd$loadings

#imputation
imp <- mice(br2m, seed=25109)# o nome da base e a seed sempre essa 25109
#stripplot(imp, pch=20, cex=1.2)# pra ver a imputação - se quiser - demora pra rodar
br2m <- complete(imp, 1)#sempre a m como  destino da mputação , empre esbrher a 1
# aí repetir as fatoriais
KMO(br2m)# 
cortest.bartlett(br2m) # 
nfactors(br2m, rotate = "varimax")
fit<-princomp(br2m,cor=TRUE)# AFE
summary(fit)# aqui olha o cumuçlative prop - razoavel 0.6 ou mais
plot(fit,type="lines")
pfm<- fa(br2m,nfactors=4,rotate = "varimax")
pfm
pfm$loadings


#onda 3

#listwise
KMO(br3d)# 
cortest.bartlett(br3d) #C002
nfactors(br3d, rotate = "varimax")
fit<-princomp(br3d,cor=TRUE)# AFE
summary(fit)# 
plot(fit,type="lines")
pfd<- fa(br3d,nfactors=4,rotate = "varimax")
pfd
pfd$loadings

#imputation
imp <- mice(br3m, seed=25109)# o nome da base e a seed sempre essa 25109
#stripplot(imp, pch=20, cex=1.2)# pra ver a imputação - se quiser - demora pra rodar
br3m <- complete(imp, 1)#sempre a m como  destino da mputação , empre esbrher a 1
# aí repetir as fatoriais
KMO(br3m)# 
cortest.bartlett(br3m) # ok
nfactors(br3m, rotate = "varimax")#sugere 4 ó
fit<-princomp(br3m,cor=TRUE)# AFE
summary(fit)# aqui olha o cumuçlative prop - razoavel 0.6 ou mais
plot(fit,type="lines")
pfm<- fa(br3m,nfactors=4,rotate = "varimax")
pfm
pfm$loadings


# onda 5

#listwise
KMO(br5d)# exceto C002
cortest.bartlett(br5d) #
nfactors(br5d, rotate = "varimax")
fit<-princomp(br5d,cor=TRUE)# AFE
summary(fit)# 
plot(fit,type="lines")
pfd<- fa(br5d,nfactors=4,rotate = "varimax")
pfd
pfd$loadings

#imputation
imp <- mice(br5m, seed=25109)# o nome da base e a seed sempre essa 25109
#stripplot(imp, pch=20, cex=1.2)# pra ver a imputação - se quiser - demora pra rodar
br5m <- complete(imp, 1)#sempre a m como  destino da mputação , empre esbrher a 1
# aí repetir as fatoriais
KMO(br5m)# exceto C002
cortest.bartlett(br5m) # 
nfactors(br5m, rotate = "varimax")
fit<-princomp(br5m,cor=TRUE)# AFE
summary(fit)# aqui olha o cumuçlative prop - razoavel 0.6 ou mais
plot(fit,type="lines")
pfm<- fa(br5m,nfactors=4,rotate = "varimax")
pfm
pfm$loadings

# onda 6

#listwise
KMO(br6d)# exceto E035
cortest.bartlett(br6d) #
nfactors(br6d, rotate = "varimax")
fit<-princomp(br6d,cor=TRUE)# AFE
summary(fit)# 
plot(fit,type="lines")
pfd<- fa(br6d,nfactors=4,rotate = "varimax")
pfd
pfd$loadings

#imputation
imp <- mice(br6m, seed=25109)# o nome da base e a seed sempre essa 25109
#stripplot(imp, pch=20, cex=1.2)# pra ver a imputação - se quiser - demora pra rodar
br6m <- complete(imp, 1)#sempre a m como  destino da mputação , empre esbrher a 1
# aí repetir as fatoriais
KMO(br6m)# 
cortest.bartlett(br6m) # exceto E035
nfactors(br6m, rotate = "varimax")
fit<-princomp(br6m,cor=TRUE)# AFE
summary(fit)# aqui olha o cumuçlative prop - razoavel 0.6 ou mais
plot(fit,type="lines")
pfm<- fa(br6m,nfactors=4,rotate = "varimax")
pfm
pfm$loadings

#onda 7

#listwise
KMO(br7d)# ok
cortest.bartlett(br7d) #
nfactors(br7d, rotate = "varimax")
fit<-princomp(br7d,cor=TRUE)# AFE
summary(fit)# 
plot(fit,type="lines")
pfd<- fa(br7d,nfactors=4,rotate = "varimax")
pfd
pfd$loadings

#imputation
imp <- mice(br7m, seed=25109)# o nome da base e a seed sempre essa 25109
#stripplot(imp, pch=20, cex=1.2)# pra ver a imputação - se quiser - demora pra rodar
br7m <- complete(imp, 1)#sempre a m como  destino da mputação , empre esbrher a 1
# aí repetir as fatoriais
KMO(br7m)# exceto C002
cortest.bartlett(br7m) # 
nfactors(br7m, rotate = "varimax")
fit<-princomp(br7m,cor=TRUE)# AFE
summary(fit)# aqui olha o cumuçlative prop - razoavel 0.6 ou mais
plot(fit,type="lines")
pfm<- fa(br7m,nfactors=4,rotate = "varimax")
pfm
pfm$loadings

