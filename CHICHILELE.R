#chile
library(tidyverse)
library(lavaan)
library(semPlot)
library(psych)
library(GPArotation)
library("mice")
#baixar os bancos
library(haven)

merge <- read_sav("moreno_clivagem_merge.sav")
chi <- merge %>%
  filter(COUNTRY_ALPHA == "CHL")
CHILEobj <- summary(as.factor(chi$S002VS))
CHILEobj# todos o Lucas fez, menos o 4(terei q fazê-lo)
rm(chi, merge)

# #onda 2
ch2 <- read_sav("onda 2_89-93/moreno_clivagem_chile1990.sav")
ch2 <- remove_labels(ch2)
ch2m <- ch2[,26:42]
summary(ch2m)#verificar
ch2d <- ch2[,26:42]%>% na.omit()
summary(ch2d)#verificar

#onda3
ch3 <- read_sav("onda 3 _ 94-98/moreno_clivagem_chile1996.sav")
ch3 <- remove_labels(ch3)
ch3m <- ch3[,26:41]
summary(ch3m)#verificar
ch3d <- ch3[,26:41]%>% na.omit()
summary(ch3d)#verificar

#onda4 - Lucas nao fez, tive que fazer do início
merge <- read_sav("moreno_clivagem_merge.sav")
ch4 <- merge %>%
  filter(COUNTRY_ALPHA == "CHL")
ch4 <- ch4 %>%
  filter(S002VS == 4)
library(labelled)
ch4 <- remove_labels(ch4)
rm(merge)
ch4m <- ch4[,26:44]
summary(ch4m)#pra testar - ver quais são NAN
ch4m <- subset(ch4m, select=-c(B006, F141, F144_02))
summary(ch4m)#pra testar de novo
ch4d <-ch4m[,1:16]%>% na.omit()
summary(ch4d)#pra ver se deu certo

# onda 5
ch5 <- read_sav("onda 5 --- 2005-09/moreno_clivagem_chile2006.sav")
ch5 <- remove_labels(ch5)
ch5m <- ch5[,26:40]
summary(ch5m)#verificar
ch5d <- ch5[,26:40]%>% na.omit()
summary(ch5d)#verificar


# onda 6
ch6 <- read_sav("onda 6 --- 2010 -14/moreno_clivagem_chile2012.sav")
ch6 <- remove_labels(ch6)
ch6m <- ch6[,26:40]
summary(ch6m)#verificar
ch6d <- ch6[,26:40]%>% na.omit()
summary(ch6d)#verificar

# onda 7
ch7 <- read_sav("onda 7 - 2017-2022/moreno_clivagem_chile2018.sav")
ch7 <- remove_labels(ch7)
ch7m <- ch7[,26:42]
summary(ch7m)#verificar
ch7d <- ch7[,26:42]%>% na.omit()
summary(ch7d)#verificar

#
# analis.........................


#onda 2
#listwise
KMO(ch2d)# deu ruim para B006 e C001
cortest.bartlett(ch2d) #
nfactors(ch2d, rotate = "varimax")
fit<-princomp(ch2d,cor=TRUE)# AFE
summary(fit)# 
plot(fit,type="lines")
pfd<- fa(ch2d,nfactors=4,rotate = "varimax")
pfd
pfd$loadings

#imputation
imp <- mice(ch2m, seed=25109)# o nome da base e a seed sempre essa 25109
#stripplot(imp, pch=20, cex=1.2)# pra ver a imputação - se quiser - demora pra rodar
ch2m <- complete(imp, 1)#sempre a m como  destino da mputação , empre eschher a 1
# aí repetir as fatoriais
KMO(ch2m)# exceto B006
cortest.bartlett(ch2m) # 
nfactors(ch2m, rotate = "varimax")
fit<-princomp(ch2m,cor=TRUE)# AFE
summary(fit)# aqui olha o cumuçlative prop - razoavel 0.6 ou mais
plot(fit,type="lines")
pfm<- fa(ch2m,nfactors=4,rotate = "varimax")
pfm
pfm$loadings


#onda 3

#listwise
KMO(ch3d)# exceto #E035
cortest.bartlett(ch3d) 
nfactors(ch3d, rotate = "varimax")
fit<-princomp(ch3d,cor=TRUE)# AFE
summary(fit)# 
plot(fit,type="lines")
pfd<- fa(ch3d,nfactors=4,rotate = "varimax")
pfd
pfd$loadings

#imputation
imp <- mice(ch3m, seed=25109)# o nome da base e a seed sempre essa 25109
#stripplot(imp, pch=20, cex=1.2)# pra ver a imputação - se quiser - demora pra rodar
ch3m <- complete(imp, 1)#sempre a m como  destino da mputação , empre eschher a 1
# aí repetir as fatoriais
KMO(ch3m)# 
cortest.bartlett(ch3m) # ok
nfactors(ch3m, rotate = "varimax")#sugere 4 ó
fit<-princomp(ch3m,cor=TRUE)# AFE
summary(fit)# aqui olha o cumuçlative prop - razoavel 0.6 ou mais
plot(fit,type="lines")
pfm<- fa(ch3m,nfactors=4,rotate = "varimax")
pfm
pfm$loadings


# onda 4
#listwise
KMO(ch4d)# 
cortest.bartlett(ch4d) #C002
nfactors(ch4d, rotate = "varimax")
fit<-princomp(ch4d,cor=TRUE)# AFE
summary(fit)# 
plot(fit,type="lines")
pfd<- fa(ch4d,nfactors=4,rotate = "varimax")
pfd
pfd$loadings

#imputation
imp <- mice(ch4m, seed=25109)# o nome da base e a seed sempre essa 25109
#stripplot(imp, pch=20, cex=1.2)# pra ver a imputação - se quiser - demora pra rodar
ch4m <- complete(imp, 1)#sempre a m como  destino da mputação , empre eschher a 1
# aí repetir as fatoriais
KMO(ch4m)# 
cortest.bartlett(ch4m) # ok
nfactors(ch4m, rotate = "varimax")#sugere 4 ó
fit<-princomp(ch4m,cor=TRUE)# AFE
summary(fit)# aqui olha o cumuçlative prop - razoavel 0.6 ou mais
plot(fit,type="lines")
pfm<- fa(ch4m,nfactors=4,rotate = "varimax")
pfm
pfm$loadings

#

# onda 5

#listwise
KMO(ch5d)# exceto C002
cortest.bartlett(ch5d) #
nfactors(ch5d, rotate = "varimax")
fit<-princomp(ch5d,cor=TRUE)# AFE
summary(fit)# 
plot(fit,type="lines")
pfd<- fa(ch5d,nfactors=4,rotate = "varimax")
pfd
pfd$loadings

#imputation
imp <- mice(ch5m, seed=25109)# o nome da base e a seed sempre essa 25109
#stripplot(imp, pch=20, cex=1.2)# pra ver a imputação - se quiser - demora pra rodar
ch5m <- complete(imp, 1)#sempre a m como  destino da mputação , empre eschher a 1
# aí repetir as fatoriais
KMO(ch5m)# exceto C002
cortest.bartlett(ch5m) # 
nfactors(ch5m, rotate = "varimax")
fit<-princomp(ch5m,cor=TRUE)# AFE
summary(fit)# aqui olha o cumuçlative prop - razoavel 0.6 ou mais
plot(fit,type="lines")
pfm<- fa(ch5m,nfactors=4,rotate = "varimax")
pfm
pfm$loadings

# onda 6

#listwise
KMO(ch6d)# exceto E035
cortest.bartlett(ch6d) #
nfactors(ch6d, rotate = "varimax")
fit<-princomp(ch6d,cor=TRUE)# AFE
summary(fit)# 
plot(fit,type="lines")
pfd<- fa(ch6d,nfactors=4,rotate = "varimax")
pfd
pfd$loadings

#imputation
imp <- mice(ch6m, seed=25109)# o nome da base e a seed sempre essa 25109
#stripplot(imp, pch=20, cex=1.2)# pra ver a imputação - se quiser - demora pra rodar
ch6m <- complete(imp, 1)#sempre a m como  destino da mputação , empre eschher a 1
# aí repetir as fatoriais
KMO(ch6m)# 
cortest.bartlett(ch6m) # exceto E035
nfactors(ch6m, rotate = "varimax")
fit<-princomp(ch6m,cor=TRUE)# AFE
summary(fit)# aqui olha o cumuçlative prop - razoavel 0.6 ou mais
plot(fit,type="lines")
pfm<- fa(ch6m,nfactors=4,rotate = "varimax")
pfm
pfm$loadings

#onda 7

#listwise
KMO(ch7d)# ok
cortest.bartlett(ch7d) #
nfactors(ch7d, rotate = "varimax")
fit<-princomp(ch7d,cor=TRUE)# AFE
summary(fit)# 
plot(fit,type="lines")
pfd<- fa(ch7d,nfactors=4,rotate = "varimax")
pfd
pfd$loadings

#imputation
imp <- mice(ch7m, seed=25109)# o nome da base e a seed sempre essa 25109
#stripplot(imp, pch=20, cex=1.2)# pra ver a imputação - se quiser - demora pra rodar
ch7m <- complete(imp, 1)#sempre a m como  destino da mputação , empre eschher a 1
# aí repetir as fatoriais
KMO(ch7m)# exceto C002
cortest.bartlett(ch7m) # 
nfactors(ch7m, rotate = "varimax")
fit<-princomp(ch7m,cor=TRUE)# AFE
summary(fit)# aqui olha o cumuçlative prop - razoavel 0.6 ou mais
plot(fit,type="lines")
pfm<- fa(ch7m,nfactors=4,rotate = "varimax")
pfm
pfm$loadings
