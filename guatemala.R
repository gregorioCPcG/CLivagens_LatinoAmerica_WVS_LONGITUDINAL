#guatemala

library(tidyverse)
library(lavaan)
library(semPlot)
library(psych)
library(GPArotation)
library(labelled)
library(haven)
library("mice")

guatemala <- read_csv("novapasta/guatemala.csv")
table(guatemala$S002VS) # 5 e 7


#guat5

guat05 <- guatemala %>%
  filter(S002VS == 5)
guat05d <- guat05[,26:44]
summary(guat05d)#PRA VER AS QUE NÃO FORAM FEITAS NESSA ONDA
guat05m <- subset(guat05d, select=-c(B006,F141,F144_02, E034))#PERGUNTAS NAO FEITAS
summary(guat05m)# CONFERIR E REFAZER SE NECESSÁRIO, NAO TEVE TER 'NaN'
guat05d <- subset(guat05d, select=-c(B006,F141, F144_02,E034))%>% na.omit()
summary(guat05d)


#listwise

KMO(guat05d)# todos acima de 0.5 (menos E036)
cortest.bartlett(guat05d) #
nfactors(guat05d, rotate = "varimax")
fit<-princomp(guat05d,cor=TRUE)# AFE
summary(fit)# variância ruim
plot(fit,type="lines")
pfa5d<- fa(guat05d,nfactors=4,rotate = "varimax")
pfa5d
pfa5d$loadings
D_guat05 <- pfa5d$loadings

#imputation

imp <- mice(guat05m, seed=25109)# o nome da base e a seed sempre essa 25109
#stripplot(imp, pch=20, cex=1.2)# pra ver a imputação - se quiser - demora pra rodar
guat05m <- complete(imp, 1)#sempre a m como  destino da mputação , empre esguather a 1

# aí repetir as fatoriais

KMO(guat05m)# todos acima de 0.5 (menos B008 e E035)
cortest.bartlett(guat05m) # 
nfactors(guat05m, rotate = "varimax")
fit<-princomp(guat05m,cor=TRUE)# AFE
summary(fit)# aqui olha o cumuçlative prop - razoavel 0.6 ou mais
plot(fit,type="lines")
pf5m<- fa(guat05m,nfactors=4,rotate = "varimax")
pf5m
pf5m$loadings
M_guat05m <- pf5m$loadings

#
rm(pf5m, pfa5d, guat05, guat05d, guat05m, imp, fit)

#guat7

#

guat07 <- guatemala %>%
  filter(S002VS == 7)
guat07d <- guat07[,26:44]
summary(guat07d)#PRA VER AS QUE NÃO FORAM FEITAS NESSA ONDA
guat07m <- subset(guat07d, select=-c(B006, F141))#PERGUNTAS NAO FEITAS
summary(guat07m)# CONFERIR E REFAZER SE NECESSÁRIO, NAO TEVE TER 'NaN'
guat07d <- subset(guat07d, select=-c(B006, F141))%>% na.omit()
summary(guat07d)


#listwise

KMO(guat07d)# todos acima de 0.5 (menos C002)
cortest.bartlett(guat07d) #
nfactors(guat07d, rotate = "varimax")
fit<-princomp(guat07d,cor=TRUE)# AFE
summary(fit)# variância ruim
plot(fit,type="lines")
pfa7.m<- fa(guat07d,nfactors=4,rotate = "varimax")
pfa7.m
pfa7.m$loadings
D_guat07 <- pfa7.m$loadings

#imputation

imp <- mice(guat07m, seed=25109)# o nome da base e a seed sempre essa 25109
#stripplot(imp, pch=20, cex=1.2)# pra ver a imputação - se quiser - demora pra rodar
guat07m <- complete(imp, 1)#sempre a m como  destino da mputação , empre esguather a 1
summary(guat07m)
# aí repetir as fatoriais

KMO(guat07m)# todos acima de 0.5 (menos C002)
cortest.bartlett(guat07m) # 
nfactors(guat07m, rotate = "varimax")
fit<-princomp(guat07m,cor=TRUE)# AFE
summary(fit)# aqui olha o cumuçlative prop - razoavel 0.6 ou mais
plot(fit,type="lines")
pf7<- fa(guat07m,nfactors=4,rotate = "varimax")
pf7
pf7$loadings
M_guat07m <- pf7$loadings

