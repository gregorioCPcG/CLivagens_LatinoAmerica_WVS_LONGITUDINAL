#Haiti

library(tidyverse)
library(lavaan)
library(semPlot)
library(psych)
library(GPArotation)
library(labelled)
library(haven)
library("mice")
haiti6 <- read_csv("novapasta/haiti.csv")


#onda 6

haiti6d <- haiti6[,26:44]
summary(haiti6d)#PRA VER AS QUE NÃO FORAM FEITAS NESSA ONDA
haiti6m <- subset(haiti6d, select=-c(B006,F141,F144_02, E034))#PERGUNTAS NAO FEITAS
summary(haiti6m)# CONFERIR E REFAZER SE NECESSÁRIO, NAO TEVE TER 'NaN'
haiti6d <- subset(haiti6d, select=-c(B006,F141, F144_02,E034))%>% na.omit()
summary(haiti6d)


#listwise

KMO(haiti6d)# B008, C001, E018
cortest.bartlett(haiti6d) #
nfactors(haiti6d, rotate = "varimax")
fit<-princomp(haiti6d,cor=TRUE)# AFE
summary(fit)# variância ruim
plot(fit,type="lines")
pf6d<- fa(haiti6d,nfactors=4,rotate = "varimax")
pf6d
pf6d$loadings
D_haiti6 <- pf6d$loadings

#imputation

imp <- mice(haiti6m, seed=25109)# o nome da base e a seed sempre essa 25109
#stripplot(imp, pch=20, cex=1.2)# pra ver a imputação - se quiser - demora pra rodar
haiti6m <- complete(imp, 1)#sempre a m como  destino da mputação , empre eshaitiher a 1

# aí repetir as fatoriais

KMO(haiti6m)# varias problemáticas
cortest.bartlett(haiti6m) # 
nfactors(haiti6m, rotate = "varimax")
fit<-princomp(haiti6m,cor=TRUE)# AFE
summary(fit)# aqui olha o cumuçlative prop - razoavel 0.6 ou mais
plot(fit,type="lines")
pf6m<- fa(haiti6m,nfactors=4,rotate = "varimax")
pf6m
pf6m$loadings
M_haiti6m <- pf6m$loadings
