#Equador

library(tidyverse)
library(lavaan)
library(semPlot)
library(psych)
library(GPArotation)
library(labelled)
library(haven)
library("mice")

equador <- read_csv("novapasta/equador.csv")
#Quais ondas?
table(equador$S002VS) # 6 e 7

#equ6

#

equ06 <- equador %>%
  filter(S002VS == 6)
equ06d <- equ06[,26:44]
summary(equ06d)#PRA VER AS QUE NÃO FORAM FEITAS NESSA ONDA
equ06m <- subset(equ06d, select=-c(B006, F141, F144_02, E034))#PERGUNTAS NAO FEITAS
summary(equ06m)# CONFERIR E REFAZER SE NECESSÁRIO, NAO TEVE TER 'NaN'
equ06d <- subset(equ06d, select=-c(B006, F141, F144_02, E034))%>% na.omit()
summary(equ06d)


#listwise

KMO(equ06d)# todos acima de 0.5 (menos C002)
cortest.bartlett(equ06d) #
nfactors(equ06d, rotate = "varimax")
fit<-princomp(equ06d,cor=TRUE)# AFE
summary(fit)# variância ruim
plot(fit,type="lines")
pfa5<- fa(equ06d,nfactors=4,rotate = "varimax")
pfa5
pfa5$loadings
D_equ06 <- pfa5$loadings

#imputation

imp <- mice(equ06m, seed=25109)# o nome da base e a seed sempre essa 25109
#stripplot(imp, pch=20, cex=1.2)# pra ver a imputação - se quiser - demora pra rodar
equ06m <- complete(imp, 1)#sempre a m como  destino da mputação , empre esequher a 1

# aí repetir as fatoriais

KMO(equ06m)# todos acima de 0.5 (menos C002)
cortest.bartlett(equ06m) # 
nfactors(equ06m, rotate = "varimax")
fit<-princomp(equ06m,cor=TRUE)# AFE
summary(fit)# aqui olha o cumuçlative prop - razoavel 0.6 ou mais
plot(fit,type="lines")
pfequ<- fa(equ06m,nfactors=4,rotate = "varimax")
pfequ
pfequ$loadings
M_equ06m <- pfequ$loadings


#equ7

#

equ07 <- equador %>%
  filter(S002VS == 7)
equ07d <- equ07[,26:44]
summary(equ07d)#PRA VER AS QUE NÃO FORAM FEITAS NESSA ONDA
equ07m <- subset(equ07d, select=-c(B006, F141))#PERGUNTAS NAO FEITAS
summary(equ07m)# CONFERIR E REFAZER SE NECESSÁRIO, NAO TEVE TER 'NaN'
equ07d <- subset(equ07d, select=-c(B006, F141))%>% na.omit()
summary(equ07d)


#listwise

KMO(equ07d)# todos acima de 0.5 (menos C002)
cortest.bartlett(equ07d) #
nfactors(equ07d, rotate = "varimax")
fit<-princomp(equ07d,cor=TRUE)# AFE
summary(fit)# variância ruim
plot(fit,type="lines")
pfa5<- fa(equ07d,nfactors=4,rotate = "varimax")
pfa5
pfa5$loadings
D_equ07 <- pfa5$loadings

#imputation

imp <- mice(equ07m, seed=25109)# o nome da base e a seed sempre essa 25109
#stripplot(imp, pch=20, cex=1.2)# pra ver a imputação - se quiser - demora pra rodar
equ07m <- complete(imp, 1)#sempre a m como  destino da mputação , empre esequher a 1

# aí repetir as fatoriais

KMO(equ07m)# todos acima de 0.5 (menos C002)
cortest.bartlett(equ07m) # 
nfactors(equ07m, rotate = "varimax")
fit<-princomp(equ07m,cor=TRUE)# AFE
summary(fit)# aqui olha o cumuçlative prop - razoavel 0.6 ou mais
plot(fit,type="lines")
pfequ7<- fa(equ07m,nfactors=4,rotate = "varimax")
pfequ7
pfequ7$loadings
M_equ07m <- pfequ7$loadings

