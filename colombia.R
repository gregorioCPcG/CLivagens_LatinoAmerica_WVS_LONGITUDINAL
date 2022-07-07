# Colombia
library(tidyverse)
library(lavaan)
library(semPlot)
library(psych)
library(GPArotation)
library(labelled)
library(haven)
library("mice")

Colombia <- read_csv("novapasta/Colombia.csv")
table(Colombia$S002VS) # 3, 5,6 e 7

#col3
# FOI FEITA EM DUAS ETAPAS (1997 E 1998) E AS QUESTÕES NAO FORAM REPETIDAS, NÃO SENDO APLICÁVEVL - NOTA DE RODAPÉ

# já vamo rodando tudo junto - modelo arquivo argentina

#col5

col05 <- Colombia %>%
  filter(S002VS == 5)
col05d <- col05[,26:44]
summary(col05d)#PRA VER AS QUE NÃO FORAM FEITAS NESSA ONDA
col05m <- subset(col05d, select=-c(C001, C002,B006,F141,F144_02, E034))#PERGUNTAS NAO FEITAS
summary(col05m)# CONFERIR E REFAZER SE NECESSÁRIO, NAO TEVE TER 'NaN'
col05d <- subset(col05d, select=-c(C001, C002,B006,F141, F144_02,E034))%>% na.omit()
summary(col05d)


#listwise

KMO(col05d)# todos acima de 0.5 (menos B008 e E035)
cortest.bartlett(col05d) #
nfactors(col05d, rotate = "varimax")
fit<-princomp(col05d,cor=TRUE)# AFE
summary(fit)# variância ruim
plot(fit,type="lines")
pfa5<- fa(col05d,nfactors=4,rotate = "varimax")
pfa5
pfa5$loadings
D_col05 <- pfa5$loadings

#imputation

imp <- mice(col05m, seed=25109)# o nome da base e a seed sempre essa 25109
#stripplot(imp, pch=20, cex=1.2)# pra ver a imputação - se quiser - demora pra rodar
col05m <- complete(imp, 1)#sempre a m como  destino da mputação , empre escolher a 1

# aí repetir as fatoriais

KMO(col05m)# todos acima de 0.5 (menos B008 e E035)
cortest.bartlett(col05m) # 
nfactors(col05m, rotate = "varimax")
fit<-princomp(col05m,cor=TRUE)# AFE
summary(fit)# aqui olha o cumuçlative prop - razoavel 0.6 ou mais
plot(fit,type="lines")
pf5<- fa(col05m,nfactors=4,rotate = "varimax")
pf5
pf5$loadings
M_col05m <- pf5$loadings

#

#col6

#

col06 <- Colombia %>%
  filter(S002VS == 6)
col06d <- col06[,26:44]
summary(col06d)#PRA VER AS QUE NÃO FORAM FEITAS NESSA ONDA
col06m <- subset(col06d, select=-c(B006, F141, F144_02, E034))#PERGUNTAS NAO FEITAS
summary(col06m)# CONFERIR E REFAZER SE NECESSÁRIO, NAO TEVE TER 'NaN'
col06d <- subset(col06d, select=-c(B006, F141, F144_02, E034))%>% na.omit()
summary(col06d)


#listwise

KMO(col06d)# todos acima de 0.5 (menos C002)
cortest.bartlett(col06d) #
nfactors(col06d, rotate = "varimax")
fit<-princomp(col06d,cor=TRUE)# AFE
summary(fit)# variância ruim
plot(fit,type="lines")
pfa5<- fa(col06d,nfactors=4,rotate = "varimax")
pfa5
pfa5$loadings
D_col06 <- pfa5$loadings

#imputation

imp <- mice(col06m, seed=25109)# o nome da base e a seed sempre essa 25109
#stripplot(imp, pch=20, cex=1.2)# pra ver a imputação - se quiser - demora pra rodar
col06m <- complete(imp, 1)#sempre a m como  destino da mputação , empre escolher a 1

# aí repetir as fatoriais

KMO(col06m)# todos acima de 0.5 (menos C002)
cortest.bartlett(col06m) # 
nfactors(col06m, rotate = "varimax")
fit<-princomp(col06m,cor=TRUE)# AFE
summary(fit)# aqui olha o cumuçlative prop - razoavel 0.6 ou mais
plot(fit,type="lines")
pf5<- fa(col06m,nfactors=4,rotate = "varimax")
pf5
pf5$loadings
M_col06m <- pf5$loadings


#col7

#

col07 <- Colombia %>%
  filter(S002VS == 7)
col07d <- col07[,26:44]
summary(col07d)#PRA VER AS QUE NÃO FORAM FEITAS NESSA ONDA
col07m <- subset(col07d, select=-c(B006, F141))#PERGUNTAS NAO FEITAS
summary(col07m)# CONFERIR E REFAZER SE NECESSÁRIO, NAO TEVE TER 'NaN'
col07d <- subset(col07d, select=-c(B006, F141))%>% na.omit()
summary(col07d)


#listwise

KMO(col07d)# todos acima de 0.5 (menos C002)
cortest.bartlett(col07d) #
nfactors(col07d, rotate = "varimax")
fit<-princomp(col07d,cor=TRUE)# AFE
summary(fit)# variância ruim
plot(fit,type="lines")
pfa5<- fa(col07d,nfactors=4,rotate = "varimax")
pfa5
pfa5$loadings
D_col07 <- pfa5$loadings

#imputation

imp <- mice(col07m, seed=25109)# o nome da base e a seed sempre essa 25109
#stripplot(imp, pch=20, cex=1.2)# pra ver a imputação - se quiser - demora pra rodar
col07m <- complete(imp, 1)#sempre a m como  destino da mputação , empre escolher a 1

# aí repetir as fatoriais

KMO(col07m)# todos acima de 0.5 (menos C002)
cortest.bartlett(col07m) # 
nfactors(col07m, rotate = "varimax")
fit<-princomp(col07m,cor=TRUE)# AFE
summary(fit)# aqui olha o cumuçlative prop - razoavel 0.6 ou mais
plot(fit,type="lines")
pf5<- fa(col07m,nfactors=4,rotate = "varimax")
pf5
pf5$loadings
M_col07m <- pf5$loadings
