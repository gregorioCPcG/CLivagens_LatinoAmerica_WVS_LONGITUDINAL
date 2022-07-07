library(tidyverse)
library(lavaan)
library(semPlot)
library(psych)
library(GPArotation)
library(labelled)
library(haven)

argentina <- read_csv("novapasta/argentina.csv") # 



# primeiro listwise deletions
table(argentina$S002VS)
# 1 base por onda

# onda 1 - recod

arg01 <- argentina %>%
  filter(S002VS == 1)

arg01d <- arg01[,26:44]

arg01m <- subset(arg01d, select=-c(B006, B008, C001, C002, E035, E036, E039,
                                   F141,F144_02))

arg01d <- subset(arg01d, select=-c(B006, B008, C001, C002, E035, E036, E039,
                                 F141,F144_02))%>% na.omit()
summary(arg01d)
# ver no arquivo questões.xlsx qual pergunta remover de qual onda


#onda 2 recod

arg02 <- argentina %>%
  filter(S002VS == 2)
arg02d <- arg02[,26:44]

arg02m <- subset(arg02d, select=-c(B008,F144_02))
arg02d <- subset(arg02d, select=-c(B008,F144_02))%>% na.omit()
summary(arg02d)

#onda 3 recod
# já tem

arg03 <- read_sav("onda 3 _ 94-98/moreno_clivagem_argentina1995.sav")
arg03 <- remove_labels(arg03)
arg03m <- arg03[,26:41]
arg03d <- arg03[,26:41]%>% na.omit()
summary(arg03d)

#onda 4

arg04 <- argentina %>%
  filter(S002VS == 4)
arg04d <- arg04[,26:44]
summary(arg04d)

arg04m <- subset(arg04d, select=-c(B006, F141,F144_02))
arg04d <- subset(arg04d, select=-c(B006, F141,F144_02))%>% na.omit()
summary(arg04d)

#onda 5
#já tem

arg05<- read_sav("onda 5 --- 2005-09/moreno_clivagem_argentina2006.sav")
arg05 <- remove_labels(arg05)
arg05m <- arg05[,26:40]
arg05d <- arg05[,26:40]%>% na.omit()
summary(arg05d)

#onda 6
#já tem
arg06 <- read_sav("onda 6 --- 2010 -14/moreno_clivagem_argentina2013.sav")
arg06 <- remove_labels(arg06)
arg06m <- arg06[,26:40]
arg06d <- arg06[,26:40]%>% na.omit()
summary(arg06d)

#onda 7
# já tem
arg07 <- read_sav("onda 7 - 2017-2022/moreno_clivagem_argentina2017.sav")
arg07 <- remove_labels(arg07)
arg07m <- arg07[,26:42]
arg07d <- arg07[,26:42]%>% na.omit()
summary(arg07d)

#########
#onda 1 factorial listwise deletion
# tem perguntas que nao foram elaboradas - cuidado

KMO(arg01d)# todos acima de 0.5
cortest.bartlett(arg01d) # nao roda
nfactors(arg01d, rotate = "varimax")
fit<-princomp(arg01d,cor=TRUE)# AFE
summary(fit)
plot(fit,type="lines")
pf1.2<- fa(arg01d,nfactors=4,rotate = "varimax")
pf1.2$values
pf1.2
pf1.2$loadings
Darg01 <- pf1.2$loadings

#imputation

imp <- mice(arg01m, seed=23109)# o nome da base e a seed sempre essa 23109
#stripplot(imp, pch=20, cex=1.2)# pra ver a imputação - se quiser - demora pra rodar
arg01m <- complete(imp, 1)#sempre a m como  destino da mputação , empre escolher a 1

# aí repetir as fatoriais

KMO(arg01m)# todos acima de 0.5
cortest.bartlett(arg01m) # nao roda
nfactors(arg01m, rotate = "varimax")
fit<-princomp(arg01m,cor=TRUE)# AFE
summary(fit)# aqui olha o cumuçlative prop - razoavel 0.6 ou mais
plot(fit,type="lines")
pf1.2<- fa(arg01m,nfactors=4,rotate = "varimax")
pf1.2$values
pf1.2
pf1.2$loadings
Marg01m <- pf1.2$loadings

# onda 2 factorial
KMO(arg02d)# todos acima de 0.5
cortest.bartlett(arg02d) # nao roda
nfactors(arg02d, rotate = "varimax")
fit<-princomp(arg02d,cor=TRUE)# AFE
summary(fit)# variância ruim
plot(fit,type="lines")
pf1.2<- fa(arg02d,nfactors=4,rotate = "varimax")
pf1.2
pf1.2$loadings
Darg02 <- pf1.2$loadings

#imputation

imp <- mice(arg02m, seed=23109)# o nome da base e a seed sempre essa 23109
#stripplot(imp, pch=20, cex=1.2)# pra ver a imputação - se quiser - demora pra rodar
arg02m <- complete(imp, 1)#sempre a m como  destino da mputação , empre escolher a 1

# aí repetir as fatoriais

KMO(arg02m)# todos acima de 0.5
cortest.bartlett(arg02m) # nao roda
nfactors(arg02m, rotate = "varimax")
fit<-princomp(arg02m,cor=TRUE)# AFE
summary(fit)# aqui olha o cumuçlative prop - razoavel 0.6 ou mais
plot(fit,type="lines")
pf1.2<- fa(arg02m,nfactors=4,rotate = "varimax")
pf1.2
pf1.2$loadings
Marg02m <- pf1.2$loadings


#onda 3 
#listwise

KMO(arg03d)# todos acima de 0.5
cortest.bartlett(arg03d) # nao roda
nfactors(arg03d, rotate = "varimax")
fit<-princomp(arg03d,cor=TRUE)# AFE
summary(fit)# variância ruim
plot(fit,type="lines")
pfa3<- fa(arg03d,nfactors=4,rotate = "varimax")
pfa3
pfa3$loadings
D_arg03 <- pfa3$loadings

#imputation

imp <- mice(arg03m, seed=23109)# o nome da base e a seed sempre essa 23109
#stripplot(imp, pch=20, cex=1.2)# pra ver a imputação - se quiser - demora pra rodar
arg03m <- complete(imp, 1)#sempre a m como  destino da mputação , empre escolher a 1

# aí repetir as fatoriais

KMO(arg03m)# todos acima de 0.5
cortest.bartlett(arg03m) # nao roda
nfactors(arg03m, rotate = "varimax")
fit<-princomp(arg03m,cor=TRUE)# AFE
summary(fit)# aqui olha o cumuçlative prop - razoavel 0.6 ou mais
plot(fit,type="lines")
pf3<- fa(arg03m,nfactors=4,rotate = "varimax")
pf3
pf3$loadings
M_arg03m <- pf3$loadings

#onda 4 
#listwise

KMO(arg04d)# todos acima de 0.5 (menos B008)
cortest.bartlett(arg04d) # nao roda
nfactors(arg04d, rotate = "varimax")
fit<-princomp(arg04d,cor=TRUE)# AFE
summary(fit)# variância ruim
plot(fit,type="lines")
pfa4<- fa(arg04d,nfactors=4,rotate = "varimax")
pfa4
pfa4$loadings
D_arg04 <- pfa4$loadings

#imputation

imp <- mice(arg04m, seed=24109)# o nome da base e a seed sempre essa 24109
#stripplot(imp, pch=20, cex=1.2)# pra ver a imputação - se quiser - demora pra rodar
arg04m <- complete(imp, 1)#sempre a m como  destino da mputação , empre escolher a 1

# aí repetir as fatoriais

KMO(arg04m)# todos acima de 0.5 (menas b008)
cortest.bartlett(arg04m) #
nfactors(arg04m, rotate = "varimax")
fit<-princomp(arg04m,cor=TRUE)# AFE
summary(fit)# aqui olha o cumuçlative prop - razoavel 0.6 ou mais
plot(fit,type="lines")
pf4<- fa(arg04m,nfactors=4,rotate = "varimax")
pf4
pf4$loadings
M_arg04m <- pf4$loadings

#
#onda 5 
#listwise

KMO(arg05d)# todos acima de 0.5 (menos e3036 e e039)
cortest.bartlett(arg05d) #
nfactors(arg05d, rotate = "varimax")
fit<-princomp(arg05d,cor=TRUE)# AFE
summary(fit)# variância ruim
plot(fit,type="lines")
pfa5<- fa(arg05d,nfactors=4,rotate = "varimax")
pfa5
pfa5$loadings
D_arg05 <- pfa5$loadings

#imputation

imp <- mice(arg05m, seed=25109)# o nome da base e a seed sempre essa 25109
#stripplot(imp, pch=20, cex=1.2)# pra ver a imputação - se quiser - demora pra rodar
arg05m <- complete(imp, 1)#sempre a m como  destino da mputação , empre escolher a 1

# aí repetir as fatoriais

KMO(arg05m)# todos acima de 0.5 (menos C002 e E036)
cortest.bartlett(arg05m) # 
nfactors(arg05m, rotate = "varimax")
fit<-princomp(arg05m,cor=TRUE)# AFE
summary(fit)# aqui olha o cumuçlative prop - razoavel 0.6 ou mais
plot(fit,type="lines")
pf5<- fa(arg05m,nfactors=4,rotate = "varimax")
pf5
pf5$loadings
M_arg05m <- pf5$loadings

#

#onda 6 
#listwise

KMO(arg06d)# todos acima de 0.5 (menas B008)
cortest.bartlett(arg06d) #
nfactors(arg06d, rotate = "varimax")
fit<-princomp(arg06d,cor=TRUE)# AFE
summary(fit)# variância ruim
plot(fit,type="lines")
pfa6<- fa(arg06d,nfactors=4,rotate = "varimax")
pfa6
pfa6$loadings
D_arg06 <- pfa6$loadings

#imputation

imp <- mice(arg06m, seed=26109)# o nome da base e a seed sempre essa 26109
#stripplot(imp, pch=20, cex=1.2)# pra ver a imputação - se quiser - demora pra rodar
arg06m <- complete(imp, 1)#sempre a m como  destino da mputação , empre escolher a 1

# aí repetir as fatoriais

KMO(arg06m)# todos acima de 0.5
cortest.bartlett(arg06m) # nao roda
nfactors(arg06m, rotate = "varimax")
fit<-princomp(arg06m,cor=TRUE)# AFE
summary(fit)# aqui olha o cumuçlative prop - razoavel 0.6 ou mais
plot(fit,type="lines")
pf6<- fa(arg06m,nfactors=4,rotate = "varimax")
pf6
pf6$loadings
M_arg06m <- pf6$loadings

#

#onda 7 
#listwise

KMO(arg07d)# todos acima de 0.5 (menas C002, E036)
cortest.bartlett(arg07d) #
nfactors(arg07d, rotate = "varimax")
fit<-princomp(arg07d,cor=TRUE)# AFE
summary(fit)# variância ruim
plot(fit,type="lines")
pfa7<- fa(arg07d,nfactors=4,rotate = "varimax")
pfa7
pfa7$loadings
D_arg07 <- pfa7$loadings

#imputation

imp <- mice(arg07m, seed=27109)# o nome da base e a seed sempre essa 27109
#stripplot(imp, pch=20, cex=1.2)# pra ver a imputação - se quiser - demora pra rodar
arg07m <- complete(imp, 1)#sempre a m como  destino da mputação , empre escolher a 1

# aí repetir as fatoriais

KMO(arg07m)# todos acima de 0.5(exceto, C002)
cortest.bartlett(arg07m) # nao roda
nfactors(arg07m, rotate = "varimax")
fit<-princomp(arg07m,cor=TRUE)# AFE
summary(fit)# aqui olha o cumuçlative prop - razoavel 0.7 ou mais
plot(fit,type="lines")
pf7<- fa(arg07m,nfactors=4,rotate = "varimax")
pf7
pf7$loadings
M_arg07m <- pf7$loadings