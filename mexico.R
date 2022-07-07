# mexico

library(tidyverse)
library(lavaan)
library(semPlot)
library(psych)
library(GPArotation)
library(labelled)
library(haven)
library("mice")

mexico <- read_csv("novapasta/mexico.csv")
table(mexico$S002VS)#todas

# onda 1 

mex01 <- mexico %>%
  filter(S002VS == 1)

mex01d <- mex01[,26:44]

summary(mex01d)
mex01m <- subset(mex01d, select=-c(B006, B008, C001, C002, E035, E036, E039,
                                   F141,F144_02))
summary(mex01m)
mex01d <- subset(mex01d, select=-c(B006, B008, C001, C002, E035, E036, E039,
                                   F141,F144_02))%>% na.omit()


#listwise
KMO(mex01d)# todos acima de 0.5 (exceto E034)
pf01<- fa(mex01d,nfactors=4,rotate = "varimax")
pf01$loadings
# mice
imp <- mice(mex01m, seed=23109)
mex01m <- complete(imp, 1)
pf01.2<- fa(mex01m,nfactors=4,rotate = "varimax")
pf01.2$loadings


#onda 2 

mex02 <- mexico %>%
  filter(S002VS == 2)
mex02d <- mex02[,26:44]
summary(mex02d)
mex02m <- subset(mex02d, select=-c(B008,F144_02))
summary(mex02m)
mex02d <- subset(mex02d, select=-c(B008,F144_02))%>% na.omit()

#listwise
KMO(mex02d)# todos acima de 0.5- menos 1
pf02<- fa(mex02d,nfactors=4,rotate = "varimax")
pf02$loadings
# mice
imp <- mice(mex02m, seed=23109)
mex02m <- complete(imp, 1)
pf02.2<- fa(mex02m,nfactors=4,rotate = "varimax")
pf02.2$loadings



# onda 3
mex03 <- mexico %>%
  filter(S002VS == 3)
mex03d <- mex03[,26:44]
summary(mex03d)#pra conferir
mex03m <-subset(mex03d, select=-c(B006,F141,F144_02))
summary(mex03m)#pra conferir
mex03d <-subset(mex03d, select=-c(B006,F141,F144_02))%>% na.omit()

#

#listwise

KMO(mex03d)# todos acima de 0.5 (menos E036)
pfa3a<- fa(mex03d,nfactors=4,rotate = "varimax")

pfa3a$loadings

#imputation

imp <- mice(mex03m, seed=25109)# o nome da base e a seed sempre essa 25109
#stripplot(imp, pch=20, cex=1.2)# pra ver a imputação - se quiser - demora pra rodar
mex03m <- complete(imp, 1)#sempre a m como  destino da mputação , empre esmexher a 1

# aí repetir as fatoriais

KMO(mex03m)# todos acima de 0.5
pf333<- fa(mex03m,nfactors=4,rotate = "varimax")
pf333$loadings

# mex 4
#

mex04 <- mexico %>%
  filter(S002VS == 4)
mex04d <- mex04[,26:44]
summary(mex04d)#pra conferir
mex04m <-subset(mex04d, select=-c(B006,F141,F144_02))
summary(mex04m)#pra conferir
mex04d <-subset(mex04d, select=-c(B006,F141,F144_02))%>% na.omit()

#listwise

KMO(mex04d)# todos acima de 0.5 
pfa4a<- fa(mex04d,nfactors=4,rotate = "varimax")
pfa4a$loadings

#imputation

imp <- mice(mex04m, seed=25109)# o nome da base e a seed sempre essa 25109
#stripplot(imp, pch=20, cex=1.2)# pra ver a imputação - se quiser - demora pra rodar
mex04m <- complete(imp, 1)#sempre a m como  destino da mputação , empre esmexher a 1

# aí repetir as fatoriais

KMO(mex04m)# todos acima de 0.5
pf444<- fa(mex04m,nfactors=4,rotate = "varimax")
pf444$loadings


##
#mexico 5
mex05 <- mexico %>%
filter(S002VS == 5)
mex05d <- mex05[,26:44]
summary(mex05d)#PRA VER AS QUE NÃO FORAM FEITAS NESSA ONDA
mex05m <- subset(mex05d, select=-c(B006,F141
                                   ,F144_02, E034))#PERGUNTAS NAO FEITAS
summary(mex05m)# CONFERIR E REFAZER SE NECESSÁRIO, NAO TEVE TER 'NaN'
mex05d <- subset(mex05d, select=-c(B006,F141,
                                   F144_02,E034))%>% na.omit()
summary(mex05d)

#listwise
pfa5<- fa(mex05d,nfactors=4,rotate = "varimax")
pfa5$loadings

#imputation
imp <- mice(mex05m, seed=25109)# o nome da base e a seed sempre essa 25109
mex05m <- complete(imp, 1)#sempre a m como  destino da mputação , empre esmexher a 1
pf5<- fa(mex05m,nfactors=4,rotate = "varimax")
pf5$loadings


#
#
#mex6

#

mex06 <- mexico %>%
  filter(S002VS == 6)
mex06d <- mex06[,26:44]
summary(mex06d)#PRA VER AS QUE NÃO FORAM FEITAS NESSA ONDA
mex06m <- subset(mex06d, select=-c(B006, F141, F144_02, E034))#PERGUNTAS NAO FEITAS
summary(mex06m)# CONFERIR E REFAZER SE NECESSÁRIO, NAO TEVE TER 'NaN'
mex06d <- subset(mex06d, select=-c(B006, F141, F144_02, E034))%>% na.omit()
summary(mex06d)


#listwise

KMO(mex06d)# todos acima de 0.5 
pfa66x<- fa(mex06d,nfactors=4,rotate = "varimax")
pfa66x$loadings


#imputation

imp <- mice(mex06m, seed=25109)
mex06m <- complete(imp, 1)
pf6<- fa(mex06m,nfactors=4,rotate = "varimax")
pf6$loadings


# onda 7

#
#
mex07 <- mexico %>%
  filter(S002VS == 7)
mex07d <- mex07[,26:44]
summary(mex07d)#PRA VER AS QUE NÃO FORAM FEITAS NESSA ONDA
mex07m <- subset(mex07d, select=-c(B006, F141))#PERGUNTAS NAO FEITAS
summary(mex07m)# CONFERIR E REFAZER SE NECESSÁRIO, NAO TEVE TER 'NaN'
mex07d <- subset(mex07d, select=-c(B006, F141))%>% na.omit()
summary(mex07d)

#listwise
pfaxx<- fa(mex07d,nfactors=4,rotate = "varimax")
pfaxx$loadings

#imputation
imp <- mice(mex07m, seed=25109)#
mex07m <- complete(imp, 1)#
pf7m<- fa(mex07m,nfactors=4,rotate = "varimax")
pf7m$loadings

