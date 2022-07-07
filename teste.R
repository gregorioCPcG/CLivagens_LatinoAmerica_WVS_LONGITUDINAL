#fonte: https://www.youtube.com/watch?v=dmoWHRYAtcc
library(readr)
# ele começa com exploratory  #####
gcbs <- read_csv("data.csv")
gcbs <- subset(gcbs,(gender==1|gender==2),select=c(Q1:Q15,gender))
gcbs[gcbs==0] <- NA
summary(gcbs) # PARA VER              
#ele rodou deletion listwise
gcbs <-na.omit(gcbs)
library(mirt)
mod1 <- mirt(gcbs[,1:15],1,itemtype="graded")
# vamo fazer concomitante com uru03d
library(tidyverse)
uru03d <- read_csv("uru03d.csv")
mod1uru <- mirt(uru03d, 1, itemtype ="graded" )# itemtype é o modelo
# ler manual para entender melhor


# inspect mod1

coef(mod1)#limiares - interessante
coef(mod1uru)# aqui mudar poq q tem escalas diversas - no mod1 é tudo de 1 a 5

summary(mod1)
summary(mod1uru)


# second model

mod2 <- mirt(gcbs[,1:15],2)
coef(mod2)
summary(mod2)
summary(mod2, suppress=0.25)

mod2uru <- mirt(uru03d,2)
coef(mod2uru)
summary(mod2uru)
summary(mod2uru, suppress=0.39) # legal


# compare models (mod1 x mod2)

anova(mod1, mod2)#o dois é melhor - AIC, SABIC, etc...
anova(mod1uru, mod2uru)#idem

##############
# confirmatory
# é pra confirmar

fstruct1 <- 'F1 = 1,2,4,5,6,7,10,11,12,14,15
F2 = 3,8,9,13' # o cara do vídeo usou as mesmas de summary(mod2)
mod3 <-mirt(gcbs[,1:15],fstruct1)
summary(mod3)#nossa

fstruct1uru <- 'F1 = 9,10,11
F2 = 13,14,15' # usei as mesmas de summary(mod2uru)
mod3 <-mirt(uru03d,fstruct1uru)
summary(mod3)#nossa - deu confirmou

#######################
###### bi factor

fstruct2 <- c(2,2,3,2,2,2,2,3,3,2,2,2,3,2,2)#subdivide - igual ao de cima
mod4 <- bfactor(gcbs[,1:15], fstruct2)# nao sei como aplicar isso em uru3d
coef(mod4)
summary(mod4)
# uma opção por uruguai é remover as que nao deram 
# outra é colocar zero
# testar ambas, SE PRECISAR


# dif ##### incluindo o genero (uma variavel para correlac) #########

gcbs$gender2 <-ifelse(gcbs$gender==2,"F","M")
fstruct3<- mirt.model('G=1-15
F1=3,8,9,13') # olha que loco um modelo vem aí - # f1 é subfactor de G
# uru03d precisava de variáveis tipo sexo, se precisar esse aqui é o molde
# só seguir por esse caminho

#caso onedimensional 
# no exemplo
# mas da pra fazer pra cada factor

# não sei se é o caso descrever variáveis mais profundamente
# É S`Ó um EXEMPLO!!!!!!!!!!`
mg <-multipleGroup(gcbs[,1:15], fstruct3,
                   itemtype="graded",
                   group=gcbs$gender2,
                   invariance=c("Q1","Q2",
                                "Q3","free_means",
                                "free_var"),SE=TRUE)
dif <- DIF(mg, c('a1','a2','d1','d2','d3',
                 'd4'),p.adjust='BH',
           items2test=c(8:9,13),seq_stat=0.1)#demora pra carvalho, empacou no 67%, mas foi
dif
#o 8 e o 13 ok no p_value


# urudd3 4 factor comparar 

mod4uru <- mirt(uru03d,4)#exploratory - 
#demora pra um carvalho
summary(mod4uru)#comparar com fatorial listwise uru03(ver na pasta)
summary(mod4uru, suppress=0.3999)


# ver se rola pegar variavéis deles

#
#mod3uru <- mirt(uru03d,3)#exploratory com 3
anova(mod1uru, mod2uru, mod4uru)#icomparar os 4 modelos do uruguay
#
#confirmatory com 4: com base nas fatoriasi - outra ideia
#?

# uma alternativa - fazer com 1 construir - subfactores conforme as fatoriais dos países
#?


#scores - show de bola

a <- fscores(mod2uru)
a
uru03d$f1 <- a[,1]
uru03d$f2 <- a[,2]
cor.test(uru03d$f1, uru03d$f2)
plot(uru03d$f1, uru03d$f2)#ok nao é uma relação linear
#mas pode ser testada com outras variáveis
