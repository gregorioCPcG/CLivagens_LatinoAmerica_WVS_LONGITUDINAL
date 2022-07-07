# MANIPULAÇÕES PARA OBTER BASES LIMPAS PARA ANÁLISES POSTERIORES - REGRESSÕES ETC

# ONDA 1 
library(labelled)
library(haven)
library(mice)
#arg
argentina <- read_csv("novapasta/argentina.csv")
arg01 <- argentina %>%
  filter(S002VS == 1)
arg01 <-  subset(arg01,select=c(B006, B008, C001, C002, E018, E034,
                                 E035, E036, E039, F028, F116, F118, F120,
                                 F121,F141,F144_02,G006,X001,X003,X025R,X028,
                                X049,X051,Y001,E033,E179WVS,E182))
arg01m <- subset(arg01, select=-c(B006, B008, C001, C002, E035, E036, E039,
                                  F141,F144_02))  
arg01d <- subset(arg01, select=-c(B006, B008, C001, C002, E035, E036, E039,
                                   F141,F144_02))  
arg01d <- arg01d[!(is.na(arg01d$E018)), ]
arg01d <- arg01d[!(is.na(arg01d$E034)), ]
arg01d <- arg01d[!(is.na(arg01d$F028)), ]
arg01d <- arg01d[!(is.na(arg01d$F116)), ]
arg01d <- arg01d[!(is.na(arg01d$F118)), ]
arg01d <- arg01d[!(is.na(arg01d$F120)), ]
arg01d <- arg01d[!(is.na(arg01d$F121)), ]
arg01d <- arg01d[!(is.na(arg01d$G006)), ]
summary(arg01d)#pra conferir
# write.csv
write.csv(arg01d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/arg01d.csv",
          row.names = FALSE)
write.csv(arg01m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/arg01m.csv",
          row.names = FALSE)

#mex

mexico <- read_csv("novapasta/mexico.csv")
mex01 <- mexico %>%
  filter(S002VS == 1)
mex01 <-  subset(mex01,select=c(B006, B008, C001, C002, E018, E034,
                                E035, E036, E039, F028, F116, F118, F120,
                                F121,F141,F144_02,G006,X001,X003,X025R,X028,
                                X049,X051,Y001,E033,E179WVS,E182))
mex01m <- subset(mex01, select=-c(B006, B008, C001, C002, E035, E036, E039,
                                  F141,F144_02))  
mex01d <- subset(mex01, select=-c(B006, B008, C001, C002, E035, E036, E039,
                                  F141,F144_02))  
mex01d <- mex01d[!(is.na(mex01d$E018)), ]
mex01d <- mex01d[!(is.na(mex01d$E034)), ]
mex01d <- mex01d[!(is.na(mex01d$F028)), ]
mex01d <- mex01d[!(is.na(mex01d$F116)), ]
mex01d <- mex01d[!(is.na(mex01d$F118)), ]
mex01d <- mex01d[!(is.na(mex01d$F120)), ]
mex01d <- mex01d[!(is.na(mex01d$F121)), ]
mex01d <- mex01d[!(is.na(mex01d$G006)), ]
summary(mex01d)#pra conferir
# write.csv
write.csv(mex01d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/mex01d.csv",
          row.names = FALSE)
write.csv(mex01m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/mex01m.csv",
          row.names = FALSE)
#
#ONDA 2
#
#ARG
arg02 <- argentina %>%
  filter(S002VS == 2)
arg02 <- subset(arg02, select=c(B006, B008, C001, C002, E018, E034,
                                E035, E036, E039, F028, F116, F118,
                                F120, F121,F141,F144_02,G006,
                                X001,X003, X025R,X028,X049,X051,
                                Y001,E033,E179WVS,E182))
arg02m <- subset(arg02, select=-c(B008,F144_02))
arg02d <- subset(arg02, select=-c(B008,F144_02))
arg02d <- arg02d[!(is.na(arg02d$B006)), ]
arg02d <- arg02d[!(is.na(arg02d$C001)), ]
arg02d <- arg02d[!(is.na(arg02d$C002)), ]
arg02d <- arg02d[!(is.na(arg02d$E018)), ]
arg02d <- arg02d[!(is.na(arg02d$E034)), ]
arg02d <- arg02d[!(is.na(arg02d$E035)), ]
arg02d <- arg02d[!(is.na(arg02d$E039)), ]
arg02d <- arg02d[!(is.na(arg02d$F028)), ]
arg02d <- arg02d[!(is.na(arg02d$F116)), ]
arg02d <- arg02d[!(is.na(arg02d$G006)), ]
arg02d <- arg02d[!(is.na(arg02d$F118)), ]
arg02d <- arg02d[!(is.na(arg02d$F120)), ]
arg02d <- arg02d[!(is.na(arg02d$F121)), ]
arg02d <- arg02d[!(is.na(arg02d$F141)), ]
arg02d <- arg02d[!(is.na(arg02d$E036)), ]
summary(arg02d)#pra conferir
write.csv(arg02d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/arg02d.csv",
          row.names = FALSE)
write.csv(arg02m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/arg02m.csv",
          row.names = FALSE)
#mex
mex02 <- mexico %>%
  filter(S002VS == 2)
mex02 <- subset(mex02, select=c(B006, B008, C001, C002, E018, E034,
                                E035, E036, E039, F028, F116, F118,
                                F120, F121,F141,F144_02,G006,
                                X001,X003, X025R,X028,X049,X051,
                                Y001,E033,E179WVS,E182))
mex02m <- subset(mex02, select=-c(B008,F144_02))
mex02d <- subset(mex02, select=-c(B008,F144_02))
mex02d <- mex02d[!(is.na(mex02d$B006)), ]
mex02d <- mex02d[!(is.na(mex02d$C001)), ]
mex02d <- mex02d[!(is.na(mex02d$C002)), ]
mex02d <- mex02d[!(is.na(mex02d$E018)), ]
mex02d <- mex02d[!(is.na(mex02d$E034)), ]
mex02d <- mex02d[!(is.na(mex02d$E035)), ]
mex02d <- mex02d[!(is.na(mex02d$E039)), ]
mex02d <- mex02d[!(is.na(mex02d$F028)), ]
mex02d <- mex02d[!(is.na(mex02d$F116)), ]
mex02d <- mex02d[!(is.na(mex02d$G006)), ]
mex02d <- mex02d[!(is.na(mex02d$F118)), ]
mex02d <- mex02d[!(is.na(mex02d$F120)), ]
mex02d <- mex02d[!(is.na(mex02d$F121)), ]
mex02d <- mex02d[!(is.na(mex02d$F141)), ]
mex02d <- mex02d[!(is.na(mex02d$E036)), ]
summary(mex02d)#pra conferir
write.csv(mex02d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/mex02d.csv",
          row.names = FALSE)
write.csv(mex02m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/mex02m.csv",
          row.names = FALSE)
#Bra
br2 <- read_sav("onda 2_89-93/moreno_clivagem_brasil1991.sav")
br2 <- remove_labels(br2)
bra02 <- subset(br2, select=c(B006, C001, C002, E018, E034,
                                E035, E036, E039, F028, F116, F118,
                                F120, F121,F141,G006,
                                X001,X003, X025R,X028,X049,X051,
                                Y001,E033,E179WVS))
bra02m <- bra02
bra02d <- bra02
bra02d <- bra02d[!(is.na(bra02d$B006)), ]
bra02d <- bra02d[!(is.na(bra02d$C001)), ]
bra02d <- bra02d[!(is.na(bra02d$C002)), ]
bra02d <- bra02d[!(is.na(bra02d$E018)), ]
bra02d <- bra02d[!(is.na(bra02d$E034)), ]
bra02d <- bra02d[!(is.na(bra02d$E035)), ]
bra02d <- bra02d[!(is.na(bra02d$E039)), ]
bra02d <- bra02d[!(is.na(bra02d$F028)), ]
bra02d <- bra02d[!(is.na(bra02d$F116)), ]
bra02d <- bra02d[!(is.na(bra02d$G006)), ]
bra02d <- bra02d[!(is.na(bra02d$F118)), ]
bra02d <- bra02d[!(is.na(bra02d$F120)), ]
bra02d <- bra02d[!(is.na(bra02d$F121)), ]
bra02d <- bra02d[!(is.na(bra02d$F141)), ]
bra02d <- bra02d[!(is.na(bra02d$E036)), ]
summary(bra02d)#pra conferir
write.csv(bra02d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/bra02d.csv",
          row.names = FALSE)
write.csv(bra02m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/bra02m.csv",
          row.names = FALSE)
# CHILE
ch2 <- read_sav("onda 2_89-93/moreno_clivagem_chile1990.sav")
ch2 <- remove_labels(ch2)
chi02 <- subset(ch2, select=c(B006, C001, C002, E018, E034,
                               E035, E036, E039, F028, F116, F118,
                               F120, F121,F141,G006,
                               X001,X003,X028,X049,X051,
                               Y001,E033,E179WVS))
chi02m <- chi02
chi02d <- chi02
chi02d <- chi02d[!(is.na(chi02d$B006)), ]
chi02d <- chi02d[!(is.na(chi02d$C001)), ]
chi02d <- chi02d[!(is.na(chi02d$C002)), ]
chi02d <- chi02d[!(is.na(chi02d$E018)), ]
chi02d <- chi02d[!(is.na(chi02d$E034)), ]
chi02d <- chi02d[!(is.na(chi02d$E035)), ]
chi02d <- chi02d[!(is.na(chi02d$E039)), ]
chi02d <- chi02d[!(is.na(chi02d$F028)), ]
chi02d <- chi02d[!(is.na(chi02d$F116)), ]
chi02d <- chi02d[!(is.na(chi02d$G006)), ]
chi02d <- chi02d[!(is.na(chi02d$F118)), ]
chi02d <- chi02d[!(is.na(chi02d$F120)), ]
chi02d <- chi02d[!(is.na(chi02d$F121)), ]
chi02d <- chi02d[!(is.na(chi02d$F141)), ]
chi02d <- chi02d[!(is.na(chi02d$E036)), ]
summary(chi02d)#pra conferir
write.csv(chi02d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/chi02d.csv",
          row.names = FALSE)
write.csv(chi02m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/chi02m.csv",
          row.names = FALSE)

# ONDA 3
arg03 <- read_sav("onda 3 _ 94-98/moreno_clivagem_argentina1995.sav")
arg03 <- remove_labels(arg03)
arg03m <- subset(arg03,select=c(B008, C001, C002,
                                E018, E034, E035, E036,
                                E039, F028, F116, F118,
                                F120, F121,G006,
                                X001,X003, X025R,X028,
                                Y001,E033,
                                E179WVS,E182))
arg03d <- arg03m
arg03d <- arg03d[!(is.na(arg03d$B008)), ]
arg03d <- arg03d[!(is.na(arg03d$C001)), ]
arg03d <- arg03d[!(is.na(arg03d$C002)), ]
arg03d <- arg03d[!(is.na(arg03d$E018)), ]
arg03d <- arg03d[!(is.na(arg03d$E034)), ]
arg03d <- arg03d[!(is.na(arg03d$E035)), ]
arg03d <- arg03d[!(is.na(arg03d$E036)), ]
arg03d <- arg03d[!(is.na(arg03d$E039)), ]
arg03d <- arg03d[!(is.na(arg03d$F028)), ]
arg03d <- arg03d[!(is.na(arg03d$F116)), ]
arg03d <- arg03d[!(is.na(arg03d$F118)), ]
arg03d <- arg03d[!(is.na(arg03d$F120)), ]
arg03d <- arg03d[!(is.na(arg03d$F121)), ]
arg03d <- arg03d[!(is.na(arg03d$G006)), ]
summary(arg03d)
#
write.csv(arg03d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/arg03d.csv",
          row.names = FALSE)
write.csv(arg03m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/arg03m.csv",
          row.names = FALSE)
#BRA
br03 <- read_sav("onda 3 _ 94-98/moreno_clivagem_brasil1997.sav")
br03 <- remove_labels(br03)
bra03m <- subset(br03,select=c(B008, C001, C002,
                                E018, E034, E035, E036,
                                E039, F028, F116, F118,
                                F120, F121,G006,
                                X001,X003, X025R,X028,
                                Y001,E033,
                                E179WVS,E182))
bra03d <- bra03m
bra03d <- bra03d[!(is.na(bra03d$B008)), ]
bra03d <- bra03d[!(is.na(bra03d$C001)), ]
bra03d <- bra03d[!(is.na(bra03d$C002)), ]
bra03d <- bra03d[!(is.na(bra03d$E018)), ]
bra03d <- bra03d[!(is.na(bra03d$E034)), ]
bra03d <- bra03d[!(is.na(bra03d$E035)), ]
bra03d <- bra03d[!(is.na(bra03d$E036)), ]
bra03d <- bra03d[!(is.na(bra03d$E039)), ]
bra03d <- bra03d[!(is.na(bra03d$F028)), ]
bra03d <- bra03d[!(is.na(bra03d$F116)), ]
bra03d <- bra03d[!(is.na(bra03d$F118)), ]
bra03d <- bra03d[!(is.na(bra03d$F120)), ]
bra03d <- bra03d[!(is.na(bra03d$F121)), ]
bra03d <- bra03d[!(is.na(bra03d$G006)), ]
summary(bra03d)
#
write.csv(bra03d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/bra03d.csv",
          row.names = FALSE)
write.csv(bra03m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/bra03m.csv",
          row.names = FALSE)
#
#CHILE
ch3 <- read_sav("onda 3 _ 94-98/moreno_clivagem_chile1996.sav")
ch3 <- remove_labels(ch3)
ch03m <- subset(ch3,select=c(B008, C001, C002,
                              E018, E034, E035, E036,
                              E039, F028, F116, F118,
                              F120, F121,G006,
                              X001,X003, X025R,X028,
                              Y001,E033,
                              E179WVS,E182))
ch03d <- ch03m
ch03d <- ch03d[!(is.na(ch03d$B008)), ]
ch03d <- ch03d[!(is.na(ch03d$C001)), ]
ch03d <- ch03d[!(is.na(ch03d$C002)), ]
ch03d <- ch03d[!(is.na(ch03d$E018)), ]
ch03d <- ch03d[!(is.na(ch03d$E034)), ]
ch03d <- ch03d[!(is.na(ch03d$E035)), ]
ch03d <- ch03d[!(is.na(ch03d$E036)), ]
ch03d <- ch03d[!(is.na(ch03d$E039)), ]
ch03d <- ch03d[!(is.na(ch03d$F028)), ]
ch03d <- ch03d[!(is.na(ch03d$F116)), ]
ch03d <- ch03d[!(is.na(ch03d$F118)), ]
ch03d <- ch03d[!(is.na(ch03d$F120)), ]
ch03d <- ch03d[!(is.na(ch03d$F121)), ]
ch03d <- ch03d[!(is.na(ch03d$G006)), ]
summary(ch03d)
write.csv(ch03d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/chi03d.csv",
          row.names = FALSE)
write.csv(ch03m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/chi03m.csv",
          row.names = FALSE)
# mexico
mexico <- read_csv("novapasta/mexico.csv")
mex03 <- mexico %>%
  filter(S002VS == 3)
mex03 <- subset(mex03, select=c(B006, B008, C001,
                                C002, E018, E034,
                                E035, E036, E039,
                                F028, F116, F118,
                                F120, F121,F141,
                                F144_02,G006,X001,
                                X003, X025R,X028,
                                X049,X051,
                                Y001,E033,
                                E179WVS,E182))
mex03m <-subset(mex03, select=-c(B006,F141,F144_02))
mex03d <-subset(mex03, select=-c(B006,F141,F144_02))
mex03d <- mex03d[!(is.na(mex03d$B008)), ]
mex03d <- mex03d[!(is.na(mex03d$C001)), ]
mex03d <- mex03d[!(is.na(mex03d$C002)), ]
mex03d <- mex03d[!(is.na(mex03d$E018)), ]
mex03d <- mex03d[!(is.na(mex03d$E034)), ]
mex03d <- mex03d[!(is.na(mex03d$E035)), ]
mex03d <- mex03d[!(is.na(mex03d$E036)), ]
mex03d <- mex03d[!(is.na(mex03d$E039)), ]
mex03d <- mex03d[!(is.na(mex03d$F028)), ]
mex03d <- mex03d[!(is.na(mex03d$F116)), ]
mex03d <- mex03d[!(is.na(mex03d$F118)), ]
mex03d <- mex03d[!(is.na(mex03d$F120)), ]
mex03d <- mex03d[!(is.na(mex03d$F121)), ]
mex03d <- mex03d[!(is.na(mex03d$G006)), ]
summary(mex03d)
write.csv(mex03d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/mex03d.csv",
          row.names = FALSE)
write.csv(mex03m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/mex03m.csv",
          row.names = FALSE)
#
Peru <- read_csv("novapasta/Peru.csv")
peru03 <- Peru %>%
  filter(S002VS == 3)
peru03 <- subset(peru03, select=c(B006, B008, C001,
                                  C002, E018, E034,
                                  E035, E036, E039,
                                  F028, F116, F118,
                                  F120, F121,F141,
                                  F144_02,G006,X001,
                                  X003, X025R,X028,
                                  X049,X051,
                                  Y001,E033,
                                  E179WVS,E182))
peru03m <-subset(peru03, select=-c(B006,F141,F144_02))
peru03d <-subset(peru03, select=-c(B006,F141,F144_02))
peru03d <- peru03d[!(is.na(peru03d$B008)), ]
peru03d <- peru03d[!(is.na(peru03d$C001)), ]
peru03d <- peru03d[!(is.na(peru03d$C002)), ]
peru03d <- peru03d[!(is.na(peru03d$E018)), ]
peru03d <- peru03d[!(is.na(peru03d$E034)), ]
peru03d <- peru03d[!(is.na(peru03d$E035)), ]
peru03d <- peru03d[!(is.na(peru03d$E036)), ]
peru03d <- peru03d[!(is.na(peru03d$E039)), ]
peru03d <- peru03d[!(is.na(peru03d$F028)), ]
peru03d <- peru03d[!(is.na(peru03d$F116)), ]
peru03d <- peru03d[!(is.na(peru03d$F118)), ]
peru03d <- peru03d[!(is.na(peru03d$F120)), ]
peru03d <- peru03d[!(is.na(peru03d$F121)), ]
peru03d <- peru03d[!(is.na(peru03d$G006)), ]
summary(peru03d)
write.csv(peru03d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/peru03d.csv",
          row.names = FALSE)
write.csv(peru03m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/peru03m.csv",
          row.names = FALSE)
# Uruguay
uru03 <- read_sav("onda 3 _ 94-98/moreno_clivagem_uruguai1996.sav")
uru03 <- remove_labels(uru03)
uru03m <- subset(uru03,select=c(B008, C001, C002,
                              E018, E034, E035, E036,
                              E039, F028, F116, F118,
                              F120, F121,G006,
                              X001,X003, X025R,X028,
                              Y001,E033,
                              E179WVS,E182))
uru03d <- uru03m
uru03d <- uru03d[!(is.na(uru03d$B008)), ]
uru03d <- uru03d[!(is.na(uru03d$C001)), ]
uru03d <- uru03d[!(is.na(uru03d$C002)), ]
uru03d <- uru03d[!(is.na(uru03d$E018)), ]
uru03d <- uru03d[!(is.na(uru03d$E034)), ]
uru03d <- uru03d[!(is.na(uru03d$E035)), ]
uru03d <- uru03d[!(is.na(uru03d$E036)), ]
uru03d <- uru03d[!(is.na(uru03d$E039)), ]
uru03d <- uru03d[!(is.na(uru03d$F028)), ]
uru03d <- uru03d[!(is.na(uru03d$F116)), ]
uru03d <- uru03d[!(is.na(uru03d$F118)), ]
uru03d <- uru03d[!(is.na(uru03d$F120)), ]
uru03d <- uru03d[!(is.na(uru03d$F121)), ]
uru03d <- uru03d[!(is.na(uru03d$G006)), ]
summary(uru03d)
write.csv(uru03d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/uru03d.csv",
          row.names = FALSE)
write.csv(uru03m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/uru03m.csv",
          row.names = FALSE)
# Venezuela
Ven03 <- read_csv("onda 3 _ 94-98/Ven03.csv")
ven03 <- subset(Ven03, select=c(B006, B008, C001,
                                C002, E018, E034,
                                E035, E036, E039,
                                F028, F116, F118,
                                F120, F121,F141,
                                F144_02,G006,X001,
                                X003, X025R,X028,
                                X049,X051,
                                Y001,E033,
                                E179WVS,E182))
ven03m <-subset(ven03, select=-c(B006,F141,F144_02))
ven03d <-subset(ven03, select=-c(B006,F141,F144_02))
ven03d <- ven03d[!(is.na(ven03d$B008)), ]
ven03d <- ven03d[!(is.na(ven03d$C001)), ]
ven03d <- ven03d[!(is.na(ven03d$C002)), ]
ven03d <- ven03d[!(is.na(ven03d$E018)), ]
ven03d <- ven03d[!(is.na(ven03d$E034)), ]
ven03d <- ven03d[!(is.na(ven03d$E035)), ]
ven03d <- ven03d[!(is.na(ven03d$E036)), ]
ven03d <- ven03d[!(is.na(ven03d$E039)), ]
ven03d <- ven03d[!(is.na(ven03d$F028)), ]
ven03d <- ven03d[!(is.na(ven03d$F116)), ]
ven03d <- ven03d[!(is.na(ven03d$F118)), ]
ven03d <- ven03d[!(is.na(ven03d$F120)), ]
ven03d <- ven03d[!(is.na(ven03d$F121)), ]
ven03d <- ven03d[!(is.na(ven03d$G006)), ]
summary(ven03d)
write.csv(ven03d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/ven03d.csv",
          row.names = FALSE)
write.csv(ven03m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/ven03m.csv",
          row.names = FALSE)
#onda 4
#ARG
arg04 <- argentina %>%
  filter(S002VS == 4)
arg04 <- subset(arg04, select=c(B006, B008, C001, C002,
                                E018, E034, E035, E036, E039, F028, F116,
                                F118, F120, F121,F141,F144_02,G006,X001,X003, X025R,
                                X028,X049,X051,Y001,E033,E179WVS,E182))
arg04m <-subset(arg04, select=-c(B006,F141,F144_02))
arg04d <-subset(arg04, select=-c(B006,F141,F144_02))
arg04d <- arg04d[!(is.na(arg04d$B008)), ]
arg04d <- arg04d[!(is.na(arg04d$C001)), ]
arg04d <- arg04d[!(is.na(arg04d$C002)), ]
arg04d <- arg04d[!(is.na(arg04d$E018)), ]
arg04d <- arg04d[!(is.na(arg04d$E034)), ]
arg04d <- arg04d[!(is.na(arg04d$E035)), ]
arg04d <- arg04d[!(is.na(arg04d$E036)), ]
arg04d <- arg04d[!(is.na(arg04d$E039)), ]
arg04d <- arg04d[!(is.na(arg04d$F028)), ]
arg04d <- arg04d[!(is.na(arg04d$F116)), ]
arg04d <- arg04d[!(is.na(arg04d$F118)), ]
arg04d <- arg04d[!(is.na(arg04d$F120)), ]
arg04d <- arg04d[!(is.na(arg04d$F121)), ]
arg04d <- arg04d[!(is.na(arg04d$G006)), ]
summary(arg04d)
write.csv(arg04d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/arg04d.csv",
          row.names = FALSE)
write.csv(arg04m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/arg04m.csv",
          row.names = FALSE)

#mex
mex04 <- mexico %>%
  filter(S002VS == 4)
mex04 <- subset(mex04, select=c(B006, B008, C001, C002,
                                E018, E034, E035, E036, E039, F028, F116,
                                F118, F120, F121,F141,F144_02,G006,X001,X003, X025R,
                                X028,X049,X051,Y001,E033,E179WVS,E182))
mex04m <-subset(mex04, select=-c(B006,F141,F144_02))
mex04d <-subset(mex04, select=-c(B006,F141,F144_02))
mex04d <- mex04d[!(is.na(mex04d$B008)), ]
mex04d <- mex04d[!(is.na(mex04d$C001)), ]
mex04d <- mex04d[!(is.na(mex04d$C002)), ]
mex04d <- mex04d[!(is.na(mex04d$E018)), ]
mex04d <- mex04d[!(is.na(mex04d$E034)), ]
mex04d <- mex04d[!(is.na(mex04d$E035)), ]
mex04d <- mex04d[!(is.na(mex04d$E036)), ]
mex04d <- mex04d[!(is.na(mex04d$E039)), ]
mex04d <- mex04d[!(is.na(mex04d$F028)), ]
mex04d <- mex04d[!(is.na(mex04d$F116)), ]
mex04d <- mex04d[!(is.na(mex04d$F118)), ]
mex04d <- mex04d[!(is.na(mex04d$F120)), ]
mex04d <- mex04d[!(is.na(mex04d$F121)), ]
mex04d <- mex04d[!(is.na(mex04d$G006)), ]
summary(mex04d)
write.csv(mex04d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/mex04d.csv",
          row.names = FALSE)
write.csv(mex04m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/mex04m.csv",
          row.names = FALSE)
#Peru
peru04 <- Peru %>%
  filter(S002VS == 4)
peru04 <- subset(peru04, select=c(B006, B008, C001, C002,
                                  E018, E034, E035, E036, E039, F028, F116,
                                  F118, F120, F121,F141,F144_02,G006,X001,X003, X025R,
                                  X028,X049,X051,Y001,E033,E179WVS,E182))
peru04m <-subset(peru04, select=-c(B006,F141,F144_02))
peru04d <-subset(peru04, select=-c(B006,F141,F144_02))
peru04d <- peru04d[!(is.na(peru04d$B008)), ]
peru04d <- peru04d[!(is.na(peru04d$C001)), ]
peru04d <- peru04d[!(is.na(peru04d$C002)), ]
peru04d <- peru04d[!(is.na(peru04d$E018)), ]
peru04d <- peru04d[!(is.na(peru04d$E034)), ]
peru04d <- peru04d[!(is.na(peru04d$E035)), ]
peru04d <- peru04d[!(is.na(peru04d$E036)), ]
peru04d <- peru04d[!(is.na(peru04d$E039)), ]
peru04d <- peru04d[!(is.na(peru04d$F028)), ]
peru04d <- peru04d[!(is.na(peru04d$F116)), ]
peru04d <- peru04d[!(is.na(peru04d$F118)), ]
peru04d <- peru04d[!(is.na(peru04d$F120)), ]
peru04d <- peru04d[!(is.na(peru04d$F121)), ]
peru04d <- peru04d[!(is.na(peru04d$G006)), ]
summary(peru04d)
write.csv(peru04d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/peru04d.csv",
          row.names = FALSE)
write.csv(peru04m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/peru04m.csv",
          row.names = FALSE)


#Chi4
merge <- read_sav("moreno_clivagem_merge.sav")
ch4 <- merge %>%
  filter(COUNTRY_ALPHA == "CHL")
ch4 <- ch4 %>%
  filter(S002VS == 4)
ch4 <- remove_labels(ch4)
rm(merge)
ch04 <- subset(ch4, select=c(B006, B008, C001, C002,
                              E018, E034, E035, E036, E039, F028, F116,
                              F118, F120, F121,F141,F144_02,G006,X001,X003, X025R,
                              X028,X049,X051,Y001,E033,E179WVS,E182))
ch04m <-subset(ch04, select=-c(B006,F141,F144_02))
ch04d <-subset(ch04, select=-c(B006,F141,F144_02))
ch04d <- ch04d[!(is.na(ch04d$B008)), ]
ch04d <- ch04d[!(is.na(ch04d$C001)), ]
ch04d <- ch04d[!(is.na(ch04d$C002)), ]
ch04d <- ch04d[!(is.na(ch04d$E018)), ]
ch04d <- ch04d[!(is.na(ch04d$E034)), ]
ch04d <- ch04d[!(is.na(ch04d$E035)), ]
ch04d <- ch04d[!(is.na(ch04d$E036)), ]
ch04d <- ch04d[!(is.na(ch04d$E039)), ]
ch04d <- ch04d[!(is.na(ch04d$F028)), ]
ch04d <- ch04d[!(is.na(ch04d$F116)), ]
ch04d <- ch04d[!(is.na(ch04d$F118)), ]
ch04d <- ch04d[!(is.na(ch04d$F120)), ]
ch04d <- ch04d[!(is.na(ch04d$F121)), ]
ch04d <- ch04d[!(is.na(ch04d$G006)), ]
summary(ch04d)
write.csv(ch04d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/ch04d.csv",
          row.names = FALSE)
write.csv(ch04m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/ch04m.csv",
          row.names = FALSE)
#Venezuela
ven04 <- read_csv("onda 4 99-2004/Ven04.csv")
ven04 <- subset(ven04, select=c(B006, B008, C001, C002,
                                E018, E034, E035, E036, E039, F028, F116,
                                F118, F120, F121,F141,F144_02,G006,X001,X003, X025R,
                                X028,X049,X051,Y001,E033,E179WVS,E182))
ven04m <-subset(ven04, select=-c(B006,F141,F144_02))
ven04d <-subset(ven04, select=-c(B006,F141,F144_02))
ven04d <- ven04d[!(is.na(ven04d$B008)), ]
ven04d <- ven04d[!(is.na(ven04d$C001)), ]
ven04d <- ven04d[!(is.na(ven04d$C002)), ]
ven04d <- ven04d[!(is.na(ven04d$E018)), ]
ven04d <- ven04d[!(is.na(ven04d$E034)), ]
ven04d <- ven04d[!(is.na(ven04d$E035)), ]
ven04d <- ven04d[!(is.na(ven04d$E036)), ]
ven04d <- ven04d[!(is.na(ven04d$E039)), ]
ven04d <- ven04d[!(is.na(ven04d$F028)), ]
ven04d <- ven04d[!(is.na(ven04d$F116)), ]
ven04d <- ven04d[!(is.na(ven04d$F118)), ]
ven04d <- ven04d[!(is.na(ven04d$F120)), ]
ven04d <- ven04d[!(is.na(ven04d$F121)), ]
ven04d <- ven04d[!(is.na(ven04d$G006)), ]
summary(ven04d)
write.csv(ven04d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/ven04d.csv",
          row.names = FALSE)
write.csv(ven04m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/ven04m.csv",
          row.names = FALSE)

# onda 5

# ARG


arg05<- read_sav("onda 5 --- 2005-09/moreno_clivagem_argentina2006.sav")
arg05 <- remove_labels(arg05)
arg05m <- subset(arg05, select=c(B008, C001, C002,
                                 E018, E035, E036, E039,
                                 F028, F116, F118, F120, F121,
                                 G006,X001,X003, X025R,X028,
                                 X049,X051,Y001,E033,E179WVS,E182))
arg05d <- arg05m
arg05d <- arg05d[!(is.na(arg05d$B008)), ]
arg05d <- arg05d[!(is.na(arg05d$C001)), ]
arg05d <- arg05d[!(is.na(arg05d$C002)), ]
arg05d <- arg05d[!(is.na(arg05d$E018)), ]
arg05d <- arg05d[!(is.na(arg05d$E035)), ]
arg05d <- arg05d[!(is.na(arg05d$E036)), ]
arg05d <- arg05d[!(is.na(arg05d$E039)), ]
arg05d <- arg05d[!(is.na(arg05d$F028)), ]
arg05d <- arg05d[!(is.na(arg05d$F116)), ]
arg05d <- arg05d[!(is.na(arg05d$F118)), ]
arg05d <- arg05d[!(is.na(arg05d$F120)), ]
arg05d <- arg05d[!(is.na(arg05d$F121)), ]
arg05d <- arg05d[!(is.na(arg05d$G006)), ]
summary(arg05d)

write.csv(arg05d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/arg05d.csv",
          row.names = FALSE)
write.csv(arg05m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/arg05m.csv",
          row.names = FALSE)
#
#BRA 05

br5 <- read_sav("onda 5 --- 2005-09/moreno_clivagem_brasil2006.sav")
br5 <- remove_labels(br5)
bra05m <- subset(br5, select=c(B008, C001, C002,
                                 E018, E035, E036, E039,
                                 F028, F116, F118, F120, F121,
                                 G006,X001,X003, X025R,X028,
                                 X049,X051,Y001,E033,E179WVS,E182))
bra05d <- bra05m
bra05d <- bra05d[!(is.na(bra05d$B008)), ]
bra05d <- bra05d[!(is.na(bra05d$C001)), ]
bra05d <- bra05d[!(is.na(bra05d$C002)), ]
bra05d <- bra05d[!(is.na(bra05d$E018)), ]
bra05d <- bra05d[!(is.na(bra05d$E035)), ]
bra05d <- bra05d[!(is.na(bra05d$E036)), ]
bra05d <- bra05d[!(is.na(bra05d$E039)), ]
bra05d <- bra05d[!(is.na(bra05d$F028)), ]
bra05d <- bra05d[!(is.na(bra05d$F116)), ]
bra05d <- bra05d[!(is.na(bra05d$F118)), ]
bra05d <- bra05d[!(is.na(bra05d$F120)), ]
bra05d <- bra05d[!(is.na(bra05d$F121)), ]
bra05d <- bra05d[!(is.na(bra05d$G006)), ]
summary(bra05d)

write.csv(bra05d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/bra05d.csv",
          row.names = FALSE)
write.csv(bra05m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/bra05m.csv",
          row.names = FALSE)
# Chile

ch5 <- read_sav("onda 5 --- 2005-09/moreno_clivagem_chile2006.sav")
ch5 <- remove_labels(ch5)
chi05m <- subset(ch5, select=c(B008, C001, C002,
                               E018, E035, E036, E039,
                               F028, F116, F118, F120, F121,
                               G006,X001,X003, X025R,X028,
                               X049,X051,Y001,E033,E179WVS,E182))
chi05d <- chi05m
chi05d <- chi05d[!(is.na(chi05d$B008)), ]
chi05d <- chi05d[!(is.na(chi05d$C001)), ]
chi05d <- chi05d[!(is.na(chi05d$C002)), ]
chi05d <- chi05d[!(is.na(chi05d$E018)), ]
chi05d <- chi05d[!(is.na(chi05d$E035)), ]
chi05d <- chi05d[!(is.na(chi05d$E036)), ]
chi05d <- chi05d[!(is.na(chi05d$E039)), ]
chi05d <- chi05d[!(is.na(chi05d$F028)), ]
chi05d <- chi05d[!(is.na(chi05d$F116)), ]
chi05d <- chi05d[!(is.na(chi05d$F118)), ]
chi05d <- chi05d[!(is.na(chi05d$F120)), ]
chi05d <- chi05d[!(is.na(chi05d$F121)), ]
chi05d <- chi05d[!(is.na(chi05d$G006)), ]
summary(chi05d)

write.csv(chi05d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/chi05d.csv",
          row.names = FALSE)
write.csv(chi05m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/chi05m.csv",
          row.names = FALSE)
#Colombia
Colombia <- read_csv("novapasta/Colombia.csv")
col05 <- Colombia %>%
  filter(S002VS == 5)

col05m <- subset(col05, select=c(B008, C001, C002,
                                 E018, E035, E036, E039,
                                 F028, F116, F118, F120, F121,
                                 G006,X001,X003, X025R,X028,
                                 X049,X051,Y001,E033,E179WVS,E182))
summary(col05m)#ver quais tirar (NAN)
col05m <- subset(col05m, select=-c(C001,C002,X049,X051,Y001))#especificidade col05
col05d <- col05m
col05d <- col05d[!(is.na(col05d$B008)), ]
col05d <- col05d[!(is.na(col05d$E018)), ]
col05d <- col05d[!(is.na(col05d$E035)), ]
col05d <- col05d[!(is.na(col05d$E036)), ]
col05d <- col05d[!(is.na(col05d$E039)), ]
col05d <- col05d[!(is.na(col05d$F028)), ]
col05d <- col05d[!(is.na(col05d$F116)), ]
col05d <- col05d[!(is.na(col05d$F118)), ]
col05d <- col05d[!(is.na(col05d$F120)), ]
col05d <- col05d[!(is.na(col05d$F121)), ]
col05d <- col05d[!(is.na(col05d$G006)), ]
summary(col05d)

write.csv(col05d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/col05d.csv",
          row.names = FALSE)
write.csv(col05m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/col05m.csv",
          row.names = FALSE)


# Guatemala 5
guatemala <- read_csv("novapasta/guatemala.csv")
guat05 <- guatemala %>%
  filter(S002VS == 5)

guat05m <- subset(guat05, select=c(B008, C001, C002,
                                 E018, E035, E036, E039,
                                 F028, F116, F118, F120, F121,
                                 G006,X001,X003, X025R,X028,
                                 X049,X051,Y001,E033,E179WVS,E182))
summary(guat05m)#verificar se tem NAN - não tem ok - prosseguir
guat05d <- guat05m
guat05d <- guat05d[!(is.na(guat05d$B008)), ]
guat05d <- guat05d[!(is.na(guat05d$C001)), ]
guat05d <- guat05d[!(is.na(guat05d$C002)), ]
guat05d <- guat05d[!(is.na(guat05d$E018)), ]
guat05d <- guat05d[!(is.na(guat05d$E035)), ]
guat05d <- guat05d[!(is.na(guat05d$E036)), ]
guat05d <- guat05d[!(is.na(guat05d$E039)), ]
guat05d <- guat05d[!(is.na(guat05d$F028)), ]
guat05d <- guat05d[!(is.na(guat05d$F116)), ]
guat05d <- guat05d[!(is.na(guat05d$F118)), ]
guat05d <- guat05d[!(is.na(guat05d$F120)), ]
guat05d <- guat05d[!(is.na(guat05d$F121)), ]
guat05d <- guat05d[!(is.na(guat05d$G006)), ]
summary(guat05d)

write.csv(guat05d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/guat05d.csv",
          row.names = FALSE)
write.csv(guat05m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/guat05m.csv",
          row.names = FALSE)
#México
mex05 <- mexico %>%
  filter(S002VS == 5)

mex05m <- subset(mex05, select=c(B008, C001, C002,
                                 E018, E035, E036, E039,
                                 F028, F116, F118, F120, F121,
                                 G006,X001,X003, X025R,X028,
                                 X049,X051,Y001,E033,E179WVS,E182))
summary(mex05m)#verificar se tem NAN - não tem ok - prosseguir
mex05d <- mex05m
mex05d <- mex05d[!(is.na(mex05d$B008)), ]
mex05d <- mex05d[!(is.na(mex05d$C001)), ]
mex05d <- mex05d[!(is.na(mex05d$C002)), ]
mex05d <- mex05d[!(is.na(mex05d$E018)), ]
mex05d <- mex05d[!(is.na(mex05d$E035)), ]
mex05d <- mex05d[!(is.na(mex05d$E036)), ]
mex05d <- mex05d[!(is.na(mex05d$E039)), ]
mex05d <- mex05d[!(is.na(mex05d$F028)), ]
mex05d <- mex05d[!(is.na(mex05d$F116)), ]
mex05d <- mex05d[!(is.na(mex05d$F118)), ]
mex05d <- mex05d[!(is.na(mex05d$F120)), ]
mex05d <- mex05d[!(is.na(mex05d$F121)), ]
mex05d <- mex05d[!(is.na(mex05d$G006)), ]
summary(mex05d)

write.csv(mex05d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/mex05d.csv",
          row.names = FALSE)
write.csv(mex05m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/mex05m.csv",
          row.names = FALSE)
# Peru five
peru05 <- Peru %>%
  filter(S002VS == 5)

peru05m <- subset(peru05, select=c(B008, C001, C002,
                                   E018, E035, E036, E039,
                                   F028, F116, F118, F120, F121,
                                   G006,X001,X003, X025R,X028,
                                   X049,X051,Y001,E033,E179WVS,E182))
summary(peru05m)#verificar se tem NAN - tem! caso especial
peru05m <- subset(peru05m, select=-c(X049, G006,F118,F116,F120,F121))
peru05d <- peru05m
peru05d <- peru05d[!(is.na(peru05d$B008)), ]
peru05d <- peru05d[!(is.na(peru05d$C001)), ]
peru05d <- peru05d[!(is.na(peru05d$C002)), ]
peru05d <- peru05d[!(is.na(peru05d$E018)), ]
peru05d <- peru05d[!(is.na(peru05d$E035)), ]
peru05d <- peru05d[!(is.na(peru05d$E036)), ]
peru05d <- peru05d[!(is.na(peru05d$E039)), ]
peru05d <- peru05d[!(is.na(peru05d$F028)), ]
summary(peru05d)

write.csv(peru05d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/peru05d.csv",
          row.names = FALSE)
write.csv(peru05m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/peru05m.csv",
          row.names = FALSE)
# uru
uru05 <- read_sav("onda 5 --- 2005-09/moreno_clivagem_uruguai2006.sav")
uru05 <- remove_labels(uru05)
uru05m <- subset(uru05, select=c(B008, C001, C002,
                               E018, E035, E036, E039,
                               F028, F116, F118, F120, F121,
                               G006,X001,X003, X025R,X028,
                               X049,X051,Y001,E033,E179WVS,E182))
uru05d <- uru05m
uru05d <- uru05d[!(is.na(uru05d$B008)), ]
uru05d <- uru05d[!(is.na(uru05d$C001)), ]
uru05d <- uru05d[!(is.na(uru05d$C002)), ]
uru05d <- uru05d[!(is.na(uru05d$E018)), ]
uru05d <- uru05d[!(is.na(uru05d$E035)), ]
uru05d <- uru05d[!(is.na(uru05d$E036)), ]
uru05d <- uru05d[!(is.na(uru05d$E039)), ]
uru05d <- uru05d[!(is.na(uru05d$F028)), ]
uru05d <- uru05d[!(is.na(uru05d$F116)), ]
uru05d <- uru05d[!(is.na(uru05d$F118)), ]
uru05d <- uru05d[!(is.na(uru05d$F120)), ]
uru05d <- uru05d[!(is.na(uru05d$F121)), ]
uru05d <- uru05d[!(is.na(uru05d$G006)), ]
summary(uru05d)

write.csv(uru05d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/uru05d.csv",
          row.names = FALSE)
write.csv(uru05m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/uru05m.csv",
          row.names = FALSE)
# ONDA SEIS 


# ARGENTINA 


arg06 <- read_sav("onda 6 --- 2010 -14/moreno_clivagem_argentina2013.sav")
arg06 <- remove_labels(arg06)
arg06m <- subset(arg06, select=c(B008, C001, C002, E018, E035,
                                 E036, E039, F028, F116, F118, F120, F121,
                                 G006,X001,X003, X025R,X049,
                                 Y001,E033,E179WVS))
arg06d <- arg06m
arg06d <- arg06d[!(is.na(arg06d$B008)), ]
arg06d <- arg06d[!(is.na(arg06d$C001)), ]
arg06d <- arg06d[!(is.na(arg06d$C002)), ]
arg06d <- arg06d[!(is.na(arg06d$E018)), ]
arg06d <- arg06d[!(is.na(arg06d$E035)), ]
arg06d <- arg06d[!(is.na(arg06d$E036)), ]
arg06d <- arg06d[!(is.na(arg06d$E039)), ]
arg06d <- arg06d[!(is.na(arg06d$F028)), ]
arg06d <- arg06d[!(is.na(arg06d$F116)), ]
arg06d <- arg06d[!(is.na(arg06d$F118)), ]
arg06d <- arg06d[!(is.na(arg06d$F120)), ]
arg06d <- arg06d[!(is.na(arg06d$F121)), ]
arg06d <- arg06d[!(is.na(arg06d$G006)), ]
write.csv(arg06d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/arg06d.csv",
          row.names = FALSE)
write.csv(arg06m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/arg06m.csv",
          row.names = FALSE)

#

br6 <- read_sav("onda 6 --- 2010 -14/moreno_clivagem_brasil2014.sav")
bra06 <- br6
bra06 <- remove_labels(bra06)
bra06m <- subset(bra06, select=c(B008, C001, C002, E018, E035,
                                 E036, E039, F028, F116, F118, F120, F121,
                                 G006,X001,X003, X025R,X049,
                                 Y001,E033,E179WVS))
bra06d <- bra06m
bra06d <- bra06d[!(is.na(bra06d$B008)), ]
bra06d <- bra06d[!(is.na(bra06d$C001)), ]
bra06d <- bra06d[!(is.na(bra06d$C002)), ]
bra06d <- bra06d[!(is.na(bra06d$E018)), ]
bra06d <- bra06d[!(is.na(bra06d$E035)), ]
bra06d <- bra06d[!(is.na(bra06d$E036)), ]
bra06d <- bra06d[!(is.na(bra06d$E039)), ]
bra06d <- bra06d[!(is.na(bra06d$F028)), ]
bra06d <- bra06d[!(is.na(bra06d$F116)), ]
bra06d <- bra06d[!(is.na(bra06d$F118)), ]
bra06d <- bra06d[!(is.na(bra06d$F120)), ]
bra06d <- bra06d[!(is.na(bra06d$F121)), ]
bra06d <- bra06d[!(is.na(bra06d$G006)), ]
write.csv(bra06d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/bra06d.csv",
          row.names = FALSE)
write.csv(bra06m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/bra06m.csv",
          row.names = FALSE)
#  Uru
uru06 <- read_sav("onda 6 --- 2010 -14/moreno_clivagem_uruguai2011.sav")
uru06 <- remove_labels(uru06)
uru06m <- subset(uru06, select=c(B008, C001, C002, E018, E035,
                                 E036, E039, F028, F116, F118, F120, F121,
                                 G006,X001,X003, X025R,X049,
                                 Y001,E033,E179WVS))
uru06d <- uru06m
uru06d <- uru06d[!(is.na(uru06d$B008)), ]
uru06d <- uru06d[!(is.na(uru06d$C001)), ]
uru06d <- uru06d[!(is.na(uru06d$C002)), ]
uru06d <- uru06d[!(is.na(uru06d$E018)), ]
uru06d <- uru06d[!(is.na(uru06d$E035)), ]
uru06d <- uru06d[!(is.na(uru06d$E036)), ]
uru06d <- uru06d[!(is.na(uru06d$E039)), ]
uru06d <- uru06d[!(is.na(uru06d$F028)), ]
uru06d <- uru06d[!(is.na(uru06d$F116)), ]
uru06d <- uru06d[!(is.na(uru06d$F118)), ]
uru06d <- uru06d[!(is.na(uru06d$F120)), ]
uru06d <- uru06d[!(is.na(uru06d$F121)), ]
uru06d <- uru06d[!(is.na(uru06d$G006)), ]
write.csv(uru06d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/uru06d.csv",
          row.names = FALSE)
write.csv(uru06m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/uru06m.csv",
          row.names = FALSE)
# Chile

ch6 <- read_sav("onda 6 --- 2010 -14/moreno_clivagem_chile2012.sav")
ch6 <- remove_labels(ch6)
ch6m <- subset(ch6, select=c(B008, C001, C002, E018, E035,
                             E036, E039, F028, F116, F118, F120, F121,
                             G006,X001,X003, X025R,X049,
                             Y001,E033,E179WVS))
ch6d <- ch6m
ch6d <- ch6d[!(is.na(ch6d$B008)), ]
ch6d <- ch6d[!(is.na(ch6d$C001)), ]
ch6d <- ch6d[!(is.na(ch6d$C002)), ]
ch6d <- ch6d[!(is.na(ch6d$E018)), ]
ch6d <- ch6d[!(is.na(ch6d$E035)), ]
ch6d <- ch6d[!(is.na(ch6d$E036)), ]
ch6d <- ch6d[!(is.na(ch6d$E039)), ]
ch6d <- ch6d[!(is.na(ch6d$F028)), ]
ch6d <- ch6d[!(is.na(ch6d$F116)), ]
ch6d <- ch6d[!(is.na(ch6d$F118)), ]
ch6d <- ch6d[!(is.na(ch6d$F120)), ]
ch6d <- ch6d[!(is.na(ch6d$F121)), ]
ch6d <- ch6d[!(is.na(ch6d$G006)), ]
write.csv(ch6d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/ch6d.csv",
          row.names = FALSE)
write.csv(ch6m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/ch6m.csv",
          row.names = FALSE)
# Mexico
mex06 <- mexico %>%
  filter(S002VS == 6)
mex06m <- subset(mex06, select=c(B006, B008, C001, C002, E018, E034, E035, E036, E039,
                                 F028, F116, F118, F120, F121,F141,F144_02,G006,X001,
                                 X003, X025R,X028,X049,X051,Y001,E033,E179WVS,E182))
mex06m <- subset(mex06m, select=-c(B006, F141, F144_02, E034,E182))
mex06d <- mex06m
mex06d <- mex06d[!(is.na(mex06d$B008)), ]
mex06d <- mex06d[!(is.na(mex06d$C001)), ]
mex06d <- mex06d[!(is.na(mex06d$C002)), ]
mex06d <- mex06d[!(is.na(mex06d$E018)), ]
mex06d <- mex06d[!(is.na(mex06d$E035)), ]
mex06d <- mex06d[!(is.na(mex06d$E036)), ]
mex06d <- mex06d[!(is.na(mex06d$E039)), ]
mex06d <- mex06d[!(is.na(mex06d$F028)), ]
mex06d <- mex06d[!(is.na(mex06d$F116)), ]
mex06d <- mex06d[!(is.na(mex06d$F118)), ]
mex06d <- mex06d[!(is.na(mex06d$F120)), ]
mex06d <- mex06d[!(is.na(mex06d$F121)), ]
mex06d <- mex06d[!(is.na(mex06d$G006)), ]
summary(mex06d)# pra conferir

write.csv(mex06d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/mex06d.csv",
          row.names = FALSE)
write.csv(mex06m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/mex06m.csv",
          row.names = FALSE)

# COLOMBIA 6
col06 <- Colombia %>%
  filter(S002VS == 6)
col06m <- subset(col06, select=c(B006, B008, C001, C002, E018, E034, E035, E036, E039,
                                 F028, F116, F118, F120, F121,F141,F144_02,G006,X001,
                                 X003, X025R,X028,X049,X051,Y001,E033,E179WVS,E182))
col06m <- subset(col06m, select=-c(B006, F141, F144_02, E034,E182))
col06d <- col06m
col06d <- col06d[!(is.na(col06d$B008)), ]
col06d <- col06d[!(is.na(col06d$C001)), ]
col06d <- col06d[!(is.na(col06d$C002)), ]
col06d <- col06d[!(is.na(col06d$E018)), ]
col06d <- col06d[!(is.na(col06d$E035)), ]
col06d <- col06d[!(is.na(col06d$E036)), ]
col06d <- col06d[!(is.na(col06d$E039)), ]
col06d <- col06d[!(is.na(col06d$F028)), ]
col06d <- col06d[!(is.na(col06d$F116)), ]
col06d <- col06d[!(is.na(col06d$F118)), ]
col06d <- col06d[!(is.na(col06d$F120)), ]
col06d <- col06d[!(is.na(col06d$F121)), ]
col06d <- col06d[!(is.na(col06d$G006)), ]
summary(col06d)# pra conferir

write.csv(col06d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/col06d.csv",
          row.names = FALSE)
write.csv(col06m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/col06m.csv",
          row.names = FALSE)
# Haiti

haiti6 <- read_csv("novapasta/haiti.csv")
haiti6m <- subset(haiti6, select=c(B006, B008, C001, C002, E018, E034, E035, E036, E039,
                                   F028, F116, F118, F120, F121,F141,F144_02,G006,X001,
                                   X003, X025R,X028,X049,X051,Y001,E033,E179WVS,E182))
haiti6m <- subset(haiti6m, select=-c(B006, F141, F144_02, E034,E182))
haiti6d <- haiti6m
haiti6d <- haiti6d[!(is.na(haiti6d$B008)), ]
haiti6d <- haiti6d[!(is.na(haiti6d$C001)), ]
haiti6d <- haiti6d[!(is.na(haiti6d$C002)), ]
haiti6d <- haiti6d[!(is.na(haiti6d$E018)), ]
haiti6d <- haiti6d[!(is.na(haiti6d$E035)), ]
haiti6d <- haiti6d[!(is.na(haiti6d$E036)), ]
haiti6d <- haiti6d[!(is.na(haiti6d$E039)), ]
haiti6d <- haiti6d[!(is.na(haiti6d$F028)), ]
haiti6d <- haiti6d[!(is.na(haiti6d$F116)), ]
haiti6d <- haiti6d[!(is.na(haiti6d$F118)), ]
haiti6d <- haiti6d[!(is.na(haiti6d$F120)), ]
haiti6d <- haiti6d[!(is.na(haiti6d$F121)), ]
haiti6d <- haiti6d[!(is.na(haiti6d$G006)), ]
summary(haiti6d)# pra conferir

write.csv(haiti6d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/haiti6d.csv",
          row.names = FALSE)
write.csv(haiti6m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/haiti6m.csv",
          row.names = FALSE)
# Peru
peru06 <- Peru %>%
  filter(S002VS == 6)
peru06m <- subset(peru06, select=c(B006, B008, C001, C002, E018, E034, E035, E036, E039,
                                   F028, F116, F118, F120, F121,F141,F144_02,G006,X001,
                                   X003, X025R,X028,X049,X051,Y001,E033,E179WVS,E182))
peru06m <- subset(peru06m, select=-c(B006, F141, F144_02, E034,E182))
peru06d <- peru06m
peru06d <- peru06d[!(is.na(peru06d$B008)), ]
peru06d <- peru06d[!(is.na(peru06d$C001)), ]
peru06d <- peru06d[!(is.na(peru06d$C002)), ]
peru06d <- peru06d[!(is.na(peru06d$E018)), ]
peru06d <- peru06d[!(is.na(peru06d$E035)), ]
peru06d <- peru06d[!(is.na(peru06d$E036)), ]
peru06d <- peru06d[!(is.na(peru06d$E039)), ]
peru06d <- peru06d[!(is.na(peru06d$F028)), ]
peru06d <- peru06d[!(is.na(peru06d$F116)), ]
peru06d <- peru06d[!(is.na(peru06d$F118)), ]
peru06d <- peru06d[!(is.na(peru06d$F120)), ]
peru06d <- peru06d[!(is.na(peru06d$F121)), ]
peru06d <- peru06d[!(is.na(peru06d$G006)), ]
summary(peru06d)# pra conferir

write.csv(peru06d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/peru06d.csv",
          row.names = FALSE)
write.csv(peru06m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/peru06m.csv",
          row.names = FALSE)

#
#
#Ecuador 6
equador <- read_csv("novapasta/equador.csv")
equ06 <- equador %>%
  filter(S002VS == 6)
equad06m <- subset(equ06, select=c(B006, B008, C001, C002, E018, E034, E035, E036, E039,
                                     F028, F116, F118, F120, F121,F141,F144_02,G006,X001,
                                     X003, X025R,X028,X049,X051,Y001,E033,E179WVS,E182))
equad06m <- subset(equad06m, select=-c(B006, F141, F144_02, E034,E182))
equad06d <- equad06m
equad06d <- equad06d[!(is.na(equad06d$B008)), ]
equad06d <- equad06d[!(is.na(equad06d$C001)), ]
equad06d <- equad06d[!(is.na(equad06d$C002)), ]
equad06d <- equad06d[!(is.na(equad06d$E018)), ]
equad06d <- equad06d[!(is.na(equad06d$E035)), ]
equad06d <- equad06d[!(is.na(equad06d$E036)), ]
equad06d <- equad06d[!(is.na(equad06d$E039)), ]
equad06d <- equad06d[!(is.na(equad06d$F028)), ]
equad06d <- equad06d[!(is.na(equad06d$F116)), ]
equad06d <- equad06d[!(is.na(equad06d$F118)), ]
equad06d <- equad06d[!(is.na(equad06d$F120)), ]
equad06d <- equad06d[!(is.na(equad06d$F121)), ]
equad06d <- equad06d[!(is.na(equad06d$G006)), ]
summary(equad06d)# pra conferir

write.csv(equad06d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/equad06d.csv",
          row.names = FALSE)
write.csv(equad06m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/equad06m.csv",
          row.names = FALSE)

#
#
#
#ONDA 7


# ARG

arg07 <- read_sav("onda 7 - 2017-2022/moreno_clivagem_argentina2017.sav")
arg07 <- remove_labels(arg07)
arg07m <- subset(arg07, select=c(B008, C001,
                                 C002, E018, E034, E035, E036, E039, F028,
                                 F116, F118, F120, F121,F144_02,G006,X001,X003,
                                 X025R,X028,X049,X051,Y001,E033,E179WVS))
summary(arg07m)# verificar se tem NaN - OK
arg07d <- arg07m
arg07d <- arg07d[!(is.na(arg07d$B008)), ]
arg07d <- arg07d[!(is.na(arg07d$C001)), ]
arg07d <- arg07d[!(is.na(arg07d$C002)), ]
arg07d <- arg07d[!(is.na(arg07d$E018)), ]
arg07d <- arg07d[!(is.na(arg07d$E034)), ]
arg07d <- arg07d[!(is.na(arg07d$E035)), ]
arg07d <- arg07d[!(is.na(arg07d$E036)), ]
arg07d <- arg07d[!(is.na(arg07d$E039)), ]
arg07d <- arg07d[!(is.na(arg07d$F028)), ]
arg07d <- arg07d[!(is.na(arg07d$F116)), ]
arg07d <- arg07d[!(is.na(arg07d$F118)), ]
arg07d <- arg07d[!(is.na(arg07d$F120)), ]
arg07d <- arg07d[!(is.na(arg07d$F121)), ]
arg07d <- arg07d[!(is.na(arg07d$F144_02)), ]
arg07d <- arg07d[!(is.na(arg07d$G006)), ]
summary(arg07d)#verificar

write.csv(arg07d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/arg07d.csv",
          row.names = FALSE)
write.csv(arg07m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/arg07m.csv",
          row.names = FALSE)


# BRA

br7 <- read_sav("onda 7 - 2017-2022/moreno_clivagem_brasil2018.sav")
br7 <- remove_labels(br7)
br7m <- subset(br7, select=c(B008, C001,
                             C002, E018, E034, E035, E036, E039, F028,
                             F116, F118, F120, F121,F144_02,G006,X001,X003,
                             X025R,X028,X049,X051,Y001,E033,E179WVS))
summary(br7m)# verificar se tem NaN - OK
br7d <- br7m
br7d <- br7d[!(is.na(br7d$B008)), ]
br7d <- br7d[!(is.na(br7d$C001)), ]
br7d <- br7d[!(is.na(br7d$C002)), ]
br7d <- br7d[!(is.na(br7d$E018)), ]
br7d <- br7d[!(is.na(br7d$E034)), ]
br7d <- br7d[!(is.na(br7d$E035)), ]
br7d <- br7d[!(is.na(br7d$E036)), ]
br7d <- br7d[!(is.na(br7d$E039)), ]
br7d <- br7d[!(is.na(br7d$F028)), ]
br7d <- br7d[!(is.na(br7d$F116)), ]
br7d <- br7d[!(is.na(br7d$F118)), ]
br7d <- br7d[!(is.na(br7d$F120)), ]
br7d <- br7d[!(is.na(br7d$F121)), ]
br7d <- br7d[!(is.na(br7d$F144_02)), ]
br7d <- br7d[!(is.na(br7d$G006)), ]
summary(br7d)#verificar

write.csv(br7d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/br7d.csv",
          row.names = FALSE)
write.csv(br7m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/br7m.csv",
          row.names = FALSE)

# CHile

ch7 <- read_sav("onda 7 - 2017-2022/moreno_clivagem_chile2018.sav")
ch7 <- remove_labels(ch7)
ch7m <- subset(ch7, select=c(B008, C001,
                             C002, E018, E034, E035, E036, E039, F028,
                             F116, F118, F120, F121,F144_02,G006,X001,X003,
                             X025R,X028,X049,X051,Y001,E033,E179WVS))
summary(ch7m)# verificar se tem NaN - OK
ch7d <- ch7m
ch7d <- ch7d[!(is.na(ch7d$B008)), ]
ch7d <- ch7d[!(is.na(ch7d$C001)), ]
ch7d <- ch7d[!(is.na(ch7d$C002)), ]
ch7d <- ch7d[!(is.na(ch7d$E018)), ]
ch7d <- ch7d[!(is.na(ch7d$E034)), ]
ch7d <- ch7d[!(is.na(ch7d$E035)), ]
ch7d <- ch7d[!(is.na(ch7d$E036)), ]
ch7d <- ch7d[!(is.na(ch7d$E039)), ]
ch7d <- ch7d[!(is.na(ch7d$F028)), ]
ch7d <- ch7d[!(is.na(ch7d$F116)), ]
ch7d <- ch7d[!(is.na(ch7d$F118)), ]
ch7d <- ch7d[!(is.na(ch7d$F120)), ]
ch7d <- ch7d[!(is.na(ch7d$F121)), ]
ch7d <- ch7d[!(is.na(ch7d$F144_02)), ]
ch7d <- ch7d[!(is.na(ch7d$G006)), ]
summary(ch7d)#verificar

write.csv(ch7d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/ch7d.csv",
          row.names = FALSE)
write.csv(ch7m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/ch7m.csv",
          row.names = FALSE)


# VENEZUELA - caso especial não foi tabulada antes - tive que retomar do zero
library(readxl)
F00011820_WVS_Wave_7_Venezuela_Excel_v3_0 <- read_excel("onda 7 - 2017-2022/F00011820-WVS_Wave_7_Venezuela_Excel_v3.0.xlsx")
VEN <- remove_labels(F00011820_WVS_Wave_7_Venezuela_Excel_v3_0)
VEN$B008 <- VEN$`Q111: Protecting environment vs. Economic growth`
VEN$C001 <- VEN$`Q33: Jobs scarce: Men should have more right to a job than women`
VEN$C002 <- VEN$`Q34: Jobs scarce: Employers should give priority to (nation) people than immigrants`
VEN$E018 <- VEN$`Q45: Future changes: Greater respect for authority` 
VEN$E034 <- VEN$`Q42: Basic kinds of attitudes concerning society`
VEN$E035 <- VEN$`Q106: Income equality vs larger income differences`
VEN$E036 <- VEN$`Q107: Private vs state ownership of business`
VEN$E039 <- VEN$`Q109: Competition good or harmful`
VEN$F028 <- VEN$`Q171: How often do you attend religious services`
VEN$F034 <- VEN$`Q173: Religious person`
VEN$F063 <- VEN$`Q164: Importance of God`
VEN$F116 <- VEN$`Q180: Justifiable: Cheating on taxes`
VEN$F118 <- VEN$`Q182: Justifiable: Homosexuality`
VEN$F120 <- VEN$`Q184: Justifiable: Abortion`
VEN$F121 <- VEN$`Q185: Justifiable: Divorce`
VEN$F144_02 <- VEN$`Q195: Justifiable: Death penalty`
VEN$G006 <- VEN$`Q254: National pride`
VEN$X001 <- VEN$`Q260: Sex`
VEN$X003 <- VEN$`Q262: Age`
VEN$X025R <- VEN$`Q275R: Highest educational level: Respondent (recoded into 3 groups)`
VEN$X049 <- VEN$`G_TOWNSIZE: Settlement size_8 groups`
VEN$X028 <- VEN$`Q279: Employment status`
VEN$X051 <- VEN$`Q290: Ethnic group`
VEN$E179WVS <- VEN$`Q223: Which party would you vote for if there were a national election tomorrow`
VEN$E033 <- VEN$`Q240: Left-right political scale`
VEN$Y001 <- VEN$`Y001: Post-Materialist index 12-item`

ven07 <- subset(VEN,
                 select =c(B008, C001,
                           C002, E018, E034, E035, E036, E039, F028,
                           F116, F118, F120, F121,F144_02,G006,X001,X003,
                           X025R,X049, X028, X051,Y001,E033,E179WVS))
ven07m <- Ven07
ven07$B008[ven07$B008 == 3] <- NA # gerar missing
ven07$G006[ven07$G006 == 5] <- NA
ven07d <- subset(ven07,
                 select =c(B008, C001,
                           C002, E018, E034, E035, E036, E039, F028,
                           F116, F118, F120, F121,F144_02,G006,X001,X003,
                           X025R,X049, X028, X051,Y001,E033,E179WVS))%>% na.omit()

write.csv(ven07d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/ven07d.csv",
          row.names = FALSE)
write.csv(ven07,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/ven07m.csv",
          row.names = FALSE)

#México
str(mexico)
mex07 <- mexico %>%
  filter(S002VS == 7)
mex07m <- subset(mex07, select=c(B006, B008, C001, C002, E018,
                                 E034, E035, E036, E039, F028, F116, F118, F120,
                                 F121,F141,F144_02,G006,X001,X003, X025R,
                                 X028,X049,X051,Y001,E033,E179WVS,E182))
summary(mex07m)# pra verificar - p/ remover NANS
mex07m <- subset(mex07m, select=-c(B006,F141,E182))
mex07d <- mex07m

mex07d <- mex07d[!(is.na(mex07d$B008)), ]
mex07d <- mex07d[!(is.na(mex07d$C001)), ]
mex07d <- mex07d[!(is.na(mex07d$C002)), ]
mex07d <- mex07d[!(is.na(mex07d$E018)), ]
mex07d <- mex07d[!(is.na(mex07d$E034)), ]
mex07d <- mex07d[!(is.na(mex07d$E035)), ]
mex07d <- mex07d[!(is.na(mex07d$E036)), ]
mex07d <- mex07d[!(is.na(mex07d$E039)), ]
mex07d <- mex07d[!(is.na(mex07d$F028)), ]
mex07d <- mex07d[!(is.na(mex07d$F116)), ]
mex07d <- mex07d[!(is.na(mex07d$F118)), ]
mex07d <- mex07d[!(is.na(mex07d$F120)), ]
mex07d <- mex07d[!(is.na(mex07d$F121)), ]
mex07d <- mex07d[!(is.na(mex07d$F144_02)), ]
mex07d <- mex07d[!(is.na(mex07d$G006)), ]
summary(mex07d)#verificar

write.csv(mex07d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/mex07d.csv",
          row.names = FALSE)
write.csv(mex07m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/mex07m.csv",
          row.names = FALSE)


# Bolivia
b07 <- read_csv("novapasta/Bolivia.csv") # 
table(b07$S002VS)#verificar se é só 7 mesmo - OK
bol07 <- b07
bol07m <- subset(bol07, select=c(B006, B008, C001, C002, E018,
                                 E034, E035, E036, E039, F028, F116, F118, F120,
                                 F121,F141,F144_02,G006,X001,X003, X025R,
                                 X028,X049,X051,Y001,E033,E179WVS,E182))
summary(bol07m)# pra verificar - p/ remover NANS
bol07m <- subset(bol07m, select=-c(B006,F141,E182))
bol07d <- bol07m
bol07d <- bol07d[!(is.na(bol07d$B008)), ]
bol07d <- bol07d[!(is.na(bol07d$C001)), ]
bol07d <- bol07d[!(is.na(bol07d$C002)), ]
bol07d <- bol07d[!(is.na(bol07d$E018)), ]
bol07d <- bol07d[!(is.na(bol07d$E034)), ]
bol07d <- bol07d[!(is.na(bol07d$E035)), ]
bol07d <- bol07d[!(is.na(bol07d$E036)), ]
bol07d <- bol07d[!(is.na(bol07d$E039)), ]
bol07d <- bol07d[!(is.na(bol07d$F028)), ]
bol07d <- bol07d[!(is.na(bol07d$F116)), ]
bol07d <- bol07d[!(is.na(bol07d$F118)), ]
bol07d <- bol07d[!(is.na(bol07d$F120)), ]
bol07d <- bol07d[!(is.na(bol07d$F121)), ]
bol07d <- bol07d[!(is.na(bol07d$F144_02)), ]
bol07d <- bol07d[!(is.na(bol07d$G006)), ]
summary(bol07d)#verificar

write.csv(bol07d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/bol07d.csv",
          row.names = FALSE)
write.csv(bol07m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/bol07m.csv",
          row.names = FALSE)
# COLOMBIA
col07 <- Colombia %>%
  filter(S002VS == 7)
col07m <- subset(col07, select=c(B006, B008, C001, C002, E018,
                                 E034, E035, E036, E039, F028, F116, F118, F120,
                                 F121,F141,F144_02,G006,X001,X003, X025R,
                                 X028,X049,X051,Y001,E033,E179WVS,E182))
summary(col07m)# pra verificar - p/ remover NANS
col07m <- subset(col07m, select=-c(B006,F141,E182))
col07d <- col07m
col07d <- col07d[!(is.na(col07d$B008)), ]
col07d <- col07d[!(is.na(col07d$C001)), ]
col07d <- col07d[!(is.na(col07d$C002)), ]
col07d <- col07d[!(is.na(col07d$E018)), ]
col07d <- col07d[!(is.na(col07d$E034)), ]
col07d <- col07d[!(is.na(col07d$E035)), ]
col07d <- col07d[!(is.na(col07d$E036)), ]
col07d <- col07d[!(is.na(col07d$E039)), ]
col07d <- col07d[!(is.na(col07d$F028)), ]
col07d <- col07d[!(is.na(col07d$F116)), ]
col07d <- col07d[!(is.na(col07d$F118)), ]
col07d <- col07d[!(is.na(col07d$F120)), ]
col07d <- col07d[!(is.na(col07d$F121)), ]
col07d <- col07d[!(is.na(col07d$F144_02)), ]
col07d <- col07d[!(is.na(col07d$G006)), ]
summary(col07d)#verificar

write.csv(col07d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/col07d.csv",
          row.names = FALSE)
write.csv(col07m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/col07m.csv",
          row.names = FALSE)
# 
equ07 <- equador %>%
  filter(S002VS == 7)
equ07m <- subset(equ07, select=c(B006, B008, C001, C002, E018,
                                 E034, E035, E036, E039, F028, F116, F118, F120,
                                 F121,F141,F144_02,G006,X001,X003, X025R,
                                 X028,X049,X051,Y001,E033,E179WVS,E182))
summary(equ07m)# pra verificar - p/ remover NANS
equ07m <- subset(equ07m, select=-c(B006,F141,E182))
equ07d <- equ07m
equ07d <- equ07d[!(is.na(equ07d$B008)), ]
equ07d <- equ07d[!(is.na(equ07d$C001)), ]
equ07d <- equ07d[!(is.na(equ07d$C002)), ]
equ07d <- equ07d[!(is.na(equ07d$E018)), ]
equ07d <- equ07d[!(is.na(equ07d$E034)), ]
equ07d <- equ07d[!(is.na(equ07d$E035)), ]
equ07d <- equ07d[!(is.na(equ07d$E036)), ]
equ07d <- equ07d[!(is.na(equ07d$E039)), ]
equ07d <- equ07d[!(is.na(equ07d$F028)), ]
equ07d <- equ07d[!(is.na(equ07d$F116)), ]
equ07d <- equ07d[!(is.na(equ07d$F118)), ]
equ07d <- equ07d[!(is.na(equ07d$F120)), ]
equ07d <- equ07d[!(is.na(equ07d$F121)), ]
equ07d <- equ07d[!(is.na(equ07d$F144_02)), ]
equ07d <- equ07d[!(is.na(equ07d$G006)), ]
summary(equ07d)#verificar

write.csv(equ07d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/equ07d.csv",
          row.names = FALSE)
write.csv(equ07m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/equ07m.csv",
          row.names = FALSE)
#
guat07 <- guatemala %>%
  filter(S002VS == 7)
guat07m <- subset(guat07, select=c(B006, B008, C001, C002, E018,
                                   E034, E035, E036, E039, F028, F116, F118, F120,
                                   F121,F141,F144_02,G006,X001,X003, X025R,
                                   X028,X049,X051,Y001,E033,E179WVS,E182))
summary(guat07m)# pra verificar - p/ remover NANS
guat07m <- subset(guat07m, select=-c(B006,F141,E182))
guat07d <- guat07m
guat07d <- guat07d[!(is.na(guat07d$B008)), ]
guat07d <- guat07d[!(is.na(guat07d$C001)), ]
guat07d <- guat07d[!(is.na(guat07d$C002)), ]
guat07d <- guat07d[!(is.na(guat07d$E018)), ]
guat07d <- guat07d[!(is.na(guat07d$E034)), ]
guat07d <- guat07d[!(is.na(guat07d$E035)), ]
guat07d <- guat07d[!(is.na(guat07d$E036)), ]
guat07d <- guat07d[!(is.na(guat07d$E039)), ]
guat07d <- guat07d[!(is.na(guat07d$F028)), ]
guat07d <- guat07d[!(is.na(guat07d$F116)), ]
guat07d <- guat07d[!(is.na(guat07d$F118)), ]
guat07d <- guat07d[!(is.na(guat07d$F120)), ]
guat07d <- guat07d[!(is.na(guat07d$F121)), ]
guat07d <- guat07d[!(is.na(guat07d$F144_02)), ]
guat07d <- guat07d[!(is.na(guat07d$G006)), ]
summary(guat07d)#verificar

write.csv(guat07d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/guat07d.csv",
          row.names = FALSE)
write.csv(guat07m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/guat07m.csv",
          row.names = FALSE)

#
nicaragua07 <- read_csv("novapasta/nicaragua.csv")
nicaragua07m <- subset(nicaragua07, select=c(B006, B008, C001, C002, E018,
                                             E034, E035, E036, E039, F028, F116, F118, F120,
                                             F121,F141,F144_02,G006,X001,X003, X025R,
                                             X028,X049,X051,Y001,E033,E179WVS,E182))
summary(nicaragua07m)# pra verificar - p/ remover NANS
nicaragua07m <- subset(nicaragua07m, select=-c(B006,F141,E182))
nicaragua07d <- nicaragua07m
nicaragua07d <- nicaragua07d[!(is.na(nicaragua07d$B008)), ]
nicaragua07d <- nicaragua07d[!(is.na(nicaragua07d$C001)), ]
nicaragua07d <- nicaragua07d[!(is.na(nicaragua07d$C002)), ]
nicaragua07d <- nicaragua07d[!(is.na(nicaragua07d$E018)), ]
nicaragua07d <- nicaragua07d[!(is.na(nicaragua07d$E034)), ]
nicaragua07d <- nicaragua07d[!(is.na(nicaragua07d$E035)), ]
nicaragua07d <- nicaragua07d[!(is.na(nicaragua07d$E036)), ]
nicaragua07d <- nicaragua07d[!(is.na(nicaragua07d$E039)), ]
nicaragua07d <- nicaragua07d[!(is.na(nicaragua07d$F028)), ]
nicaragua07d <- nicaragua07d[!(is.na(nicaragua07d$F116)), ]
nicaragua07d <- nicaragua07d[!(is.na(nicaragua07d$F118)), ]
nicaragua07d <- nicaragua07d[!(is.na(nicaragua07d$F120)), ]
nicaragua07d <- nicaragua07d[!(is.na(nicaragua07d$F121)), ]
nicaragua07d <- nicaragua07d[!(is.na(nicaragua07d$F144_02)), ]
nicaragua07d <- nicaragua07d[!(is.na(nicaragua07d$G006)), ]
summary(nicaragua07d)#verificar

write.csv(nicaragua07d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/nicaragua07d.csv",
          row.names = FALSE)
write.csv(nicaragua07m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/nicaragua07m.csv",
          row.names = FALSE)

#
peru07 <- Peru %>%
  filter(S002VS == 7)
Peru07m <- subset(peru07, select=c(B006, B008, C001, C002, E018,
                                   E034, E035, E036, E039, F028, F116, F118, F120,
                                   F121,F141,F144_02,G006,X001,X003, X025R,
                                   X028,X049,X051,Y001,E033,E179WVS,E182))
summary(Peru07m)# pra verificar - p/ remover NANS
Peru07m <- subset(Peru07m, select=-c(B006,F141,E182))
Peru07d <- Peru07m
Peru07d <- Peru07d[!(is.na(Peru07d$B008)), ]
Peru07d <- Peru07d[!(is.na(Peru07d$C001)), ]
Peru07d <- Peru07d[!(is.na(Peru07d$C002)), ]
Peru07d <- Peru07d[!(is.na(Peru07d$E018)), ]
Peru07d <- Peru07d[!(is.na(Peru07d$E034)), ]
Peru07d <- Peru07d[!(is.na(Peru07d$E035)), ]
Peru07d <- Peru07d[!(is.na(Peru07d$E036)), ]
Peru07d <- Peru07d[!(is.na(Peru07d$E039)), ]
Peru07d <- Peru07d[!(is.na(Peru07d$F028)), ]
Peru07d <- Peru07d[!(is.na(Peru07d$F116)), ]
Peru07d <- Peru07d[!(is.na(Peru07d$F118)), ]
Peru07d <- Peru07d[!(is.na(Peru07d$F120)), ]
Peru07d <- Peru07d[!(is.na(Peru07d$F121)), ]
Peru07d <- Peru07d[!(is.na(Peru07d$F144_02)), ]
Peru07d <- Peru07d[!(is.na(Peru07d$G006)), ]
summary(Peru07d)#verificar

write.csv(Peru07d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/Peru07d.csv",
          row.names = FALSE)
write.csv(Peru07m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/Peru07m.csv",
          row.names = FALSE)

# Porto RIco

PRico <- read_csv("novapasta/P.Rico.csv")
PortoRico07 <- PRico %>%
  filter(S002VS == 7)
PortoRico07m <- subset(PortoRico07, select=c(B006, B008, C001, C002, E018,
                                             E034, E035, E036, E039, F028, F116, F118, F120,
                                             F121,F141,F144_02,G006,X001,X003, X025R,
                                             X028,X049,X051,Y001,E033,E179WVS,E182))
summary(PortoRico07m)# pra verificar - p/ remover NANS
PortoRico07m <- subset(PortoRico07m, select=-c(B006,F141,E182,E034, X049, X051, Y001))#E034 nao tem em Porto Rico na onda 7
PortoRico07d <- PortoRico07m
PortoRico07d <- PortoRico07d[!(is.na(PortoRico07d$B008)), ]
PortoRico07d <- PortoRico07d[!(is.na(PortoRico07d$C001)), ]
PortoRico07d <- PortoRico07d[!(is.na(PortoRico07d$C002)), ]
PortoRico07d <- PortoRico07d[!(is.na(PortoRico07d$E018)), ]
PortoRico07d <- PortoRico07d[!(is.na(PortoRico07d$E035)), ]
PortoRico07d <- PortoRico07d[!(is.na(PortoRico07d$E036)), ]
PortoRico07d <- PortoRico07d[!(is.na(PortoRico07d$E039)), ]
PortoRico07d <- PortoRico07d[!(is.na(PortoRico07d$F028)), ]
PortoRico07d <- PortoRico07d[!(is.na(PortoRico07d$F116)), ]
PortoRico07d <- PortoRico07d[!(is.na(PortoRico07d$F118)), ]
PortoRico07d <- PortoRico07d[!(is.na(PortoRico07d$F120)), ]
PortoRico07d <- PortoRico07d[!(is.na(PortoRico07d$F121)), ]
PortoRico07d <- PortoRico07d[!(is.na(PortoRico07d$F144_02)), ]
PortoRico07d <- PortoRico07d[!(is.na(PortoRico07d$G006)), ]
summary(PortoRico07d)#verificar

write.csv(PortoRico07d,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/PortoRico07d.csv",
          row.names = FALSE)
write.csv(PortoRico07m,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/bases_porpaiseonda_recod/PortoRico07m.csv",
          row.names = FALSE)
