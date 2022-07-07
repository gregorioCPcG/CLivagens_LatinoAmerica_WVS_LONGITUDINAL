# merge

library(haven)
merge <- read_sav("moreno_clivagem_merge.sav")

str(merge) # para consultar labels das variáveis

moreno_clivagem_chile1990 <- read_sav("onda 2_89-93/moreno_clivagem_chile1990.sav") #usei essa do Lucas para ficar igual

library(tidyverse)


argentina <- merge %>%
  filter(COUNTRY_ALPHA == "ARG")

#fiz comparaçoes argentina vs a do lucas

table(merge$COUNTRY_ALPHA)

library(labelled)
argentina <- remove_labels(argentina)
#str(argentina)



write.csv(argentina,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/novapasta/argentina.csv",
          row.names = FALSE)

#
Bolivia <- merge %>%
  filter(COUNTRY_ALPHA == "BOL")

Bolivia <- remove_labels(Bolivia)


write.csv(Bolivia,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/novapasta/Bolivia.csv",
          row.names = FALSE)

# mais países
#chile, brazi e uru já

Colombia <- merge %>%
  filter(COUNTRY_ALPHA == "COL")

Colombia <- remove_labels(Colombia)

write.csv(Colombia,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/novapasta/Colombia.csv",
          row.names = FALSE)

#
equador <- merge %>%
  filter(COUNTRY_ALPHA == "ECU")

equador <- remove_labels(equador)

write.csv(equador,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/novapasta/equador.csv",
          row.names = FALSE)

#

haiti <- merge %>%
  filter(COUNTRY_ALPHA == "HTI")

haiti <- remove_labels(haiti)

summary(haiti$S002VS)#só a 6


write.csv(haiti,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/novapasta/haiti.csv",
          row.names = FALSE)
#

mexico <- merge %>%
  filter(COUNTRY_ALPHA == "MEX")

mexico <- remove_labels(mexico)

write.csv(mexico,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/novapasta/mexico.csv",
          row.names = FALSE)
#

nicaragua <- merge %>%
  filter(COUNTRY_ALPHA == "NIC")

nicaragua <- remove_labels(nicaragua)


write.csv(nicaragua,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/novapasta/nicaragua.csv",
          row.names = FALSE)
#

Peru <- merge %>%
  filter(COUNTRY_ALPHA == "PER")

Peru <- remove_labels(Peru)


write.csv(Peru,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/novapasta/Peru.csv",
          row.names = FALSE)
#

P.Rico <- merge %>%
  filter(COUNTRY_ALPHA == "PRT")

P.Rico <- remove_labels(P.Rico)


write.csv(P.Rico,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/novapasta/P.Rico.csv",
          row.names = FALSE)

#


Venezu <- merge %>%
  filter(COUNTRY_ALPHA == "VEN")

Venezu <- remove_labels(Venezu)


write.csv(Venezu,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/novapasta/Venezu.csv",
          row.names = FALSE)




#

#Guatemala
#

guatemala <- merge %>%
  filter(COUNTRY_ALPHA == "GTM")

guatemala <- remove_labels(guatemala)


write.csv(guatemala,
          "D:/SCRIPTS TALK ABOUT/WVS/TAREFAS/novapasta/guatemala.csv",
          row.names = FALSE)

#summary(moreno_clivagem_chile1990[,24:44])
#summary(arg02[,25:45])
