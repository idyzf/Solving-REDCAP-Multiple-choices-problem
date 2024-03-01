library(openxlsx)
library(dplyr)
rm(list=ls())
clinical_features<- read.xlsx("clinical_v4.xlsx")
names(clinical_featues)
'%ni%' <- Negate("%in%") 
BD_v <- clinical_features %>% 
  dplyr::select(c(ID), contains('Locali')) %>% 
  tidyr::pivot_longer(!ID) %>% 
  dplyr::filter(value == 'Checked')

BD_v %>% 
  dplyr::count(ID)

## Ideli voltou
summary(as.factor(BD_v$name))
BD_v$loc <- BD_v$name
BD_v <- BD_v %>% mutate(loc = recode(loc,  "Localização.(choice=Frontal)" = "Frontal", 
                                     "Localização.(choice=Multicêntrico)" = "Multicentrico",
                                     "Localização.(choice=Multifocal)" = "Miltifocal",
                                     "Localização.(choice=Occipital)"= "Occipital",
                                     "Localização.(choice=Outro)" = "Outro",
                                     "Localização.(choice=Parietal)"="Parietal",
                                     "Localização.(choice=Temporal)" = "Temporal"))

summary(as.factor(BD_v$loc))

contagem <- BD_v %>%  dplyr::count(ID)
contagem$ene <- contagem$n
contagem1 <- subset(contagem, contagem$ene != 1)
contagem1$ene <- "More Than one"
contagem2<- subset(BD_v, BD_v$ID %ni% contagem1$ID)
contagem1$loc <- contagem1$ene
contagem1 <- contagem1[,-2:-3]
contagem2 <- contagem2[,-2:-3]
Loc <- rbind(contagem1, contagem2)

dados<- left_join(clinical_features, Loc)
clinical_features_unique <- distinct(clinical_features)
dados <- left_join(clinical_features_unique, Loc, by = "ID")
names(dados)
clinical_stepwise <- dados[,c(1, 30, 53, 39, 83, 34, 18, 10, 75, 20)]
write.xlsx(clinical_stepwise, "clinical_stepwise_variables.xlsx")
