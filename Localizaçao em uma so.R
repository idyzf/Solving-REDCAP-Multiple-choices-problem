library(openxlsx)

clinical_featues<- read.xlsx("clinical_v2_mutvswt.xlsx")
names(clinical_featues)
clinical_featues<- clinical_featues[-141:-145,]
summary(as.factor(clinical_featues$IDH1))

'%ni%' <- Negate("%in%") 
clinical_wt <- read.xlsx("~/Library/CloudStorage/GoogleDrive-ideli.zanesco@gmail.com/Meu Drive/Mestrado/ideli (Drive lu)/completo-ruvIII/clinical_v4-22-05-2023.xlsx")
patients_wt <- clinical_wt$ID

sobrando<- subset(clinical_featues, clinical_featues$ID %ni% patients_wt)
summary(as.factor(sobrando$IDH1))
swt<- subset(sobrando, sobrando$IDH1=='Wild Type')
sobrando_wt

clinical_featues<- subset(clinical_featues, clinical_featues$ID %ni% swt$ID)
wt <- subset(clinical_featues, clinical_featues$IDH1 == "Wild Type") 
mut <-subset(clinical_featues, clinical_featues$IDH1 == "Mutated")
m<- median(mut$Idade.no.diagnóstico)
summary(mut$Idade.no.diagnóstico)
summary(as.factor(mut$Lateralidade))
#Welinton resolvendo o problema da localização
BD_v <- wt %>% 
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

#Welinton resolvendo o problema da localização
BD_v <- mut %>% 
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
Locmut <- rbind(contagem1, contagem2)

Loctotal <- rbind(Loc, Locmut)

mut<- left_join(mut, Locmut)
wt<- left_join(wt, Loc)

summary(as.factor(mut$Sobrevida.Global))
summary(mut$Sobrevida.Global)
