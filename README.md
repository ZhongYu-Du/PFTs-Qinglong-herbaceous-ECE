# PFTs-HMs-herbaceous
### Plant functional traits determine metals accumulation among dominant herbaceous species in an antimony mining of Karst Zone, China
### Zhongyu Du

#####R code#######
```
#----Plant functional traits manuscript---#
#-----Soil properties-------#
rm(list=ls())
options(scipen = 200)
{
  library(FD)
  library(readxl)
  library(ggplot2)
  library(ggpubr)
  library(cowplot)
  library(ggthemes)
  library(dplyr)
  library(car)
  library(lme4)
  library(lmerTest)
  library(agricolae)
  library(rcompanion)
  library(FSA)
  library(FactoMineR)
  library(factoextra)
  library(ggrepel)
  library(smatr)
  library(corrplot)
  library(Hmisc)
  library(ggplot2)
  library(ggcorrplot)
  library(merTools)
  library(multcompView)
  library(tidyverse)
  library(HH)
  library(doBy)
  library(tibble)
  library(ggpmisc)
  library(piecewiseSEM)
}


####------Table 1  Soil-----####
rm(list = ls())
data_soil<-read_xlsx("data-all.xlsx",sheet="Soil-all")
data_soil$black<-factor(data_soil$black,levels = c("NMS","MS"))

#shapiro
attach(data_soil)
shapiro.test(SWC)#
shapiro.test(pH)#
shapiro.test(SOM)#

shapiro.test(TN)
shapiro.test(TP)
shapiro.test(TK)
shapiro.test(AN)
shapiro.test(AP)
shapiro.test(AK)

shapiro.test(TCa)
shapiro.test(TSb)
shapiro.test(TAs)
shapiro.test(ECa) #
shapiro.test(ASb)
shapiro.test(AAs)

shapiro.test(CN)
shapiro.test(CP)
shapiro.test(NP)
shapiro.test(CCa)
shapiro.test(NCa)
shapiro.test(PCa)


##One-way ANOVA
#pH
res_ph <- aov(pH~black)
summary(res_ph)

#
cc_ph <- residuals(res_ph)
shapiro.test(cc_ph)
#
leveneTest(pH~black)

#SWC
res_swc <- aov(SWC~black)

summary(res_swc)

#
cc_swc <- residuals(res_swc)
shapiro.test(cc_swc)
#
leveneTest(SWC~black)

#SOM
res_som <- lm(SOM~black)
summary(res_som)
anova(res_som)

#
cc_som <- residuals(res_som)
shapiro.test(cc_som)
#
leveneTest(SOM~black)

#TN(log(TN))
res_TN <- lm(log(TN)~black)
summary(res_TN)
anova(res_TN)

#
cc_TN <- residuals(res_TN)
shapiro.test(cc_TN)
#
leveneTest(log(TN)~black)


#TP(log(TP))
res_TP <- lm(log(TP)~black)
summary(res_TP)
anova(res_TP)

#
cc_TP <- residuals(res_TP)
shapiro.test(cc_TP)
#
leveneTest(log(TP)~black)

#TK
res_TK <- lm(log(TK)~black)
summary(res_TK)
anova(res_TK)

#
cc_TK <- residuals(res_TK)
shapiro.test(cc_TK)
#
leveneTest(TK~black)

#AN log(AN)
res_AN <- lm(log(AN)~black)
summary(res_AN)
anova(res_AN)

#
cc_AN <- residuals(res_AN)
shapiro.test(cc_AN)
#
leveneTest(log10(AN)~black)

#AP--------------------
res_AP <- lm(log10(AP)~black)
summary(res_AP)
anova(res_AP)

#
cc_AP <- residuals(res_AP)
shapiro.test(cc_AP)
#
leveneTest(log10(AP)~black)

wilcox.test(AP~black, paired=F)



#AK-------------------
res_AK <- lm(log10(AK)~black)
summary(res_AK)
anova(res_AK)

#
cc_AK <- residuals(res_AK)
shapiro.test(cc_AK)
#
leveneTest(log(AK)~black)

wilcox.test(AK~black)


#TCa log(TCa)
res_TCa <- lm(log(TCa)~black)
summary(res_TCa)
anova(res_TCa)

#
cc_TCa <- residuals(res_TCa)
shapiro.test(cc_TCa)
#
leveneTest(log(TCa)~black)

#TSb log(TSb)--------------------
res_TSb <- lm(log10(TSb)~black)
summary(res_TSb)
anova(res_TSb)

#
cc_TSb <- residuals(res_TSb)
shapiro.test(cc_TSb)
#
leveneTest(log(TSb)~black)
wilcox.test(TSb~black)


#TAs log(TAs)
res_TAs <- lm(log(TAs)~black)
summary(res_TAs)
anova(res_TAs)

#
cc_TAs <- residuals(res_TAs)
shapiro.test(cc_TAs)
#
leveneTest(log(TAs)~black)

#ASb log(ASb)--------------------
res_ASb <- lm(log10(ASb)~black)
summary(res_ASb)
anova(res_ASb)

#
cc_ASb <- residuals(res_ASb)
shapiro.test(cc_ASb)
#
leveneTest(log10(ASb)~black)
wilcox.test(ASb~black)

#AAs log(AAs)
res_AAs <- lm(log10(AAs)~black)
summary(res_AAs)
anova(res_AAs)

#
cc_AAs <- residuals(res_AAs)
shapiro.test(cc_AAs)
#
leveneTest(log10(AAs)~black)

#ECa
res_ECa <- lm(ECa~black)
summary(res_ECa)
anova(res_ECa)

#
cc_ECa <- residuals(res_ECa)
shapiro.test(cc_ECa)
#
leveneTest(ECa~black)


#C:N--------------------
res_CN <- lm(CN~black)
summary(res_CN)
anova(res_CN)

#
cc_CN <- residuals(res_CN)
shapiro.test(cc_CN)
#
leveneTest(log10(CN)~black)
wilcox.test(CN~black)

#C:P log(CP)
res_CP <- lm(log(CP)~black)
summary(res_CP)
anova(res_CP)

#
cc_CP <- residuals(res_CP)
shapiro.test(cc_CP)
#
leveneTest(log(CP)~black)

#C:Ca log(CCa)
res_CCa <- lm(log(CCa)~black)
summary(res_CCa)
anova(res_CCa)


#
cc_CCa <- residuals(res_CCa)
shapiro.test(cc_CCa)
#
leveneTest(log(CCa)~black)

#N:P log(NP)
res_NP <- lm(log(NP)~black)
summary(res_NP)
anova(res_NP)

#
cc_NP <- residuals(res_NP)
shapiro.test(cc_NP)
#
leveneTest(log(NP)~black)


#N:Ca log(NCa)----
res_NCa <- lm(log(NCa)~black)
summary(res_NCa)
anova(res_NCa)

#
cc_NCa <- residuals(res_NCa)
shapiro.test(cc_NCa)
#
leveneTest(log(NCa)~black)
#
wilcox.test(NCa~black)


#P:Ca log(PCa)---
res_PCa <- lm(log(PCa)~black)
summary(res_PCa)
anova(res_PCa)

#
cc_PCa <- residuals(res_PCa)
shapiro.test(cc_PCa)
#
leveneTest(log(PCa)~black)
#
wilcox.test(PCa~black)



#####-----------Table 2   Plant functional traits-------######
##PFTs
rm(list = ls())
data_pfts <- read_xlsx("data-all.xlsx",sheet="plant-trait") %>% 
  mutate(black = as.factor(black),
         species = as.factor(species))

###Two-way ANOVA
attach(data_pfts)
#height ---------------------非参数
res_two_height <- lm(log10(Height) ~ black * species)
summary(res_two_height)
anova(res_two_height)
#正态性检验
cc_two_height <- residuals(res_two_height)
shapiro.test(cc_two_height)
#方差齐性检验
leveneTest(log10(Height) ~ black * species)
#非参数
scheirerRayHare(Height ~ black * species, data = data_pfts)

#LT ---------------------非参数
res_two_LT <- lm(log10(LT) ~ black * species)
summary(res_two_LT)
anova(res_two_LT)
#正态性检验
cc_two_LT <- residuals(res_two_LT)
shapiro.test(cc_two_LT)
#方差齐性检验
leveneTest(log10(LT) ~ black * species)
#非参数
scheirerRayHare(LT ~ black * species, data = data_pfts)

#LA ---------------------非参数
res_two_LA <- lm(log10(LA) ~ black * species)
summary(res_two_LA)
anova(res_two_LA)
#正态性检验
cc_two_LA <- residuals(res_two_LA)
shapiro.test(cc_two_LA)
#方差齐性检验
leveneTest(log10(LA) ~ black * species)
#非参数
scheirerRayHare(LA ~ black * species, data = data_pfts)


#SLA ---------------------非参数
res_two_SLA <- lm(log10(SLA) ~ black * species)
summary(res_two_SLA)
anova(res_two_SLA)
#正态性检验
cc_two_SLA <- residuals(res_two_SLA)
shapiro.test(cc_two_SLA)
#方差齐性检验
leveneTest(log10(SLA) ~ black * species)
#非参数
scheirerRayHare(SLA ~ black * species, data = data_pfts)

####----four species analysis------###
#One-way ANOVA
####sign analysis
##AC
data_pfts_AC <- subset(data_pfts, species == "AC")
attach(data_pfts_AC)
#height
res_AC_height <- lm(Height~black)
summary(res_AC_height)
anova(res_AC_height)

#正态性检验
cc_AC_height <- residuals(res_AC_height)
shapiro.test(cc_AC_height)
#方差齐性检验
leveneTest(Height~black)

#LT
res_AC_LT <- lm(LT~black)
summary(res_AC_LT)
anova(res_AC_LT)

#正态性检验
cc_AC_LT <- residuals(res_AC_LT)
shapiro.test(cc_AC_LT)
#方差齐性检验
leveneTest(LT~black)

#LA----------非参数
res_AC_LA <- lm(LA~black)
summary(res_AC_LA)
anova(res_AC_LA)

#正态性检验
cc_AC_LA <- residuals(res_AC_LA)
shapiro.test(cc_AC_LA)
#方差齐性检验
leveneTest(log(LA)~black)

wilcox.test(LA~black, exact=FALSE)

#SLA----------非参数
res_AC_SLA <- lm(SLA~black)
summary(res_AC_SLA)
anova(res_AC_SLA)

#正态性检验
cc_AC_SLA <- residuals(res_AC_SLA)
shapiro.test(cc_AC_SLA)
#方差齐性检验
leveneTest(log(SLA)~black)

wilcox.test(SLA~black, exact=FALSE)

##M
data_pfts_M <- subset(data_pfts, species == "M")
attach(data_pfts_M)
#height
res_M_height <- lm(log(Height)~black)
summary(res_M_height)
anova(res_M_height)

#正态性检验
cc_M_height <- residuals(res_M_height)
shapiro.test(cc_M_height)
#方差齐性检验
leveneTest(log(Height)~black)

#LT
res_M_LT <- lm(LT~black)
summary(res_M_LT)
anova(res_M_LT)

#正态性检验
cc_M_LT <- residuals(res_M_LT)
shapiro.test(cc_M_LT)
#方差齐性检验
leveneTest(LT~black)

#LA
res_M_LA <- lm(log(LA)~black)
summary(res_M_LA)
anova(res_M_LA)

#正态性检验
cc_M_LA <- residuals(res_M_LA)
shapiro.test(cc_M_LA)
#方差齐性检验
leveneTest(log(LA)~black)


#SLA
res_M_SLA <- lm(SLA~black)
summary(res_M_SLA)
anova(res_M_SLA)

#正态性检验
cc_M_SLA <- residuals(res_M_SLA)
shapiro.test(cc_M_SLA)
#方差齐性检验
leveneTest(log(SLA)~black)

##DG
data_pfts_DG <- subset(data_pfts, species == "DG")
attach(data_pfts_DG)
#height-------------非参数
res_DG_height <- lm(log10(Height)~black)
summary(res_DG_height)
anova(res_DG_height)

#正态性检验
cc_DG_height <- residuals(res_DG_height)
shapiro.test(cc_DG_height)
#方差齐性检验
leveneTest(log(Height)~black)

wilcox.test(Height~black, exact=FALSE)

#LT---------非参数
res_DG_LT <- lm(LT~black)
summary(res_DG_LT)
anova(res_DG_LT)

#正态性检验
cc_DG_LT <- residuals(res_DG_LT)
shapiro.test(cc_DG_LT)
#方差齐性检验
leveneTest(LT~black)

wilcox.test(LT~black, exact=FALSE)

#LA--------非参数
res_DG_LA <- lm(log(LA)~black)
summary(res_DG_LA)
anova(res_DG_LA)

#正态性检验
cc_DG_LA <- residuals(res_DG_LA)
shapiro.test(cc_DG_LA)
#方差齐性检验
leveneTest(log(LA)~black)

wilcox.test(LA~black, exact = F)

#SLA
res_DG_SLA <- lm(log(SLA)~black)
summary(res_DG_SLA)
anova(res_DG_SLA)

#正态性检验
cc_DG_SLA <- residuals(res_DG_SLA)
shapiro.test(cc_DG_SLA)
#方差齐性检验
leveneTest(log(SLA)~black)

##PHC
data_pfts_PHC <- subset(data_pfts, species == "PHC")
attach(data_pfts_PHC)
#height
res_PHC_height <- lm(log10(Height)~black)
summary(res_PHC_height)
anova(res_PHC_height)

#正态性检验
cc_PHC_height <- residuals(res_PHC_height)
shapiro.test(cc_PHC_height)
#方差齐性检验
leveneTest(log10(Height)~black)


#LT---------非参数
res_PHC_LT <- lm(LT~black)
summary(res_PHC_LT)
anova(res_PHC_LT)

#正态性检验
cc_PHC_LT <- residuals(res_PHC_LT)
shapiro.test(cc_PHC_LT)
#方差齐性检验
leveneTest(LT~black)

wilcox.test(LT~black, exact=FALSE)

#LA
res_PHC_LA <- lm(log(LA)~black)
summary(res_PHC_LA)
anova(res_PHC_LA)

#正态性检验
cc_PHC_LA <- residuals(res_PHC_LA)
shapiro.test(cc_PHC_LA)
#方差齐性检验
leveneTest(log(LA)~black)

#SLA
res_PHC_SLA <- lm(log(SLA)~black)
summary(res_PHC_SLA)
anova(res_PHC_SLA)

#正态性检验
cc_PHC_SLA <- residuals(res_PHC_SLA)
shapiro.test(cc_PHC_SLA)
#方差齐性检验
leveneTest(log(SLA)~black)

detach(data_pfts_PHC)




######     Fig.1   Plant functional traits ####
#作图
data_pfts_fig<-read_xlsx("data-all.xlsx", sheet="plant-1-3")
data_pfts_fig$black<-factor(data_pfts_fig$black,levels = c("NMS","MS"))
data_pfts_fig$Var<-factor(data_pfts_fig$Var,levels=c("Height","LT",
                               "LA","SLA"))
data_pfts_fig$species<-factor(data_pfts_fig$species,levels = c("Artemisia_argyi","Miscanthus_sinensis",
                                         "Ficus_tikoua","Ageratina_adenophora"))


labs <- c("Height (cm)","Leaf thickness (mm)", "Leaf area (cm2)", "Specific leaf area (cm2/g)")

names(labs) <- c("Height","LT", "LA", "SLA")

pfts_fig1 <- ggplot(data_pfts_fig,aes(black, Value))+
  geom_boxplot(aes(color = black, fill = black, alpha = 0.2),
               size = 0.8)+
  stat_summary(fun="mean",geom="point",
               shape=20,size=3,
               color="black",alpha=1)+
  ylab("Plant functional traits")+xlab("Sites")+
  theme_bw()+
  
  theme(axis.line = element_line(color='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  facet_grid(Var~species,scales = "free",
             labeller = labeller(Var = labs))+
  
  theme(legend.position="none")+
  theme(axis.ticks.length=unit(0.2,"cm"))+
  theme(strip.text.x = element_text(size = 14, face = "italic"),
        strip.text.y = element_text(size = 14))+
  #theme(legend.text=element_text(size=100))+
  theme(title=element_text(size=18))+
  theme(axis.text.x = element_text(size = 14, color = "black"))+
  theme(axis.text.y = element_text(size = 14, color = "black"))
pfts_fig1
#ggsave("pfts_fig1.pdf", width = 12, height = 11)

![image](https://github.com/ZhongYu-Du/PFTs-HMs-herbaceous/assets/59045023/4037d00d-293f-46be-aeea-1346710e58ec)





```
