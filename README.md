# PFTs-HMs-herbaceous
### Plant functional traits determine metals accumulation among dominant herbaceous species in an antimony mining of Karst Zone, China
### Zhongyu Du

#####R code#######
```
#----Plant functional traits manuscript---#

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


##########################################################################################
#################################     Manuscript    ######################################
##########################################################################################


########### Table 1 ############
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



########### Table 2 ############
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




########### Figure 1 ############
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


########### Figure 2 ############
rm(list = ls())
data_lm_all <- read_xlsx("data-all.xlsx", sheet = "LnRR-plant-ele-fig-lm")
data_lm_all$species <- factor(data_lm_all$species, levels = c("AC", "M", "DG", "PHC"))
str(data_lm_all)

font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))
#########All species#########
C_all <- ggplot(data_lm_all, aes(Ln_RR,C,group = HMs))+
  geom_point(size=2.5,aes(color=HMs))+
  geom_smooth(aes(color=HMs),method = 'lm', se = F, show.legend=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 12))+
  stat_poly_eq(data=data_lm_all,
               aes(Ln_RR,C,color = HMs,label=paste(..adj.rr.label..,..p.value.label..,sep = "~~~~")),
               formula = y~x,parse=T,size = 4)+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "", y = "LnRR C")+
  theme(legend.position = c(0.2, 0.15))

mm = lm(Ln_RR ~ N, data = subset(data_lm_all, HMs == "Sb"))
summary(mm)

mm = lm(Ln_RR ~ N, data = subset(data_lm_all, HMs == "As"))
summary(mm)


N_all <- ggplot(data_lm_all, aes(Ln_RR,N,group = HMs))+
  geom_point(size=2.5,aes(color=HMs))+
  geom_smooth(aes(color=HMs),method = 'lm', se = F, show.legend=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  stat_poly_eq(data=data_lm_all,
               aes(Ln_RR,N,color = HMs,label=paste(..adj.rr.label..,..p.value.label..,sep = "~~~~")),
               formula = y~x,parse=T,size = 4)+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "", y = "LnRR N")+
  theme(legend.position = "none")


P_all <- ggplot(data_lm_all, aes(Ln_RR,P,group = HMs))+
  geom_point(size=2.5,aes(color=HMs))+
  geom_smooth(aes(color=HMs),method = 'lm', se = F, show.legend=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  stat_poly_eq(data=data_lm_all,
               aes(Ln_RR,P,color = HMs,label=paste(..adj.rr.label..,..p.value.label..,sep = "~~~~")),
               formula = y~x,parse=T,size = 4)+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "", y = "LnRR P")+
  theme(legend.position = "none")

Ca_all <- ggplot(data_lm_all, aes(Ln_RR,Ca,group = HMs))+
  geom_point(size=2.5,aes(color=HMs))+
  geom_smooth(aes(color=HMs),method = 'lm', se = F, show.legend=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  stat_poly_eq(data=data_lm_all,
               aes(Ln_RR,Ca,color = HMs,label=paste(..adj.rr.label..,..p.value.label..,sep = "~~~~")),
               formula = y ~ x,parse=T,size = 4)+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "", y = "LnRR Ca")+
  theme(legend.position = "none")


ele_all_species <- cowplot::plot_grid(C_all, N_all, P_all, Ca_all, ncol = 2, align = "hv")
ele_all_species
#ggsave("ele_all_species.pdf", width = 6, height = 6)



########### Figure 3 ############
CN_all <- ggplot(data_lm_all, aes(Ln_RR,CN,group = HMs))+
  geom_point(size=2.5,aes(color=HMs))+
  geom_smooth(aes(color=HMs),method = 'lm', se = F, show.legend=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  stat_poly_eq(data=data_lm_all,
               aes(Ln_RR,CN,color=HMs,label=paste(..adj.rr.label..,..p.value.label..,sep = "~~~~")),
               formula = y~x,parse=T,size = 4)+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "", y = "LnRR C:N")+
  theme(legend.position = c(0.2, 0.15))

CP_all <- ggplot(data_lm_all, aes(Ln_RR,CP,group = HMs))+
  geom_point(size=2.5,aes(color=HMs))+
  geom_smooth(aes(color=HMs),method = 'lm', se = F, show.legend=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  stat_poly_eq(data=data_lm_all,
               aes(Ln_RR,CP,color=HMs,label=paste(..adj.rr.label..,..p.value.label..,sep = "~~~~")),
               formula = y~x,parse=T,size = 4)+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "", y = "LnRR C:P")+
  theme(legend.position = "none")

CCa_all <- ggplot(data_lm_all, aes(Ln_RR,CCa,group = HMs))+
  geom_point(size=2.5,aes(color=HMs))+
  geom_smooth(aes(color=HMs),method = 'lm', se = F, show.legend=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  stat_poly_eq(data=data_lm_all,
               aes(Ln_RR,CCa,color=HMs,label=paste(..adj.rr.label..,..p.value.label..,sep = "~~~~")),
               formula = y~x,parse=T,size = 4)+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "", y = "LnRR C:Ca")+
  theme(legend.position = "none")

NP_all <- ggplot(data_lm_all, aes(Ln_RR,NP,group = HMs))+
  geom_point(size=2.5,aes(color=HMs))+
  geom_smooth(aes(color=HMs),method = 'lm', se = F, show.legend=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  stat_poly_eq(data=data_lm_all,
               aes(Ln_RR,NP,color=HMs,label=paste(..adj.rr.label..,..p.value.label..,sep = "~~~~")),
               formula = y~x,parse=T,size = 4)+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "", y = "LnRR N:P")+
  theme(legend.position = "none")


NCa_all <- ggplot(data_lm_all, aes(Ln_RR,NCa,group = HMs))+
  geom_point(size=2.5,aes(color=HMs))+
  geom_smooth(aes(color=HMs),method = 'lm', se = F, show.legend=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  stat_poly_eq(data=data_lm_all,
               aes(Ln_RR,NCa,color=HMs,label=paste(..adj.rr.label..,..p.value.label..,sep = "~~~~")),
               formula = y~x,parse=T,size = 4)+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "", y = "LnRR N:Ca")+
  theme(legend.position = "none")

PCa_all <- ggplot(data_lm_all, aes(Ln_RR,PCa,group = HMs))+
  geom_point(size=2.5,aes(color=HMs))+
  geom_smooth(aes(color=HMs),method = 'lm', se = F, show.legend=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  stat_poly_eq(data=data_lm_all,
               aes(Ln_RR,PCa,color=HMs,
                   label=paste(..adj.rr.label..,..p.value.label..,sep = "~~~~")),
               formula = y~x,parse=T,size = 4)+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "", y = "LnRR P:Ca")+
  theme(legend.position = "none")


all_species_ele_ratio <- cowplot::plot_grid(CN_all,CP_all,CCa_all,NP_all, NCa_all, PCa_all, ncol = 2,align = "hv")
all_species_ele_ratio
#ggsave("all_species_ele_ratio.pdf",all_species_ele_ratio, width = 8, height = 10)




########### Figure 4 ############
##PCA 1 were used construct SEM
#(Chen et al., 2013 Functional Ecology)
#(Jiang et al., 2023 Science of the Total Environment)

###PCA### plant Sb/As为最终因子---使用版
rm(list = ls())
data_PCA_all <- read_xlsx("data-all.xlsx", sheet = "SEM-all-species-data")

#soil 不包括重金属
res_PCA_soil <- PCA(data_PCA_all[,3:19], scale = T)
summary(res_PCA_soil) #44.52%


#soil HMs
res_PCA_soil_HMS <- PCA(data_PCA_all[,20:23], scale = T)
summary(res_PCA_soil_HMS) #84.46%

#plant形态指标
res_PCA_plant_mor <- PCA(data_PCA_all[,36:39], scale = T)
summary(res_PCA_plant_mor) #41.81%

#plant CNPCa
res_PCA_plant_C <- PCA(data_PCA_all[,24:27], scale = T)
summary(res_PCA_plant_C)#44.06%

#plant 化学计量比
res_PCA_plant_ra <- PCA(data_PCA_all[,28:33], scale = T)
summary(res_PCA_plant_ra)#43.83%

#合并数据(变量的PC1和贡献率)
#soil_PC
data_PCA1_soil_PC_var <- data.frame(cbind(res_PCA_soil$var$coord[,1],
                                  res_PCA_soil$var$contrib[,1]))
names(data_PCA1_soil_PC_var) <- c("PC1","contrib")

data_PCA1_soil_PC_var <- data_PCA1_soil_PC_var[order(data_PCA1_soil_PC_var$'contrib', decreasing = TRUE),]

data_PCA1_soil_PC_var

#soil_HMS
data_PCA1_soil_HMS_var <- data.frame(cbind(res_PCA_soil_HMS$var$coord[,1],
                                           res_PCA_soil_HMS$var$contrib[,1]))
names(data_PCA1_soil_HMS_var) <- c("PC1","contrib")
data_PCA1_soil_HMS_var <- data_PCA1_soil_HMS_var[order(data_PCA1_soil_HMS_var$'contrib', decreasing = TRUE),]

#plant mor
data_PCA1_plant_mor_var <- data.frame(cbind(res_PCA_plant_mor$var$coord[,1],
                                            res_PCA_plant_mor$var$contrib[,1]))
names(data_PCA1_plant_mor_var) <- c("PC1","contrib")
data_PCA1_plant_mor_var <- data_PCA1_plant_mor_var[order(data_PCA1_plant_mor_var$'contrib', decreasing = TRUE),]

#plant ELE
data_PCA1_plant_ele_var <- data.frame(cbind(res_PCA_plant_C$var$coord[,1],
                                            res_PCA_plant_C$var$contrib[,1]))
names(data_PCA1_plant_ele_var) <- c("PC1","contrib")
data_PCA1_plant_ele_var <- data_PCA1_plant_ele_var[order(data_PCA1_plant_ele_var$'contrib', decreasing = TRUE),]

#plant Ratio
data_PCA1_plant_ra_var <- data.frame(cbind(res_PCA_plant_ra$var$coord[,1],
                                           res_PCA_plant_ra$var$contrib[,1]))

names(data_PCA1_plant_ra_var) <- c("PC1","contrib")
data_PCA1_plant_ra_var <- data_PCA1_plant_ra_var[order(data_PCA1_plant_ra_var$'contrib', decreasing = TRUE),]
data_PCA1_plant_ra_var


#合并数据(PC1得分-用于SEM)
data_SEM <- data.frame(cbind(res_PCA_soil$ind$coord[,1],
                             res_PCA_soil_HMS$ind$coord[,1],
                             res_PCA_plant_mor$ind$coord[,1],
                             res_PCA_plant_C$ind$coord[,1],
                             res_PCA_plant_ra$ind$coord[,1],
                             data_PCA_all[,34:35])) # Sb As

names(data_SEM) <- c("soil","SHMS","MOR","CNP","RA","Sb","As")
head(data_SEM)


###SEM Sb
model1 <- psem(
  lm(Sb ~ soil + SHMS + MOR + RA + CNP, data_SEM),
  lm(MOR ~ soil + CNP, data_SEM),
  lm(RA ~ soil + SHMS + CNP, data_SEM),
  MOR %~~% RA,
  soil %~~% SHMS,
  data_SEM
)

summary(model1)
AIC(model1, AIC.type = "dsep")


###SEM As
model2 <- psem(
  lm(As ~ soil + SHMS + RA + CNP, data_SEM),
  lm(MOR ~ soil +  CNP, data_SEM),
  lm(RA ~ soil + SHMS + CNP, data_SEM),
  MOR %~~% RA,
  soil %~~% SHMS,
  data_SEM
)

summary(model2)
AIC(model2, AIC.type = "dsep")


#随机森林模型揭示重要性(与SEM联合)
head(data_SEM)
data_SEM %>% colnames()

###Sb
set.seed(123)
#构建随机森林模型
model_Sb <- randomForest::randomForest(Sb ~ soil + SHMS + MOR + CNP + RA,
                                    data = data_SEM,
                                    ntree = 500,
                                    importance = T,
                                    proximity = T)#计算各个观测之间的相似性

##计算模型的显著性
library(rfPermute)

set.seed(123)
model_p_Sb <- rfPermute(Sb ~ soil + SHMS + MOR + CNP + RA,
                     data = data_SEM,
                     ntree = 500,
                     nrep = 1000,
                     num.cores = 1)

#提取预测变量的重要性得分(标准化后的得分)
model_p_scale_Sb <- data.frame(importance(model_p_Sb, scale = TRUE), check.names = FALSE)
model_p_scale_Sb


#模型结果转化为数据集
model_plot_data_Sb <- data.frame(tibble(Var = rownames(model_p_scale_Sb),
                                     IncMSE = model_p_scale_Sb[,1],
                                     IncMSEp = model_p_scale_Sb[,2]))
#模型结果排序(降序)
model_plot_data_Sb <- model_plot_data_Sb[order(model_plot_data_Sb$'IncMSE', decreasing = TRUE),]
model_plot_data_Sb$Var <- factor(model_plot_data_Sb$Var,
                                 levels = c("MOR","RA","CNP","soil","SHMS"),
                                 labels = c("Pla_MOR","Pla_RAT","Pla_ELE","Soil_PC","Soil_HMs"))

#加显著性
model_plot_data_Sb$col <- ifelse(model_plot_data_Sb$IncMSEp <= 0.05, "Yes","No")
model_plot_data_Sb$sig <- c("**","**","*"," "," ")
#写入重金属类型
model_plot_data_Sb$HMS <- c("Sb", "Sb", "Sb","Sb", "Sb")



###As
set.seed(123)
#构建随机森林模型
model_As <- randomForest::randomForest(As ~ soil + SHMS + MOR + CNP + RA,
                                       data = data_SEM,
                                       ntree = 500,
                                       importance = T,
                                       proximity = T)#计算各个观测之间的相似性

##计算模型的显著性
library(rfPermute)

set.seed(123)
model_p_As <- rfPermute(As ~ soil + SHMS + MOR + CNP + RA,
                        data = data_SEM,
                        ntree = 500,
                        nrep = 1000,
                        num.cores = 1)

#提取预测变量的重要性得分(标准化后的得分)
model_p_scale_As <- data.frame(importance(model_p_As, scale = TRUE), check.names = FALSE)
model_p_scale_As


#模型结果转化为数据集
model_plot_data_As <- data.frame(tibble(Var = rownames(model_p_scale_As),
                                        IncMSE = model_p_scale_As[,1],
                                        IncMSEp = model_p_scale_As[,2]))
#模型结果排序(降序)
model_plot_data_As <- model_plot_data_As[order(model_plot_data_As$'IncMSE', decreasing = TRUE),]
model_plot_data_As$Var <- factor(model_plot_data_As$Var,
                                 levels = c("MOR","RA","CNP","SHMS","soil"),
                                 labels = c("Pla_MOR","Pla_RAT","Pla_ELE","Soil_HMs","Soil_PC"))

#加显著性
model_plot_data_As$col <- ifelse(model_plot_data_As$IncMSEp <= 0.05, "Yes","No")
model_plot_data_As$sig <- c("**","**","**","*","*")
#写入重金属类型
model_plot_data_As$HMS <- c("As", "As", "As","As", "As")


#合并Sb和As数据集
model_plot_data_Sb
model_plot_data_As

model_plot_data_HMS <- merge(model_plot_data_Sb, model_plot_data_As, all = T)
model_plot_data_HMS


#绘图主题
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))


RF_Sb <- ggplot(model_plot_data_Sb, aes(Var, IncMSE, fill = Var)) +
  geom_col(width = 0.5) +
  labs(title = NULL, x = NULL, y = 'Increase in MSE (%)', fill = NULL) +
  theme_bw()+
  facet_grid(.~HMS)+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black')) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  scale_y_continuous(expand = c(0, 0), limit = c(0, 15))+
  theme(legend.position = "none")+
  geom_text(mapping = aes(y = IncMSE+0.8, label = sig), vjust = 0.5, size = 10)+
  font+
  annotate('text', label = sprintf('italic(R^2) == %.2f', 37.96),
           x = 1, y = 12, size = 6, parse = TRUE)+
  coord_flip()+
  
  theme(strip.text.x = element_text(size = 20))
RF_Sb

RF_As <- ggplot(model_plot_data_As, aes(Var, IncMSE, fill = Var)) +
  geom_col(width = 0.5) +
  labs(title = NULL, x = NULL, y = 'Increase in MSE (%)', fill = NULL) +
  theme_bw()+
  facet_grid(.~HMS)+
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = 'black')) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  scale_y_continuous(expand = c(0, 0), limit = c(0, 20))+
  theme(legend.position = "none")+
  geom_text(mapping = aes(y = IncMSE+0.8, label = sig), vjust = 0.5, size = 10)+
  font+
  annotate('text', label = sprintf('italic(R^2) == %.2f', 56.62),
           x = 1, y = 16, size = 6, parse = TRUE)+
  coord_flip()+
  theme(strip.text.x = element_text(size = 20))
RF_As

#合并两张图
fig_RF <- cowplot::plot_grid(RF_Sb,
                             RF_As,
                             align = "hv",
                             labels = c("(c)","(d)"),
                             label_size = 20)
fig_RF

#ggsave("fig_RF_SEM-0911.pdf", width = 9, height = 3.5)






##########################################################################################
############################     Supplementary Information    ############################ 
##########################################################################################
##########    Figure S2  ########
##LnRR figre--One-way ANOVA
rm(list = ls())

data_ele_RR <- read_xlsx("data-all.xlsx",sheet="LnRR-plant-ele-fig") %>% 
  mutate(Organ = as.factor(Organ),
         species = as.factor(species))

##Aboveground
data_analysis_above <- subset(data_ele_RR, Organ == "aboveground")

#C------非参数
res_above_C <- aov(C~species, data = data_analysis_above)
anova(res_above_C)

#正态性检验
cc_above_C <- residuals(res_above_C)
shapiro.test(cc_above_C)
#方差齐性检验
leveneTest(C ~ species, data = data_analysis_above)
#非参数
kruskal.test(C ~ species, data = data_analysis_above)
#多重比较
above_post_C_res <- dunnTest(C ~ species,method="bonferroni",data = data_analysis_above)
above_post_letter_C_res <- cldList(P.adj~Comparison,
                                          data = above_post_C_res$res[order(above_post_C_res$res$Z,
                                                                                   decreasing = T),],
                                          threshold = 0.05 ) 


#N
res_above_N <- aov(N~species, data = data_analysis_above)
anova(res_above_N)
#正态性检验
cc_above_N <- residuals(res_above_N)
shapiro.test(cc_above_N)
#方差齐性检验
leveneTest(N ~ species, data = data_analysis_above)
#多重比较
above_post_N_res <- HSD.test(res_above_N, "species")


#P
res_above_P <- aov(P~species, data = data_analysis_above)
anova(res_above_P)
#正态性检验
cc_above_P <- residuals(res_above_P)
shapiro.test(cc_above_P)
#方差齐性检验
leveneTest(P ~ species, data = data_analysis_above)
#多重比较
above_post_P_res <- HSD.test(res_above_P, "species")

#Ca
res_above_Ca <- aov(Ca~species, data = data_analysis_above)
anova(res_above_Ca)
#正态性检验
cc_above_Ca <- residuals(res_above_Ca)
shapiro.test(cc_above_Ca)
#方差齐性检验
leveneTest(Ca ~ species, data = data_analysis_above)
#非参数
kruskal.test(Ca ~ species, data = data_analysis_above)
#多重比较
above_post_Ca_res <- dunnTest(Ca ~ species,method="bonferroni",data = data_analysis_above)
above_post_letter_Ca_res <- cldList(P.adj~Comparison,
                                   data = above_post_Ca_res$res[order(above_post_Ca_res$res$Z,
                                                                     decreasing = T),],
                                   threshold = 0.05 ) 

#Sb
res_above_Sb <- aov(Sb~species, data = data_analysis_above)
anova(res_above_Sb)
#正态性检验
cc_above_Sb <- residuals(res_above_Sb)
shapiro.test(cc_above_Sb)
#方差齐性检验
leveneTest(Sb ~ species, data = data_analysis_above)
#多重比较
above_post_Sb_res <- HSD.test(res_above_Sb, "species")

#As
res_above_As <- aov(As~species, data = data_analysis_above)
anova(res_above_As)
#正态性检验
cc_above_As <- residuals(res_above_As)
shapiro.test(cc_above_As)
#方差齐性检验
leveneTest(As ~ species, data = data_analysis_above)
#多重比较
above_post_As_res <- HSD.test(res_above_As, "species")

#CN
res_above_CN <- aov(CN~species, data = data_analysis_above)
anova(res_above_CN)
#正态性检验
cc_above_CN <- residuals(res_above_CN)
shapiro.test(cc_above_CN)
#方差齐性检验
leveneTest(CN ~ species, data = data_analysis_above)
#多重比较
above_post_CN_res <- HSD.test(res_above_CN, "species")

#CP
res_above_CP <- aov(CP~species, data = data_analysis_above)
anova(res_above_CP)
#正态性检验
cc_above_CP <- residuals(res_above_CP)
shapiro.test(cc_above_CP)
#方差齐性检验
leveneTest(CP ~ species, data = data_analysis_above)
#多重比较
above_post_CP_res <- HSD.test(res_above_CP, "species")


#CCa-----非参数
res_above_CCa <- aov(CCa~species, data = data_analysis_above)
anova(res_above_CCa)
#正态性检验
cc_above_CCa <- residuals(res_above_CCa)
shapiro.test(cc_above_CCa)
#方差齐性检验
leveneTest(CCa ~ species, data = data_analysis_above)
#多重比较
above_post_CCa_res <- HSD.test(res_above_CCa, "species")
#非参数
kruskal.test(CCa ~ species, data = data_analysis_above)
#多重比较
above_post_CCa_res <- dunnTest(CCa ~ species,method="bonferroni",data = data_analysis_above)
above_post_letter_CCa_res <- cldList(P.adj~Comparison,
                                    data = above_post_CCa_res$res[order(above_post_CCa_res$res$Z,
                                                                       decreasing = T),],
                                    threshold = 0.05 ) 

#NP
res_above_NP <- aov(NP~species, data = data_analysis_above)
anova(res_above_NP)
#正态性检验
cc_above_NP <- residuals(res_above_NP)
shapiro.test(cc_above_NP)
#方差齐性检验
leveneTest(NP ~ species, data = data_analysis_above)
#多重比较
above_post_NP_res <- HSD.test(res_above_NP, "species")

#NCa
res_above_NCa <- aov(NCa~species, data = data_analysis_above)
anova(res_above_NCa)
#正态性检验
cc_above_NCa <- residuals(res_above_NCa)
shapiro.test(cc_above_NCa)
#方差齐性检验
leveneTest(NCa ~ species, data = data_analysis_above)
#多重比较
above_post_NCa_res <- HSD.test(res_above_NCa, "species")


#PCa
res_above_PCa <- aov(PCa~species, data = data_analysis_above)
anova(res_above_PCa)
#正态性检验
cc_above_PCa <- residuals(res_above_PCa)
shapiro.test(cc_above_PCa)
#方差齐性检验
leveneTest(PCa ~ species, data = data_analysis_above)
#多重比较
above_post_PCa_res <- HSD.test(res_above_PCa, "species")


#整合数据进行作图
#线性分析(获得置信区间并去掉截距)
CL_C <- lm(C~species-1, data = data_analysis_above)
CL_N <- lm(N~species-1, data = data_analysis_above)
CL_P <- lm(P~species-1, data = data_analysis_above)
CL_Ca <- lm(Ca~species-1, data = data_analysis_above)
CL_Sb <- lm(Sb~species-1, data = data_analysis_above)
CL_As <- lm(As~species-1, data = data_analysis_above)

CL_CN <- lm(CN~species-1, data = data_analysis_above)
CL_CP <- lm(CP~species-1, data = data_analysis_above)
CL_CCa <- lm(CCa~species-1, data = data_analysis_above)

CL_NP <- lm(NP~species-1, data = data_analysis_above)
CL_NCa <- lm(NCa~species-1, data = data_analysis_above)

CL_PCa <- lm(PCa~species-1, data = data_analysis_above)

#转换为数据集
#C-非参数
data_fig_C_CL <- data.frame(confint(CL_C))
data_fig_C_CL$Group <- c("AC","DG","M","PHC")
data_fig_C_CL <- merge(data_fig_C_CL, above_post_letter_C_res[,1:2],by = "Group", all = T)
data_fig_C_CL

#N
data_fig_N_CL <- data.frame(confint(CL_N))
data_fig_N_CL$Group <- c("AC","DG","M","PHC")
data_fig_N_CL <- merge(data_fig_N_CL[,1:3], rownames_to_column(above_post_N_res$groups,"Group")[,-2],by = "Group", all = T)
names(data_fig_N_CL) <- c("Group", "X2.5..", "X97.5..", "Letter")
data_fig_N_CL

#P
data_fig_P_CL <- data.frame(confint(CL_P))
data_fig_P_CL$Group <- c("AC","DG","M","PHC")
data_fig_P_CL <- merge(data_fig_P_CL[,1:3], rownames_to_column(above_post_P_res$groups,"Group")[,-2],by = "Group", all = T)
names(data_fig_P_CL) <- c("Group", "X2.5..", "X97.5..", "Letter")
data_fig_P_CL


#Ca---非参数
data_fig_Ca_CL <- data.frame(confint(CL_Ca))
data_fig_Ca_CL$Group <- c("AC","DG","M","PHC")
data_fig_Ca_CL <- merge(data_fig_Ca_CL, above_post_letter_Ca_res[,1:2],by = "Group", all = T)
data_fig_Ca_CL

#Sb
data_fig_Sb_CL <- data.frame(confint(CL_Sb))
data_fig_Sb_CL$Group <- c("AC","DG","M","PHC")
data_fig_Sb_CL <- merge(data_fig_Sb_CL[,1:3], rownames_to_column(above_post_Sb_res$groups,"Group")[,-2],by = "Group", all = T)
names(data_fig_Sb_CL) <- c("Group", "X2.5..", "X97.5..", "Letter")
data_fig_Sb_CL

#As
data_fig_As_CL <- data.frame(confint(CL_As))
data_fig_As_CL$Group <- c("AC","DG","M","PHC")
data_fig_As_CL <- merge(data_fig_As_CL[,1:3], rownames_to_column(above_post_As_res$groups,"Group")[,-2],by = "Group", all = T)
names(data_fig_As_CL) <- c("Group", "X2.5..", "X97.5..", "Letter")
data_fig_As_CL


#CN
data_fig_CN_CL <- data.frame(confint(CL_CN))
data_fig_CN_CL$Group <- c("AC","DG","M","PHC")
data_fig_CN_CL <- merge(data_fig_CN_CL[,1:3], rownames_to_column(above_post_CN_res$groups,"Group")[,-2],by = "Group", all = T)
names(data_fig_CN_CL) <- c("Group", "X2.5..", "X97.5..", "Letter")
data_fig_CN_CL

#CP
data_fig_CP_CL <- data.frame(confint(CL_CP))
data_fig_CP_CL$Group <- c("AC","DG","M","PHC")
data_fig_CP_CL <- merge(data_fig_CP_CL[,1:3], rownames_to_column(above_post_CP_res$groups,"Group")[,-2],by = "Group", all = T)
names(data_fig_CP_CL) <- c("Group", "X2.5..", "X97.5..", "Letter")
data_fig_CP_CL

#CCa--非参数
data_fig_CCa_CL <- data.frame(confint(CL_CCa))
data_fig_CCa_CL$Group <- c("AC","DG","M","PHC")
data_fig_CCa_CL <- merge(data_fig_CCa_CL, above_post_letter_CCa_res[,1:2],by = "Group", all = T)
data_fig_CCa_CL

#NP
data_fig_NP_CL <- data.frame(confint(CL_NP))
data_fig_NP_CL$Group <- c("AC","DG","M","PHC")
data_fig_NP_CL <- merge(data_fig_NP_CL[,1:3], rownames_to_column(above_post_NP_res$groups,"Group")[,-2],by = "Group", all = T)
names(data_fig_NP_CL) <- c("Group", "X2.5..", "X97.5..", "Letter")
data_fig_NP_CL


#NCa
data_fig_NCa_CL <- data.frame(confint(CL_NCa))
data_fig_NCa_CL$Group <- c("AC","DG","M","PHC")
data_fig_NCa_CL <- merge(data_fig_NCa_CL[,1:3], rownames_to_column(above_post_NCa_res$groups,"Group")[,-2],by = "Group", all = T)
names(data_fig_NCa_CL) <- c("Group", "X2.5..", "X97.5..", "Letter")
data_fig_NCa_CL

#PCa
data_fig_PCa_CL <- data.frame(confint(CL_PCa))
data_fig_PCa_CL$Group <- c("AC","DG","M","PHC")
data_fig_PCa_CL <- merge(data_fig_PCa_CL[,1:3], rownames_to_column(above_post_PCa_res$groups,"Group")[,-2],by = "Group", all = T)
names(data_fig_PCa_CL) <- c("Group", "X2.5..", "X97.5..", "Letter")
data_fig_PCa_CL


#合并
ele_data_above <- rbind(data_fig_C_CL,
                  data_fig_N_CL,
                  data_fig_P_CL,
                  data_fig_Ca_CL,
                  
                  data_fig_Sb_CL,
                  data_fig_As_CL,
                  
                  data_fig_CN_CL,
                  data_fig_CP_CL,
                  data_fig_CCa_CL,
                  data_fig_NP_CL,
                  
                  data_fig_NCa_CL,
                  data_fig_PCa_CL)


ele_data_above$ele <- c("C","C","C","C",
                  "N","N","N","N",
                  "P","P","P","P",
                  "Ca","Ca","Ca","Ca",
                  
                  "Sb","Sb","Sb","Sb",
                  "As","As","As","As",
                  
                  "C:N","C:N","C:N","C:N",
                  "C:P","C:P","C:P","C:P",
                  "C:Ca","C:Ca","C:Ca","C:Ca",
                  "N:P","N:P","N:P","N:P",
                  "N:Ca","N:Ca","N:Ca","N:Ca",
                  "P:Ca","P:Ca","P:Ca","P:Ca")


ele_data_above$organ <- c("Aboveground", "Aboveground", "Aboveground", "Aboveground",
                    "Aboveground", "Aboveground", "Aboveground", "Aboveground",
                    "Aboveground", "Aboveground", "Aboveground", "Aboveground",
                    "Aboveground", "Aboveground", "Aboveground", "Aboveground",
                    "Aboveground", "Aboveground", "Aboveground", "Aboveground",
                    "Aboveground", "Aboveground", "Aboveground", "Aboveground",
                    "Aboveground", "Aboveground", "Aboveground", "Aboveground",
                    "Aboveground", "Aboveground", "Aboveground", "Aboveground",
                    "Aboveground", "Aboveground", "Aboveground", "Aboveground",
                    "Aboveground", "Aboveground", "Aboveground", "Aboveground",
                    "Aboveground", "Aboveground", "Aboveground", "Aboveground",
                    "Aboveground", "Aboveground", "Aboveground", "Aboveground")



ele_data_above$mean <- (ele_data_above$X2.5.. + ele_data_above$X97.5..)/2

names(ele_data_above)<- c("species","LowCL","UPCL","Letter", "ele", "organ","Mean")
ele_data_above


##underground
data_analysis_under <- subset(data_ele_RR, Organ == "underground")

#C
res_under_C <- aov(C~species, data = data_analysis_under)
anova(res_under_C)
#正态性检验
cc_under_C <- residuals(res_under_C)
shapiro.test(cc_under_C)
#方差齐性检验
leveneTest(C ~ species, data = data_analysis_under)
#多重比较
under_post_C_res <- HSD.test(res_under_C, "species")

#非参数
kruskal.test(C ~ species, data = data_analysis_under)
#多重比较
under_post_C_res <- dunnTest(C ~ species,method="bonferroni",data = data_analysis_under)
under_post_letter_C_res <- cldList(P.adj~Comparison,
                                   data = under_post_C_res$res[order(under_post_C_res$res$Z,
                                                                     decreasing = T),],
                                   threshold = 0.05 ) 


#N
res_under_N <- aov(N~species, data = data_analysis_under)
anova(res_under_N)
#正态性检验
cc_under_N <- residuals(res_under_N)
shapiro.test(cc_under_N)
#方差齐性检验
leveneTest(N ~ species, data = data_analysis_under)
#多重比较
under_post_N_res <- HSD.test(res_under_N, "species")

#P
res_under_P <- aov(P~species, data = data_analysis_under)
anova(res_under_P)
#正态性检验
cc_under_P <- residuals(res_under_P)
shapiro.test(cc_under_P)
#方差齐性检验
leveneTest(P ~ species, data = data_analysis_under)
#多重比较
under_post_P_res <- HSD.test(res_under_P, "species")

#Ca
res_under_Ca <- aov(Ca~species, data = data_analysis_under)
anova(res_under_Ca)
#正态性检验
cc_under_Ca <- residuals(res_under_Ca)
shapiro.test(cc_under_Ca)
#方差齐性检验
leveneTest(Ca ~ species, data = data_analysis_under)
#非参数
kruskal.test(Ca ~ species, data = data_analysis_under)
#多重比较
under_post_Ca_res <- dunnTest(Ca ~ species,method="bonferroni",data = data_analysis_under)
under_post_letter_Ca_res <- cldList(P.adj~Comparison,
                                    data = under_post_Ca_res$res[order(under_post_Ca_res$res$Z,
                                                                       decreasing = T),],
                                    threshold = 0.05 ) 

#Sb
res_under_Sb <- aov(Sb~species, data = data_analysis_under)
anova(res_under_Sb)
#正态性检验
cc_under_Sb <- residuals(res_under_Sb)
shapiro.test(cc_under_Sb)
#方差齐性检验
leveneTest(Sb ~ species, data = data_analysis_under)
#多重比较
under_post_Sb_res <- HSD.test(res_under_Sb, "species")

#As----非参数
res_under_As <- aov(As~species, data = data_analysis_under)
anova(res_under_As)
#正态性检验
cc_under_As <- residuals(res_under_As)
shapiro.test(cc_under_As)
#方差齐性检验
leveneTest(As ~ species, data = data_analysis_under)
#非参数
kruskal.test(As ~ species, data = data_analysis_under)
#多重比较
under_post_As_res <- dunnTest(As ~ species,method="bonferroni",data = data_analysis_under)
under_post_letter_As_res <- cldList(P.adj~Comparison,
                                    data = under_post_Ca_res$res[order(under_post_Ca_res$res$Z,
                                                                       decreasing = T),],
                                    threshold = 0.05 ) 


#CN
res_under_CN <- aov(CN~species, data = data_analysis_under)
anova(res_under_CN)
#正态性检验
cc_under_CN <- residuals(res_under_CN)
shapiro.test(cc_under_CN)
#方差齐性检验
leveneTest(CN ~ species, data = data_analysis_under)
#多重比较
under_post_CN_res <- HSD.test(res_under_CN, "species")

#CP
res_under_CP <- aov(CP~species, data = data_analysis_under)
anova(res_under_CP)
#正态性检验
cc_under_CP <- residuals(res_under_CP)
shapiro.test(cc_under_CP)
#方差齐性检验
leveneTest(CP ~ species, data = data_analysis_under)
#多重比较
under_post_CP_res <- HSD.test(res_under_CP, "species")

#CCa------非参数
res_under_CCa <- aov(CCa~species, data = data_analysis_under)
anova(res_under_CCa)
#正态性检验
cc_under_CCa <- residuals(res_under_CCa)
shapiro.test(cc_under_CCa)
#方差齐性检验
leveneTest(CCa ~ species, data = data_analysis_under)
#多重比较
under_post_CCa_res <- HSD.test(res_under_CCa, "species")
#非参数
kruskal.test(CCa ~ species, data = data_analysis_under)
#多重比较
under_post_CCa_res <- dunnTest(CCa ~ species,method="bonferroni",data = data_analysis_under)
under_post_letter_CCa_res <- cldList(P.adj~Comparison,
                                    data = under_post_CCa_res$res[order(under_post_CCa_res$res$Z,
                                                                       decreasing = T),],
                                    threshold = 0.05 ) 


#NP------非参数
res_under_NP <- aov(NP~species, data = data_analysis_under)
anova(res_under_NP)
#正态性检验
cc_under_NP <- residuals(res_under_NP)
shapiro.test(cc_under_NP)
#方差齐性检验
leveneTest(NP ~ species, data = data_analysis_under)
#非参数
kruskal.test(NP ~ species, data = data_analysis_under)
#多重比较
under_post_NP_res <- dunnTest(NP ~ species,method="bonferroni",data = data_analysis_under)
under_post_letter_NP_res <- cldList(P.adj~Comparison,
                                    data = under_post_NP_res$res[order(under_post_Ca_res$res$Z,
                                                                       decreasing = T),],
                                    threshold = 0.05 ) 

#NCa
res_under_NCa <- aov(NCa~species, data = data_analysis_under)
anova(res_under_NCa)
#正态性检验
cc_under_NCa <- residuals(res_under_NCa)
shapiro.test(cc_under_NCa)
#方差齐性检验
leveneTest(NCa ~ species, data = data_analysis_under)
#多重比较
under_post_NCa_res <- HSD.test(res_under_NCa, "species")


#PCa
res_under_PCa <- aov(PCa~species, data = data_analysis_under)
anova(res_under_PCa)
#正态性检验
cc_under_PCa <- residuals(res_under_PCa)
shapiro.test(cc_under_PCa)
#方差齐性检验
leveneTest(PCa ~ species, data = data_analysis_under)
#多重比较
under_post_PCa_res <- HSD.test(res_under_PCa, "species")


#整合数据进行作图(underground)
#线性分析(获得置信区间并去掉截距)
CL_C <- lm(C~species-1, data = data_analysis_under)
CL_N <- lm(N~species-1, data = data_analysis_under)
CL_P <- lm(P~species-1, data = data_analysis_under)
CL_Ca <- lm(Ca~species-1, data = data_analysis_under)
CL_Sb <- lm(Sb~species-1, data = data_analysis_under)
CL_As <- lm(As~species-1, data = data_analysis_under)

CL_CN <- lm(CN~species-1, data = data_analysis_under)
CL_CP <- lm(CP~species-1, data = data_analysis_under)
CL_CCa <- lm(CCa~species-1, data = data_analysis_under)

CL_NP <- lm(NP~species-1, data = data_analysis_under)
CL_NCa <- lm(NCa~species-1, data = data_analysis_under)

CL_PCa <- lm(PCa~species-1, data = data_analysis_under)

#转换为数据集
#C-非参数
data_fig_C_CL <- data.frame(confint(CL_C))
data_fig_C_CL$Group <- c("AC","DG","M","PHC")
data_fig_C_CL <- merge(data_fig_C_CL, under_post_letter_C_res[,1:2],by = "Group", all = T)
data_fig_C_CL

#N
data_fig_N_CL <- data.frame(confint(CL_N))
data_fig_N_CL$Group <- c("AC","DG","M","PHC")
data_fig_N_CL <- merge(data_fig_N_CL[,1:3], rownames_to_column(under_post_N_res$groups,"Group")[,-2],by = "Group", all = T)
names(data_fig_N_CL) <- c("Group", "X2.5..", "X97.5..", "Letter")
data_fig_N_CL

#P
data_fig_P_CL <- data.frame(confint(CL_P))
data_fig_P_CL$Group <- c("AC","DG","M","PHC")
data_fig_P_CL <- merge(data_fig_P_CL[,1:3], rownames_to_column(under_post_P_res$groups,"Group")[,-2],by = "Group", all = T)
names(data_fig_P_CL) <- c("Group", "X2.5..", "X97.5..", "Letter")
data_fig_P_CL


#Ca---非参数
data_fig_Ca_CL <- data.frame(confint(CL_Ca))
data_fig_Ca_CL$Group <- c("AC","DG","M","PHC")
data_fig_Ca_CL <- merge(data_fig_Ca_CL, under_post_letter_Ca_res[,1:2],by = "Group", all = T)
data_fig_Ca_CL

#Sb
data_fig_Sb_CL <- data.frame(confint(CL_Sb))
data_fig_Sb_CL$Group <- c("AC","DG","M","PHC")
data_fig_Sb_CL <- merge(data_fig_Sb_CL[,1:3], rownames_to_column(under_post_Sb_res$groups,"Group")[,-2],by = "Group", all = T)
names(data_fig_Sb_CL) <- c("Group", "X2.5..", "X97.5..", "Letter")
data_fig_Sb_CL

#As---非参数
data_fig_As_CL <- data.frame(confint(CL_As))
data_fig_As_CL$Group <- c("AC","DG","M","PHC")
data_fig_As_CL <- merge(data_fig_As_CL, under_post_letter_As_res[,1:2],by = "Group", all = T)
data_fig_As_CL


#CN
data_fig_CN_CL <- data.frame(confint(CL_CN))
data_fig_CN_CL$Group <- c("AC","DG","M","PHC")
data_fig_CN_CL <- merge(data_fig_CN_CL[,1:3], rownames_to_column(under_post_CN_res$groups,"Group")[,-2],by = "Group", all = T)
names(data_fig_CN_CL) <- c("Group", "X2.5..", "X97.5..", "Letter")
data_fig_CN_CL

#CP
data_fig_CP_CL <- data.frame(confint(CL_CP))
data_fig_CP_CL$Group <- c("AC","DG","M","PHC")
data_fig_CP_CL <- merge(data_fig_CP_CL[,1:3], rownames_to_column(under_post_CP_res$groups,"Group")[,-2],by = "Group", all = T)
names(data_fig_CP_CL) <- c("Group", "X2.5..", "X97.5..", "Letter")
data_fig_CP_CL

#CCa--非参数
data_fig_CCa_CL <- data.frame(confint(CL_CCa))
data_fig_CCa_CL$Group <- c("AC","DG","M","PHC")
data_fig_CCa_CL <- merge(data_fig_CCa_CL, under_post_letter_CCa_res[,1:2],by = "Group", all = T)
data_fig_CCa_CL

#NP---非参数
data_fig_NP_CL <- data.frame(confint(CL_NP))
data_fig_NP_CL$Group <- c("AC","DG","M","PHC")
data_fig_NP_CL <- merge(data_fig_NP_CL, under_post_letter_NP_res[,1:2],by = "Group", all = T)
data_fig_NP_CL


#NCa
data_fig_NCa_CL <- data.frame(confint(CL_NCa))
data_fig_NCa_CL$Group <- c("AC","DG","M","PHC")
data_fig_NCa_CL <- merge(data_fig_NCa_CL[,1:3], rownames_to_column(under_post_NCa_res$groups,"Group")[,-2],by = "Group", all = T)
names(data_fig_NCa_CL) <- c("Group", "X2.5..", "X97.5..", "Letter")
data_fig_NCa_CL

#PCa
data_fig_PCa_CL <- data.frame(confint(CL_PCa))
data_fig_PCa_CL$Group <- c("AC","DG","M","PHC")
data_fig_PCa_CL <- merge(data_fig_PCa_CL[,1:3], rownames_to_column(under_post_PCa_res$groups,"Group")[,-2],by = "Group", all = T)
names(data_fig_PCa_CL) <- c("Group", "X2.5..", "X97.5..", "Letter")
data_fig_PCa_CL


#合并
ele_data_under <- rbind(data_fig_C_CL,
                        data_fig_N_CL,
                        data_fig_P_CL,
                        data_fig_Ca_CL,
                        
                        data_fig_Sb_CL,
                        data_fig_As_CL,
                        
                        data_fig_CN_CL,
                        data_fig_CP_CL,
                        data_fig_CCa_CL,
                        data_fig_NP_CL,
                        
                        data_fig_NCa_CL,
                        data_fig_PCa_CL)


ele_data_under$ele <- c("C","C","C","C",
                        "N","N","N","N",
                        "P","P","P","P",
                        "Ca","Ca","Ca","Ca",
                        
                        "Sb","Sb","Sb","Sb",
                        "As","As","As","As",
                        
                        "C:N","C:N","C:N","C:N",
                        "C:P","C:P","C:P","C:P",
                        "C:Ca","C:Ca","C:Ca","C:Ca",
                        "N:P","N:P","N:P","N:P",
                        "N:Ca","N:Ca","N:Ca","N:Ca",
                        "P:Ca","P:Ca","P:Ca","P:Ca")


ele_data_under$organ <- c("Underground", "Underground", "Underground", "Underground",
                          "Underground", "Underground", "Underground", "Underground",
                          "Underground", "Underground", "Underground", "Underground",
                          "Underground", "Underground", "Underground", "Underground",
                          "Underground", "Underground", "Underground", "Underground",
                          "Underground", "Underground", "Underground", "Underground",
                          "Underground", "Underground", "Underground", "Underground",
                          "Underground", "Underground", "Underground", "Underground",
                          "Underground", "Underground", "Underground", "Underground",
                          "Underground", "Underground", "Underground", "Underground",
                          "Underground", "Underground", "Underground", "Underground",
                          "Underground", "Underground", "Underground", "Underground")



ele_data_under$mean <- (ele_data_under$X2.5.. + ele_data_under$X97.5..)/2

names(ele_data_under)<- c("species","LowCL","UPCL","Letter", "ele", "organ","Mean")
ele_data_under


###合并地上部和地下部
data_fig_plant_LnRR <- rbind(ele_data_above, ele_data_under)
data_fig_plant_LnRR

data_fig_plant_LnRR$organ <- factor(data_fig_plant_LnRR$organ, levels = c("Aboveground", "Underground"))
data_fig_plant_LnRR$species <- factor(data_fig_plant_LnRR$species, levels = c("PHC", "DG","M","AC"))
data_fig_plant_LnRR$ele <- factor(data_fig_plant_LnRR$ele, 
                           levels = c("C", "N","P","Ca",
                                      "C:N","C:P","C:Ca","N:P",
                                      "N:Ca","P:Ca", "Sb", "As"))

font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

##作图
fig_ele_plant_lnRR <- ggplot(data_fig_plant_LnRR,aes(species,Mean, color = organ))+
  geom_pointrange(aes(ymin = LowCL, ymax = UPCL),size = 1.2, position = position_dodge(0.5))+
  geom_text(aes(label=Letter, y = UPCL + 0.05), size = 6.5, 
            show.legend = FALSE,position = position_dodge(0.5))+
  ylab("LnRR")+xlab("Species")+
  theme_bw()+
  coord_flip()+
  theme(axis.line = element_line(color='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  facet_wrap(.~ele,scales = "free",ncol=4)+
  
  geom_hline(yintercept = 0, size=0.8,linetype=2,color="blue")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  
  theme(legend.position="bottom",
        legend.title=element_text(size=18))+
  theme(axis.ticks.length=unit(0.2,"cm"))+
  labs(color="Organ", size = 18)+
  theme(legend.text = element_text(size = 18))

fig_ele_plant_lnRR

#ggsave("fig_ele_plant_lnRR_0912.pdf", width = 12, height = 12, dpi = 600)






##########    Figure S3 and S4  ########
setwd("F:\\博士研究生\\2021贵州晴隆\\C优势草本功能性状\\R语言计算")
data_ele<-read_xlsx("data-all.xlsx",sheet="plant-all-ele")
data_ele$black<-factor(data_ele$black,levels = c("NMS","MS"))
data_ele$species<-factor(data_ele$species)
data_ele$organ<-factor(data_ele$organ,levels = c("aboveground","underground"))
data_ele$group <- factor(data_ele$group)
head(data_ele)
data_ele
data_ele$species <- factor(data_ele$species,
                           levels = c('AC','DG','M',"PHC"),
                           labels = c("Artemisia argyi","Ficus tikoua",
                                      "Miscanthus sinensis","Ageratina adenophora"))
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

lm_Sb_As <-ggplot(data_ele, aes(Sb,As,group = species))+
  geom_point(size=2.5,aes(color=species))+
  geom_smooth(aes(color=species),method = 'lm', se = T, show.legend=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  facet_wrap(.~species,scales="free_x",nrow=2,ncol=2)+
  stat_poly_eq(data=data_ele,
               aes(Sb,As,label=paste(..adj.rr.label..,..p.value.label..,sep = "~~~~")),
               formula = y~x,parse=T,size = 5,colour="black")+
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  theme(strip.text.x = element_text(size = 20, face = "italic"))+
  theme(legend.position = "none")+
  font+
  labs(x = "Sb concentration in plant (mg kg-1)", y = "As concentration in plant (mg kg-1)")
lm_Sb_As
#ggsave("lm_Sb_As.pdf",lm_Sb_As, width = 8, height = 8, dpi = 600)



###all species
asp = ggplot(data_ele, aes(Sb,As))+
  geom_point(size=2.5,aes(color=species))+
  geom_smooth(method = 'lm', se = T, show.legend=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  stat_poly_eq(data=data_ele,
               aes(Sb,As,label=paste(..adj.rr.label..,..p.value.label..,sep = "~~~~")),
               formula = y~x,parse=T,size = 5)+
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  theme(strip.text.x = element_text(size = 20, face = "italic"))+
  theme(legend.position = "none")+
  font+
  labs(x = expression(paste("Sb concentration in plant (mg ",kg^-1,")")), 
                            y = expression(paste("As concentration in plant (mg ",kg^-1,")")))
#ggsave("All-species.pdf",asp, width = 5, height = 5)




##########    Figure S5  ########
#########Four species#########
rm(list = ls())
data_lm_all <- read_xlsx("data-all.xlsx", sheet = "LnRR-plant-ele-fig-lm")
data_lm_all$species <- factor(data_lm_all$species, levels = c("AC", "M", "DG", "PHC"))
str(data_lm_all)

font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

C <- ggplot(data_lm_all, aes(Ln_RR,C,group = HMs))+
  geom_point(size=2.5,aes(color=HMs))+
  geom_smooth(aes(color=HMs),method = 'lm', se = F, show.legend=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  facet_wrap(.~species,scales="free_x",nrow=1,ncol=4)+
  stat_poly_eq(data=data_lm_all,
               aes(Ln_RR,C,label=paste(..adj.rr.label..,..p.value.label..,sep = "~~~~")),
               formula = y~x,parse=T,size = 4,colour="black")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "", y = "LnRR C")


N <- ggplot(data_lm_all, aes(Ln_RR,N,group = HMs))+
  geom_point(size=2.5,aes(color=HMs))+
  geom_smooth(aes(color=HMs),method = 'lm', se = F, show.legend=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  facet_wrap(.~species,scales="free_x",nrow=1,ncol=4)+
  stat_poly_eq(data=data_lm_all,
               aes(Ln_RR,N,label=paste(..adj.rr.label..,..p.value.label..,sep = "~~~~")),
               formula = y~x,parse=T,size = 4,colour="black")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "", y = "LnRR N")


P <- ggplot(data_lm_all, aes(Ln_RR,P,group = HMs))+
  geom_point(size=2.5,aes(color=HMs))+
  geom_smooth(aes(color=HMs),method = 'lm', se = F, show.legend=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  facet_wrap(.~species,scales="free_x",nrow=1,ncol=4)+
  stat_poly_eq(data=data_lm_all,
               aes(Ln_RR,P,label=paste(..adj.rr.label..,..p.value.label..,sep = "~~~~")),
               formula = y~x,parse=T,size = 4,colour="black")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "", y = "LnRR P")


Ca <- ggplot(data_lm_all, aes(Ln_RR,Ca,group = HMs))+
  geom_point(size=2.5,aes(color=HMs))+
  geom_smooth(aes(color=HMs),method = 'lm', se = F, show.legend=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  facet_wrap(.~species,scales="free_x",nrow=1,ncol=4)+
  stat_poly_eq(data=data_lm_all,
               aes(Ln_RR,Ca,label=paste(..adj.rr.label..,..p.value.label..,sep = "~~~~")),
               formula = y~x,parse=T,size = 4,colour="black")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "", y = "LnRR Ca")


ele <- cowplot::plot_grid(C,N,P,Ca, ncol = 1,align = "hv")

#ggsave("ele.pdf", width = 9, height = 10)

CN <- ggplot(data_lm_all, aes(Ln_RR,CN,group = HMs))+
  geom_point(size=2.5,aes(color=HMs))+
  geom_smooth(aes(color=HMs),method = 'lm', se = F, show.legend=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  facet_wrap(.~species,scales="free_x",nrow=1,ncol=4)+
  stat_poly_eq(data=data_lm_all,
               aes(Ln_RR,CN,label=paste(..adj.rr.label..,..p.value.label..,sep = "~~~~")),
               formula = y~x,parse=T,size = 4,colour="black")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "", y = "LnRR C:N")

CP <- ggplot(data_lm_all, aes(Ln_RR,CP,group = HMs))+
  geom_point(size=2.5,aes(color=HMs))+
  geom_smooth(aes(color=HMs),method = 'lm', se = F, show.legend=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  facet_wrap(.~species,scales="free_x",nrow=1,ncol=4)+
  stat_poly_eq(data=data_lm_all,
               aes(Ln_RR,CP,label=paste(..adj.rr.label..,..p.value.label..,sep = "~~~~")),
               formula = y~x,parse=T,size = 4,colour="black")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "", y = "LnRR C:P")

CCa <- ggplot(data_lm_all, aes(Ln_RR,CCa,group = HMs))+
  geom_point(size=2.5,aes(color=HMs))+
  geom_smooth(aes(color=HMs),method = 'lm', se = F, show.legend=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  facet_wrap(.~species,scales="free_x",nrow=1,ncol=4)+
  stat_poly_eq(data=data_lm_all,
               aes(Ln_RR,CCa,label=paste(..adj.rr.label..,..p.value.label..,sep = "~~~~")),
               formula = y~x,parse=T,size = 4,colour="black")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "", y = "LnRR C:Ca")

NP <- ggplot(data_lm_all, aes(Ln_RR,NP,group = HMs))+
  geom_point(size=2.5,aes(color=HMs))+
  geom_smooth(aes(color=HMs),method = 'lm', se = F, show.legend=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  facet_wrap(.~species,scales="free_x",nrow=1,ncol=4)+
  stat_poly_eq(data=data_lm_all,
               aes(Ln_RR,NP,label=paste(..adj.rr.label..,..p.value.label..,sep = "~~~~")),
               formula = y~x,parse=T,size = 4,colour="black")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "", y = "LnRR N:P")


NCa <- ggplot(data_lm_all, aes(Ln_RR,NCa,group = HMs))+
  geom_point(size=2.5,aes(color=HMs))+
  geom_smooth(aes(color=HMs),method = 'lm', se = F, show.legend=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  facet_wrap(.~species,scales="free_x",nrow=1,ncol=4)+
  stat_poly_eq(data=data_lm_all,
               aes(Ln_RR,NCa,label=paste(..adj.rr.label..,..p.value.label..,sep = "~~~~")),
               formula = y~x,parse=T,size = 4,colour="black")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "", y = "LnRR N:Ca")

PCa <- ggplot(data_lm_all, aes(Ln_RR,PCa,group = HMs))+
  geom_point(size=2.5,aes(color=HMs))+
  geom_smooth(aes(color=HMs),method = 'lm', se = F, show.legend=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  facet_wrap(.~species,scales="free_x",nrow=1,ncol=4)+
  stat_poly_eq(data=data_lm_all,
               aes(Ln_RR,PCa,
                   label=paste(..adj.rr.label..,..p.value.label..,sep = "~~~~")),
               formula = y~x,parse=T,size = 4)+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "", y = "LnRR P:Ca")


ele_ratio <- cowplot::plot_grid(CN,CP,CCa,NP, NCa, PCa, ncol = 1,align = "hv")
ele_ratio
#ggsave("ele_ratio.pdf", width = 9, height = 16)

```
