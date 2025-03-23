# PFTs-Metal(loid)s-herbaceous
### Plant traits regulated metal(loid)s in dominant herbs in an antimony mining area of the Karst Zone, China
### Zhongyu Du

### Abstract
Understanding how plant functional traits respond to mining activities and impact metal(loid) accumulation in dominant species is crucial for exploring the driving mechanisms behind plant community succession and predicting the ecological restoration potential of these plants. In this study, we investigated four dominant herbaceous species (Artemisia argyi, Miscanthus sinensis, Ficus tikoua, and Ageratina Adenophora) growing on antimony (Sb) mining sites (MS) with high Sb and arsenic (As) levels, as well as non-mining sites (NMS). The aim was to analyze the variations in functional traits and their contribution to Sb and As concentrations in plants. Our results indicate that mining activities enhanced soil nitrogen (N) limitation, phosphorus (P) enrichment, while significantly reducing the plant height of three species, except for F. tikoua. The four species absorbed more calcium (Ca) to ensure higher tolerance to Sb and As levels, which is related to the activation of Ca signaling pathways and defense mechanisms. Furthermore, plant Sb and As concentrations were dependent on soil metal(loid) levels and plant element stoichiometry. Overall, these findings highlight the regulatory role of plant element traits in metal(loid) concentrations, warranting widespread attention and further study in the future.

## Description of the data and file structure
Soil-all: The soil dataset of this study. Site and black are the sampling sites (SK and SFK) and area (mining site and no-mining site). SWC, soil water content (%); pH, soil pH; TN, total nitrogen content (g kg-1); TP, total phosphorus content (g kg-1); TK, total potassium content (g kg-1); AN, available nitrogen content (mg kg-1); AP, available phosphorus content (mg kg-1); AK, available potassium content (mg kg-1); TCa, available potassium content (mg kg-1); TSb, total antimony content (mg kg-1); TAs, total arsenic content (mg kg-1); ASb, available antimony content (mg kg-1); AAs, available arsenic content (mg kg-1); ECa, exchangeable calcium content (mg kg-1). CN, C:N ratio; CP, C:P ratio; CCa, C:Ca ratio; NP, N:P ratio; NCa, N:Ca ratio; PCa, P:Ca ratio.

plant-trait: Site and black are the sampling sites (SK and SFK) and area (mining site and no-mining site). Species: AC is Artemisia argyi; M is Miscanthus sinensis; PHC is Ageratina Adenophora; DG is Ficus tikoua. Height is plant natural height (cm); SLA is plant leaf specific leaf area (mm2 mg-1); LT is plant leaf thickness (mm); LA is plant leaf area (cm2).

plant-1-3: The same as plant trait dataset.

LnRR-plant-ele-fig-lm: The plant organs (aboveground and underground parts) metals (Sb and As) content, nutrient elements and element ratios.

SEM-all-species-data: All data of the soil and species.

plant-all-ele: The same as LnRR-plant-ele-fig-lm dataset.
### R code
```
############################  Plant functional traits   ###################################
# packages
rm(list=ls())
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
  library(rsq)
  library(rsq)
  library(performance)
  library(glmmTMB)
  library(glmm.hp)
}

############################  Figure 1 map   ###################################

#Laoding packages
library(sf)
library(tidyverse)
library(ggspatial)
library(ggplot2)
library(cowplot)
library(colorspace)
library(terra)
library(raster)


setwd("F:\\Thesis_data\\Method_map")
site <- read.csv("site_plot.csv")

china_pro <- sf::st_read("中华人民共和国.json")
qinglong = sf::st_read("晴隆县.json")

font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

China_overall = ggplot(data = china_pro) + 
  geom_sf(size=1,fill = "gray90",color="black") +
  theme_bw()+
  font+
  annotation_scale(location='bl') +
  # 添加指北针
  annotation_north_arrow(location = "tl", which_north = "false",
                         style = north_arrow_fancy_orienteering) +
  geom_point(data = site, aes(x = Lat, y = Lon), color = "red", size = 2)
China_overall
#ggsave("China_overall1.pdf", China_overall, width = 8, height = 8)


####DEM
xfun::dir_create("F:Thesis_data\\Method_map")

chn_map <- raster::getData(name = "alt", res = 2.5, country = "CHN", mask = TRUE, path = "F:\\博士研究生\\Thesis_data\\Method_map")
plot(chn_map)

####GZQL#####
ql_cj = crop(chn_map, qinglong)
ql_ym = mask(ql_cj, qinglong)
plot(ql_ym)

# as data frame
data_GZQL = data.frame(rasterToPoints(ql_ym$CHN_msk_alt))
colnames(data_GZQL) = c("Longitude", "Latitude", "DEM")
max(data_GZQL$DEM)
min(data_GZQL$DEM)

site_gz <- subset(site, Site == "GZQL")

map_GZQL <- ggplot()+
  geom_sf(data = qinglong, fill = NA, color = NA)+
  annotation_north_arrow(location="tr", style = north_arrow_nautical(fill = c("grey40","white"),line_col = "grey20"))+  # 添加指北针
  geom_tile(data=data_GZQL,aes(x=Longitude, y=Latitude,fill=DEM),show.legend = T)+
  theme_few()+
  geom_point(data = site_gz, aes(x = Lat, y = Lon), size = 3) + 
  xlab("Longitude (E °)") + ylab("Latitude (N °)") + 
  annotation_scale(location = "br", text_cex = 1.3) + font +
  scale_fill_gradientn(colours = terrain.colors(1000),limits = c(600, 2000), breaks = seq(600, 2000, by = 700),name = "Altitude (m)")+
  geom_point(data = site_gz, aes(x = Lat, y = Lon, color = Type), size = 3) +
  scale_color_manual(values = c("blue", "red"))+
  labs(color = "")+
  theme(legend.text=element_text(size=18),
        legend.title=element_text(size=14,face="bold"),
        axis.line = element_line(colour = "black"), 
        panel.background = element_rect(fill = 'white', colour = 'black'))

map_GZQL

ggsave("map_GZQL.pdf", map_GZQL, height = 8)


#############################  Soil parts #####################################
rm(list = ls())
data_soil<-read_xlsx("data-all.xlsx",sheet="Soil-all") %>% as.data.frame()
data_soil$site<-factor(data_soil$site,levels = c("NMS","MS"))
data_soil$block<-factor(data_soil$block)
data_soil$plot <- factor(data_soil$plot)
head(data_soil)

###  Mean/n/SE
du_stats <- function(x){
  mean<-mean(x)
  n<-length(x)
  se<-sd(x)/sqrt(n)
  v<-var(x)
  return(c(mean=mean, se=se,n=n,V=v))
}
mean_res <- summaryBy(SWC+pH+SOM+TN+TP+TK+AN+AP+AK+
                        TCa+TSb+TAs+ECa+ASb+AAs+
                        CN+CP+NP+CCa+NCa+PCa~site, data = data_soil, FUN = du_stats)


#write.csv(mean_res,"Table 1 Soil mean se .csv")


### Residual normality test
res = NULL
nn = names(data_soil)[-c(1:3)]

for(i in seq_along(nn)){
  a <- lm(formula(paste0(nn[i], "~ site")), data = data_soil)
  aa <- shapiro.test(a$residuals)
  aaa <- cbind(aa$statistic, aa$p.value)
  res[[i]] = aaa
}
names(res) = nn
norm_soil_res <- as.data.frame(t(sapply(res, "[", i = 1:max(sapply(res, length)))))
colnames(norm_soil_res) <- c("W", "p-value")
norm_soil_res$name <- rownames(norm_soil_res)
norm_soil_res$sig <- ifelse(norm_soil_res$`p-value` > 0.05, "", "SIG")
norm_soil_res

### All LMMs
lmer_res = NULL
n = names(data_soil)[-c(1:3)]

for(i in seq_along(n)){
  lmer_mod <- lmer(formula(paste0(n[i], "~ site + (1|block)")), data = data_soil)
  summ <- summary(lmer_mod)$coefficients[2, c(1:5)]
  lmer_res[[i]] = summ
}
names(lmer_res) = n

lmer_ress <- as.data.frame(t(sapply(lmer_res, "[", i = 1:max(sapply(lmer_res, length)))))
lmer_ress$name <- rownames(lmer_ress)
lmer_ress$sig <- ifelse(lmer_ress$`Pr(>|t|)` > 0.05, "", "SIG")
lmer_ress




###  TP/TK/AP/AK/TCa/PCa/TSb/TAs/ASb/AAs not normality (log or sqrt)
library(parameters)
# TP  (log)
norm_TP <- lm(log(TP) ~ site, data = data_soil)
shapiro.test(norm_TP$residuals)

lmms_TP <- lmer(log(TP) ~ site + (1|block), data = data_soil)
Anova(lmms_TP)
summary(lmms_TP)


# TK  (log)
norm_TK <- lm(log(TK) ~ site, data = data_soil)
shapiro.test(norm_TK$residuals)

lmms_TK <- lmer(log(TK) ~ site + (1|block), data = data_soil)
Anova(lmms_TK)
summary(lmms_TK)

# TCa (log)
norm_TCa <- lm(log(TCa) ~ site, data = data_soil)
shapiro.test(norm_TCa$residuals)

lmms_TCa <- lmer(log(TCa) ~ site + (1|block), data = data_soil)
Anova(lmms_TCa)
summary(lmms_TCa)

# TAs (log)
norm_TAs <- lm(log(TAs) ~ site, data = data_soil)
shapiro.test(norm_TAs$residuals)

lmms_TAs <- lmer(log(TAs) ~ site + (1|block), data = data_soil)
Anova(lmms_TAs)
summary(lmms_TAs)

# AAs (log)
norm_AAs <- lm(log(AAs) ~ site, data = data_soil)
shapiro.test(norm_AAs$residuals)

lmms_AAs <- lmer(log(AAs) ~ site + (1|block), data = data_soil)
Anova(lmms_AAs)
summary(lmms_AAs)





#############################  Plant functional traits  ############################

#############################  Figure  2   #############################

data_pfts_fig<-read_xlsx("data-all.xlsx", sheet="plant-1-3")
data_pfts_fig$site<-factor(data_pfts_fig$site,levels = c("NMS","MS"))
data_pfts_fig$Var<-factor(data_pfts_fig$Var,levels=c("Height","LT",
                               "LA","SLA"))
data_pfts_fig$species<-factor(data_pfts_fig$species,levels = c("Artemisia_argyi","Miscanthus_sinensis",
                                         "Ficus_tikoua","Ageratina_adenophora"))


labs <- c("Height (cm)","Leaf thickness (mm)", "Leaf area (cm2)", "Specific leaf area (mm2/mg)")

names(labs) <- c("Height","LT", "LA", "SLA")

pfts_fig1 <- ggplot(data_pfts_fig,aes(site, Value))+
  geom_boxplot(aes(color = site, fill = site, alpha = 0.2),
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





#########   LMMs relationship  Plant Sb/As and C/N/P/Ca
library(MuMIn)
library(rsq)

mm = lmer(Ln_RR ~ C + (1|block) + (1|species), data = subset(data_lm_all, HMs == "Sb"))
summary(mm)
Anova(mm)
rsq(mm)


mm = lmer(Ln_RR ~ C + (1|block) + (1|species), data = subset(data_lm_all, HMs == "As"))
summary(mm)
Anova(mm)
AIC(mm)
rsq(mm)

mm = lmer(Ln_RR ~ N + (1|block) + (1|species), data = subset(data_lm_all, HMs == "Sb"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ N + (1|block) + (1|species), data = subset(data_lm_all, HMs == "As"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ P + (1|block) + (1|species), data = subset(data_lm_all, HMs == "Sb"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ P + (1|block) + (1|species), data = subset(data_lm_all, HMs == "As"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ Ca + (1|block)+ (1|species), data = subset(data_lm_all, HMs == "Sb"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ Ca + (1|block) + (1|species), data = subset(data_lm_all, HMs == "As"))
summary(mm)
Anova(mm)
rsq(mm)


#########All species  plotting
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
               aes(Ln_RR,C,color = HMs,label=paste(..rr.label..,..p.value.label..,sep = "~~~~")),
               formula = y~x,parse=T,size = 4)+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "", y = "LnRR C")+
  theme(legend.position = c(0.2, 0.15))


N_all <- ggplot(data_lm_all, aes(Ln_RR,N,group = HMs))+
  geom_point(size=2.5,aes(color=HMs))+
  geom_smooth(aes(color=HMs),method = 'lm', se = F, show.legend=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  stat_poly_eq(data=data_lm_all,
               aes(Ln_RR,N,color = HMs,label=paste(..rr.label.., ..p.value.label.., sep = "~~~~")),
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
               aes(Ln_RR,P,color = HMs,label=paste(..rr.label..,..p.value.label..,sep = "~~~~")),
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
               aes(Ln_RR,Ca,color = HMs,label=paste(..rr.label..,..p.value.label..,sep = "~~~~")),
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
#ggsave("ele_all_species3.pdf",ele_all_species, width = 8, height = 8)


########################## Elements ratios
mm = lmer(Ln_RR ~ CN + (1|block) + (1|species), data = subset(data_lm_all, HMs == "Sb"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ CN + (1|block) + (1|species), data = subset(data_lm_all, HMs == "As"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ CP + (1|block) + (1|species), data = subset(data_lm_all, HMs == "Sb"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ CP + (1|block) + (1|species), data = subset(data_lm_all, HMs == "As"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ CCa + (1|block) + (1|species), data = subset(data_lm_all, HMs == "Sb"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ CCa + (1|block) + (1|species), data = subset(data_lm_all, HMs == "As"))
summary(mm)
Anova(mm)
rsq(mm)


mm = lmer(Ln_RR ~ NP+ (1|block) + (1|species), data = subset(data_lm_all, HMs == "Sb"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ NP + (1|block) + (1|species), data = subset(data_lm_all, HMs == "As"))
summary(mm)
Anova(mm)
rsq(mm)


mm = lmer(Ln_RR ~ NCa + (1|block) + (1|species), data = subset(data_lm_all, HMs == "Sb"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ NCa + (1|block) + (1|species), data = subset(data_lm_all, HMs == "As"))
summary(mm)
Anova(mm)
rsq(mm)


mm = lmer(Ln_RR ~ PCa + (1|block) + (1|species), data = subset(data_lm_all, HMs == "Sb"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ PCa + (1|block) + (1|species), data = subset(data_lm_all, HMs == "As"))
summary(mm)
Anova(mm)
rsq(mm)


CN_all <- ggplot(data_lm_all, aes(Ln_RR,CN,group = HMs))+
  geom_point(size=2.5,aes(color=HMs))+
  geom_smooth(aes(color=HMs),method = 'lm', se = F, show.legend=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  stat_poly_eq(data=data_lm_all,
               aes(Ln_RR,CN,color=HMs,label=paste(rr.label,..p.value.label..,sep = "~~~~")),
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
               aes(Ln_RR,CP,color=HMs,label=paste(rr.label,..p.value.label..,sep = "~~~~")),
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
               aes(Ln_RR,CCa,color=HMs,label=paste(rr.label,..p.value.label..,sep = "~~~~")),
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
               aes(Ln_RR,NP,color=HMs,label=paste(..rr.label..,..p.value.label..,sep = "~~~~")),
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
               aes(Ln_RR,NCa,color=HMs,label=paste(..rr.label..,..p.value.label..,sep = "~~~~")),
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
                   label=paste(..rr.label..,..p.value.label..,sep = "~~~~")),
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
#ggsave("all_species_ele_ratio2.pdf",all_species_ele_ratio, width = 8, height = 10)


###########Figure S5#########
#########Four species#########
rm(list = ls())
data_lm_all <- read_xlsx("data-all.xlsx", sheet = "LnRR-plant-ele-fig-lm")
data_lm_all$species <- factor(data_lm_all$species, levels = c("AC", "M", "DG", "PHC"), labels = c("Artemisia argyi", "Miscanthus sinensis", "Ficus tikoua", "Ageratina adenophora"))
str(data_lm_all)


####  AC / Artemisia argyi

##  C
mm = lmer(Ln_RR ~ C + (1|block), data = subset(data_lm_all, HMs == "Sb" & species == "Artemisia argyi"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ C + (1|block), data = subset(data_lm_all, HMs == "As" & species == "Artemisia argyi"))
summary(mm)
Anova(mm)
rsq(mm)

##  N
mm = lmer(Ln_RR ~ N + (1|block), data = subset(data_lm_all, HMs == "Sb" & species == "Artemisia argyi"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ N + (1|block), data = subset(data_lm_all, HMs == "As" & species == "Artemisia argyi"))
summary(mm)
Anova(mm)
rsq(mm)

##  P
mm = lmer(Ln_RR ~ P + (1|block), data = subset(data_lm_all, HMs == "Sb" & species == "Artemisia argyi"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ P + (1|block), data = subset(data_lm_all, HMs == "As" & species == "Artemisia argyi"))
summary(mm)
Anova(mm)
rsq(mm)


##  Ca
mm = lmer(Ln_RR ~ Ca + (1|block), data = subset(data_lm_all, HMs == "Sb" & species == "Artemisia argyi"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ Ca + (1|block), data = subset(data_lm_all, HMs == "As" & species == "Artemisia argyi"))
summary(mm)
Anova(mm)
rsq(mm)


####  M / Miscanthus sinensis

##  C
mm = lmer(Ln_RR ~ C + (1|block), data = subset(data_lm_all, HMs == "Sb" & species == "Miscanthus sinensis"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ C + (1|block), data = subset(data_lm_all, HMs == "As" & species == "Miscanthus sinensis"))
summary(mm)
Anova(mm)
rsq(mm)

##  N
mm = lmer(Ln_RR ~ N + (1|block), data = subset(data_lm_all, HMs == "Sb" & species == "Miscanthus sinensis"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ N + (1|block), data = subset(data_lm_all, HMs == "As" & species == "Miscanthus sinensis"))
summary(mm)
Anova(mm)
rsq(mm)

##  P
mm = lmer(Ln_RR ~ P + (1|block), data = subset(data_lm_all, HMs == "Sb" & species == "Miscanthus sinensis"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ P + (1|block), data = subset(data_lm_all, HMs == "As" & species == "Miscanthus sinensis"))
summary(mm)
Anova(mm)
rsq(mm)


##  Ca
mm = lmer(Ln_RR ~ Ca + (1|block), data = subset(data_lm_all, HMs == "Sb" & species == "Miscanthus sinensis"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ Ca + (1|block), data = subset(data_lm_all, HMs == "As" & species == "Miscanthus sinensis"))
summary(mm)
Anova(mm)
rsq(mm)



####  DG / Ficus tikoua

##  C
mm = lmer(Ln_RR ~ C + (1|block), data = subset(data_lm_all, HMs == "Sb" & species == "Ficus tikoua"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ C + (1|block), data = subset(data_lm_all, HMs == "As" & species == "Ficus tikoua"))
summary(mm)
Anova(mm)
rsq(mm)

##  N
mm = lmer(Ln_RR ~ N + (1|block), data = subset(data_lm_all, HMs == "Sb" & species == "Ficus tikoua"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ N + (1|block), data = subset(data_lm_all, HMs == "As" & species == "Ficus tikoua"))
summary(mm)
Anova(mm)
rsq(mm)

##  P
mm = lmer(Ln_RR ~ P + (1|block), data = subset(data_lm_all, HMs == "Sb" & species == "Ficus tikoua"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ P + (1|block), data = subset(data_lm_all, HMs == "As" & species == "Ficus tikoua"))
summary(mm)
Anova(mm)
rsq(mm)


##  Ca
mm = lmer(Ln_RR ~ Ca + (1|block), data = subset(data_lm_all, HMs == "Sb" & species == "Ficus tikoua"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ Ca + (1|block), data = subset(data_lm_all, HMs == "As" & species == "Ficus tikoua"))
summary(mm)
Anova(mm)
rsq(mm)



####  PHC / Ageratina adenophora

##  C
mm = lmer(Ln_RR ~ C + (1|block), data = subset(data_lm_all, HMs == "Sb" & species == "Ageratina adenophora"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ C + (1|block), data = subset(data_lm_all, HMs == "As" & species == "Ageratina adenophora"))
summary(mm)
Anova(mm)
rsq(mm)

##  N
mm = lmer(Ln_RR ~ N + (1|block), data = subset(data_lm_all, HMs == "Sb" & species == "Ageratina adenophora"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ N + (1|block), data = subset(data_lm_all, HMs == "As" & species == "Ageratina adenophora"))
summary(mm)
Anova(mm)
rsq(mm)

##  P
mm = lmer(Ln_RR ~ P + (1|block), data = subset(data_lm_all, HMs == "Sb" & species == "Ageratina adenophora"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ P + (1|block), data = subset(data_lm_all, HMs == "As" & species == "Ageratina adenophora"))
summary(mm)
Anova(mm)
rsq(mm)


##  Ca
mm = lmer(Ln_RR ~ Ca + (1|block), data = subset(data_lm_all, HMs == "Sb" & species == "Ageratina adenophora"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ Ca + (1|block), data = subset(data_lm_all, HMs == "As" & species == "Ageratina adenophora"))
summary(mm)
Anova(mm)
rsq(mm)



#####  Ratios  ####

####  AC / Artemisia argyi

##  C:N
mm = lmer(Ln_RR ~ CN + (1|block), data = subset(data_lm_all, HMs == "Sb" & species == "Artemisia argyi"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ CN + (1|block), data = subset(data_lm_all, HMs == "As" & species == "Artemisia argyi"))
summary(mm)
Anova(mm)
rsq(mm)

##  C:P
mm = lmer(Ln_RR ~ CP + (1|block), data = subset(data_lm_all, HMs == "Sb" & species == "Artemisia argyi"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ CP + (1|block), data = subset(data_lm_all, HMs == "As" & species == "Artemisia argyi"))
summary(mm)
Anova(mm)
rsq(mm)

##  C:Ca
mm = lmer(Ln_RR ~ CCa + (1|block), data = subset(data_lm_all, HMs == "Sb" & species == "Artemisia argyi"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ CCa + (1|block), data = subset(data_lm_all, HMs == "As" & species == "Artemisia argyi"))
summary(mm)
Anova(mm)
rsq(mm)


##  N:P
mm = lmer(Ln_RR ~ NP + (1|block), data = subset(data_lm_all, HMs == "Sb" & species == "Artemisia argyi"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ NP + (1|block), data = subset(data_lm_all, HMs == "As" & species == "Artemisia argyi"))
summary(mm)
Anova(mm)
rsq(mm)


##  N:Ca
mm = lmer(Ln_RR ~ NCa + (1|block), data = subset(data_lm_all, HMs == "Sb" & species == "Artemisia argyi"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ NCa + (1|block), data = subset(data_lm_all, HMs == "As" & species == "Artemisia argyi"))
summary(mm)
Anova(mm)
rsq(mm)


##  P:Ca
mm = lmer(Ln_RR ~ PCa + (1|block), data = subset(data_lm_all, HMs == "Sb" & species == "Artemisia argyi"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ PCa + (1|block), data = subset(data_lm_all, HMs == "As" & species == "Artemisia argyi"))
summary(mm)
Anova(mm)
rsq(mm)




####  M / Miscanthus sinensis

##  C:N
mm = lmer(Ln_RR ~ CN + (1|block), data = subset(data_lm_all, HMs == "Sb" & species == "Miscanthus sinensis"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ CN + (1|block), data = subset(data_lm_all, HMs == "As" & species == "Miscanthus sinensis"))
summary(mm)
Anova(mm)
rsq(mm)

##  C:P
mm = lmer(Ln_RR ~ CP + (1|block), data = subset(data_lm_all, HMs == "Sb" & species == "Miscanthus sinensis"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ CP + (1|block), data = subset(data_lm_all, HMs == "As" & species == "Miscanthus sinensis"))
summary(mm)
Anova(mm)
rsq(mm)

##  C:Ca
mm = lmer(Ln_RR ~ CCa + (1|block), data = subset(data_lm_all, HMs == "Sb" & species == "Miscanthus sinensis"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ CCa + (1|block), data = subset(data_lm_all, HMs == "As" & species == "Miscanthus sinensis"))
summary(mm)
Anova(mm)
rsq(mm)


##  N:P
mm = lmer(Ln_RR ~ NP + (1|block), data = subset(data_lm_all, HMs == "Sb" & species == "Miscanthus sinensis"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ NP + (1|block), data = subset(data_lm_all, HMs == "As" & species == "Miscanthus sinensis"))
summary(mm)
Anova(mm)
rsq(mm)


##  N:Ca
mm = lmer(Ln_RR ~ NCa + (1|block), data = subset(data_lm_all, HMs == "Sb" & species == "Miscanthus sinensis"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ NCa + (1|block), data = subset(data_lm_all, HMs == "As" & species == "Miscanthus sinensis"))
summary(mm)
Anova(mm)
rsq(mm)


##  P:Ca
mm = lmer(Ln_RR ~ PCa + (1|block), data = subset(data_lm_all, HMs == "Sb" & species == "Miscanthus sinensis"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ PCa + (1|block), data = subset(data_lm_all, HMs == "As" & species == "Miscanthus sinensis"))
summary(mm)
Anova(mm)
rsq(mm)


####  DG / Ficus tikoua

##  C:N
mm = lmer(Ln_RR ~ CN + (1|block), data = subset(data_lm_all, HMs == "Sb" & species == "Ficus tikoua"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ CN + (1|block), data = subset(data_lm_all, HMs == "As" & species == "Ficus tikoua"))
summary(mm)
Anova(mm)
rsq(mm)

##  C:P
mm = lmer(Ln_RR ~ CP + (1|block), data = subset(data_lm_all, HMs == "Sb" & species == "Ficus tikoua"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ CP + (1|block), data = subset(data_lm_all, HMs == "As" & species == "Ficus tikoua"))
summary(mm)
Anova(mm)
rsq(mm)

##  C:Ca
mm = lmer(Ln_RR ~ CCa + (1|block), data = subset(data_lm_all, HMs == "Sb" & species == "Ficus tikoua"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ CCa + (1|block), data = subset(data_lm_all, HMs == "As" & species == "Ficus tikoua"))
summary(mm)
Anova(mm)
rsq(mm)


##  N:P
mm = lmer(Ln_RR ~ NP + (1|block), data = subset(data_lm_all, HMs == "Sb" & species == "Ficus tikoua"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ NP + (1|block), data = subset(data_lm_all, HMs == "As" & species == "Ficus tikoua"))
summary(mm)
Anova(mm)
rsq(mm)


##  N:Ca
mm = lmer(Ln_RR ~ NCa + (1|block), data = subset(data_lm_all, HMs == "Sb" & species == "Ficus tikoua"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ NCa + (1|block), data = subset(data_lm_all, HMs == "As" & species == "Ficus tikoua"))
summary(mm)
Anova(mm)
rsq(mm)


##  P:Ca
mm = lmer(Ln_RR ~ PCa + (1|block), data = subset(data_lm_all, HMs == "Sb" & species == "Ficus tikoua"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ PCa + (1|block), data = subset(data_lm_all, HMs == "As" & species == "Ficus tikoua"))
summary(mm)
Anova(mm)
rsq(mm)



####  PHC / Ageratina adenophora

##  C:N
mm = lmer(Ln_RR ~ CN + (1|block), data = subset(data_lm_all, HMs == "Sb" & species == "Ageratina adenophora"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ CN + (1|block), data = subset(data_lm_all, HMs == "As" & species == "Ageratina adenophora"))
summary(mm)
Anova(mm)
rsq(mm)

##  C:P
mm = lmer(Ln_RR ~ CP + (1|block), data = subset(data_lm_all, HMs == "Sb" & species == "Ageratina adenophora"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ CP + (1|block), data = subset(data_lm_all, HMs == "As" & species == "Ageratina adenophora"))
summary(mm)
Anova(mm)
rsq(mm)

##  C:Ca
mm = lmer(Ln_RR ~ CCa + (1|block), data = subset(data_lm_all, HMs == "Sb" & species == "Ageratina adenophora"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ CCa + (1|block), data = subset(data_lm_all, HMs == "As" & species == "Ageratina adenophora"))
summary(mm)
Anova(mm)
rsq(mm)


##  N:P
mm = lmer(Ln_RR ~ NP + (1|block), data = subset(data_lm_all, HMs == "Sb" & species == "Ageratina adenophora"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ NP + (1|block), data = subset(data_lm_all, HMs == "As" & species == "Ageratina adenophora"))
summary(mm)
Anova(mm)
rsq(mm)


##  N:Ca
mm = lmer(Ln_RR ~ NCa + (1|block), data = subset(data_lm_all, HMs == "Sb" & species == "Ageratina adenophora"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ NCa + (1|block), data = subset(data_lm_all, HMs == "As" & species == "Ageratina adenophora"))
summary(mm)
Anova(mm)
rsq(mm)


##  P:Ca
mm = lmer(Ln_RR ~ PCa + (1|block), data = subset(data_lm_all, HMs == "Sb" & species == "Ageratina adenophora"))
summary(mm)
Anova(mm)
rsq(mm)

mm = lmer(Ln_RR ~ PCa + (1|block), data = subset(data_lm_all, HMs == "As" & species == "Ageratina adenophora"))
summary(mm)
Anova(mm)
rsq(mm)


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
               aes(Ln_RR,C,color=HMs,label=paste(..rr.label..,..p.value.label..,sep = "~~~~")),
               formula = y~x,parse=T,size = 5)+
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
               aes(Ln_RR,N,color=HMs,label=paste(..rr.label..,..p.value.label..,sep = "~~~~")),
               formula = y~x,parse=T,size = 5)+
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
               aes(Ln_RR,P,color=HMs,label=paste(..rr.label..,..p.value.label..,sep = "~~~~")),
               formula = y~x,parse=T,size = 5)+
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
               aes(Ln_RR,Ca,color=HMs,label=paste(..rr.label..,..p.value.label..,sep = "~~~~")),
               formula = y~x,parse=T,size = 5,label.x = 0.02)+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "", y = "LnRR Ca")

library(ggpubr)
ele <- ggarrange(C, N, P, Ca, ncol = 1, align = "hv", common.legend = T, legend = "bottom")
ele

#ggsave("ele1.pdf", ele, width = 12, height = 16)


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
               aes(Ln_RR,CN,color=HMs,label=paste(..rr.label..,..p.value.label..,sep = "~~~~")),
               formula = y~x,parse=T,size = 4)+
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
               aes(Ln_RR,CP,color=HMs,label=paste(..rr.label..,..p.value.label..,sep = "~~~~")),
               formula = y~x,parse=T,size = 4)+
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
               aes(Ln_RR,CCa,color=HMs,label=paste(..rr.label..,..p.value.label..,sep = "~~~~")),
               formula = y~x,parse=T,size = 4)+
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
               aes(Ln_RR,NP,color=HMs,label=paste(..rr.label..,..p.value.label..,sep = "~~~~")),
               formula = y~x,parse=T,size = 4)+
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
               aes(Ln_RR,NCa,color=HMs,label=paste(..rr.label..,..p.value.label..,sep = "~~~~")),
               formula = y~x,parse=T,size = 4)+
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
               aes(Ln_RR,PCa,color=HMs,
                   label=paste(..rr.label..,..p.value.label..,sep = "~~~~")),
               formula = y~x,parse=T,size = 4)+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "", y = "LnRR P:Ca")


ele_ratio <- ggarrange(CN,CP,CCa,NP, NCa, PCa, ncol = 1, align = "hv", common.legend = T, legend = "bottom")
ele_ratio
#ggsave("ele_ratio1.pdf", ele_ratio, width = 12, height = 18)


##################  SEM ###################################
#(1)soil
#(2)soil
#(3)PFTs_mor: Height/LT/LA/SLA 
#(4)PFTs_ele: C/N/P/Ca  
#(5)PFTs_ratio: CN/CP/CCa/NP/NCa/PCa 

#(6)PFTs_HMs: Sb/As 

##PCA 1 were used construct SEM
#(Chen et al., 2013 Functional Ecology)
#(Jiang et al., 2023 Science of the Total Environment)

###PCA### plant Sb/As
rm(list = ls())
data_PCA_all <- read_xlsx("data-all.xlsx", sheet = "SEM-all-species-data") 

### Corr plot
tdc <- cor(data_PCA_all[,-(1:3)], method="spearman")

cor <- ggcorrplot(tdc, method = c("square"), type = c("full"), ggtheme = ggplot2::theme_void, title = " ", show.legend = TRUE, legend.title = "Correlation", show.diag = T, 
                  colors = c("#6D9EC1", "white", "#E46726"), outline.color = "black", 
                  hc.order = F, hc.method = "single", lab = F, lab_col = "black", 
                  lab_size = 2, p.mat = NULL, sig.level = 0.05, insig = c("pch"), pch = 4, pch.col = "white", pch.cex = 4.5, tl.cex = 12, 
                  tl.col = "black", tl.srt = 90, digits = 2)
cor
#ggsave("Corr.pdf", cor, width = 8, height = 8)



#soil
res_PCA_soil <- PCA(data_PCA_all[,4:20], scale = T)
summary(res_PCA_soil) #44.52% PC1 / 73.55% PC2
fviz_contrib(res_PCA_soil, choice = "var", axes = 1, top = 12)
fviz_contrib(res_PCA_soil, choice = "var", axes = 2, top = 12)
# significant information

dim_res_PCA_soil = dimdesc(res_PCA_soil, axes = 1:3, proba = 0.05) # proba significant level

#The r and p value between responses and PC axis
dim_soil1 <- dim_res_PCA_soil$Dim.1$quanti %>% as.data.frame()
dim_soil1$factor <- rownames(dim_soil1)
dim_soil2 <- dim_res_PCA_soil$Dim.2$quanti %>% as.data.frame()
dim_soil2$factor <- rownames(dim_soil2)


dim_soil <- merge(dim_soil1, dim_soil2, by = "factor", all=T)
#write.csv(dim_soil, "dim_soil.csv")

#soil HMs
res_PCA_soil_HMS <- PCA(data_PCA_all[,21:24], scale = T)
summary(res_PCA_soil_HMS) #84.46% PC1 /98.17% PC2
fviz_contrib(res_PCA_soil_HMS, choice = "var", axes = 1, top = 12)

dim_res_PCA_soil_HMS = dimdesc(res_PCA_soil_HMS, axes = 1:2, proba = 0.05) # proba significant level

#The r and p value between responses and PC axis
dim_res_PCA_soil_HMS$Dim.1$quanti
dim_res_PCA_soil_HMS$Dim.2$quanti


#plant mor.
res_PCA_plant_mor <- PCA(data_PCA_all[,37:40], scale = T)
summary(res_PCA_plant_mor) #41.81% PC1 / 67.50% PC2 / 89.19%PC3 
fviz_contrib(res_PCA_plant_mor, choice = "var", axes = 1, top = 12)
fviz_contrib(res_PCA_plant_mor, choice = "var", axes = 2, top = 12)
fviz_contrib(res_PCA_plant_mor, choice = "var", axes = 3, top = 12)

dim_res_PCA_plant_mor = dimdesc(res_PCA_plant_mor, axes = 1:2, proba = 0.05) # proba significant level

#The r and p value between responses and PC axis
dim_res_PCA_plant_mor$Dim.1$quanti
dim_res_PCA_plant_mor$Dim.2$quanti
dim_res_PCA_plant_mor$Dim.3$quanti

#plant CNPCa
res_PCA_plant_C <- PCA(data_PCA_all[,25:28], scale = T)
summary(res_PCA_plant_C)#44.06% PC1 / 73.70% PC2
fviz_contrib(res_PCA_plant_C, choice = "var", axes = 1, top = 12)
fviz_contrib(res_PCA_plant_C, choice = "var", axes = 2, top = 12)

dim_res_PCA_plant_C = dimdesc(res_PCA_plant_C, axes = 1:2, proba = 0.05) # proba significant level

#The r and p value between responses and PC axis
dim_res_PCA_plant_C$Dim.1$quanti
dim_res_PCA_plant_C$Dim.2$quanti


#plant ratios
res_PCA_plant_ra <- PCA(data_PCA_all[,29:34], scale = T)
summary(res_PCA_plant_ra)#43.83%PC1 / 75.30% PC2
fviz_contrib(res_PCA_plant_ra, choice = "var", axes = 1, top = 12)
fviz_contrib(res_PCA_plant_ra, choice = "var", axes = 2, top = 12)

dim_res_PCA_plant_ra = dimdesc(res_PCA_plant_ra, axes = 1:2, proba = 0.05) # proba significant level

#The r and p value between responses and PC axis
dim_res_PCA_plant_ra$Dim.1$quanti
dim_res_PCA_plant_ra$Dim.2$quanti



#merge data
#soil_PC
data_PCA1_soil_PC_var <- data.frame(res_PCA_soil$ind$coord[,1:2])
names(data_PCA1_soil_PC_var) <- c("soilPC1", "soilPC2")


#soil_HMS
data_PCA1_soil_HMS_var <- data.frame(res_PCA_soil_HMS$ind$coord[,1])
names(data_PCA1_soil_HMS_var) <- c("soilHMsPC1")


#plant mor
data_PCA1_plant_mor_var <- data.frame(res_PCA_plant_mor$ind$coord[, 1:3])
names(data_PCA1_plant_mor_var) <- c("pmorPC1","pmorPC2", "pmorPC3")

#plant ELE
data_PCA1_plant_ele_var <- data.frame(res_PCA_plant_C$ind$coord[, 1:2])
names(data_PCA1_plant_ele_var) <- c("pelePC1", "pelePC2")



#plant Ratio
data_PCA1_plant_ra_var <- data.frame(res_PCA_plant_ra$ind$coord[,1:2])

names(data_PCA1_plant_ra_var) <- c("pratPC1", "pratPC2")

data_PCA1_plant_ra_var


#merge data
data_SEM <- data.frame(cbind(data_PCA_all[,1:3],
                             data_PCA1_soil_PC_var,
                             data_PCA1_soil_HMS_var,
                             data_PCA1_plant_mor_var,
                             data_PCA1_plant_ele_var,
                             data_PCA1_plant_ra_var,
                             data_PCA_all[,35:36])) # Sb As
data_scale_sem <- data_SEM[,4:15] %>% scale() %>%  as.data.frame() %>% cbind(data_SEM[,1:3])

head(data_scale_sem)


##### PCA figure 
font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))

psoil <- fviz_pca_var(res_PCA_soil, repel=T, gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),col.var = "contrib") + theme_bw() + ggtitle("Soil properties") + font + theme(text = element_text(size = 12)) + theme(plot.title = element_text(hjust = 0.5))
phms <- fviz_pca_var(res_PCA_soil_HMS, repel=T, gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),col.var = "contrib") + theme_bw() + ggtitle("Soil metal concentrations") + font + theme(text = element_text(size = 12)) + theme(plot.title = element_text(hjust = 0.5))
pmor <- fviz_pca_var(res_PCA_plant_mor, repel=T, gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),col.var = "contrib") + theme_bw() + ggtitle("Plant morphology") + font + theme(text = element_text(size = 12)) + theme(plot.title = element_text(hjust = 0.5))
pele <- fviz_pca_var(res_PCA_plant_C, repel=T, gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),col.var = "contrib") + theme_bw() + ggtitle("Plant element concentrations") + font + theme(text = element_text(size = 12)) + theme(plot.title = element_text(hjust = 0.5))
prat <- fviz_pca_var(res_PCA_plant_ra, repel=T, gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),col.var = "contrib") + theme_bw() + ggtitle("Plant element ratios") + font + theme(text = element_text(size = 12)) + theme(plot.title = element_text(hjust = 0.5))
library(ggpubr)
pca <- ggarrange(psoil, phms, pmor, pele, prat, ncol = 2, nrow = 3, common.legend = T, legend = "right")
#ggsave("PCA.pdf", pca, width = 9, height = 10)






# Random forest
head(data_scale_sem)

###Sb
set.seed(123)
model_Sb <- randomForest::randomForest(Sb ~ soilHMsPC1 + soilPC1 + soilPC1 + pmorPC1 + pmorPC2 + pmorPC3 + pelePC1 + pelePC2 + pratPC1 + pratPC2,
                                    data = data_scale_sem,
                                    ntree = 500,
                                    importance = T,
                                    proximity = T)#计算各个观测之间的相似性


library(rfPermute)

set.seed(123)
model_p_Sb <- rfPermute(Sb ~ soilHMsPC1 + soilPC1 + soilPC1 + pmorPC1 + pmorPC2 + pmorPC3 + pelePC1 + pelePC2 + pratPC1 + pratPC2,
                     data = data_scale_sem,
                     ntree = 500,
                     nrep = 1000,
                     num.cores = 1)


model_p_scale_Sb <- data.frame(importance(model_p_Sb, scale = TRUE), check.names = FALSE)
model_p_scale_Sb



model_plot_data_Sb <- data.frame(tibble(Var = rownames(model_p_scale_Sb),
                                     IncMSE = model_p_scale_Sb[,1],
                                     IncMSEp = model_p_scale_Sb[,2]))

model_plot_data_Sb <- model_plot_data_Sb[order(model_plot_data_Sb$'IncMSE', decreasing = TRUE),]
model_plot_data_Sb$Var <- factor(model_plot_data_Sb$Var,
                                 levels = c("pmorPC2","pmorPC3","pmorPC1","pratPC1","pratPC2", "pelePC1", "pelePC2", "soilPC1", "soilHMsPC1"),
                                 labels = c("Pla_MOR_PC2", "Pla_MOR_PC3", "Pla_MOR_PC1", "Pla_RAT_PC1", "Pla_RAT_PC2","Pla_ELE_PC1", "Pla_ELE_PC2", "Soil_PC1", "Soil_HMs"))


model_plot_data_Sb$sig <- ifelse(model_plot_data_Sb$IncMSEp > 0.05, " ", ifelse(model_plot_data_Sb$IncMSEp < 0.01, "**", "*"))

model_plot_data_Sb$HMS <- rep("Sb", 9)



###As
set.seed(123)
model_As <- randomForest::randomForest(As ~ soilHMsPC1 + soilPC1 + soilPC1 + pmorPC1 + pmorPC2 + pmorPC3 + pelePC1 + pelePC2 + pratPC1 + pratPC2,
                                       data = data_scale_sem,
                                       ntree = 500,
                                       importance = T,
                                       proximity = T)
library(rfPermute)

set.seed(123)
model_p_As <- rfPermute(As ~ soilHMsPC1 + soilPC1 + soilPC1 + pmorPC1 + pmorPC2 + pmorPC3 + pelePC1 + pelePC2 + pratPC1 + pratPC2,
                        data = data_scale_sem,
                        ntree = 500,
                        nrep = 1000,
                        num.cores = 1)

model_p_scale_As <- data.frame(importance(model_p_As, scale = TRUE), check.names = FALSE)
model_p_scale_As



model_plot_data_As <- data.frame(tibble(Var = rownames(model_p_scale_As),
                                        IncMSE = model_p_scale_As[,1],
                                        IncMSEp = model_p_scale_As[,2]))

model_plot_data_As <- model_plot_data_As[order(model_plot_data_As$'IncMSE', decreasing = TRUE),]
model_plot_data_As$Var <- factor(model_plot_data_As$Var,
                                 levels = c("pelePC2","pmorPC1","pmorPC3","pratPC1","pmorPC2", "pelePC1", "soilPC1", "pratPC2", "soilHMsPC1"),
                                 labels = c("Pla_ELE_PC2", "Pla_MOR_PC1", "Pla_MOR_PC3", "Pla_RAT_PC1", "Pla_MOR_PC2","Pla_ELE_PC1", "Soil_PC1", "Pla_RAT_PC2", "Soil_HMs"))

model_plot_data_As$sig <- ifelse(model_plot_data_As$IncMSEp > 0.05, " ", ifelse(model_plot_data_As$IncMSEp < 0.01, "**", "*"))
model_plot_data_As$HMS <- rep("As", 9)


#merge data
model_plot_data_Sb
model_plot_data_As

model_plot_data_HMS <- merge(model_plot_data_Sb, model_plot_data_As, all = T)
model_plot_data_HMS


#themes
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
  annotate('text', label = sprintf('italic(R^2) == %.2f', 27.57),
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
  annotate('text', label = sprintf('italic(R^2) == %.2f', 63.20),
           x = 1, y = 16, size = 6, parse = TRUE)+
  coord_flip()+
  theme(strip.text.x = element_text(size = 20))
RF_As

#merge figures
fig_RF <- cowplot::plot_grid(RF_Sb,
                             RF_As,
                             ncol = 1,
                             align = "hv",
                             labels = c("(b)","(c)"),
                             label_size = 20)
fig_RF

#ggsave("fig_RF_SEM-0506.pdf",fig_RF, width = 4, height = 6.5)



###relationship between Sb and As in plant
data_SEM

font = theme(axis.title.x=element_text(size=18, color = "black"),axis.text.x=element_text(size=16, color = "black"),
             axis.title.y=element_text(size=18, color = "black"),axis.text.y=element_text(size=16, color = "black"))


mm <- lmer(Sb ~ As + (site|block) + (1|species), data = data_SEM)
summary(mm)
Anova(mm)
rsq(mm)

Sb_As <- ggplot(data_SEM, aes(Sb,As))+
  geom_point(size=2.5)+
  geom_smooth(method = 'lm', se = T, show.legend=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  
  stat_poly_eq(data=data_SEM,
               aes(Sb,As,label=paste(..rr.label..,..p.value.label..,sep = "~~~~")),
               formula = y~x,parse=T,size = 4,colour="black")+
  font +
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(x = "Sb concentration in plant (mg kg-1)", y = "As concentration in plant (mg kg-1)")
Sb_As
#ggsave("fig_plant_Sb_As.pdf", width = 5, height = 5, dpi = 600)



### Four species in relationship between Sb and As

data_ele<-read_xlsx("data-all.xlsx",sheet="plant-all-ele")
data_ele$site<-factor(data_ele$site,levels = c("NMS","MS"))
data_ele$species<-factor(data_ele$species)
data_ele$organ<-factor(data_ele$organ,levels = c("aboveground","underground"), labels = c("Aboveground", "Underground"))
data_ele$group <- factor(data_ele$group)
head(data_ele)

data_ele$species <- factor(data_ele$species,
                           levels = c('AC','DG','M',"PHC"),
                           labels = c("Artemisia argyi","Ficus tikoua",
                                      "Miscanthus sinensis","Ageratina adenophora"))


## AC
mm <- lmer(Sb ~ As + (1|block), data = subset(data_ele, species == "Artemisia argyi"))
Anova(mm)
rsq(mm)

## DG
mm <- lmer(Sb ~ As + (1|block), data = subset(data_ele, species == "Ficus tikoua"))
Anova(mm)
rsq(mm)

## M
mm <- lmer(Sb ~ As + (1|block), data = subset(data_ele, species == "Miscanthus sinensis"))
Anova(mm)
rsq(mm)

## PHC
mm <- lmer(Sb ~ As + (1|block), data = subset(data_ele, species == "Ageratina adenophora"))
Anova(mm)
rsq(mm)


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
               aes(Sb,As,label=paste(..rr.label..,..p.value.label..,sep = "~~~~")),
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
mm <- lmer(Sb ~ As + (1|block), data = data_ele)
Anova(mm)
rsq(mm)

asp = ggplot(data_ele, aes(Sb,As))+
  geom_point(size=2.5,aes(color=species))+
  geom_smooth(method = 'lm', se = T, show.legend=FALSE)+
  theme_bw()+
  theme(axis.text=element_text(colour='black',size=9),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank())+
  stat_poly_eq(data=data_ele,
               aes(Sb,As,label=paste(..rr.label..,..p.value.label..,sep = "~~~~")),
               formula = y~x,parse=T,size = 5)+
  theme(strip.text.x = element_text(size = 18))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  theme(legend.position = c(0.7, 0.2))+
  theme(legend.text = element_text(size=12, face="italic")) +
  font+
  labs(x = expression(paste("Sb concentration in plant (mg ",kg^-1,")")), 
                            y = expression(paste("As concentration in plant (mg ",kg^-1,")")))
asp
#ggsave("All-species.pdf",asp, width = 5, height = 5)
                      



########################   Orgin data in C N P C Sb As  ########################
####-------Figure 3-1CNP C N P Ca-------####
rm(list=ls())
d3<-read_xlsx("data-all.xlsx",sheet="plant-all-ele")
d3$site<-factor(d3$site,levels = c("NMS","MS"))
d3$species<-factor(d3$species)
d3$organ<-factor(d3$organ,levels = c("aboveground","underground"), labels = c("Aboveground part","Underground part"))
d3$group <- factor(d3$group)
head(d3)


str(d3)




#Two-way ANOVA
library(emmeans)
library(multcomp)
library(rstatix)

#####  Aboveground part
### C
original_C <- lmer(C ~ site + species + site * species + (1|block), data = subset(d3, organ == "Aboveground part"))
Anova(original_C)


# 控制 site, 看species
post_C <- emmeans(original_C, ~ species|site, adjust = "tukey")
comp_C <- contrast(post_C, method = "pairwise", adjust = "tukey")  %>% data.frame()  %>% mutate("Sig" = ifelse(p.value < 0.01, "**", ifelse(p.value < 0.05, "*", "")))
comp_C


### N
original_N <- lmer(N ~ site + species + site * species + (1|block), data = subset(d3, organ == "Aboveground part"))
Anova(original_N)

post_N <- emmeans(original_N, ~ site * species, adjust = "tukey")
comp_N <- contrast(post_N, method = "pairwise", adjust = "tukey")  %>% data.frame()  %>% mutate("Sig" = ifelse(p.value < 0.01, "**", ifelse(p.value < 0.05, "*", "")))


### P
original_P <- lmer(P ~ site + species + site * species + (1|block), data = subset(d3, organ == "Aboveground part"))
Anova(original_P)

post_P <- emmeans(original_P, ~ site|species, adjust = "tukey")
comp_P <- contrast(post_P, method = "pairwise", adjust = "tukey")  %>% data.frame()  %>% mutate("Sig" = ifelse(p.value < 0.01, "**", ifelse(p.value < 0.05, "*", "")))


### Ca
original_Ca <- lmer(Ca ~ site + species + site * species + (1|block), data = subset(d3, organ == "Aboveground part"))
Anova(original_Ca)

post_Ca <- emmeans(original_Ca, ~ site * species, adjust = "tukey")
comp_Ca <- contrast(post_Ca, method = "pairwise", adjust = "tukey")  %>% data.frame()  %>% mutate("Sig" = ifelse(p.value < 0.01, "**", ifelse(p.value < 0.05, "*", "")))



### Sb
original_Sb <- lmer(Sb ~ site + species + site * species + (1|block), data = subset(d3, organ == "Aboveground part"))
Anova(original_Sb)

post_Sb <- emmeans(original_Sb, ~ site|species, adjust = "tukey")
comp_Sb <- contrast(post_Sb, method = "pairwise", adjust = "tukey")  %>% data.frame()  %>% mutate("Sig" = ifelse(p.value < 0.01, "**", ifelse(p.value < 0.05, "*", "")))



### As
original_As <- lmer(As ~ site + species + site * species + (1|block), data = subset(d3, organ == "Aboveground part"))
Anova(original_As)

post_As <- emmeans(original_As, ~ site|species, adjust = "tukey")
comp_As <- contrast(post_As, method = "pairwise", adjust = "tukey")  %>% data.frame()  %>% mutate("Sig" = ifelse(p.value < 0.01, "**", ifelse(p.value < 0.05, "*", "")))



### C:N
original_CN <- lmer(CN ~ site + species + site * species + (1|block), data = subset(d3, organ == "Aboveground part"))
Anova(original_CN)

post_CN <- emmeans(original_CN, ~ site * species, adjust = "tukey")
comp_CN <- contrast(post_CN, method = "pairwise", adjust = "tukey")  %>% data.frame()  %>% mutate("Sig" = ifelse(p.value < 0.01, "**", ifelse(p.value < 0.05, "*", "")))


### C:P
original_CP <- lmer(CP ~ site + species + site * species + (1|block), data = subset(d3, organ == "Aboveground part"))
Anova(original_CP)

post_CP <- emmeans(original_CP, ~ site|species, adjust = "tukey")
comp_CP <- contrast(post_CP, method = "pairwise", adjust = "tukey")  %>% data.frame()  %>% mutate("Sig" = ifelse(p.value < 0.01, "**", ifelse(p.value < 0.05, "*", "")))


### C:Ca
original_CCa <- lmer(CCa ~ site + species + site * species + (1|block), data = subset(d3, organ == "Aboveground part"))
Anova(original_CCa)

post_CCa <- emmeans(original_CCa, ~ site * species, adjust = "tukey")
comp_CCa <- contrast(post_CCa, method = "pairwise", adjust = "tukey")  %>% data.frame()  %>% mutate("Sig" = ifelse(p.value < 0.01, "**", ifelse(p.value < 0.05, "*", "")))

### N:P
original_NP <- lmer(NP ~ site + species + site * species + (1|block), data = subset(d3, organ == "Aboveground part"))
Anova(original_NP)

post_NP <- emmeans(original_NP, ~ site|species, adjust = "tukey")
comp_NP <- contrast(post_NP, method = "pairwise", adjust = "tukey")  %>% data.frame()  %>% mutate("Sig" = ifelse(p.value < 0.01, "**", ifelse(p.value < 0.05, "*", "")))



### N:Ca
original_NCa <- lmer(NCa ~ site + species + site * species + (1|block), data = subset(d3, organ == "Aboveground part"))
Anova(original_NCa)

post_NCa <- emmeans(original_NCa, ~ site * species, adjust = "tukey")
comp_NCa <- contrast(post_NCa, method = "pairwise", adjust = "tukey")  %>% data.frame()  %>% mutate("Sig" = ifelse(p.value < 0.01, "**", ifelse(p.value < 0.05, "*", "")))


### P:Ca
original_PCa <- lmer(PCa ~ site + species + site * species + (1|block), data = subset(d3, organ == "Aboveground part"))
Anova(original_PCa)

post_PCa <- emmeans(original_PCa, ~ site|species, adjust = "tukey")
comp_PCa <- contrast(post_PCa, method = "pairwise", adjust = "tukey")  %>% data.frame()  %>% mutate("Sig" = ifelse(p.value < 0.01, "**", ifelse(p.value < 0.05, "*", "")))





#####  Underground part
### C
original_C <- lmer(C ~ site + species + site * species + (1|block), data = subset(d3, organ == "Underground part"))
Anova(original_C)

post_C <- emmeans(original_C, ~ site|species, adjust = "tukey")
comp_C <- contrast(post_C, method = "pairwise", adjust = "tukey")  %>% data.frame()  %>% mutate("Sig" = ifelse(p.value < 0.01, "**", ifelse(p.value < 0.05, "*", "")))

### N
original_N <- lmer(N ~ site + species + site * species + (1|block), data = subset(d3, organ == "Underground part"))
Anova(original_N)

post_N <- emmeans(original_N, ~ site * species, adjust = "tukey")
comp_N <- contrast(post_N, method = "pairwise", adjust = "tukey")  %>% data.frame()  %>% mutate("Sig" = ifelse(p.value < 0.01, "**", ifelse(p.value < 0.05, "*", "")))


### P
original_P <- lmer(P ~ site + species + site * species + (1|block), data = subset(d3, organ == "Underground part"))
Anova(original_P)

post_P <- emmeans(original_P, ~ site|species, adjust = "tukey")
comp_P <- contrast(post_P, method = "pairwise", adjust = "tukey")  %>% data.frame()  %>% mutate("Sig" = ifelse(p.value < 0.01, "**", ifelse(p.value < 0.05, "*", "")))


### Ca
original_Ca <- lmer(Ca ~ site + species + site * species + (1|block), data = subset(d3, organ == "Underground part"))
Anova(original_Ca)

post_Ca <- emmeans(original_Ca, ~ site * species, adjust = "tukey")
comp_Ca <- contrast(post_Ca, method = "pairwise", adjust = "tukey")  %>% data.frame()  %>% mutate("Sig" = ifelse(p.value < 0.01, "**", ifelse(p.value < 0.05, "*", "")))



### Sb
original_Sb <- lmer(Sb ~ site + species + site * species + (1|block), data = subset(d3, organ == "Underground part"))
Anova(original_Sb)

post_Sb <- emmeans(original_Sb, ~ site * species, adjust = "tukey")
comp_Sb <- contrast(post_Sb, method = "pairwise", adjust = "tukey")  %>% data.frame()  %>% mutate("Sig" = ifelse(p.value < 0.01, "**", ifelse(p.value < 0.05, "*", "")))



### As
original_As <- lmer(As ~ site + species + site * species + (1|block), data = subset(d3, organ == "Underground part"))
Anova(original_As)

post_As <- emmeans(original_As, ~ site|species, adjust = "tukey")
comp_As <- contrast(post_As, method = "pairwise", adjust = "tukey")  %>% data.frame()  %>% mutate("Sig" = ifelse(p.value < 0.01, "**", ifelse(p.value < 0.05, "*", "")))



### C:N
original_CN <- lmer(CN ~ site + species + site * species + (1|block), data = subset(d3, organ == "Underground part"))
Anova(original_CN)

post_CN <- emmeans(original_CN, ~ site * species, adjust = "tukey")
comp_CN <- contrast(post_CN, method = "pairwise", adjust = "tukey")  %>% data.frame()  %>% mutate("Sig" = ifelse(p.value < 0.01, "**", ifelse(p.value < 0.05, "*", "")))


### C:P
original_CP <- lmer(CP ~ site + species + site * species + (1|block), data = subset(d3, organ == "Underground part"))
Anova(original_CP)

post_CP <- emmeans(original_CP, ~ site|species, adjust = "tukey")
comp_CP <- contrast(post_CP, method = "pairwise", adjust = "tukey")  %>% data.frame()  %>% mutate("Sig" = ifelse(p.value < 0.01, "**", ifelse(p.value < 0.05, "*", "")))


### C:Ca
original_CCa <- lmer(CCa ~ site + species + site * species + (1|block), data = subset(d3, organ == "Underground part"))
Anova(original_CCa)

post_CCa <- emmeans(original_CCa, ~ site * species, adjust = "tukey")
comp_CCa <- contrast(post_CCa, method = "pairwise", adjust = "tukey")  %>% data.frame()  %>% mutate("Sig" = ifelse(p.value < 0.01, "**", ifelse(p.value < 0.05, "*", "")))

### N:P
original_NP <- lmer(NP ~ site + species + site * species + (1|block), data = subset(d3, organ == "Underground part"))
Anova(original_NP)

post_NP <- emmeans(original_NP, ~ site|species, adjust = "tukey")
comp_NP <- contrast(post_NP, method = "pairwise", adjust = "tukey")  %>% data.frame()  %>% mutate("Sig" = ifelse(p.value < 0.01, "**", ifelse(p.value < 0.05, "*", "")))



### N:Ca
original_NCa <- lmer(NCa ~ site + species + site * species + (1|block), data = subset(d3, organ == "Underground part"))
Anova(original_NCa)

post_NCa <- emmeans(original_NCa, ~ site|species, adjust = "tukey")
comp_NCa <- contrast(post_NCa, method = "pairwise", adjust = "tukey")  %>% data.frame()  %>% mutate("Sig" = ifelse(p.value < 0.01, "**", ifelse(p.value < 0.05, "*", "")))


### P:Ca
original_PCa <- lmer(PCa ~ site + species + site * species + (1|block), data = subset(d3, organ == "Underground part"))
Anova(original_PCa)

post_PCa <- emmeans(original_PCa, ~ site * species, adjust = "tukey")
comp_PCa <- contrast(post_PCa, method = "pairwise", adjust = "tukey")  %>% data.frame()  %>% mutate("Sig" = ifelse(p.value < 0.01, "**", ifelse(p.value < 0.05, "*", "")))


######   Sb (原始数据 log)
### C
CSb <- lmer(log(Sb) ~ log(C) + (1|block) + (1|species), data = d3)
summary(CSb)
Anova(CSb)
### N
NSb <- lmer(log(Sb) ~ log(N) + (1|block) + (1|species), data = d3)
summary(NSb)
### P
PSb <- lmer(log(Sb) ~ log(P) + (1|block) + (1|species), data = d3)
summary(PSb)
### Ca
CaSb <- lmer(log(Sb) ~ log(Ca) + (1|block) + (1|species), data = d3)
summary(CaSb)

### CN
CNSb <- lmer(log(Sb) ~ log(CN) + (1|block) + (1|species), data = d3)
summary(CNSb)
### CP
CPSb <- lmer(log(Sb) ~ log(CP) + (1|block) + (1|species), data = d3)
summary(CPSb)
### CCa
CCaSb <- lmer(log(Sb) ~ log(CCa) + (1|block) + (1|species), data = d3)
summary(CCaSb)
### NP
NPSb <- lmer(log(Sb) ~ log(NP) + (1|block) + (1|species), data = d3)
summary(NPSb)
### NCa
NCaSb <- lmer(log(Sb) ~ log(NCa) + (1|block) + (1|species), data = d3)
summary(NCaSb)
### PCa
PCaSb <- lmer(log(Sb) ~ log(PCa) + (1|block) + (1|species), data = d3)
summary(PCaSb)


######   As
### C
CAs <- lmer(log(As) ~ log(C) + (1|block) + (1|species), data = d3)
summary(CAs)
Anova(CAs)
### N
NAs <- lmer(log(As) ~ log(N) + (1|block) + (1|species), data = d3)
summary(NAs)
### P
PAs <- lmer(log(As) ~ log(P) + (1|block) + (1|species), data = d3)
summary(PAs)
### Ca
CaAs <- lmer(log(As) ~ log(Ca) + (1|block) + (1|species), data = d3)
summary(CaAs)

### CN
CNAs <- lmer(log(As) ~ log(CN) + (1|block) + (1|species), data = d3)
summary(CNAs)
### CP
CPAs <- lmer(log(As) ~ log(CP) + (1|block) + (1|species), data = d3)
summary(CPAs)
### CCa
CCaAs <- lmer(log(As) ~ log(CCa) + (1|block) + (1|species), data = d3)
summary(CCaAs)
### NP
NPAs <- lmer(log(As) ~ log(NP) + (1|block) + (1|species), data = d3)
summary(NPAs)
### NCa
NCaAs <- lmer(log(As) ~ log(NCa) + (1|block) + (1|species), data = d3)
summary(NCaAs)
### PCa
PCaAs <- lmer(log(As) ~ log(PCa) + (1|block) + (1|species), data = d3)
summary(PCaAs)





###data analysis
#aboveground
aboveground_data<-subset(d3,organ=="Aboveground part")


aboveground_aov_C_res <- lm(log10(C)~group,data=aboveground_data)
aboveground_aov_N_res <- lm(log10(N)~group,data=aboveground_data)
aboveground_aov_P_res <- lm(log10(P)~group,data=aboveground_data)
aboveground_aov_Sb_res <- lm(log10(Sb)~group,data=aboveground_data)
aboveground_aov_As_res <- lm(log10(As)~group,data=aboveground_data)
aboveground_aov_Ca_res <- lm(log10(Ca)~group,data=aboveground_data)

summary(aboveground_aov_C_res)
summary(aboveground_aov_N_res)
summary(aboveground_aov_P_res)
summary(aboveground_aov_Sb_res)
summary(aboveground_aov_As_res)
summary(aboveground_aov_Ca_res)


shapiro.test(aboveground_aov_C_res$residuals)
shapiro.test(aboveground_aov_N_res$residuals)
shapiro.test(aboveground_aov_P_res$residuals)
shapiro.test(aboveground_aov_Sb_res$residuals)
shapiro.test(aboveground_aov_As_res$residuals)
shapiro.test(aboveground_aov_Ca_res$residuals)


aboveground_post_C_res <- HSD.test(aboveground_aov_C_res, "group")
aboveground_post_N_res <- HSD.test(aboveground_aov_N_res, "group")
aboveground_post_P_res <- HSD.test(aboveground_aov_P_res, "group")
aboveground_post_Sb_res <- HSD.test(aboveground_aov_Sb_res, "group")
aboveground_post_As_res <- HSD.test(aboveground_aov_As_res, "group")

aboveground_post_Ca_res <- dunnTest(Ca~group,method="bonferroni",data=aboveground_data)
aboveground_post_letter_Ca_res <- cldList(P.adj~Comparison,
                                          data = aboveground_post_Ca_res$res[order(aboveground_post_Ca_res$res$Z,
                                                                                   decreasing = TRUE),],
                                          threshold = 0.05 ) 

#aboveground_C
ab_C <- ggplot(aboveground_data, aes(x=species, y=C, fill = site))+
  geom_bar(stat="summary", fun=mean, position =position_dodge(0.9),colour="black", width = 0.8)+
  stat_summary(fun.data = 'mean_se', geom = 'errorbar',position =position_dodge(0.85),
               width = 0.35)+
  labs(x = "", y = expression(paste("C (g ",kg^-1,")"))) +
  theme(legend.title = element_blank())+
  theme(axis.ticks.length=unit(0.2,"cm"))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  coord_cartesian(ylim = c(300, 500))+
  scale_y_continuous(expand=c(0,0))+
  theme(legend.text=element_text(size=12))+
  theme(title=element_text(size=14))+
  theme(axis.text.x = element_text(size = 12, color = "black"))+
  theme(axis.text.y = element_text(size = 12, color = "black"))+
  theme(strip.text.x = element_text(size = 14))


#aboveground_N
ab_N <- ggplot(aboveground_data, aes(x=species, y=N, fill =site))+
  geom_bar(stat="summary", fun=mean, position =position_dodge(0.9),colour="black", width = 0.8)+
  stat_summary(fun.data = 'mean_se', geom = 'errorbar',position =position_dodge(0.85),
               width = 0.35)+
  labs(x = "", y = expression(paste("N (g ",kg^-1,")"))) +
  theme(legend.title = element_blank())+
  theme(axis.ticks.length=unit(0.2,"cm"))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  coord_cartesian(ylim = c(0, 20))+
  scale_y_continuous(expand=c(0,0))+
  theme(legend.text=element_text(size=12))+
  theme(title=element_text(size=14))+
  theme(axis.text.x = element_text(size = 12, color = "black"))+
  theme(axis.text.y = element_text(size = 12, color = "black"))+
  theme(strip.text.x = element_text(size = 14))

#aboveground_P
ab_P <- ggplot(aboveground_data, aes(x=species, y=P, fill =site))+
  geom_bar(stat="summary", fun=mean, position =position_dodge(0.9),colour="black", width = 0.8)+
  stat_summary(fun.data = 'mean_se', geom = 'errorbar',position =position_dodge(0.85),
               width = 0.35)+
  labs(x = "", y = expression(paste("P (g ",kg^-1,")"))) +
  theme(legend.title = element_blank())+
  theme(axis.ticks.length=unit(0.2,"cm"))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  coord_cartesian(ylim = c(0, 4))+
  scale_y_continuous(expand=c(0,0))+
  theme(legend.text=element_text(size=12))+
  theme(title=element_text(size=14))+
  theme(axis.text.x = element_text(size = 12, color = "black"))+
  theme(axis.text.y = element_text(size = 12, color = "black"))+
  theme(strip.text.x = element_text(size = 14))

#aboveground_Sb
ab_Sb <- ggplot(aboveground_data, aes(x=species, y=Sb, fill =site))+
  geom_bar(stat="summary", fun=mean, position =position_dodge(0.9),colour="black", width = 0.8)+
  stat_summary(fun.data = 'mean_se', geom = 'errorbar',position =position_dodge(0.85),
               width = 0.35)+
  labs(x = "", y = expression(paste("Sb (mg ",kg^-1,")"))) +
  theme(legend.title = element_blank())+
  theme(axis.ticks.length=unit(0.2,"cm"))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  coord_cartesian(ylim = c(0, 50))+
  scale_y_continuous(expand=c(0,0))+
  theme(legend.text=element_text(size=12))+
  theme(title=element_text(size=14))+
  theme(axis.text.x = element_text(size = 12, color = "black"))+
  theme(axis.text.y = element_text(size = 12, color = "black"))+
  theme(strip.text.x = element_text(size = 14))

#aboveground_As
ab_As <- ggplot(aboveground_data, aes(x=species, y=As, fill =site))+
  geom_bar(stat="summary", fun=mean, position =position_dodge(0.9),colour="black", width = 0.8)+
  stat_summary(fun.data = 'mean_se', geom = 'errorbar',position =position_dodge(0.85),
               width = 0.35)+
  labs(x = "", y = expression(paste("As (mg ",kg^-1,")"))) +
  theme(legend.title = element_blank())+
  theme(axis.ticks.length=unit(0.2,"cm"))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  coord_cartesian(ylim = c(0, 270))+
  scale_y_continuous(expand=c(0,0))+
  theme(legend.text=element_text(size=12))+
  theme(title=element_text(size=14))+
  theme(axis.text.x = element_text(size = 12, color = "black"))+
  theme(axis.text.y = element_text(size = 12, color = "black"))+
  theme(strip.text.x = element_text(size = 14))

#aboveground_Ca
ab_Ca <- ggplot(aboveground_data, aes(x=species, y=Ca, fill =site))+
  geom_bar(stat="summary", fun=mean, position =position_dodge(0.9),colour="black", width = 0.8)+
  stat_summary(fun.data = 'mean_se', geom = 'errorbar',position =position_dodge(0.85),
               width = 0.35)+
  labs(x = "", y = expression(paste("Ca (mg ",kg^-1,")"))) +
  theme(legend.title = element_blank())+
  theme(axis.ticks.length=unit(0.2,"cm"))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  coord_cartesian(ylim = c(0, 35))+
  scale_y_continuous(expand=c(0,0))+
  theme(legend.text=element_text(size=12))+
  theme(title=element_text(size=14))+
  theme(axis.text.x = element_text(size = 12, color = "black"))+
  theme(axis.text.y = element_text(size = 12, color = "black"))+
  theme(strip.text.x = element_text(size = 14))



#underground
underground_data<-subset(d3,organ=="Underground part")

underground_aov_C_res <- aov(log10(C)~group,data=underground_data)
underground_aov_N_res <- aov(log(N)~group,data=underground_data)
underground_aov_P_res <- aov(log10(P)~group,data=underground_data)
underground_aov_Sb_res <- aov(log10(Sb)~group,data=underground_data)
underground_aov_As_res <- aov(log10(As)~group,data=underground_data)
underground_aov_Ca_res <- aov(log10(Ca)~group,data=underground_data)

summary(underground_aov_C_res)
summary(underground_aov_N_res)
summary(underground_aov_P_res)
summary(underground_aov_Sb_res)
summary(underground_aov_As_res)
summary(underground_aov_Ca_res)

shapiro.test(underground_aov_C_res$residuals)
shapiro.test(underground_aov_N_res$residuals)
shapiro.test(underground_aov_P_res$residuals)
shapiro.test(underground_aov_Sb_res$residuals)
shapiro.test(underground_aov_As_res$residuals)
shapiro.test(underground_aov_Ca_res$residuals)

underground_k_C_res <- kruskal.test(C~group,data=underground_data)
underground_post_C_res <- dunnTest(C~group,method="bonferroni",data=underground_data)
underground_post_letter_C_res <- cldList(P.adj~Comparison,
                                         data = underground_post_C_res$res[order(underground_post_C_res$res$Z,decreasing = TRUE),],
                                         threshold = 0.05 ) 
underground_post_N_res <- HSD.test(underground_aov_N_res, "group")
underground_post_P_res <- HSD.test(underground_aov_P_res, "group")

underground_post_Sb_res <- HSD.test(underground_aov_Sb_res, "group")
underground_post_As_res <- dunnTest(As~group,method="bonferroni",data=underground_data)
underground_post_letter_As_res <- cldList(P.adj~Comparison,
                                          data = underground_post_As_res$res[order(underground_post_As_res$res$Z,decreasing = TRUE),],
                                          threshold = 0.05 ) 
underground_post_Ca_res <- dunnTest(Ca~group,method="bonferroni",data=underground_data)
underground_post_letter_Ca_res <- cldList(P.adj~Comparison,
                                          data = underground_post_Ca_res$res[order(underground_post_Ca_res$res$Z,decreasing = TRUE),],
                                          threshold = 0.05 ) 

############ Figure ########
#underground_C
un_C <- ggplot(underground_data, aes(x=species, y=C, fill =site))+
  geom_bar(stat="summary", fun=mean, position =position_dodge(0.9),colour="black", width = 0.8)+
  stat_summary(fun.data = 'mean_se', geom = 'errorbar',position =position_dodge(0.85),
               width = 0.35)+
  labs(x = "", y = expression(paste("C (g ",kg^-1,")"))) +
  theme(legend.title = element_blank())+
  theme(axis.ticks.length=unit(0.2,"cm"))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  coord_cartesian(ylim = c(300, 500))+
  scale_y_continuous(expand=c(0,0))+
  theme(legend.text=element_text(size=12))+
  theme(title=element_text(size=14))+
  theme(axis.text.x = element_text(size = 12, color = "black"))+
  theme(axis.text.y = element_text(size = 12, color = "black"))+
  theme(strip.text.x = element_text(size = 14))
#underground_N
un_N <- ggplot(underground_data, aes(x=species, y=N, fill =site))+
  geom_bar(stat="summary", fun=mean, position =position_dodge(0.9),colour="black", width = 0.8)+
  stat_summary(fun.data = 'mean_se', geom = 'errorbar',position =position_dodge(0.85),
               width = 0.35)+
  labs(x = "", y = expression(paste("N (g ",kg^-1,")"))) +
  theme(legend.title = element_blank())+
  theme(axis.ticks.length=unit(0.2,"cm"))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  coord_cartesian(ylim = c(0, 20))+
  scale_y_continuous(expand=c(0,0))+
  theme(legend.text=element_text(size=12))+
  theme(title=element_text(size=14))+
  theme(axis.text.x = element_text(size = 12, color = "black"))+
  theme(axis.text.y = element_text(size = 12, color = "black"))+
  theme(strip.text.x = element_text(size = 14))
#underground_P
un_P <- ggplot(underground_data, aes(x=species, y=P, fill =site))+
  geom_bar(stat="summary", fun=mean, position =position_dodge(0.9),colour="black", width = 0.8)+
  stat_summary(fun.data = 'mean_se', geom = 'errorbar',position =position_dodge(0.85),
               width = 0.35)+
  labs(x = "", y = expression(paste("P (g ",kg^-1,")"))) +
  theme(legend.title = element_blank())+
  theme(axis.ticks.length=unit(0.2,"cm"))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  coord_cartesian(ylim = c(0, 4))+
  scale_y_continuous(expand=c(0,0))+
  theme(legend.text=element_text(size=12))+
  theme(title=element_text(size=14))+
  theme(axis.text.x = element_text(size = 12, color = "black"))+
  theme(axis.text.y = element_text(size = 12, color = "black"))+
  theme(strip.text.x = element_text(size = 14))

#underground_Sb
un_Sb <- ggplot(underground_data, aes(x=species, y=Sb, fill =site))+
  geom_bar(stat="summary", fun=mean, position =position_dodge(0.9),colour="black", width = 0.8)+
  stat_summary(fun.data = 'mean_se', geom = 'errorbar',position =position_dodge(0.85),
               width = 0.35)+
  labs(x = "", y = expression(paste("Sb (mg ",kg^-1,")"))) +
  theme(legend.title = element_blank())+
  theme(axis.ticks.length=unit(0.2,"cm"))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  coord_cartesian(ylim = c(0, 250))+
  scale_y_continuous(expand=c(0,0))+
  theme(legend.text=element_text(size=12))+
  theme(title=element_text(size=14))+
  theme(axis.text.x = element_text(size = 12, color = "black"))+
  theme(axis.text.y = element_text(size = 12, color = "black"))+
  theme(strip.text.x = element_text(size = 14))

#underground_As
un_As <- ggplot(underground_data, aes(x=species, y=As, fill =site))+
  geom_bar(stat="summary", fun=mean, position =position_dodge(0.9),colour="black", width = 0.8)+
  stat_summary(fun.data = 'mean_se', geom = 'errorbar',position =position_dodge(0.85),
               width = 0.35)+
  labs(x = "", y = expression(paste("As (mg ",kg^-1,")"))) +
  theme(legend.title = element_blank())+
  theme(axis.ticks.length=unit(0.2,"cm"))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  coord_cartesian(ylim = c(0, 800))+
  scale_y_continuous(expand=c(0,0))+
  theme(legend.text=element_text(size=12))+
  theme(title=element_text(size=14))+
  theme(axis.text.x = element_text(size = 12, color = "black"))+
  theme(axis.text.y = element_text(size = 12, color = "black"))+
  theme(strip.text.x = element_text(size = 14))


#underground_Ca
un_Ca <- ggplot(underground_data, aes(x=species, y=Ca, fill =site))+
  geom_bar(stat="summary", fun=mean, position =position_dodge(0.9),colour="black", width = 0.8)+
  stat_summary(fun.data = 'mean_se', geom = 'errorbar',position =position_dodge(0.85),
               width = 0.35)+
  labs(x = "", y = expression(paste("Ca (mg ",kg^-1,")"))) +
  theme(legend.title = element_blank())+
  theme(axis.ticks.length=unit(0.2,"cm"))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  coord_cartesian(ylim = c(0, 30))+
  scale_y_continuous(expand=c(0,0))+
  theme(legend.text=element_text(size=12))+
  theme(title=element_text(size=14))+
  theme(axis.text.x = element_text(size = 12, color = "black"))+
  theme(axis.text.y = element_text(size = 12, color = "black"))+
  theme(strip.text.x = element_text(size = 14))

p_ele <- ggarrange(ab_C, un_C, ab_Sb, un_Sb,
                 ab_N, un_N, ab_As, un_As,
                 ab_P, un_P, ab_Ca, un_Ca,
                 align = "hv", common.legend = T, legend = "right")

p_ele
#ggsave("plant_element.pdf", p_ele, width = 14, height = 8)



#######-------Figure 4CNP-------####
###data analysis
#aboveground
aboveground_data<-subset(d3,organ=="Aboveground part")

aboveground_aov_CN_res <- aov(log10(CN)~group,data=aboveground_data)
aboveground_aov_CP_res <- aov(log10(CP)~group,data=aboveground_data)
aboveground_aov_NP_res <- aov(log10(NP)~group,data=aboveground_data)

aboveground_aov_CCa_res <- aov(log10(CCa)~group,data=aboveground_data)
aboveground_aov_NCa_res <- aov(log10(NCa)~group,data=aboveground_data)
aboveground_aov_PCa_res <- aov(log10(PCa)~group,data=aboveground_data)

summary(aboveground_aov_CN_res)
summary(aboveground_aov_CP_res)
summary(aboveground_aov_NP_res)
summary(aboveground_aov_CCa_res)
summary(aboveground_aov_NCa_res)
summary(aboveground_aov_PCa_res)

shapiro.test(aboveground_aov_CN_res$residuals)
shapiro.test(aboveground_aov_CP_res$residuals)
shapiro.test(aboveground_aov_NP_res$residuals)
shapiro.test(aboveground_aov_CCa_res$residuals)
shapiro.test(aboveground_aov_NCa_res$residuals)
shapiro.test(aboveground_aov_PCa_res$residuals)

aboveground_post_CN_res <- HSD.test(aboveground_aov_CN_res, "group")
aboveground_post_CP_res <- HSD.test(aboveground_aov_CP_res, "group")
aboveground_post_NP_res <- HSD.test(aboveground_aov_NP_res, "group")
aboveground_post_CCa_res <- HSD.test(aboveground_aov_CCa_res, "group")
aboveground_post_NCa_res <- HSD.test(aboveground_aov_NCa_res, "group")
aboveground_post_PCa_res <- HSD.test(aboveground_aov_PCa_res, "group")

#aboveground_CN
ab_CN <- ggplot(aboveground_data, aes(x=species, y=CN, fill =site))+
  geom_bar(stat="summary", fun=mean, position =position_dodge(0.9),colour="black", width = 0.8)+
  stat_summary(fun.data = 'mean_se', geom = 'errorbar',position =position_dodge(0.85),
               width = 0.35)+
  ylab("C:N")+xlab("")+
  theme(legend.title = element_blank())+
  theme(axis.ticks.length=unit(0.2,"cm"))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  coord_cartesian(ylim = c(0, 150))+
  scale_y_continuous(expand=c(0,0))+
  theme(legend.text=element_text(size=12))+
  theme(title=element_text(size=14))+
  theme(axis.text.x = element_text(size = 12, color = "black"))+
  theme(axis.text.y = element_text(size = 12, color = "black"))+
  theme(strip.text.x = element_text(size = 14))

#aboveground_CP
ab_CP <- ggplot(aboveground_data, aes(x=species, y=CP, fill =site))+
  geom_bar(stat="summary", fun=mean, position =position_dodge(0.9),colour="black", width = 0.8)+
  stat_summary(fun.data = 'mean_se', geom = 'errorbar',position =position_dodge(0.85),
               width = 0.35)+
  ylab("C:P")+xlab("")+
  theme(legend.title = element_blank())+
  theme(axis.ticks.length=unit(0.2,"cm"))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  coord_cartesian(ylim = c(0, 1000))+
  scale_y_continuous(expand=c(0,0))+
  theme(legend.text=element_text(size=12))+
  theme(title=element_text(size=14))+
  theme(axis.text.x = element_text(size = 12, color = "black"))+
  theme(axis.text.y = element_text(size = 12, color = "black"))+
  theme(strip.text.x = element_text(size = 14))

#aboveground_NP
ab_NP <- ggplot(aboveground_data, aes(x=species, y=NP, fill =site))+
  geom_bar(stat="summary", fun=mean, position =position_dodge(0.9),colour="black", width = 0.8)+
  stat_summary(fun.data = 'mean_se', geom = 'errorbar',position =position_dodge(0.85),
               width = 0.35)+
  ylab("N:P")+xlab("")+
  theme(legend.title = element_blank())+
  theme(axis.ticks.length=unit(0.2,"cm"))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  coord_cartesian(ylim = c(0, 15))+
  scale_y_continuous(expand=c(0,0))+
  theme(legend.text=element_text(size=12))+
  theme(title=element_text(size=14))+
  theme(axis.text.x = element_text(size = 12, color = "black"))+
  theme(axis.text.y = element_text(size = 12, color = "black"))+
  theme(strip.text.x = element_text(size = 14))

#aboveground_CCa
ab_CCa <- ggplot(aboveground_data, aes(x=species, y=CCa, fill =site))+
  geom_bar(stat="summary", fun=mean, position =position_dodge(0.9),colour="black", width = 0.8)+
  stat_summary(fun.data = 'mean_se', geom = 'errorbar',position =position_dodge(0.85),
               width = 0.35)+
  ylab("C:Ca")+xlab("")+
  theme(legend.title = element_blank())+
  theme(axis.ticks.length=unit(0.2,"cm"))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  coord_cartesian(ylim = c(0, 200))+
  scale_y_continuous(expand=c(0,0))+
  theme(legend.text=element_text(size=12))+
  theme(title=element_text(size=14))+
  theme(axis.text.x = element_text(size = 12, color = "black"))+
  theme(axis.text.y = element_text(size = 12, color = "black"))+
  theme(strip.text.x = element_text(size = 14))

#aboveground_NCa
ab_NCa <- ggplot(aboveground_data, aes(x=species, y=NCa, fill =site))+
  geom_bar(stat="summary", fun=mean, position =position_dodge(0.9),colour="black", width = 0.8)+
  stat_summary(fun.data = 'mean_se', geom = 'errorbar',position =position_dodge(0.85),
               width = 0.35)+
  ylab("N:Ca")+xlab("")+
  theme(legend.title = element_blank())+
  theme(axis.ticks.length=unit(0.2,"cm"))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  coord_cartesian(ylim = c(0, 3.5))+
  scale_y_continuous(expand=c(0,0))+
  theme(legend.text=element_text(size=12))+
  theme(title=element_text(size=14))+
  theme(axis.text.x = element_text(size = 12, color = "black"))+
  theme(axis.text.y = element_text(size = 12, color = "black"))+
  theme(strip.text.x = element_text(size = 14))

#aboveground_PCa
ab_PCa <- ggplot(aboveground_data, aes(x=species, y=PCa, fill =site))+
  geom_bar(stat="summary", fun=mean, position =position_dodge(0.9),colour="black", width = 0.8)+
  stat_summary(fun.data = 'mean_se', geom = 'errorbar',position =position_dodge(0.85),
               width = 0.35)+
  ylab("P:Ca")+xlab("")+
  theme(legend.title = element_blank())+
  theme(axis.ticks.length=unit(0.2,"cm"))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  coord_cartesian(ylim = c(0, 0.5))+
  scale_y_continuous(expand=c(0,0))+
  theme(legend.text=element_text(size=12))+
  theme(title=element_text(size=14))+
  theme(axis.text.x = element_text(size = 12, color = "black"))+
  theme(axis.text.y = element_text(size = 12, color = "black"))+
  theme(strip.text.x = element_text(size = 14))

#underground
underground_data<-subset(d3,organ=="Underground part")

underground_aov_CN_res <- aov(log10(CN)~group,data=underground_data)
underground_aov_CP_res <- aov(log10(CP)~group,data=underground_data)
underground_aov_NP_res <- aov(log10(NP)~group,data=underground_data)
underground_aov_CCa_res <- aov(log10(CCa)~group,data=underground_data)
underground_aov_NCa_res <- aov(log10(NCa)~group,data=underground_data)
underground_aov_PCa_res <- aov(log10(PCa)~group,data=underground_data)

summary(underground_aov_CN_res)
summary(underground_aov_CP_res)
summary(underground_aov_NP_res)
summary(underground_aov_CCa_res)
summary(underground_aov_NCa_res)
summary(underground_aov_PCa_res)

shapiro.test(underground_aov_CN_res$residuals)
shapiro.test(underground_aov_CP_res$residuals)
shapiro.test(underground_aov_NP_res$residuals)
shapiro.test(underground_aov_CCa_res$residuals)
shapiro.test(underground_aov_NCa_res$residuals)
shapiro.test(underground_aov_PCa_res$residuals)


underground_post_CN_res <- HSD.test(underground_aov_CN_res, "group")
underground_post_CP_res <- HSD.test(underground_aov_CP_res, "group")
underground_post_NP_res <- dunnTest(NP~group,method="bonferroni",data=underground_data)
underground_post_letter_NP_res <- cldList(P.adj~Comparison,
                                          data = underground_post_NP_res$res[order(underground_post_NP_res$res$Z,decreasing = TRUE),],#?????????????????????
                                          threshold = 0.05 ) 
underground_post_CCa_res <- HSD.test(underground_aov_CCa_res, "group")
underground_post_NCa_res <- HSD.test(underground_aov_NCa_res, "group")
underground_post_PCa_res <- HSD.test(underground_aov_PCa_res, "group")


#underground_CN
un_CN <- ggplot(underground_data, aes(x=species, y=CN, fill =site))+
  geom_bar(stat="summary", fun=mean, position =position_dodge(0.9),colour="black", width = 0.8)+
  stat_summary(fun.data = 'mean_se', geom = 'errorbar',position =position_dodge(0.85),
               width = 0.35)+
  ylab("C:N")+xlab("")+
  theme(legend.title = element_blank())+
  theme(axis.ticks.length=unit(0.2,"cm"))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  coord_cartesian(ylim = c(0, 150))+
  scale_y_continuous(expand=c(0,0))+
  theme(legend.text=element_text(size=12))+
  theme(title=element_text(size=14))+
  theme(axis.text.x = element_text(size = 12, color = "black"))+
  theme(axis.text.y = element_text(size = 12, color = "black"))+
  theme(strip.text.x = element_text(size = 14))

#underground_CP
un_CP <- ggplot(underground_data, aes(x=species, y=CP, fill =site))+
  geom_bar(stat="summary", fun=mean, position =position_dodge(0.9),colour="black", width = 0.8)+
  stat_summary(fun.data = 'mean_se', geom = 'errorbar',position =position_dodge(0.85),
               width = 0.35)+
  ylab("C:P")+xlab("")+
  theme(legend.title = element_blank())+
  theme(axis.ticks.length=unit(0.2,"cm"))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  coord_cartesian(ylim = c(0, 1000))+
  scale_y_continuous(expand=c(0,0))+
  theme(legend.text=element_text(size=12))+
  theme(title=element_text(size=14))+
  theme(axis.text.x = element_text(size = 12, color = "black"))+
  theme(axis.text.y = element_text(size = 12, color = "black"))+
  theme(strip.text.x = element_text(size = 14))

#underground_NP
un_NP <- ggplot(underground_data, aes(x=species, y=NP, fill =site))+
  geom_bar(stat="summary", fun=mean, position =position_dodge(0.9),colour="black", width = 0.8)+
  stat_summary(fun.data = 'mean_se', geom = 'errorbar',position =position_dodge(0.85),
               width = 0.35)+
  ylab("N:P")+xlab("")+
  theme(legend.title = element_blank())+
  theme(axis.ticks.length=unit(0.2,"cm"))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  coord_cartesian(ylim = c(0, 15))+
  scale_y_continuous(expand=c(0,0))+
  theme(legend.text=element_text(size=12))+
  theme(title=element_text(size=14))+
  theme(axis.text.x = element_text(size = 12, color = "black"))+
  theme(axis.text.y = element_text(size = 12, color = "black"))+
  theme(strip.text.x = element_text(size = 14))

#underground_CCa
un_CCa <- ggplot(underground_data, aes(x=species, y=CCa, fill =site))+
  geom_bar(stat="summary", fun=mean, position =position_dodge(0.9),colour="black", width = 0.8)+
  stat_summary(fun.data = 'mean_se', geom = 'errorbar',position =position_dodge(0.85),
               width = 0.35)+
  ylab("C:Ca")+xlab("")+
  theme(legend.title = element_blank())+
  theme(axis.ticks.length=unit(0.2,"cm"))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  coord_cartesian(ylim = c(0, 200))+
  scale_y_continuous(expand=c(0,0))+
  theme(legend.text=element_text(size=12))+
  theme(title=element_text(size=14))+
  theme(axis.text.x = element_text(size = 12, color = "black"))+
  theme(axis.text.y = element_text(size = 12, color = "black"))+
  theme(strip.text.x = element_text(size = 14))

#underground_NCa
un_NCa <- ggplot(underground_data, aes(x=species, y=NCa, fill =site))+
  geom_bar(stat="summary", fun=mean, position =position_dodge(0.9),colour="black", width = 0.8)+
  stat_summary(fun.data = 'mean_se', geom = 'errorbar',position =position_dodge(0.85),
               width = 0.35)+
  ylab("N:Ca")+xlab("")+
  theme(legend.title = element_blank())+
  theme(axis.ticks.length=unit(0.2,"cm"))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  coord_cartesian(ylim = c(0, 3.5))+
  scale_y_continuous(expand=c(0,0))+
  theme(legend.text=element_text(size=12))+
  theme(title=element_text(size=14))+
  theme(axis.text.x = element_text(size = 12, color = "black"))+
  theme(axis.text.y = element_text(size = 12, color = "black"))+
  theme(strip.text.x = element_text(size = 14))

#underground_PCa
un_PCa <- ggplot(underground_data, aes(x=species, y=PCa, fill =site))+
  geom_bar(stat="summary", fun=mean, position =position_dodge(0.9),colour="black", width = 0.8)+
  stat_summary(fun.data = 'mean_se', geom = 'errorbar',position =position_dodge(0.85),
               width = 0.35)+
  ylab("P:Ca")+xlab("")+
  theme(legend.title = element_blank())+
  theme(axis.ticks.length=unit(0.2,"cm"))+
  theme_bw()+
  theme(panel.grid=element_blank())+
  coord_cartesian(ylim = c(0, 0.5))+
  scale_y_continuous(expand=c(0,0))+
  theme(legend.text=element_text(size=12))+
  theme(title=element_text(size=14))+
  theme(axis.text.x = element_text(size = 12, color = "black"))+
  theme(axis.text.y = element_text(size = 12, color = "black"))+
  theme(strip.text.x = element_text(size = 14))

plot_grid(ab_CN, un_CN, ab_CCa, un_CCa,
          ab_CP, un_CP, ab_NCa, un_NCa,
          ab_NP, un_NP, ab_PCa, un_PCa,
          ncol=4,
          nrow=3,
          align="vh")

p_ratio <- ggarrange(ab_CN, un_CN, ab_CCa, un_CCa,
                   ab_CP, un_CP, ab_NCa, un_NCa,
                   ab_NP, un_NP, ab_PCa, un_PCa,
                   align = "hv", common.legend = T, legend = "right")

#ggsave("plant_ratio.pdf", p_ratio, width = 14, height = 8)

```
