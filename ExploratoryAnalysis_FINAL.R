####
# STUDY: INTERGENERATIONAL COHABITATION AND WELFARE ATTITUDES AMONG EUROPEAN YOUNG ADULTS
####

#### Gonzalo Arévalo-Iglesias, University of Bremen 

##
# EXPLORATORY ANALYSES
##

##############################################################

# LOAD PROCESSED DATA 

load("ESS2016_session.RData") 

library(ggplot2)
library(cowplot)
library(gridExtra)
library(extrafont)
library(psych)
library(polycor)
library(dplyr)
library(plyr)
library(plotly)
library(ggpubr)
library(psych)
library(rcompanion)
library(sjstats)
library(Hmisc)

loadfonts() # Load the available fonts in Windows

##############################################################

## STEP 1: DEPENDENT VARIABLES

# 1.1) DISTRIBUTION OF DEPENDENT VARIABLES

## ATTITUDES TOWARDS UNEMPLOYMENT BENEFITS

## Proportions for countries and regimes 
round(prop.table(table(ESS2016$cntry[A18_39Ind16],ESS2016$gvslvue_grp[A18_39Ind16]),1),2) 
round(prop.table(table(ESS2016$regime[A18_39Ind16],ESS2016$gvslvue_grp[A18_39Ind16]),1),2)

## Plot 
gvslvue_cntry<-ggplot(data=na.omit(ESS2016[A18_39Ind16, c("cntry", "gvslvue_grp")])) +
aes(x = cntry,fill=gvslvue_grp)+
geom_bar(position="fill")+
scale_fill_manual(values = c("#e41a1d","#f5d214","#289b91","lightgreen","#e1fafa"),
labels=c("Disagree strongly","Disagree","Neither agree nor disagree",
"Agree","Agree strongly"))+
scale_y_reverse()+
labs(x="Country",y="",fill="Government responsibility for unemployed",title="")+
 theme(plot.title = element_text(family="Times New Roman",size=20,face="bold",hjust=0.5),
        axis.title=element_text(family = "Times New Roman", size = 13,face="bold"),
        axis.text = element_text(family = "Times New Roman", size = 12,face="bold"),
        axis.text.x = element_text(hjust=1,margin=margin(-20,0,0,0), size = 13, angle=90,face="bold"),
        axis.ticks.x = element_blank(),
        legend.title = element_text(family = "Times New Roman", size = 13,face="bold"),
        legend.text = element_text(family = "Times New Roman", size = 11,face="bold"),
        panel.background = element_rect(fill = "white"))+
guides(fill = guide_legend(reverse = TRUE))

ggsave(
  filename="gvslvue_cntry.tiff",plot=gvslvue_cntry,device="tiff",
  width=30,height=20,units="cm",dpi=300)


## ATTITUDES TOWARDS BASIC INCOME

## Proportions for countries and regimes 
round(prop.table(table(ESS2016$cntry[A18_39Ind16],ESS2016$basinc[A18_39Ind16]),1),2)
round(prop.table(table(ESS2016$regime[A18_39Ind16],ESS2016$basinc[A18_39Ind16]),1),2)

## Plot 
basinc_cntry<-ggplot(data=na.omit(ESS2016[A18_39Ind16, c("cntry", "basinc")])) +
aes(x = cntry,fill=basinc)+
geom_bar(position="fill")+
scale_fill_manual(values = c("#e41a1d","#f5d214","#289b91","#e1fafa"),
labels=c("Strongly against","Against","In favour","Strongly in favour"))+
scale_y_reverse()+
labs(x="Country",y="",fill="Implementation of basic income",title="")+
 theme(plot.title = element_text(family="Times New Roman",size=20,face="bold",hjust=0.5),
        axis.title=element_text(family = "Times New Roman", size = 13,face="bold"),
        axis.text = element_text(family = "Times New Roman", size = 12,face="bold"),
        axis.text.x = element_text(hjust=1,margin=margin(-20,0,0,0), size = 13, angle=90,face="bold"),
        axis.ticks.x = element_blank(),
        legend.title = element_text(family = "Times New Roman", size = 13,face="bold"),
        legend.text = element_text(family = "Times New Roman", size = 11,face="bold"),
        panel.background = element_rect(fill = "white"))+
guides(fill = guide_legend(reverse = TRUE))

ggsave(
  filename="basinc_cntry.tiff",plot=basinc_cntry,device="tiff",
  width=30,height=20,units="cm",dpi=300)


# 1.2) RELATIONS BETWEEN BOTH DEPENDENT VARIABLES

## Correlation:
cor(ESS2016$basinc_num,ESS2016$gvslvue_grp_num,use="complete.obs") # In the whole ESS sample
cor(ESS2016$basinc_num[A18_39Ind16],ESS2016$gvslvue_grp_num[A18_39Ind16],use="complete.obs") # In the young adult subsample
# They are roughly the same (0.12)


## Can they be combined in a single index?
alpha(data.frame(cbind(ESS2016$basinc,ESS2016$gvslvue_grp))) # No
# Also adding attitudes towards redistribution to the index
alpha(data.frame(cbind(as.factor(ESS2016$gincdif),ESS2016$basinc,ESS2016$gvslvue_grp))) No

##############################################################

## STEP 2: INDEPENDENT VARIABLES

## 2.1) INTERGENERATIONAL COHABITATION


# PROPORTIONS OF COHABITATION BY COUNTRY
round(prop.table(table(ESS2016$cntry[A18_39Ind16],ESS2016$lwparents_bin[A18_39Ind16]),1),2)

# PLOT: COHABITATION WITH PARENTS IN EACH COUNTRY
## A plot which every country in the sample

ESS2016$orderplot<-factor(ESS2016$cntry,levels=c("Spain","Italy","Poland","Belgium",
"Portugal","Hungary","Switzerland","Czechia","Germany","Netherlands","Estonia",
"Norway","Sweden","Austria","France","Finland"))

ChR_cntry<-ggplot(data = na.omit(ESS2016[A18_39Ind16, c("orderplot", "lwparents_bin")])) +
  aes(x = factor(orderplot), fill = lwparents_bin) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#289b91", "#f5d214"),
                    labels=c("No","Yes"))+
  labs(x = "", y = "",title="",fill="Lives with parents") +
  theme(plot.title=element_text(family="Times New Roman",size=20,face="bold",hjust=0.5),
        axis.title=element_text(family = "Times New Roman", size = 13,face="bold"),
        axis.text = element_text(family = "Times New Roman", size = 12,face="bold"),
        axis.text.x = element_text(hjust=1,margin=margin(-20,0,0,0), size = 13, angle=90,face="bold"),
        axis.ticks.x = element_blank(),
        legend.title = element_text(family = "Times New Roman", size = 13,face="bold"),
        legend.text = element_text(family = "Times New Roman", size = 11,face="bold"),
        panel.background = element_rect(fill = "white")) 

ggsave(
  filename="ChR_cntry.tiff",plot=ChR_cntry,device="tiff",
  width=30,height=20,units="cm",dpi=300)
             
##########

# PROPORTIONS OF COHABITATION WITH PARENTS BY REGIME AND AGE GROUP

# For all individuals between 18 and 39
ChR_table<-round(prop.table(cbind(summary(na.omit(ESS2016$lwparents_bin[Med18_39Ind16])),
summary(na.omit(ESS2016$lwparents_bin[Con18_39Ind16])),
summary(na.omit(ESS2016$lwparents_bin[Sca18_39Ind16])),
summary(na.omit(ESS2016$lwparents_bin[East18_39Ind16])),
summary(na.omit(ESS2016$lwparents_bin[A18_39Ind16]))),margin=2),2)
colnames(ChR_table)=c("Mediterranean","Continental","Scandinavian","Eastern","Total")
ChR_table

# PLOT: COHABITATION WITH PARENTS BY AGE IN EACH REGIME

pMed<-ggplot(data = na.omit(ESS2016[Med18_39Ind16, c("agea", "lwparents_bin")])) +
  aes(x = factor(agea), fill = lwparents_bin) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#289b91", "#f5d214"),
                    labels=c("No","Yes"))+
  labs(x = "Age", y = "",fill="Lives with parents") +
  ggtitle("Mediterranean") +
  theme(plot.title = element_text(family="Times New Roman",face="bold",
        size = 15, hjust = 0.5,vjust=-1.5),
        axis.title=element_text(family = "Times New Roman",face="bold", size = 13,),
        axis.text = element_text(family = "Times New Roman",face="bold", size = 12),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(family = "Times New Roman",face="bold", size = 13),
        legend.text = element_text(family = "Times New Roman",face="bold", size = 11),
        panel.background = element_rect(fill = "white"))+
  scale_x_discrete(breaks = seq(18, 39, 2))
pMed<-pMed+theme(legend.position = "none")
pMed

pCon<-ggplot(data = na.omit(ESS2016[Con18_39Ind16, c("agea", "lwparents_bin")])) +
  aes(x = factor(agea), fill = lwparents_bin) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#289b91", "#f5d214"),
                    labels=c("No","Yes"))+
  labs(x = "Age", y = "",fill="Lives with parents") +
  ggtitle("Continental") +
  theme(plot.title = element_text(family="Times New Roman",face="bold",
        size = 15, hjust = 0.5,vjust=-1.5),
        axis.title=element_text(family = "Times New Roman",face="bold", size = 13),
        axis.text = element_text(family = "Times New Roman",face="bold", size = 12),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(family = "Times New Roman",face="bold", size = 13),
        legend.text = element_text(family = "Times New Roman",face="bold", size = 11),
        panel.background = element_rect(fill = "white"))+
  scale_x_discrete(breaks = seq(18, 39, 2))
pCon<-pCon+theme(legend.position = "none")
pCon

pSca<-ggplot(data = na.omit(ESS2016[Sca18_39Ind16, c("agea", "lwparents_bin")])) +
  aes(x = factor(agea), fill = lwparents_bin) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#289b91", "#f5d214"),
                    labels=c("No","Yes"))+
  labs(x = "Age", y = "",fill="Lives with parents") +
  ggtitle("Scandinavian") +
  theme(plot.title = element_text(family="Times New Roman",face="bold",
        size = 15, hjust = 0.5,vjust=-1.5),
        axis.title=element_text(family = "Times New Roman",face="bold", size = 13),
        axis.text = element_text(family = "Times New Roman",face="bold", size = 12),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(family = "Times New Roman",face="bold", size = 13),
        legend.text = element_text(family = "Times New Roman",face="bold", size = 11),
        panel.background = element_rect(fill = "white"))+
  scale_x_discrete(breaks = seq(18, 39, 2))
pSca<-pSca+theme(legend.position = "none")
pSca

pEast<-ggplot(data = na.omit(ESS2016[East18_39Ind16, c("agea", "lwparents_bin")])) +
  aes(x = factor(agea), fill = lwparents_bin) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#289b91", "#f5d214"),
                    labels=c("No","Yes"))+
  labs(x = "Age", y = "",fill="Lives with parents") +
  ggtitle("Eastern") +
  theme(plot.title = element_text(family="Times New Roman",face="bold",
        size = 15, hjust = 0.5,vjust=-1.5),
        axis.title=element_text(family = "Times New Roman",face="bold", size = 13),
        axis.text = element_text(family = "Times New Roman",face="bold", size = 12),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(family = "Times New Roman",face="bold", size = 13),
        legend.text = element_text(family = "Times New Roman",face="bold", size = 11),
        panel.background = element_rect(fill = "white"))+
  scale_x_discrete(breaks = seq(18, 39, 2))
pEast<-pEast+theme(legend.position = "none")
pEast 

legend <- get_legend(pMed + theme(legend.position = "left",
legend.margin = margin(0, 40, 0, 0)))

ChR_rgm<-grid.arrange(arrangeGrob(pMed, pSca, nrow = 2),
             arrangeGrob(pCon, pEast, nrow = 2),
             legend,
             ncol = 3, widths = c(2, 2, 1))

ggsave(
  filename="ChR_rgm.tiff",plot=ChR_rgm,device="tiff",
  width=35,height=20,units="cm",dpi=300)

##########

## Proportions of cohabitation with parents by gender

pMed<-ggplot(data = na.omit(ESS2016[Med18_39Ind16, c("gndr", "lwparents_bin")])) +
  aes(x = factor(gndr), fill = lwparents_bin) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#289b91", "#f5d214"),
                    labels=c("No","Yes"))+
  labs(x = "", y = "",fill="Lives with parents") +
  ggtitle("Mediterranean") +
  theme(plot.title = element_text(family="Times New Roman",face="bold",
        size = 15, hjust = 0.5,vjust=-1.5),
        axis.title=element_text(family = "Times New Roman",face="bold", size = 13,),
        axis.text = element_text(family = "Times New Roman",face="bold", size = 12),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(family = "Times New Roman",face="bold", size = 13),
        legend.text = element_text(family = "Times New Roman",face="bold", size = 11),
        panel.background = element_rect(fill = "white"))
pMed<-pMed+theme(legend.position = "none")
pMed

pCon<-ggplot(data = na.omit(ESS2016[Con18_39Ind16, c("gndr", "lwparents_bin")])) +
  aes(x = factor(gndr), fill = lwparents_bin) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#289b91", "#f5d214"),
                    labels=c("No","Yes"))+
  labs(x = "", y = "",fill="Lives with parents") +
  ggtitle("Continental") +
  theme(plot.title = element_text(family="Times New Roman",face="bold",
        size = 15, hjust = 0.5,vjust=-1.5),
        axis.title=element_text(family = "Times New Roman",face="bold", size = 13),
        axis.text = element_text(family = "Times New Roman",face="bold", size = 12),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(family = "Times New Roman",face="bold", size = 13),
        legend.text = element_text(family = "Times New Roman",face="bold", size = 11),
        panel.background = element_rect(fill = "white"))
pCon<-pCon+theme(legend.position = "none")
pCon

pSca<-ggplot(data = na.omit(ESS2016[Sca18_39Ind16, c("gndr", "lwparents_bin")])) +
  aes(x = factor(gndr), fill = lwparents_bin) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#289b91", "#f5d214"),
                    labels=c("No","Yes"))+
  labs(x = "", y = "",fill="Lives with parents") +
  ggtitle("Scandinavian") +
  theme(plot.title = element_text(family="Times New Roman",face="bold",
        size = 15, hjust = 0.5,vjust=-1.5),
        axis.title=element_text(family = "Times New Roman",face="bold", size = 13),
        axis.text = element_text(family = "Times New Roman",face="bold", size = 12),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(family = "Times New Roman",face="bold", size = 13),
        legend.text = element_text(family = "Times New Roman",face="bold", size = 11),
        panel.background = element_rect(fill = "white"))
pSca<-pSca+theme(legend.position = "none")
pSca

pEast<-ggplot(data = na.omit(ESS2016[East18_39Ind16, c("gndr", "lwparents_bin")])) +
  aes(x = factor(gndr), fill = lwparents_bin) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#289b91", "#f5d214"),
                    labels=c("No","Yes"))+
  labs(x = "", y = "",fill="Lives with parents") +
  ggtitle("Eastern") +
  theme(plot.title = element_text(family="Times New Roman",face="bold",
        size = 15, hjust = 0.5,vjust=-1.5),
        axis.title=element_text(family = "Times New Roman",face="bold", size = 13),
        axis.text = element_text(family = "Times New Roman",face="bold", size = 12),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(family = "Times New Roman",face="bold", size = 13),
        legend.text = element_text(family = "Times New Roman",face="bold", size = 11),
        panel.background = element_rect(fill = "white"))
pEast<-pEast+theme(legend.position = "none")
pEast 

legend <- get_legend(pMed + theme(legend.position = "left",
legend.margin = margin(0, 50, 0, 0)))

ChR_gndr<-grid.arrange(arrangeGrob(pMed, pSca, nrow = 2),
             arrangeGrob(pCon, pEast, nrow = 2),
             legend,
             ncol = 3, widths = c(2, 2, 1))

# In line with the expectations, young men have an average higher rate of cohabitation with parents than young women.

ggsave(
  filename="ChR_gndr.tiff",plot=ChR_gndr,device="tiff",
  width=30,height=20,units="cm",dpi=300)


##########

# 2.2) ACTIVITY STATUS


# PROPORTIONS PER COUNTRY
round(prop.table(table(ESS2016$cntry[A18_39Ind16],ESS2016$act_4c[A18_39Ind16]),1),2)

# PROPORTIONS PER REGIME
t1<-round(prop.table(cbind(summary(na.omit(ESS2016$act_4c[Med18_39Ind16])),
summary(na.omit(ESS2016$act_4c[Con18_39Ind16])),
summary(na.omit(ESS2016$act_4c[Sca18_39Ind16])),
summary(na.omit(ESS2016$act_4c[East18_39Ind16])),
summary(na.omit(ESS2016$act_4c[A18_39Ind16]))),margin=2),2)
colnames(t1)=c("Mediterranean","Continental","Scandinavian","Eastern","Total")
t1

# PLOT: PROPORTIONS PER REGIME AND AGE GROUP
pMed<-ggplot(data = na.omit(ESS2016[Med18_39Ind16, c("agea", "act_4c")])) +
  aes(x = factor(agea), fill = act_4c) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#e1fafa","#289b91","#f5d214","#e41a1d"),
                    labels=c("Securely employed","Atypically employed","NEET","In education"))+
  labs(x = "Age", y = "",fill="Activity") +
  ggtitle("Mediterranean") +
  theme(plot.title = element_text(family="Times New Roman",face="bold",
        size = 15, hjust = 0.5,vjust=-1.5),
        axis.title=element_text(family = "Times New Roman", face="bold",size = 13),
        axis.text = element_text(family = "Times New Roman", face="bold",size = 12),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(family = "Times New Roman", face="bold",size = 13),
        legend.text = element_text(family = "Times New Roman", face="bold",size = 11),
        panel.background = element_rect(fill = "white"))+
  scale_x_discrete(breaks = seq(18, 39, 2))
pMed<-pMed+theme(legend.position = "none")

pCon<-ggplot(data = na.omit(ESS2016[Con18_39Ind16, c("agea", "act_4c")])) +
  aes(x = factor(agea), fill = act_4c) +
  geom_bar(position = "fill") +
 scale_fill_manual(values = c("#e1fafa","#289b91","#f5d214","#e41a1d"),
                    labels=c("Securely employed","Atypically employed","NEEP","In education"))+
  labs(x = "Age", y = "",fill="Activity") +
  ggtitle("Continental") +
  theme(plot.title = element_text(family="Times New Roman",face="bold",
        size = 15, hjust = 0.5,vjust=-1.5),
        axis.title=element_text(family = "Times New Roman",face="bold", size = 13),
        axis.text = element_text(family = "Times New Roman",face="bold", size = 12),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(family = "Times New Roman",face="bold", size = 13),
        legend.text = element_text(family = "Times New Roman",face="bold", size = 11),
        panel.background = element_rect(fill = "white"))+
  scale_x_discrete(breaks = seq(18, 39, 2))
pCon<-pCon+theme(legend.position = "none")

pSca<-ggplot(data = na.omit(ESS2016[Sca18_39Ind16, c("agea", "act_4c")])) +
  aes(x = factor(agea), fill = act_4c) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#e1fafa","#289b91","#f5d214","#e41a1d"),
                    labels=c("Securely employed","Atypically employed","NEEP","In education"))+
  labs(x = "Age", y = "",fill="Activity") +
  ggtitle("Scandinavian") +
  theme(plot.title = element_text(family="Times New Roman",face="bold",
        size = 15, hjust = 0.5,vjust=-1.5),
        axis.title=element_text(family = "Times New Roman",face="bold", size = 13),
        axis.text = element_text(family = "Times New Roman",face="bold", size = 12),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(family = "Times New Roman",face="bold", size = 13),
        legend.text = element_text(family = "Times New Roman",face="bold", size = 11),
        panel.background = element_rect(fill = "white"))+
  scale_x_discrete(breaks = seq(18, 39, 2))
pSca<-pSca+theme(legend.position = "none")

pEast<-ggplot(data = na.omit(ESS2016[East18_39Ind16, c("agea", "act_4c")])) +
  aes(x = factor(agea), fill = act_4c) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#e1fafa","#289b91","#f5d214","#e41a1d"),
                    labels=c("Securely employed","Atypically employed","NEEP","In education"))+
  labs(x = "Age", y = "",fill="Activity") +
  ggtitle("Eastern") +
  theme(plot.title = element_text(family="Times New Roman",face="bold",
        size = 15, hjust = 0.5,vjust=-1.5),
        axis.title=element_text(family = "Times New Roman", face="bold",size = 13),
        axis.text = element_text(family = "Times New Roman", face="bold",size = 12),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(family = "Times New Roman", face="bold",size = 13),
        legend.text = element_text(family = "Times New Roman", face="bold",size = 11),
        panel.background = element_rect(fill = "white"))+
  scale_x_discrete(breaks = seq(18, 39, 2))
pEast<-pEast+theme(legend.position = "none")

legend <- get_legend(pMed + theme(legend.position = "left",
legend.margin = margin(0, 20, 0, 0)))

act_rgm<-grid.arrange(arrangeGrob(pMed, pSca, nrow = 2),
             arrangeGrob(pCon, pEast, nrow = 2),
             legend,
             ncol = 3, widths = c(2, 2, 1))

ggsave(
  filename="act_rgm.tiff",plot=act_rgm,device="tiff",
  width=40,height=20,units="cm",dpi=300)

# The results are very consistent with the expectations. For all age groups, Southern countries exhibit
# a much lower share of securely employed, with high rates of unemployment and atypical employment.
# The share of people in education is comparatively high in Scandinavian and Southern countries.

########################################################################################

# 2.3) CORRELATION BETWEEN ACTIVITY STATUS AND COHABITATION BY REGIME AND AGE GROUP

t15<-cbind(c(cramerV(table(ESS2016$act_4c[Med18_39Ind16],ESS2016$lwparents_bin[Med18_39Ind16])),
cramerV(table(ESS2016$act_4c[Med18_24Ind16],ESS2016$lwparents_bin[Med18_24Ind16])),
cramerV(table(ESS2016$act_4c[Med25_29Ind16],ESS2016$lwparents_bin[Med25_29Ind16])),
cramerV(table(ESS2016$act_4c[Med30_34Ind16],ESS2016$lwparents_bin[Med30_34Ind16])),
cramerV(table(ESS2016$act_4c[Med35_39Ind16],ESS2016$lwparents_bin[Med35_39Ind16]))),
c(cramerV(table(ESS2016$act_4c[Con18_39Ind16],ESS2016$lwparents_bin[Con18_39Ind16])),
cramerV(table(ESS2016$act_4c[Con18_24Ind16],ESS2016$lwparents_bin[Con18_24Ind16])),
cramerV(table(ESS2016$act_4c[Con25_29Ind16],ESS2016$lwparents_bin[Con25_29Ind16])),
cramerV(table(ESS2016$act_4c[Con30_34Ind16],ESS2016$lwparents_bin[Con30_34Ind16])),
cramerV(table(ESS2016$act_4c[Con35_39Ind16],ESS2016$lwparents_bin[Con35_39Ind16]))),
c(cramerV(table(ESS2016$act_4c[Sca18_39Ind16],ESS2016$lwparents_bin[Sca18_39Ind16])),
cramerV(table(ESS2016$act_4c[Sca18_24Ind16],ESS2016$lwparents_bin[Sca18_24Ind16])),
cramerV(table(ESS2016$act_4c[Sca25_29Ind16],ESS2016$lwparents_bin[Sca25_29Ind16])),
cramerV(table(ESS2016$act_4c[Sca30_34Ind16],ESS2016$lwparents_bin[Sca30_34Ind16])),
cramerV(table(ESS2016$act_4c[Sca35_39Ind16],ESS2016$lwparents_bin[Sca35_39Ind16]))),
c(cramerV(table(ESS2016$act_4c[East18_39Ind16],ESS2016$lwparents_bin[East18_39Ind16])),
cramerV(table(ESS2016$act_4c[East18_24Ind16],ESS2016$lwparents_bin[East18_24Ind16])),
cramerV(table(ESS2016$act_4c[East25_29Ind16],ESS2016$lwparents_bin[East25_29Ind16])),
cramerV(table(ESS2016$act_4c[East30_34Ind16],ESS2016$lwparents_bin[East30_34Ind16])),
cramerV(table(ESS2016$act_4c[East35_39Ind16],ESS2016$lwparents_bin[East35_39Ind16]))))
t15<-round(t15,2)
colnames(t15)=c("Mediterranean","Continental","Scandinavian","Eastern")
rownames(t15)=c("18-39","18-24","25-29","30-34","35-39")
t15

# As expected, the correlation between activity status and cohabitation with parents is higher in Mediterranean and
# Eastern countries, lower in Scandinavian, and somewhere in-between for Continental countries. 

########################################################################################

## STEP 3: POLICY FACTORS 

# 3.1) INVESTMENT IN SOCIAL PROTECTION BENEFITS

Eurostat_SPB16$cntry<-factor(c("Austria","Belgium","Switzerland","Czechia","Germany","Estonia","Spain","Finland",
"France","Hungary","Italy","Netherlands","Norway","Poland","Portugal","Sweden"))

Eurostat_SPB16$OBS_VALUE<-Eurostat_SPB16$OBS_VALUE/1000
Eurostat_SPB16<-Eurostat_SPB16[order(Eurostat_SPB16$OBS_VALUE),]

SPB_cntry<-ggplot(Eurostat_SPB16, aes(x = reorder(cntry, OBS_VALUE), y = OBS_VALUE, fill=cntry)) +
  geom_bar(stat="identity") +
  labs(x = "Country", y = "Investment in 1000 PPS/inhabitant",title="") +
  scale_fill_manual(values=rep("#f5d214",16))+
  ylim(0,15) +
  theme_minimal()+
  theme(
    plot.title = element_text(family="Times New Roman",size=20,face="bold",hjust=0.5),
    plot.background = element_rect(fill = "white"),    
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, family = "Times New Roman",size=12,face="bold"),  # Rotate and set font for x-axis labels
    axis.text.y = element_text(family = "Times New Roman",face="bold"),  # Set font for y-axis labels
    axis.title = element_text(family = "Times New Roman",face="bold"),  # Set font for axis titles
    legend.position = "none")  # Set font for legend title

SPB_cntry<-SPB_cntry + geom_text(data=Eurostat_SPB16,
       aes(x = cntry,
           y = OBS_VALUE + 0.1,
           label = round(OBS_VALUE,2),
           vjust = -0.5,
           family = "Times New Roman"))

ggsave(
  filename="SPB_cntry.tiff",plot=SPB_cntry,device="tiff",
  width=20,height=20,units="cm",dpi=300)

###

# 3.2) INVESTMENT IN LABOUR MARKETS (DISSAGGREGATION)

OECD_LMP<-read.csv2("OECD_UnemploymentPolicy.csv",sep=",")

OECD_LMP$Country<-as.factor(OECD_LMP$Country)
OECD_LMP$Country=revalue(OECD_LMP$Country,c("Czech Republic"="Czechia"))
OECD_LMP$Time<-as.factor(OECD_LMP$Time)
OECD_LMP$Programmes<-as.factor(OECD_LMP$Programmes)
OECD_LMP$Value<-as.numeric(OECD_LMP$Value)

# Imput missing value for Italy
OECD_LMP<-OECD_LMP[c(which(OECD_LMP$Country!="Italy" & OECD_LMP$Time=="2016"),
which(OECD_LMP$Country=="Italy" & OECD_LMP$Time=="2015")),]

# Create the short dataset we need with dissaggregation of the investment in active and passive
OECD_LMP[,c("Country","Programmes","Time")]

Total<-OECD_LMP$Value[which(OECD_LMP$Programmes=="Total")]
names(Total)<-OECD_LMP$Country[which(OECD_LMP$Programmes=="Total")]

Active_Programmes<-OECD_LMP$Value[which(OECD_LMP$Programmes=="Active programmes (10-70)")]
names(Active_Programmes)<-OECD_LMP$Country[which(OECD_LMP$Programmes=="Active programmes (10-70)")]

Passive_Measures<-OECD_LMP$Value[which(OECD_LMP$Programmes=="Passive measures (80-90)")]
names(Passive_Measures)<-OECD_LMP$Country[which(OECD_LMP$Programmes=="Passive measures (80-90)")]

Active<-Active_Programmes
Passive<-Passive_Measures
Country<-OECD_LMP$Country[which(OECD_LMP$Programmes=="Passive measures (80-90)")]

plotdata<-data.frame(Country,Active,Passive,Total)
plotdata<-plotdata %>% arrange(Total)

plotdata_long <- tidyr::pivot_longer(plotdata, cols = c(Active, Passive), names_to = "Type", values_to = "Value")

plotdata_long$Country<-factor(plotdata_long$Country,levels=plotdata$Country)

# PLOT: INVESTMENT IN LABOUR MARKETS DISSAGGREGATED IN ACTIVE AND PASSIVE MEASURES

par(mfrow=c(2,2))
par(mar=c(4,4,3,4))
par(family="Times New Roman",cex.axis = 1.2, cex.lab = 1.2)

LMI_cntry<-ggplot(plotdata_long, aes(x = Country, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "", y = "Investment in % of GDP", title="",fill = "Type") +
  theme_minimal()+
  scale_fill_manual(values = c("#289b91", "#f5d214"),
                    labels=c("Active","Passive"))+
  theme(
    plot.title = element_text(family="Times New Roman",face="bold",hjust=0.5),
    plot.background = element_rect(fill = "white"),  # Set background to white
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, family = "Times New Roman",size=12,face="bold"),  # Rotate and set font for x-axis labels
    axis.text.y = element_text(family = "Times New Roman",face="bold"),  # Set font for y-axis labels
    axis.title = element_text(family = "Times New Roman",face="bold"),  # Set font for axis titles
    legend.text = element_text(family = "Times New Roman",face="bold"),  # Set font for legend text
    legend.title = element_text(family = "Times New Roman",face="bold"))  # Set font for legend title


LMI_cntry<-LMI_cntry + geom_text(data=plotdata_long[seq(1, nrow(plotdata_long), 2),],
       aes(x = Country,
           y = Total + 0.01,
           label = as.character(Total)),
           vjust = -0.5,
           family = "Times New Roman") +
      geom_text(data=plotdata_long[seq(1, nrow(plotdata_long), 2),],
       aes(x = Country,
           y = Total-Value+0.01,
           label = as.character(Value)),
           vjust = -0.5,
           family = "Times New Roman") +
      geom_text(data=plotdata_long[seq(1, nrow(plotdata_long), 2),],
       aes(x = Country,
           y = 0.01,
           label = as.character(Total-Value),
           vjust = -0.5,
           family = "Times New Roman"))

LMI_cntry<-ggpar(LMI_cntry, 
      font.title = c(20, "bold"),
      font.subtitle = c(10),
      font.caption = c(12,"bold"),
      font.x = c(12),
      font.y = c(12)#,
      #palette="Set3"
      )

ggsave(
  filename="LMI_cntry.tiff",plot=LMI_cntry,device="tiff",
  width=20,height=20,units="cm",dpi=300)


##########

# CORRELATIONS MATRIX, ALL VARIABLES

ESS2016$lwparents_num<-numeric(nrow(ESS2016))
ESS2016$lwparents_num[which(ESS2016$lwparents_bin=="Yes")]=1
ESS2016$lwparents_num[which(ESS2016$lwparents_bin=="No")]=0
ESS2016$lwparents_num[which(is.na(ESS2016$lwparents_bin))]=NA

ESS2016$act_4c_secure<-numeric(nrow(ESS2016))
ESS2016$act_4c_secure[which(ESS2016$act_4c=="Securely employed")]=1
ESS2016$act_4c_secure[which(ESS2016$act_4c!="Securely employed")]=0
ESS2016$act_4c_secure[which(is.na(ESS2016$act_4c))]=NA

ESS2016$act_4c_atypical<-numeric(nrow(ESS2016))
ESS2016$act_4c_atypical[which(ESS2016$act_4c=="Atypically employed")]=1
ESS2016$act_4c_atypical[which(ESS2016$act_4c!="Atypically employed")]=0
ESS2016$act_4c_atypical[which(is.na(ESS2016$act_4c))]=NA

ESS2016$act_4c_NEET<-numeric(nrow(ESS2016))
ESS2016$act_4c_NEET[which(ESS2016$act_4c=="NEET")]=1
ESS2016$act_4c_NEET[which(ESS2016$act_4c!="NEET")]=0
ESS2016$act_4c_NEET[which(is.na(ESS2016$act_4c))]=NA

ESS2016$act_4c_edu<-numeric(nrow(ESS2016))
ESS2016$act_4c_edu[which(ESS2016$act_4c=="In education")]=1
ESS2016$act_4c_edu[which(ESS2016$act_4c!="In education")]=0
ESS2016$act_4c_edu[which(is.na(ESS2016$act_4c))]=NA

ESS2016$lwpartner_num<-numeric(nrow(ESS2016))
ESS2016$lwpartner_num[which(ESS2016$lwpartner=="Yes")]=1
ESS2016$lwpartner_num[which(ESS2016$lwpartner=="No")]=0
ESS2016$lwpartner_num[which(is.na(ESS2016$lwpartner))]=NA

ESS2016$gndr_num<-numeric(nrow(ESS2016))
ESS2016$gndr_num[which(ESS2016$gndr=="Male")]=1
ESS2016$gndr_num[which(ESS2016$gndr=="Female")]=0
ESS2016$gndr_num[which(ESS2016$gndr=="No answer")]=NA

ESS2016$eisced_num<-numeric(nrow(ESS2016))
ESS2016$eisced_num[which(ESS2016$eisced_dum=="Below higher secondary education")]=0
ESS2016$eisced_num[which(ESS2016$eisced_dum=="Completed higher secondary education")]=1
ESS2016$eisced_num[which(is.na(ESS2016$eisced_dum))]=NA

cor.data<-ESS2016[A18_39Ind16,c("gvslvue_grp_num","basinc_num","lwparents_num","act_4c_secure","act_4c_atypical",
                     "act_4c_NEET","act_4c_edu","lmr_SH","lmr_Rehm",
                     "hhinctnta_num","lwpartner_num","agea","gndr_num","eisced_num",
                     "freehms_num","rlgatnd_num",
                     "ChR","SPB_ES")]

cor.table<-cor(cor.data,use="complete.obs")
cor.table<-round(cor.table,2)

cor.tests<-rcorr(as.matrix(cor.data))
