####
# STUDY 1: COHABITATION AND DEMAND FOR PUBLIC PROVISION IN EUROPE
####

## BAYESIAN MODELS

#######################################################
# GROUP 1: THE PERSPECTIVE OF COHABITING ADULT CHILDREN
#######################################################

##############################################################

# LOAD PROCESSED DATA 

library(brms)
library(ggplot2)
library(ggpubr)
library(extrafont)

load("ESS2016_session.RData") 

comp_data<-ESS2016[,c("basinc","gvslvue_grp","basinc_num","gvslvue_grp",
"gvslvue_grp_num","lwparents_bin","act_4c","ChR","SPB_ES","hhinctnta_num","hhmmb","lwpartner",
"agea","gndr","eisced_dum","regime","cntry","anweight")]


##############################################################


###
### SUBGROUP 1.1: PREFERENCES REGARDING UNEMPLOYMENT SUPPORT
###

# MODEL 3: y~lwp*activity+lwp*ChR+controls+(1|cntry)

Bmom3_U2016<-brm(gvslvue_grp|resp_weights(anweight)~lwparents_bin*ChR+lwparents_bin*act_4c+
hhinctnta_num+hhmmb+lwpartner+agea+gndr+eisced_dum+(1+lwparents_bin|cntry),
data=(na.omit(comp_data[A18_39Ind16,])),family=cumulative(link="logit"),
init=0,chains=4,control=list(adapt_delta=0.9))

save(Bmom3_U2016,file="Bmom3_U2016.RData")
load("Bmom3_U2016.RData")


# PLOTS:

# Main effect:
LwPPlot_U2016=plot(conditional_effects(Bmom3_U2016,effects="lwparents_bin",categorical=T),plot=F)

LwPPlot_U2016[[1]]=LwPPlot_U2016[[1]]+
scale_color_manual(values=c("#e21c1b","#e58526","#4cad4b","#377cb6","#9752a2"))+
labs(y= "Probability", x = "Lives with parent(s)?")+
labs(color = "Attitudes towards unemployment benefits")+
guides(color = guide_legend(reverse=TRUE,
       override.aes=list(shape = c(19,18,17,15,4))))+
bgcolor("#BFD5E3") +
border("#BFD5E3")+
theme(text=element_text(family="Times New Roman",size=15),legend.title=element_text(size=14))

Layers_LwPPlot_U2016<-ggplot_build(LwPPlot_U2016[[1]])
Layers_LwPPlot_U2016$data[[1]]$shape=c(4,4,15,15,17,17,18,18,19,19)
LwPPlot_U2016<-ggplot_gtable(Layers_LwPPlot_U2016)
plot(LwPPlot_U2016)

ggsave(
  filename="LwPPlot_U2016.tiff",plot=LwPPlot_U2016,device="tiff",
  width=30,height=20,units="cm",dpi=300)

# Interaction with activity:
ActivityPlot_U2016<-plot(conditional_effects(Bmom3_U2016,effects = "act_4c",
conditions=make_conditions(Bmom3_U2016,"lwparents_bin"),
categorical=T,method="posterior_epred"),plot=F) 

ActivityPlot_U2016<-ActivityPlot_U2016[[1]]+
scale_color_manual(values=c("#e21c1b","#e58526","#4cad4b","#377cb6","#9752a2"))+
labs(title="",y= "Probability", x = "Lives with parent(s)?")+
labs(color = "Attitudes towards unemployment benefits")+
guides(color = guide_legend(reverse=TRUE,
       override.aes=list(shape = c(19,18,17,15,4))))+
bgcolor("#BFD5E3") +
border("#BFD5E3")+
theme(text=element_text(family="Times New Roman",size=15),legend.title=element_text(size=14))

Layers_ActivityPlot_U2016<-ggplot_build(ActivityPlot_U2016)
Layers_ActivityPlot_U2016$data[[1]]$shape=rep(c(rep(4,4),rep(15,4),rep(17,4),rep(18,4),rep(19,4)),2)
ActivityPlot_U2016<-ggplot_gtable(Layers_ActivityPlot_U2016)
plot(ActivityPlot_U2016)

ggsave(
  filename="ActivityPlot_U2016.tiff",plot=ActivityPlot_U2016,device="tiff",
  width=40,height=20,units="cm",dpi=300)

# Interaction with cohabitation norms
ChRPlot_U2016<-plot(conditional_effects(Bmom3_U2016,effects = "lwparents_bin",
conditions=make_conditions(Bmom3_U2016,"ChR",labels="Norms"),
categorical=T),method="predict",plot=F) 

ChRPlot_U2016<-ChRPlot_U2016[[1]]+
scale_color_manual(values=c("#e21c1b","#e58526","#4cad4b","#377cb6","#9752a2"))+
labs(title="",y= "Probability", x = "Lives with parent(s)?")+
labs(color = "Attitudes towards unemployment benefits")+
guides(color = guide_legend(reverse=TRUE,
       override.aes=list(shape = c(19,18,17,15,4))))+
bgcolor("#BFD5E3") +
border("#BFD5E3")+
theme(text=element_text(family="Times New Roman",size=15),legend.title=element_text(size=14))

Layers_ChRPlot_U2016<-ggplot_build(ChRPlot_U2016)
Layers_ChRPlot_U2016$data[[1]]$shape=rep(c(4,4,15,15,17,17,18,18,19,19),3)

ChRPlot_U2016<-ggplot_gtable(Layers_ChRPlot_U2016)
plot(ChRPlot_U2016)

ggsave(
  filename="ChRPlot_U2016.tiff",plot=ChRPlot_U2016,device="tiff",
  width=40,height=20,units="cm",dpi=300)

#############################################

###
### SUBGROUP 1.2: PREFERENCES REGARDING BASIC INCOME
###

# MODEL 3: y~lwp*activity+lwp*ChR+controls+(1|cntry)

Bmom3_B2016<-brm(basinc|resp_weights(anweight)~lwparents_bin*act_4c+lwparents_bin*ChR+
hhinctnta_num+hhmmb+lwpartner+agea+gndr+eisced_dum+(1+lwparents_bin|cntry),
data=(na.omit(comp_data[A18_39Ind16,])),family=cumulative(link="logit"),
init=0,chains=4,control=list(adapt_delta=0.9))

save(Bmom3_B2016,file="Bmom3_B2016.RData")
load("Bmom3_B2016.RData")

# PLOTS:

# Main effect:
LwPPlot_B2016=plot(conditional_effects(Bmom3_B2016,effects="lwparents_bin",categorical=T),plot=F)

LwPPlot_B2016[[1]]=LwPPlot_B2016[[1]]+
scale_color_manual(values=c("#e21c1b","#e58526","#4cad4b","#377cb6"))+
labs(y= "Probability", x = "Lives with parent(s)?")+
labs(color = "Against or in favour of basic income")+
guides(color = guide_legend(reverse=TRUE,
       override.aes=list(shape = c(19,17,15,4))))+
bgcolor("#BFD5E3") +
border("#BFD5E3")+
theme(text=element_text(family="Times New Roman",size=20),legend.title=element_text(size=18))

Layers_LwPPlot_B2016<-ggplot_build(LwPPlot_B2016[[1]])
Layers_LwPPlot_B2016$data[[1]]$shape=c(4,4,15,15,17,17,19,19)
LwPPlot_B2016<-ggplot_gtable(Layers_LwPPlot_B2016)
plot(LwPPlot_B2016)

ggsave(
  filename="LwPPlot_B2016.tiff",plot=LwPPlot_B2016,device="tiff",
  width=30,height=20,units="cm",dpi=300)

# Interaction with activity:

ActivityPlot_B2016<-plot(conditional_effects(Bmom3_B2016,effects = "act_4c",
conditions=make_conditions(Bmom3_B2016,"lwparents_bin"),
categorical=T,method="posterior_epred"),plot=F) 

ActivityPlot_B2016<-ActivityPlot_B2016[[1]]+
scale_color_manual(values=c("#e21c1b","#e58526","#4cad4b","#377cb6"))+
labs(title="", y= "Probability", x = "Lives with parent(s)?")+
labs(color = "Attitudes basic income")+
guides(color = guide_legend(reverse=TRUE,
       override.aes=list(shape = c(19,17,15,4))))+
bgcolor("#BFD5E3") +
border("#BFD5E3")+
theme(text=element_text(family="Times New Roman",size=18),legend.title=element_text(size=20),
legend.text=element_text(family="Times New Roman",size=18),
plot.title=element_text(hjust = 0.5))

Layers_ActivityPlot_B2016<-ggplot_build(ActivityPlot_B2016)
Layers_ActivityPlot_B2016$data[[1]]$shape=rep(c(rep(4,4),rep(15,4),rep(17,4),rep(19,4)),2)
ActivityPlot_B2016<-ggplot_gtable(Layers_ActivityPlot_B2016)
plot(ActivityPlot_B2016)

ggsave(
  filename="ActivityPlot_B2016.tiff",plot=ActivityPlot_B2016,device="tiff",
  width=50,height=20,units="cm",dpi=300)

# Interaction with cohabitation norms:
ChRPlot_B2016<-plot(conditional_effects(Bmom3_B2016,effects = "lwparents_bin",
conditions=make_conditions(Bmom3_B2016,"ChR"),
categorical=T),method="predict",plot=F) 

ChRPlot_B2016<-ChRPlot_B2016[[1]]+
scale_color_manual(values=c("#e21c1b","#e58526","#4cad4b","#377cb6"))+
labs(title="", y= "Probability", x = "Lives with parent(s)?")+
labs(color = "Against or in favour of basic income")+
guides(color = guide_legend(reverse=TRUE,
       override.aes=list(shape = c(19,17,15,4))))+
bgcolor("#BFD5E3") +
border("#BFD5E3")+
theme(text=element_text(family="Times New Roman",size=20),legend.title=element_text(size=18))

Layers_ChRPlot_B2016<-ggplot_build(ChRPlot_B2016)
Layers_ChRPlot_B2016$data[[1]]$shape=rep(c(4,4,15,15,17,17,19,19),3)
ChRPlot_B2016<-ggplot_gtable(Layers_ChRPlot_B2016)
plot(ChRPlot_B2016)

ggsave(
  filename="ChRPlot_B2016.tiff",plot=ChRPlot_B2016,device="tiff",
  width=40,height=20,units="cm",dpi=300)

#############################################

