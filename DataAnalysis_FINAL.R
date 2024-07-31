####
# STUDY 1: COHABITATION AND DEMAND FOR PUBLIC PROVISION IN EUROPE
####

## FREQUENTIST MODELS

#######################################################
# GROUP 1: THE PERSPECTIVE OF COHABITING ADULT CHILDREN
#######################################################

##############################################################

# LOAD PROCESSED DATA 

setwd("D:/BIGSSS/Dissertation/Study 1/Data & Scripts/DATA")

library(ordinal)
library(lme4)
library(interplot)
library(Hmisc)
library(MASS)
library(brant)
library(stargazer)
library(broom)
library(texreg)

#load("ESS2016_session.RData") 
load("ESS2016_edit.RData") 

comp_data<-ESS2016[,c("basinc","gvslvue_grp","basinc_num","gvslvue_grp",
"gvslvue_grp_num","lwparents_bin","act_4c","ChR","SPB_ES","hhinctnta_num","hhmmb","lwpartner",
"agea","gndr","eisced_dum","freehms_num","rlgatnd_num","regime","cntry","anweight")]

setwd("D:/BIGSSS/Dissertation/Study 1/Data & Scripts/MODELS/REVIEWS")

##############################################################

#############################################

###
### SUBGROUP 1: ATTITUDES TOWARDS UNEMPLOYMENT BENEFITS
###

# Checking the parallel slopes assumption for lwparents and activity

sf_gvslvue <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)),
    'Y>=4' = qlogis(mean(y >= 4)),
    'Y>=5' = qlogis(mean(y >= 5)))
}

(ps_gvslvue <- with(na.omit(comp_data[A18_39Ind16,]), 
      summary(gvslvue_grp_num ~ lwparents_bin+act_4c, fun=sf_gvslvue)))

ps_gvslvue[, 6] <- ps_gvslvue[, 6] - ps_gvslvue[, 5]
ps_gvslvue[, 5] <- ps_gvslvue[, 5] - ps_gvslvue[, 4]
ps_gvslvue[, 4] <- ps_gvslvue[, 4] - ps_gvslvue[, 3]
ps_gvslvue[, 3] <- ps_gvslvue[, 3] - ps_gvslvue[, 3]
ps_gvslvue # print




plot(ps_gvslvue, which=1:5, pch=1:5, xlab='logit', 
     main='Parallel slopes test', xlim=range(ps_gvslvue[,3:6]))

jpeg(file="PS_U2016.jpg",width=800,height=800)
plot(ps_gvslvue, which=1:5, pch=1:5, xlab='logit', 
     main='Parallel slopes test', xlim=range(ps_gvslvue[,3:6]))
dev.off()

ps_U2016<-polr(gvslvue_grp~lwparents_bin*act_4c,
data=(na.omit(comp_data[A18_39Ind16,])),weights=anweight,Hess=T)
brant(ps_U2016)

stargazer(brant(ps_U2016)[,c(1,3)],type="latex",title="Attitudes towards unemployment benefits: Brant Test of parallel regression")

# MODELS WITHOUT CONTROLS: y~activity+lwp & y~activity*lwp

Momalpha_U2016<-clmm(gvslvue_grp~lwparents_bin+act_4c+(1|cntry),
                   data=(na.omit(comp_data[A18_39Ind16,])),weights=anweight,link="logit")

tMomalpha_U2016<-tidy(Momalpha_U2016)[,c(1,2,3,5)]
tMomalpha_U2016<-cbind(tMomalpha_U2016[,1:3],confint(Momalpha_U2016),tMomalpha_U2016[,4])
tMomalpha_U2016[,2:4]=round(tMomalpha_U2016[,2:4],2);tMomalpha_U2016[,5:6]=round(tMomalpha_U2016[,5:6],2)
stargazer(tMomalpha_U2016[,-1],type="latex",digits=NA,summary=F,title="Attitudes towards unemployment benefits: only main predictors without interaction")

Mombeta_U2016<-clmm(gvslvue_grp~lwparents_bin*act_4c+(1|cntry),
                   data=(na.omit(comp_data[A18_39Ind16,])),weights=anweight,link="logit")

tMombeta_U2016<-tidy(Mombeta_U2016)[,c(1,2,3,5)]
tMombeta_U2016<-cbind(tMombeta_U2016[,1:3],confint(Mombeta_U2016),tMombeta_U2016[,4])
tMombeta_U2016[,2:4]=round(tMombeta_U2016[,2:4],2);tMombeta_U2016[,5:6]=round(tMombeta_U2016[,5:6],2)
stargazer(tMombeta_U2016[,-1],type="latex",digits=NA,summary=F,title="Attitudes towards unemployment benefits: only main predictors with interaction")


# MODEL 0: y~activity+controls+(1|cntry) (test the basic assumption about labour market risk)

Mom0_U2016<-clmm(gvslvue_grp~act_4c+hhinctnta_num+hhmmb+lwpartner+
agea+gndr+eisced_dum+freehms_num+rlgatnd_num+(1|cntry),
data=(na.omit(comp_data[A18_39Ind16,])),weights=anweight,link="logit")

save(Mom0_U2016,file="Mom0_U2016.RData")

tMom0_U2016<-tidy(Mom0_U2016)[,c(1,2,3,5)]
tMom0_U2016<-cbind(tMom0_U2016[,1:3],confint(Mom0_U2016),tMom0_U2016[,4])
tMom0_U2016[,2:4]=round(tMom0_U2016[,2:4],2);tMom0_U2016[,5:6]=round(tMom0_U2016[,5:6],2)
stargazer(tMom0_U2016[,-1],type="latex",digits=NA,summary=F,title="Attitudes towards unemployment benefits (M0)")

# MODEL 1: y~lwp+activity+controls+(1|cntry) (look at H1)

Mom1_U2016<-clmm(gvslvue_grp~lwparents_bin+act_4c+hhinctnta_num+hhmmb+lwpartner+
agea+gndr+eisced_dum+freehms_num+rlgatnd_num+(1|cntry),
data=(na.omit(comp_data[A18_39Ind16,])),weights=anweight,link="logit")

save(Mom1_U2016,file="Mom1_U2016.RData")

tMom1_U2016<-tidy(Mom1_U2016)[,c(1,2,3,5)]
tMom1_U2016<-cbind(tMom1_U2016[,1:3],confint(Mom1_U2016),tMom1_U2016[,4])
tMom1_U2016[,2:4]=round(tMom1_U2016[,2:4],2);tMom1_U2016[,5:6]=round(tMom1_U2016[,5:6],2)
stargazer(tMom1_U2016[,-1],type="latex",digits=NA,summary=F,title="Attitudes towards unemployment benefits (M1)")

anova(Mom0_U2016,Mom1_U2016) # Model 0 is a better fit


# MODEL 2: y~lwp*activity+controls+(1|cntry) (look at H1 and H2)

Mom2_U2016<-clmm(gvslvue_grp~lwparents_bin*act_4c+hhinctnta_num+hhmmb+lwpartner+
agea+gndr+eisced_dum+freehms_num+rlgatnd_num+(1|cntry),
data=(na.omit(comp_data[A18_39Ind16,])),weights=anweight,link="logit")

save(Mom2_U2016,file="Mom2_U2016.RData")

tMom2_U2016<-tidy(Mom2_U2016)[,c(1,2,3,5)]
tMom2_U2016<-cbind(tMom2_U2016[,1:3],confint(Mom2_U2016),tMom2_U2016[,4])
tMom2_U2016[,2:4]=round(tMom2_U2016[,2:4],2);tMom2_U2016[,5:6]=round(tMom2_U2016[,5:6],2)
stargazer(tMom2_U2016[,-1],type="latex",digits=NA,summary=F,title="Attitudes towards unemployment benefits (M2)")

anova(Mom0_U2016,Mom1_U2016,Mom2_U2016) # Model 0 is a better fit


# MODEL 3: y~lwp*ChR+controls+(1|cntry)  (look at H1 and H3)

Mom3_U2016<-clmm(gvslvue_grp~lwparents_bin*act_4c+lwparents_bin*ChR+hhinctnta_num+hhmmb+lwpartner+
agea+gndr+eisced_dum+freehms_num+rlgatnd_num+(1+lwparents_bin|cntry),
data=(na.omit(comp_data[A18_39Ind16,])),weights=anweight,link="logit")

save(Mom3_U2016,file="Mom3_U2016.RData")

tMom3_U2016<-tidy(Mom3_U2016)[,c(1,2,3,5)]
tMom3_U2016<-cbind(tMom3_U2016[,1:3],confint(Mom3_U2016),tMom3_U2016[,4])
tMom3_U2016[,2:4]=round(tMom3_U2016[,2:4],2);tMom3_U2016[,5:6]=round(tMom3_U2016[,5:6],2)
stargazer(tMom3_U2016[,-1],type="latex",digits=NA,summary=F,title="Attitudes towards unemployment benefits (M3)")

anova(Mom0_U2016,Mom2_U2016,Mom3_U2016) # Model 0 is a better fit


# MODEL 4: y~lwp*activity+lwp*ChR+controls+(1|cntry)  (look at H1 and H3)

Mom4_U2016<-clmm(gvslvue_grp~lwparents_bin*act_4c+lwparents_bin*SPB_ES+hhinctnta_num+hhmmb+lwpartner+
agea+gndr+eisced_dum+freehms_num+rlgatnd_num+(1+lwparents_bin|cntry),
data=(na.omit(comp_data[A18_39Ind16,])),weights=anweight,link="logit")

save(Mom4_U2016,file="Mom4_U2016.RData")

tMom4_U2016<-tidy(Mom4_U2016)[,c(1,2,3,5)]
tMom4_U2016<-cbind(tMom4_U2016[,1:3],confint(Mom4_U2016),tMom4_U2016[,4])
tMom4_U2016[,2:4]=round(tMom4_U2016[,2:4],2);tMom4_U2016[,5:6]=round(tMom4_U2016[,5:6],2)
stargazer(tMom4_U2016[,-1],type="latex",digits=NA,summary=F,title="Attitudes towards unemployment benefits (M4)")

### CHOSEN MODEL: Mom0_U2016
anova(Mom0_U2016,Mom1_U2016,Mom2_U2016,Mom3_U2016,Mom4_U2016)
stargazer(anova(Mom0_U2016,Mom1_U2016,Mom2_U2016,Mom3_U2016,Mom4_U2016),type="latex",summary=F,title="Attitudes towards unemployment benefits: model comparisons")

summary(Mom0_U2016)

texreg(list(Mom0_U2016,Mom1_U2016,Mom2_U2016,Mom3_U2016,Mom4_U2016),label="Models_UB",caption="Attitudes towards unemployment benefits",caption.above=T)


# REGIME-SPECIFIC MODELS

Mom2_U2016_Med<-clm(gvslvue_grp~lwparents_bin*act_4c+hhinctnta_num+hhmmb+lwpartner+
agea+gndr+eisced_dum+freehms_num+rlgatnd_num+cntry,
data=(na.omit(comp_data[Med18_39Ind16,])),weights=anweight,link="logit")

Mom2_U2016_Con<-clm(gvslvue_grp~lwparents_bin*act_4c+hhinctnta_num+hhmmb+lwpartner+
agea+gndr+eisced_dum+freehms_num+rlgatnd_num+cntry,
data=(na.omit(comp_data[Con18_39Ind16,])),weights=anweight,link="logit")

Mom2_U2016_Sca<-clm(gvslvue_grp~lwparents_bin*act_4c+hhinctnta_num+hhmmb+lwpartner+
agea+gndr+eisced_dum+freehms_num+rlgatnd_num+cntry,
data=(na.omit(comp_data[Sca18_39Ind16,])),weights=anweight,link="logit")

Mom2_U2016_East<-clm(gvslvue_grp~lwparents_bin*act_4c+hhinctnta_num+hhmmb+lwpartner+
agea+gndr+eisced_dum+freehms_num+rlgatnd_num+cntry,
data=(na.omit(comp_data[East18_39Ind16,])),weights=anweight,link="logit")

texreg(list(Mom2_U2016_Med,Mom2_U2016_Con,Mom2_U2016_Sca,Mom2_U2016_East),caption="Regime-Specific Ordinal Models for Unemployment Benefits",caption.above=T)


# LINEAR FITS:

Mlm0_U2016<-lmer(gvslvue_grp_num~act_4c+
                   hhinctnta_num+hhmmb+lwpartner+agea+gndr+eisced_dum+freehms_num+rlgatnd_num+(1|cntry),
                 data=(na.omit(comp_data[A18_39Ind16,])),weights=anweight)

summary(Mlm0_U2016);confint(Mlm0_U2016,method="Wald",level=0.95)

Mlm1_U2016<-lmer(gvslvue_grp_num~lwparents_bin+act_4c+
                   hhinctnta_num+hhmmb+lwpartner+agea+gndr+eisced_dum+freehms_num+rlgatnd_num+(1+lwparents_bin|cntry),
                 data=(na.omit(comp_data[A18_39Ind16,])),weights=anweight)

summary(Mlm1_U2016);confint(Mlm1_U2016,method="Wald",level=0.95)

Mlm2_U2016<-lmer(gvslvue_grp_num~lwparents_bin*act_4c+
hhinctnta_num+hhmmb+lwpartner+agea+gndr+eisced_dum+freehms_num+rlgatnd_num+(1+lwparents_bin|cntry),
data=(na.omit(comp_data[A18_39Ind16,])),weights=anweight)

summary(Mlm2_U2016);confint(Mlm2_U2016,method="Wald",level=0.95)

Mlm3_U2016<-lmer(gvslvue_grp_num~lwparents_bin*act_4c+lwparents_bin*ChR+
                   hhinctnta_num+hhmmb+lwpartner+agea+gndr+eisced_dum+freehms_num+rlgatnd_num+(1+lwparents_bin|cntry),
                 data=(na.omit(comp_data[A18_39Ind16,])),weights=anweight)

summary(Mlm3_U2016);confint(Mlm3_U2016,method="Wald",level=0.95)


Mlm4_U2016<-lmer(gvslvue_grp_num~lwparents_bin*act_4c+lwparents_bin*SPB_ES+
                   hhinctnta_num+hhmmb+lwpartner+agea+gndr+eisced_dum+freehms_num+rlgatnd_num+(1+lwparents_bin|cntry),
                 data=(na.omit(comp_data[A18_39Ind16,])),weights=anweight)

summary(Mlm4_U2016);confint(Mlm4_U2016,method="Wald",level=0.95)

texreg(list(Mlm0_U2016,Mlm1_U2016,Mlm2_U2016,Mlm3_U2016,Mlm4_U2016),caption="Attitudes towards unemployment benefits: linear models",caption.above=T)

##############################################################

###
### SUBGROUP 1.2: PREFERENCES REGARDING BASIC INCOME
###

# Checking the parallel slopes assumption for lwparents and activity

sf_basinc <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)),
    'Y>=4' = qlogis(mean(y >= 4)))
}

(ps_basinc <- with(na.omit(comp_data[A18_39Ind16,]), 
      summary(as.numeric(basinc) ~ lwparents_bin+act_4c, fun=sf_basinc)))

ps_basinc[, 5] <- ps_basinc[, 5] - ps_basinc[, 4]
ps_basinc[, 4] <- ps_basinc[, 4] - ps_basinc[, 3]
ps_basinc[, 3] <- ps_basinc[, 3] - ps_basinc[, 3]
ps_basinc # print

plot(ps_basinc, which=1:4, pch=1:4, xlab='logit', 
main='Parallel slopes test', xlim=range(ps_basinc[,3:5]))

jpeg(file="PS_B2016.jpg",width=800,height=800)
plot(ps_basinc, which=1:4, pch=1:4, xlab='logit', 
main='Parallel slopes test', xlim=range(ps_basinc[,3:5]))
dev.off()

ps_B2016<-polr(basinc~lwparents_bin*act_4c,
data=(na.omit(comp_data[A18_39Ind16,])),weights=anweight,Hess=T)
brant(ps_B2016)

stargazer(brant(ps_B2016)[,c(1,3)],type="latex",title="Attitudes towards basic income: Brant Test of parallel regression")

# MODELS WITHOUT CONTROLS: y~activity+lwp & y~activity*lwp

Momalpha_B2016<-clmm(basinc~lwparents_bin+act_4c+(1|cntry),
                    data=(na.omit(comp_data[A18_39Ind16,])),weights=anweight,link="logit")

tMomalpha_B2016<-tidy(Momalpha_B2016)[,c(1,2,3,5)]
tMomalpha_B2016<-cbind(tMomalpha_B2016[,1:3],confint(Momalpha_B2016),tMomalpha_B2016[,4])
tMomalpha_B2016[,2:4]=round(tMomalpha_B2016[,2:4],2);tMomalpha_B2016[,5:6]=round(tMomalpha_B2016[,5:6],2)
stargazer(tMomalpha_B2016[,-1],type="latex",digits=NA,summary=F,title="Attitudes towards basic income: only main predictors without interaction")

Mombeta_B2016<-clmm(basinc~lwparents_bin*act_4c+(1|cntry),
                   data=(na.omit(comp_data[A18_39Ind16,])),weights=anweight,link="logit")

tMombeta_B2016<-tidy(Mombeta_B2016)[,c(1,2,3,5)]
tMombeta_B2016<-cbind(tMombeta_B2016[,1:3],confint(Mombeta_B2016),tMombeta_B2016[,4])
tMombeta_B2016[,2:4]=round(tMombeta_B2016[,2:4],2);tMombeta_B2016[,5:6]=round(tMombeta_B2016[,5:6],2)
stargazer(tMombeta_B2016[,-1],type="latex",digits=NA,summary=F,title="Attitudes towards basic income: only main predictors with interaction")

# MODEL 0: y~activity+controls+(1|cntry) (look at H1)

Mom0_B2016<-clmm(basinc~act_4c+hhinctnta_num+hhmmb+lwpartner+
agea+gndr+eisced_dum+freehms_num+rlgatnd_num+(1|cntry),
data=(na.omit(comp_data[A18_39Ind16,])),weights=anweight,link="logit") 

save(Mom0_B2016,file="Mom0_B2016.RData")

tMom0_B2016<-tidy(Mom0_B2016)[,c(1,2,3,5)]
tMom0_B2016<-cbind(tMom0_B2016[,1:3],confint(Mom0_B2016),tMom0_B2016[,4])
tMom0_B2016[,2:4]=round(tMom0_B2016[,2:4],2);tMom0_B2016[,5:6]=round(tMom0_B2016[,5:6],2)
stargazer(tMom0_B2016[,-1],type="latex",digits=NA,summary=F,title="Attitudes towards basic income (M0)")


# MODEL 1: y~lwp+activity+controls+(1|cntry) (look at H1)

Mom1_B2016<-clmm(basinc~lwparents_bin+act_4c+hhinctnta_num+hhmmb+lwpartner+
agea+gndr+eisced_dum+freehms_num+rlgatnd_num+(1|cntry),
data=(na.omit(comp_data[A18_39Ind16,])),weights=anweight,link="logit")

save(Mom1_B2016,file="Mom1_B2016.RData")

tMom1_B2016<-tidy(Mom1_B2016)[,c(1,2,3,5)]
tMom1_B2016<-cbind(tMom1_B2016[,1:3],confint(Mom1_B2016),tMom1_B2016[,4])
tMom1_B2016[,2:4]=round(tMom1_B2016[,2:4],2);tMom1_B2016[,5:6]=round(tMom1_B2016[,5:6],2)
stargazer(tMom1_B2016[,-1],type="latex",digits=NA,summary=F,title="Attitudes towards basic income (M1)")

anova(Mom0_B2016,Mom1_B2016) # Model 0 is a better fit


# MODEL 2: y~lwp*activity+controls+(1|cntry) (look at H1 and H2)

Mom2_B2016<-clmm(basinc~lwparents_bin*act_4c+hhinctnta_num+hhmmb+lwpartner+
agea+gndr+eisced_dum+freehms_num+rlgatnd_num+(1|cntry),
data=(na.omit(comp_data[A18_39Ind16,])),weights=anweight,link="logit")

save(Mom2_B2016,file="Mom2_B2016.RData")

tMom2_B2016<-tidy(Mom2_B2016)[,c(1,2,3,5)]
tMom2_B2016<-cbind(tMom2_B2016[,1:3],confint(Mom2_B2016),tMom2_B2016[,4])
tMom2_B2016[,2:4]=round(tMom2_B2016[,2:4],2);tMom2_B2016[,5:6]=round(tMom2_B2016[,5:6],2)
stargazer(tMom2_B2016[,-1],type="latex",digits=NA,summary=F,title="Attitudes towards basic income (M2)")

anova(Mom0_B2016,Mom1_B2016,Mom2_B2016) # Model 2 is a better fit


# MODEL 3: y~lwp*activity+lwp*ChR+controls+(1|cntry)

Mom3_B2016<-clmm(basinc~lwparents_bin*act_4c+lwparents_bin*ChR+hhinctnta_num+hhmmb+lwpartner+
agea+gndr+eisced_dum+freehms_num+rlgatnd_num+(1+lwparents_bin|cntry),
data=(na.omit(comp_data[A18_39Ind16,])),weights=anweight,link="logit")

save(Mom3_B2016,file="Mom3_B2016.RData")

tMom3_B2016<-tidy(Mom3_B2016)[,c(1,2,3,5)]
tMom3_B2016<-cbind(tMom3_B2016[,1:3],confint(Mom3_B2016),tMom3_B2016[,4])
tMom3_B2016[,2:4]=round(tMom3_B2016[,2:4],2);tMom3_B2016[,5:6]=round(tMom3_B2016[,5:6],2)
stargazer(tMom3_B2016[,-1],type="latex",digits=NA,summary=F,title="Attitudes towards basic income (M3)")

anova(Mom2_B2016,Mom3_B2016) # Model 2 is still a better fit


# MODEL 4: y~lwp*activity+lwp*SPB+controls+(1|cntry)

Mom4_B2016<-clmm(basinc~lwparents_bin*act_4c+lwparents_bin*SPB_ES+hhinctnta_num+hhmmb+lwpartner+
agea+gndr+eisced_dum+freehms_num+rlgatnd_num+(1+lwparents_bin|cntry),
data=(na.omit(comp_data[A18_39Ind16,])),weights=anweight,link="logit")

save(Mom4_B2016,file="Mom4_B2016.RData")

tMom4_B2016<-tidy(Mom4_B2016)[,c(1,2,3,5)]
tMom4_B2016<-cbind(tMom4_B2016[,1:3],confint(Mom4_B2016),tMom4_B2016[,4])
tMom4_B2016[,2:4]=round(tMom4_B2016[,2:4],2);tMom4_B2016[,5:6]=round(tMom4_B2016[,5:6],2)
stargazer(tMom4_B2016[,-1],type="latex",digits=NA,summary=F,title="Attitudes towards basic income (M4)")

anova(Mom2_B2016,Mom4_B2016) # Model 2 is still a better fit


### SELECTED MODEL: om2_child_B2016
anova(Mom0_B2016,Mom1_B2016,Mom2_B2016,Mom3_B2016,Mom4_B2016)
stargazer(anova(Mom0_B2016,Mom1_B2016,Mom2_B2016,Mom3_B2016,Mom4_B2016),type="latex",summary=F,title="Attitudes towards basic income: model comparisons")

summary(Mom2_B2016)

texreg(list(Mom0_B2016,Mom1_B2016,Mom2_B2016,Mom3_B2016,Mom4_B2016),label="Models_BI",caption="Attitudes towards basic income",caption.above=T)


# REGIME SPECIFIC MODELS

Mom2_B2016_Med<-clm(basinc~lwparents_bin*act_4c+hhinctnta_num+hhmmb+lwpartner+
agea+gndr+eisced_dum+freehms_num+rlgatnd_num+cntry,
data=(na.omit(comp_data[Med18_39Ind16,])),weights=anweight,link="logit")

tMom2_B2016_Med<-tidy(Mom2_B2016_Med)[,c(1,2,3,5)]
tMom2_B2016_Med<-cbind(tMom2_B2016_Med[,1:3],confint(Mom2_B2016_Med)[],tMom2_B2016_Med[,4])
tMom2_B2016_Med[,2:4]=round(tMom2_B2016_Med[,2:4],2);tMom2_B2016_Med[,5:6]=round(tMom2_B2016_Med[,5:6],2)
stargazer(tMom2_B2016_Med[,-1],type="latex",digits=NA,summary=F,title="Attitudes towards basic income in Mediterranean countries (M2)")

Mom2_B2016_Con<-clm(basinc~lwparents_bin*act_4c+hhinctnta_num+hhmmb+lwpartner+
agea+gndr+eisced_dum+freehms_num+rlgatnd_num+cntry,
data=(na.omit(comp_data[Con18_39Ind16,])),weights=anweight,link="logit")

Mom2_B2016_Sca<-clm(basinc~lwparents_bin*act_4c+hhinctnta_num+hhmmb+lwpartner+
agea+gndr+eisced_dum+freehms_num+rlgatnd_num+cntry,
data=(na.omit(comp_data[Sca18_39Ind16,])),weights=anweight,link="logit")

Mom2_B2016_East<-clm(basinc~lwparents_bin*act_4c+hhinctnta_num+hhmmb+lwpartner+
agea+gndr+eisced_dum+freehms_num+rlgatnd_num+cntry,
data=(na.omit(comp_data[East18_39Ind16,])),weights=anweight,link="logit")

texreg(list(Mom2_B2016_Med,Mom2_B2016_Con,Mom2_B2016_Sca,Mom2_B2016_East),label="ModelsBI_Regime",caption="Regime-Specific Ordinal Models for Basic Income",caption.above=T)

# LINEAR FITS:

Mlm0_B2016<-lmer(basinc_num~act_4c+
                   hhinctnta_num+hhmmb+lwpartner+agea+gndr+eisced_dum+freehms_num+rlgatnd_num+(1|cntry),
                 data=(na.omit(comp_data[A18_39Ind16,])),weights=anweight)

summary(Mlm0_B2016);confint(Mlm0_B2016,method="Wald",level=0.95)

Mlm1_B2016<-lmer(basinc_num~lwparents_bin+act_4c+
                   hhinctnta_num+hhmmb+lwpartner+agea+gndr+eisced_dum+freehms_num+rlgatnd_num+(1|cntry),
                 data=(na.omit(comp_data[A18_39Ind16,])),weights=anweight)

summary(Mlm1_B2016);confint(Mlm1_B2016,method="Wald",level=0.95)

Mlm2_B2016<-lmer(basinc_num~lwparents_bin*act_4c+
hhinctnta_num+hhmmb+lwpartner+agea+gndr+eisced_dum+freehms_num+rlgatnd_num+(1|cntry),
data=(na.omit(comp_data[A18_39Ind16,])),weights=anweight)

summary(Mlm2_B2016);confint(Mlm2_B2016,method="Wald",level=0.95)

Mlm3_B2016<-lmer(basinc_num~lwparents_bin*act_4c+lwparents_bin*ChR+
hhinctnta_num+hhmmb+lwpartner+agea+gndr+eisced_dum+freehms_num+rlgatnd_num+(1+lwparents_bin|cntry),
data=(na.omit(comp_data[A18_39Ind16,])),weights=anweight)

summary(Mlm3_B2016);round(confint(Mlm3_B2016,method="Wald",level=0.95),3)

Mlm4_B2016<-lmer(basinc_num~lwparents_bin*act_4c+lwparents_bin*SPB_ES+
                   hhinctnta_num+hhmmb+lwpartner+agea+gndr+eisced_dum+freehms_num+rlgatnd_num+(1+lwparents_bin|cntry),
                 data=(na.omit(comp_data[A18_39Ind16,])),weights=anweight)

summary(Mlm4_B2016);round(confint(Mlm4_B2016,method="Wald",level=0.95),3)

texreg(list(Mlm0_B2016,Mlm1_B2016,Mlm2_B2016,Mlm3_B2016,Mlm4_B2016),caption="Attitudes towards basic income: linear models",caption.above=T)


##### REPRESENTING THE RANDOM SLOPES FOR THE INTERACTION EFFECTS IN EACH REGIME
# (USING LINEAR MODELS)


Mlm2_B2016_Med<-lmer(basinc_num~lwparents_bin*act_4c+lwparents_bin+hhinctnta_num+hhmmb+lwpartner+
agea+gndr+eisced_dum+(1|cntry),
data=(na.omit(comp_data[Med18_39Ind16,])),weights=anweight)

Mlm2_B2016_Con<-lmer(basinc_num~lwparents_bin*act_4c+lwparents_bin+hhinctnta_num+hhmmb+lwpartner+
agea+gndr+eisced_dum+(1|cntry),
data=(na.omit(comp_data[Con18_39Ind16,])),weights=anweight)

Mlm2_B2016_Sca<-lmer(basinc_num~lwparents_bin*act_4c+lwparents_bin+hhinctnta_num+hhmmb+lwpartner+
agea+gndr+eisced_dum+(1|cntry),
data=(na.omit(comp_data[Sca18_39Ind16,])),weights=anweight)

Mlm2_B2016_East<-lmer(basinc_num~lwparents_bin*act_4c+lwparents_bin+hhinctnta_num+hhmmb+lwpartner+
agea+gndr+eisced_dum+(1|cntry),
data=(na.omit(comp_data[East18_39Ind16,])),weights=anweight)

plotMed<-sjPlot::plot_model(Mlm2_B2016_Med, type="int",pred.type="re", ci.lvl=NA,title="Mediterranean")+
scale_color_manual(values=c("#377cb6","#4cad4b","#e58526","#e21c1b"))+
labs(title="Mediterranean", y= "Probability", x = "Lives with parent(s)?")+
labs(color = "Activity")+
guides(color = guide_legend(reverse=FALSE,
       override.aes=list(shape = c(19,17,15,4))))+
bgcolor("#BFD5E3") +
border("#BFD5E3")+
scale_y_continuous(limits=c(2.3,3))+
theme(text=element_text(family="Times New Roman",size=15),legend.title=element_text(size=14),
plot.title = element_text(family="Times New Roman",size=15,hjust=0.5))+
theme(legend.position = "none")
Layers_plotMed<-ggplot_build(plotMed)
Layers_plotMed$data[[1]]$shape=rep(c(19,17,15,4),2)
plotMed<-ggplot_gtable(Layers_plotMed)

plotCon<-sjPlot::plot_model(Mlm2_B2016_Con, type="int",pred.type="re", ci.lvl=NA,title="Continental")+
scale_color_manual(values=c("#377cb6","#4cad4b","#e58526","#e21c1b"))+
labs(title="Continental", y= "Probability", x = "Lives with parent(s)?")+
labs(color = "Activity")+
guides(color = guide_legend(reverse=FALSE,
       override.aes=list(shape = c(19,17,15,4))))+
bgcolor("#BFD5E3") +
border("#BFD5E3")+
scale_y_continuous(limits=c(2.3,3))+
theme(text=element_text(family="Times New Roman",size=15),legend.title=element_text(size=14),
plot.title = element_text(family="Times New Roman",size=15,hjust=0.5))+
theme(legend.position = "none")
Layers_plotCon<-ggplot_build(plotCon)
Layers_plotCon$data[[1]]$shape=rep(c(19,17,15,4),2)
plotCon<-ggplot_gtable(Layers_plotCon)

plotSca<-sjPlot::plot_model(Mlm2_B2016_Sca, type="int",pred.type="re", ci.lvl=NA,title="Scandinavian")+
scale_color_manual(values=c("#377cb6","#4cad4b","#e58526","#e21c1b"))+
labs(title="Scandinavian", y= "Probability", x = "Lives with parent(s)?")+
labs(color = "Activity")+
guides(color = guide_legend(reverse=FALSE,
       override.aes=list(shape = c(19,17,15,4))))+
bgcolor("#BFD5E3") +
border("#BFD5E3")+
scale_y_continuous(limits=c(2.3,3))+
theme(text=element_text(family="Times New Roman",size=15),legend.title=element_text(size=14),
plot.title = element_text(family="Times New Roman",size=15,hjust=0.5))+
theme(legend.position = "none")
Layers_plotSca<-ggplot_build(plotSca)
Layers_plotSca$data[[1]]$shape=rep(c(19,17,15,4),2)
plotSca<-ggplot_gtable(Layers_plotSca)

plotEast<-sjPlot::plot_model(Mlm2_B2016_East, type="int",pred.type="re", ci.lvl=NA,title="Eastern")+
scale_color_manual(values=c("#377cb6","#4cad4b","#e58526","#e21c1b"))+
labs(title="Eastern", y= "Probability", x = "Lives with parent(s)?")+
labs(color = "Activity")+
guides(color = guide_legend(reverse=FALSE,
       override.aes=list(shape = c(19,17,15,4))))+
bgcolor("#BFD5E3") +
border("#BFD5E3")+
scale_y_continuous(limits=c(2.3,3))+
theme(text=element_text(family="Times New Roman",size=15),legend.title=element_text(size=14),
plot.title = element_text(family="Times New Roman",size=15,hjust=0.5))+
theme(legend.position = "none")
legend<-get_legend(plotEast + theme(legend.position = "left",
legend.margin = margin(0, 20, 0, 0)))

Layers_plotEast<-ggplot_build(plotEast)
Layers_plotEast$data[[1]]$shape=rep(c(19,17,15,4),2)
plotEast<-ggplot_gtable(Layers_plotEast)

basinc_rgm_linear<-grid.arrange(arrangeGrob(plotMed,plotCon,nrow=2), 
             arrangeGrob(plotSca,plotEast, nrow = 2),
             legend,
             ncol=3, widths=c(2,2,1))

ggsave(
  filename="basinc_rgm_linear.tiff",plot=basinc_rgm_linear,device="tiff",
  width=30,height=20,units="cm",dpi=300)















