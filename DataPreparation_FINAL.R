####
# STUDY: INTERGENERATIONAL COHABITATION AND WELFARE ATTITUDES AMONG EUROPEAN YOUNG ADULTS
####

#### Gonzalo Arévalo-Iglesias, University of Bremen 

##
# DATA PREPARATION
##


## STEP 1: LOAD QUESTIONNAIRE VARIABLES AND SAMPLE DESIGN VARIABLES AND MERGE

library(foreign)
library(forcats)
library(plyr)
library(survey)

ESS2016<-read.spss("ESS8e02_2.sav") # Main questionnaire variables
ESS2016<-rbind.data.frame(ESS2016)
ESS2016SV<-read.spss("ESS8SDDFe01_1.sav") # Sample Design Variables
ESS2016SV<-rbind.data.frame(ESS2016SV)

ESS2016<-cbind(ESS2016,ESS2016SV) # Main and Sample Design Variables Together

#####################################################################

## STEP 2: CREATE ANALYSIS WEIGHTS AND SPECIFY SAMPLE DESIGN

# Create analysis weights

ESS2016$anweight<-ESS2016$pspwght*ESS2016$pweight

# These weights must be then introduced in the "weight" argument of the
# model fitting functions (lm or lmer (from lme4 package))

# Specify sample design

ESS2016_design <- svydesign(ids = ~psu, strata = ~stratum, weights = ~anweight,
data = ESS2016, nest=T)

#####################################################################

## STEP 3: REMOVE THOSE COUNTRIES THAT ARE NOT UNDER STUDY

ESS2016$cntry<-as.factor(ESS2016$cntry)

ESS2016<-ESS2016[-c(which(ESS2016$cntry=="Bulgaria"),which(ESS2016$cntry=="Iceland"),
which(ESS2016$cntry=="Ireland"),which(ESS2016$cntry=="Israel"),
which(ESS2016$cntry=="Lithuania"),which(ESS2016$cntry=="Russian Federation"),
which(ESS2016$cntry=="United Kingdom")),]

ESS2016$cntry<-factor(ESS2016$cntry,levels=c("Austria","Belgium","Czechia",
"Estonia","Finland","France","Germany","Hungary","Italy","Netherlands","Norway",
"Poland","Portugal","Spain","Sweden","Switzerland"))

#####################################################################

## STEP 4: DEFINE KEY VARIABLES

## 4.0) BASIC VARIABLES (AGE, GENDER, COUNTRY, REGIME)

# Age and gender as factors:
ESS2016$agea<-as.numeric(ESS2016$agea)
ESS2016$gndr<-as.factor(ESS2016$gndr)

# Classify countries in 4 welfare regimes:
ESS2016$regime<-factor(length(ESS2016$cntry),levels=c("Scandinavian","Continental",
"Mediterranean","Eastern"))
ESS2016$regime[which(ESS2016$cntry=="Austria")]="Continental"
ESS2016$regime[which(ESS2016$cntry=="Belgium")]="Continental"
ESS2016$regime[which(ESS2016$cntry=="Czechia")]="Eastern"
ESS2016$regime[which(ESS2016$cntry=="Estonia")]="Eastern"
ESS2016$regime[which(ESS2016$cntry=="Finland")]="Scandinavian"
ESS2016$regime[which(ESS2016$cntry=="France")]="Continental"
ESS2016$regime[which(ESS2016$cntry=="Germany")]="Continental"
ESS2016$regime[which(ESS2016$cntry=="Hungary")]="Eastern"
ESS2016$regime[which(ESS2016$cntry=="Italy")]="Mediterranean"
ESS2016$regime[which(ESS2016$cntry=="Netherlands")]="Continental"
ESS2016$regime[which(ESS2016$cntry=="Norway")]="Scandinavian"
ESS2016$regime[which(ESS2016$cntry=="Poland")]="Eastern"
ESS2016$regime[which(ESS2016$cntry=="Portugal")]="Mediterranean"
ESS2016$regime[which(ESS2016$cntry=="Spain")]="Mediterranean"
ESS2016$regime[which(ESS2016$cntry=="Sweden")]="Scandinavian"
ESS2016$regime[which(ESS2016$cntry=="Switzerland")]="Continental"

#####################################################################

## 4.1) DEPENDENT VARIABLES (ATTITUDES)

# Unemployment benefits
ESS2016$gvslvue<-factor(ESS2016$gvslvue,
levels=c("Not governments' responsibility at all","1","2","3","4","5","6","7","8","9",
"Entirely governments' responsibility","Refusal","Don't know","No answer"))
ESS2016$gvslvue[which(ESS2016$gincdif=="Refusal")]=NA
ESS2016$gvslvue[which(ESS2016$gincdif=="Don't know")]=NA
ESS2016$gvslvue[which(ESS2016$gincdif=="No answer")]=NA
ESS2016$gvslvue<-factor(ESS2016$gvslvue,levels=c("Not governments' responsibility at all","1","2","3","4","5","6","7","8","9",
"Entirely governments' responsibility"),ordered=T)

ESS2016$gvslvue_grp<-revalue(ESS2016$gvslvue, # Grouping to ease comparability with the other item
c("Not governments' responsibility at all"="Disagree strongly","1"="Disagree strongly","2"="Disagree","3"="Disagree",
"4"="Neither agree nor disagree","5"="Neither agree nor disagree","6"="Neither agree nor disagree","7"="Agree",
"8"="Agree","9"="Agree strongly","Entirely governments' responsibility"="Agree strongly"))

ESS2016$gvslvue_grp_num<-as.numeric(ESS2016$gvslvue_grp)# A numeric version for linear models

# Basic Income
ESS2016$basinc<-factor(ESS2016$basinc,
levels=c("Strongly against","Against","In favour","Strongly in favour",
"Refusal","Don't know","No answer"))
ESS2016$basinc[which(ESS2016$gincdif=="Refusal")]=NA
ESS2016$basinc[which(ESS2016$gincdif=="Don't know")]=NA
ESS2016$basinc[which(ESS2016$gincdif=="No answer")]=NA
ESS2016$basinc<-factor(ESS2016$basinc,levels=c("Strongly against","Against","In favour","Strongly in favour"),
ordered=T)

ESS2016$basinc_num<-as.numeric(ESS2016$basinc)# A numeric version for linear models

#####################################################################

## 4.2) MAIN INDEPENDENT VARIABLES: INTERGENERATIONAL COHABITATION

rltvar<-which(colnames(ESS2016)=="rshipa2"):which(colnames(ESS2016)=="rshipa12") # Ordering index to identify variables of HH members relationship

# Lives with parent(s)?

ESS2016$lwparents_bin<-factor(nrow(ESS2016),levels=c("No","Yes"))

for (i in 1:nrow(ESS2016)){
if (length(which(ESS2016[i,rltvar]=="Parent/parent-in-law"))>=1){ # If the respondent lives with two parents (or more, with step-parents n=9)
ESS2016$lwparents_bin[i]<-"Yes"}
else{
if ("Refusal" %in% ESS2016[i,rltvar]||"Don't know"%in%ESS2016[i,rltvar]||"No answer"%in%ESS2016[i,rltvar]||"NA"%in%ESS2016[i,rltvar]){
ESS2016$lwparents_bin[i]=="NA"} # If respondents refuse to answer, don't know or don't answer for any other reason for any of the HH members relation,
# data coded as NA
else{
ESS2016$lwparents_bin[i]<-"No"}
}}
summary(ESS2016$lwparents) # Only 191 NAs. Very satisfactory.

#####################################################################

## 4.3) MAIN INDEPENDENT VARIABLES: LABOUR MARKET MEASURES

# Labour market variables:
ESS2016$mnactic<-as.factor(ESS2016$mnactic) # Activity status: all levels
ESS2016$wrkctra<-as.factor(ESS2016$wrkctra) # Type of work contract
ESS2016$wkhct<-as.numeric(ESS2016$wkhct) # Number of contracted working hours (turned to numeric)
ESS2016$wkhct[which(ESS2016$wkhct>168)]<-NA # Classify two cases that had 555 contracted hours as NAs

# Categorical activity status variable:

ESS2016$act_4c<-factor(nrow(ESS2016),levels=c("Securely employed","Atypically employed","NEET","In education"))

for (i in 1:length(ESS2016$act_4c)){

if (as.factor(ESS2016$dngdk[i])=="Marked"|| # NAs in main activity
as.factor(ESS2016$dngna[i])=="Marked"||
as.factor(ESS2016$dngref[i])=="Marked"){
ESS2016$act_4c[i]<-NA}

if (as.factor(ESS2016$cmsrv[i])=="Marked"||  # People outside of the labour market
as.factor(ESS2016$hswrk[i])=="Marked"||
as.factor(ESS2016$dsbld[i])=="Marked"||
as.factor(ESS2016$rtrd[i])=="Marked"||
as.factor(ESS2016$dngoth[i])=="Marked"){
ESS2016$act_4c[i]<-"NEET"}

if (as.factor(ESS2016$edctn[i])=="Marked"){ # People in education
ESS2016$act_4c[i]<-"In education"}

if (as.factor(ESS2016$uempla[i])=="Marked"|| # People in unemployment
as.factor(ESS2016$uempli[i])=="Marked"){
ESS2016$act_4c[i]<-"NEET"}

if (as.factor(ESS2016$pdwrk[i])=="Marked"){   # For those in paid work
   
   if (ESS2016$wrkctra[i]=="Don't know"||   # NAs in type of contract
   ESS2016$wrkctra[i]=="No answer"||
   ESS2016$wrkctra[i]=="Not applicable"||
   ESS2016$wrkctra[i]=="Refusal"){
   ESS2016$act_4c[i]<-NA}

   if (ESS2016$wrkctra[i]=="Limited"||      # People with limited or no contract as outsiders
   ESS2016$wrkctra[i]=="No contract"){
   ESS2016$act_4c[i]<-"Atypically employed"}

   if (ESS2016$wrkctra[i]=="Unlimited"){    # For those with an unlimited contract
       
       if (is.na(ESS2016$wkhct[i])){        # NAs in contracted hours
       ESS2016$act_4c[i]<-NA}

       else{
 
       if (ESS2016$wkhct[i]<30){            # People with less than 30 hours per week as part time
       ESS2016$act_4c[i]<-"Atypically employed"}  # (consider why I picked this amount)

       if (ESS2016$wkhct[i]>=30){           # People in paid employment with an unlimited contract
       ESS2016$act_4c[i]<-"Securely employed"}   # and more than 30 hours as insiders.
}}}}

summary(ESS2016$act_4c)

#####################################################################

## 4.4) MAIN INDEPENDENT VARIABLES: COUNTRY LEVEL COHABITATION RATES

A18_39Ind16<-which(ESS2016$agea>=18 & ESS2016$agea<40) # An index for the 18-39 subsample

ChR<-c(
mean(na.omit(as.numeric(ESS2016$lwparents_bin[A18_39Ind16][which(ESS2016$cntry[A18_39Ind16]=="Austria")])-1)),
mean(na.omit(as.numeric(ESS2016$lwparents_bin[A18_39Ind16][which(ESS2016$cntry[A18_39Ind16]=="Belgium")])-1)),
mean(na.omit(as.numeric(ESS2016$lwparents_bin[A18_39Ind16][which(ESS2016$cntry[A18_39Ind16]=="Czechia")])-1)),
mean(na.omit(as.numeric(ESS2016$lwparents_bin[A18_39Ind16][which(ESS2016$cntry[A18_39Ind16]=="Estonia")])-1)),
mean(na.omit(as.numeric(ESS2016$lwparents_bin[A18_39Ind16][which(ESS2016$cntry[A18_39Ind16]=="Finland")])-1)),
mean(na.omit(as.numeric(ESS2016$lwparents_bin[A18_39Ind16][which(ESS2016$cntry[A18_39Ind16]=="France")])-1)),
mean(na.omit(as.numeric(ESS2016$lwparents_bin[A18_39Ind16][which(ESS2016$cntry[A18_39Ind16]=="Germany")])-1)),
mean(na.omit(as.numeric(ESS2016$lwparents_bin[A18_39Ind16][which(ESS2016$cntry[A18_39Ind16]=="Hungary")])-1)),
mean(na.omit(as.numeric(ESS2016$lwparents_bin[A18_39Ind16][which(ESS2016$cntry[A18_39Ind16]=="Italy")])-1)),
mean(na.omit(as.numeric(ESS2016$lwparents_bin[A18_39Ind16][which(ESS2016$cntry[A18_39Ind16]=="Netherlands")])-1)),
mean(na.omit(as.numeric(ESS2016$lwparents_bin[A18_39Ind16][which(ESS2016$cntry[A18_39Ind16]=="Norway")])-1)),
mean(na.omit(as.numeric(ESS2016$lwparents_bin[A18_39Ind16][which(ESS2016$cntry[A18_39Ind16]=="Poland")])-1)),
mean(na.omit(as.numeric(ESS2016$lwparents_bin[A18_39Ind16][which(ESS2016$cntry[A18_39Ind16]=="Portugal")])-1)),
mean(na.omit(as.numeric(ESS2016$lwparents_bin[A18_39Ind16][which(ESS2016$cntry[A18_39Ind16]=="Spain")])-1)),
mean(na.omit(as.numeric(ESS2016$lwparents_bin[A18_39Ind16][which(ESS2016$cntry[A18_39Ind16]=="Sweden")])-1)),
mean(na.omit(as.numeric(ESS2016$lwparents_bin[A18_39Ind16][which(ESS2016$cntry[A18_39Ind16]=="Switzerland")])-1))
)
names(ChR)<-c("Austria","Belgium","Czechia","Estonia","Finland",
"France","Germany","Hungary","Italy","Netherlands","Norway","Poland",
"Portugal","Spain","Sweden","Switzerland")

ESS2016$ChR<-numeric(nrow(ESS2016))
ESS2016$ChR[which(ESS2016$cntry=="Austria")]=ChR["Austria"]
ESS2016$ChR[which(ESS2016$cntry=="Belgium")]=ChR["Belgium"]
ESS2016$ChR[which(ESS2016$cntry=="Czechia")]=ChR["Czechia"]
ESS2016$ChR[which(ESS2016$cntry=="Estonia")]=ChR["Estonia"]
ESS2016$ChR[which(ESS2016$cntry=="Finland")]=ChR["Finland"]
ESS2016$ChR[which(ESS2016$cntry=="France")]=ChR["France"]
ESS2016$ChR[which(ESS2016$cntry=="Germany")]=ChR["Germany"]
ESS2016$ChR[which(ESS2016$cntry=="Hungary")]=ChR["Hungary"]
ESS2016$ChR[which(ESS2016$cntry=="Italy")]=ChR["Italy"]
ESS2016$ChR[which(ESS2016$cntry=="Netherlands")]=ChR["Netherlands"]
ESS2016$ChR[which(ESS2016$cntry=="Norway")]=ChR["Norway"]
ESS2016$ChR[which(ESS2016$cntry=="Poland")]=ChR["Poland"]
ESS2016$ChR[which(ESS2016$cntry=="Portugal")]=ChR["Portugal"]
ESS2016$ChR[which(ESS2016$cntry=="Spain")]=ChR["Spain"]
ESS2016$ChR[which(ESS2016$cntry=="Sweden")]=ChR["Sweden"]
ESS2016$ChR[which(ESS2016$cntry=="Switzerland")]=ChR["Switzerland"]

#####################################################################


## 4.5) CONTROL VARIABLES 


# HH income (decile)
ESS2016$hhinctnta<-factor(ESS2016$hinctnta,
levels=c("J - 1st decile","R - 2nd decile","C - 3rd decile","M - 4th decile", 
"F - 5th decile","S - 6th decile","K - 7th decile","P - 8th decile", 
"D - 9th decile","H - 10th decile", "Refusal","Don't know","No answer"))
ESS2016$hhinctnta[which(ESS2016$hhinctnta=="Refusal")]=NA
ESS2016$hhinctnta[which(ESS2016$hhinctnta=="Don't know")]=NA
ESS2016$hhinctnta[which(ESS2016$hhinctnta=="No answer")]=NA
ESS2016$hhinctnta<-factor(ESS2016$hhinctnta,levels=c("J - 1st decile","R - 2nd decile","C - 3rd decile","M - 4th decile", 
"F - 5th decile","S - 6th decile","K - 7th decile","P - 8th decile", "D - 9th decile","H - 10th decile"),ordered=T)

# HH income decile, numeric ordered values
ESS2016$hhinctnta_num<-as.numeric(ESS2016$hhinctnta)      # Create a numeric version of the ordinal variable

####

# Number of HH members
ESS2016$hhmmb<-as.numeric(ESS2016$hhmmb)

####

# Lives with partner?

ESS2016$lwpartner<-factor(nrow(ESS2016),levels=c("No","Yes"))

for (i in 1:nrow(ESS2016)){if ("Husband/wife/partner" %in% ESS2016[i,rltvar]){ # If partner lives in the household
ESS2016$lwpartner[i]<-"Yes"}
else{
if ("Refusal"%in% ESS2016[i,rltvar]||"Don't know"%in%ESS2016[i,rltvar]||"No answer"%in%ESS2016[i,rltvar]||"NA"%in%ESS2016[i,rltvar]){
ESS2016$lwpartner[i]=="NA"} # If respondents refuse to answer, don't know or don't answer for any other reason for any of the HH members relation,
# data coded as NA
else{                                               # If not
ESS2016$lwpartner[i]<-"No"}}}

summary(ESS2016$lwpartner) # Only 164 NAs.

####

# Education level
ESS2016$eisced<-factor(ESS2016$eisced,levels=c("ES-ISCED I , less than lower secondary",
"ES-ISCED II, lower secondary","ES-ISCED IIIa, upper tier upper secondary","ES-ISCED IIIb, lower tier upper secondary",
"ES-ISCED IV, advanced vocational, sub-degree","ES-ISCED V1, lower tertiary education, BA level",
"ES-ISCED V2, higher tertiary education, >= MA level",
"Other","Refusal","Don't know","No answer"))

ESS2016$eisced_dum<-factor(nrow(ESS2016),levels=c("Below higher secondary education","Completed higher secondary education"))
ESS2016$eisced_dum[which(as.numeric(ESS2016$eisced)<3)]="Below higher secondary education"
ESS2016$eisced_dum[which(as.numeric(ESS2016$eisced)>=3 & as.numeric(ESS2016$eisced)<8)]="Completed higher secondary education"
ESS2016$eisced_dum[which(as.numeric(ESS2016$eisced)>=8)]=NA  # We will use this dummy to replicate S&H's (2013) model

####

## Eurostat Social Investment Data

Eurostat_SPB16<-read.csv("Eurostat_SPB16.csv",dec=".")

ESS2016$SPB_ES<-numeric(nrow(ESS2016))
ESS2016$SPB_ES[which(ESS2016$cntry=="Austria")]=Eurostat_SPB16$OBS_VALUE[which(Eurostat_SPB16$geo=="AT")]
ESS2016$SPB_ES[which(ESS2016$cntry=="Belgium")]=Eurostat_SPB16$OBS_VALUE[which(Eurostat_SPB16$geo=="BE")]
ESS2016$SPB_ES[which(ESS2016$cntry=="Czechia")]=Eurostat_SPB16$OBS_VALUE[which(Eurostat_SPB16$geo=="CZ")]
ESS2016$SPB_ES[which(ESS2016$cntry=="Estonia")]=Eurostat_SPB16$OBS_VALUE[which(Eurostat_SPB16$geo=="EE")]
ESS2016$SPB_ES[which(ESS2016$cntry=="Finland")]=Eurostat_SPB16$OBS_VALUE[which(Eurostat_SPB16$geo=="FI")]
ESS2016$SPB_ES[which(ESS2016$cntry=="France")]=Eurostat_SPB16$OBS_VALUE[which(Eurostat_SPB16$geo=="FR")]
ESS2016$SPB_ES[which(ESS2016$cntry=="Germany")]=Eurostat_SPB16$OBS_VALUE[which(Eurostat_SPB16$geo=="DE")]
ESS2016$SPB_ES[which(ESS2016$cntry=="Hungary")]=Eurostat_SPB16$OBS_VALUE[which(Eurostat_SPB16$geo=="HU")]
ESS2016$SPB_ES[which(ESS2016$cntry=="Italy")]=Eurostat_SPB16$OBS_VALUE[which(Eurostat_SPB16$geo=="IT")]
ESS2016$SPB_ES[which(ESS2016$cntry=="Netherlands")]=Eurostat_SPB16$OBS_VALUE[which(Eurostat_SPB16$geo=="NL")]
ESS2016$SPB_ES[which(ESS2016$cntry=="Norway")]=Eurostat_SPB16$OBS_VALUE[which(Eurostat_SPB16$geo=="NO")]
ESS2016$SPB_ES[which(ESS2016$cntry=="Poland")]=Eurostat_SPB16$OBS_VALUE[which(Eurostat_SPB16$geo=="PL")]
ESS2016$SPB_ES[which(ESS2016$cntry=="Portugal")]=Eurostat_SPB16$OBS_VALUE[which(Eurostat_SPB16$geo=="PT")]
ESS2016$SPB_ES[which(ESS2016$cntry=="Spain")]=Eurostat_SPB16$OBS_VALUE[which(Eurostat_SPB16$geo=="ES")]
ESS2016$SPB_ES[which(ESS2016$cntry=="Sweden")]=Eurostat_SPB16$OBS_VALUE[which(Eurostat_SPB16$geo=="SE")]
ESS2016$SPB_ES[which(ESS2016$cntry=="Switzerland")]=Eurostat_SPB16$OBS_VALUE[which(Eurostat_SPB16$geo=="CH")]
ESS2016$SPB_ES[which(ESS2016$SI_ES==0)]<-NA

ESS2016$SPB_ES<-ESS2016$SPB_ES/1000 # Transform to thousand Euros per citizen (in PPS)

#####################################################################

## STEP 5) CREATE SUBSAMPLES FOR REGION AND AGE 

# AGE GROUPS

# Under 40
U40Ind16<-which(ESS2016$agea<40)  

# Subcategories
A18_39Ind16<-which(ESS2016$agea>=18 & ESS2016$agea<40)
A18_34Ind16<-which(ESS2016$agea>=18 & ESS2016$agea<35)  
A18_24Ind16<-which(ESS2016$agea>=18 & ESS2016$agea<25)  
A25_29Ind16<-which(ESS2016$agea>=25 & ESS2016$agea<30)  
A30_34Ind16<-which(ESS2016$agea>=30 & ESS2016$agea<35)  
A35_39Ind16<-which(ESS2016$agea>=35 & ESS2016$agea<40)  

# BY WELFARE REGIMES

MedInd16<-which(ESS2016$regime=="Mediterranean")

ScaInd16<-which(ESS2016$regime=="Scandinavian")

ConInd16<-which(ESS2016$regime=="Continental")

EastInd16<-c(which(ESS2016$regime=="Eastern"))         

# WELFARE REGIMES + AGE GROUPS
MedU40Ind16<-intersect(U40Ind16,MedInd16)
ScaU40Ind16<-intersect(U40Ind16,ScaInd16)
ConU40Ind16<-intersect(U40Ind16,ConInd16)
EastU40Ind16<-intersect(U40Ind16,EastInd16)

Med18_39Ind16<-intersect(A18_39Ind16,MedInd16)
Med18_34Ind16<-intersect(A18_34Ind16,MedInd16)
Med18_24Ind16<-intersect(A18_24Ind16,MedInd16)
Med25_29Ind16<-intersect(A25_29Ind16,MedInd16)
Med30_34Ind16<-intersect(A30_34Ind16,MedInd16)
Med35_39Ind16<-intersect(A35_39Ind16,MedInd16)

Con18_39Ind16<-intersect(A18_39Ind16,ConInd16)
Con18_34Ind16<-intersect(A18_34Ind16,ConInd16)
Con18_24Ind16<-intersect(A18_24Ind16,ConInd16)
Con25_29Ind16<-intersect(A25_29Ind16,ConInd16)
Con30_34Ind16<-intersect(A30_34Ind16,ConInd16)
Con35_39Ind16<-intersect(A35_39Ind16,ConInd16)

Sca18_39Ind16<-intersect(A18_39Ind16,ScaInd16)
Sca18_34Ind16<-intersect(A18_34Ind16,ScaInd16)
Sca18_24Ind16<-intersect(A18_24Ind16,ScaInd16)
Sca25_29Ind16<-intersect(A25_29Ind16,ScaInd16)
Sca30_34Ind16<-intersect(A30_34Ind16,ScaInd16)
Sca35_39Ind16<-intersect(A35_39Ind16,ScaInd16)

East18_39Ind16<-intersect(A18_39Ind16,EastInd16)
East18_34Ind16<-intersect(A18_34Ind16,EastInd16)
East18_24Ind16<-intersect(A18_24Ind16,EastInd16)
East25_29Ind16<-intersect(A25_29Ind16,EastInd16)
East30_34Ind16<-intersect(A30_34Ind16,EastInd16)
East35_39Ind16<-intersect(A35_39Ind16,EastInd16)


##############################################################

# SAVE

rm(rltvar);rm(i)

save(list=ls(),file="ESS2016_session.RData")

save(ESS2016,file="ESS2016_edit.RData")
write.csv2(ESS2016,file="ESS2016_edit.csv")

load("ESS2016_session.RData") 