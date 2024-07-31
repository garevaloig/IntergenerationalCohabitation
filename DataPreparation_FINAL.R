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
library(dplyr)
library(survey)
library(ltm)
library(ISCO08ConveRsions)

setwd("D:/BIGSSS/Dissertation/Study 1/Data & Scripts/DATA")

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

ESS2016$cntry<-droplevels(ESS2016$cntry)

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
ESS2016$regime[which(ESS2016$cntry=="Slovenia")]="Eastern"
ESS2016$regime[which(ESS2016$cntry=="Spain")]="Mediterranean"
ESS2016$regime[which(ESS2016$cntry=="Sweden")]="Scandinavian"
ESS2016$regime[which(ESS2016$cntry=="Switzerland")]="Continental"

#####################################################################

## 4.1) DEPENDENT VARIABLES (ATTITUDES)

# Unemployment benefits
ESS2016$gvslvue<-factor(ESS2016$gvslvue,
levels=c("Not governments' responsibility at all","1","2","3","4","5","6","7","8","9",
"Entirely governments' responsibility","Refusal","Don't know","No answer"))
ESS2016$gvslvue[which(ESS2016$gvslvue=="Refusal")]=NA
ESS2016$gvslvue[which(ESS2016$gvslvue=="Don't know")]=NA
ESS2016$gvslvue[which(ESS2016$gvslvue=="No answer")]=NA
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
ESS2016$basinc[which(ESS2016$basinc=="Refusal")]=NA
ESS2016$basinc[which(ESS2016$basinc=="Don't know")]=NA
ESS2016$basinc[which(ESS2016$basinc=="No answer")]=NA
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
summary(ESS2016$lwparents_bin) # Only 191 NAs. Very satisfactory.

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


# Risk-based measure (Schwander & Haeusermann, 2013)

ESS2016$emplrel<-as.factor(ESS2016$emplrel) # Employment relation

# Assigning codes to isco08 occupations
ESS2016$isco08<-as.factor(ESS2016$isco08)   # Occupation

# Recoding isco08's categories into the numeric codes

ESS2016$isco08codes<-revalue(ESS2016$isco08,
                             c("Armed forces occupations"="0",
                               "Commissioned armed forces officers"="100",
                               "Commissioned armed forces officers_duplicated_110"="110",
                               "Non-commissioned armed forces officers"="200",
                               "Non-commissioned armed forces officers_duplicated_210"="210",
                               "Armed forces occupations, other ranks"="300",
                               "Armed forces occupations, other ranks_duplicated_310"="310",
                               "Managers"="1000",
                               "Chief executives, senior officials and legislators"="1100",
                               "Legislators and senior officials"="1110",
                               "Legislators"="1111",
                               "Senior government officials"="1112",
                               "Traditional chiefs and heads of village"="1113",
                               "Senior officials of special-interest organizations"="1114",
                               "Managing directors and chief executives"="1120",
                               "Administrative and commercial managers"="1200",
                               "Business services and administration managers"="1210",
                               "Finance managers"="1211",
                               "Human resource managers"="1212",
                               "Policy and planning managers"="1213",
                               "Business services and administration managers not elsewhere classified"="1219",
                               "Sales, marketing and development managers"="1220",
                               "Sales and marketing managers"="1221",
                               "Advertising and public relations managers"="1222",
                               "Research and development managers"="1223",
                               "Production and specialised services managers"="1300",
                               "Production managers in agriculture, forestry and fisheries"="1310",
                               "Agricultural and forestry production managers"="1311",
                               "Aquaculture and fisheries production managers"="1312",
                               "Manufacturing, mining, construction, and distribution managers"="1320",
                               "Manufacturing managers"="1321",
                               "Mining managers"="1322",
                               "Construction managers"="1323",
                               "Supply, distribution and related managers"="1324",
                               "Information and communications technology service managers"="1330",
                               "Professional services managers"="1340",
                               "Child care services managers"="1341",
                               "Health services managers"="1342",
                               "Aged care services managers"="1343",
                               "Social welfare managers"="1344",
                               "Education managers"="1345",
                               "Financial and insurance services branch managers"="1346",
                               "Professional services managers not elsewhere classified"="1349",
                               "Hospitality, retail and other services managers"="1400",
                               "Hotel and restaurant managers"="1410",
                               "Hotel managers"="1411",
                               "Restaurant managers"="1412",
                               "Retail and wholesale trade managers"="1420",
                               "Other services managers"="1430",
                               "Sports, recreation and cultural centre managers"="1431",
                               "Services managers not elsewhere classified"="1439",
                               "Professionals"="2000",
                               "Science and engineering professionals"="2100",
                               "Physical and earth science professionals"="2110",
                               "Physicists and astronomers"="2111",
                               "Meteorologists"="2112",
                               "Chemists"="2113",
                               "Geologists and geophysicists"="2114",
                               "Mathematicians, actuaries and statisticians"="2120",
                               "Life science professionals"="2130",
                               "Biologists, botanists, zoologists and related professionals"="2131",
                               "Farming, forestry and fisheries advisers"="2132",
                               "Environmental protection professionals"="2133",
                               "Engineering professionals (excluding electrotechnology)"="2140",
                               "Industrial and production engineers"="2141",
                               "Civil engineers"="2142",
                               "Environmental engineers"="2143",
                               "Mechanical engineers"="2144",
                               "Chemical engineers"="2145",
                               "Mining engineers, metallurgists and related professionals"="2146",
                               "Engineering professionals not elsewhere classified"="2149",
                               "Electrotechnology engineers"="2150",
                               "Electrical engineers"="2151",
                               "Electronics engineers"="2152",
                               "Telecommunications engineers"="2153",
                               "Architects, planners, surveyors and designers"="2160",
                               "Building architects"="2161",
                               "Landscape architects"="2162",
                               "Product and garment designers"="2163",
                               "Town and traffic planners"="2164",
                               "Cartographers and surveyors"="2165",
                               "Graphic and multimedia designers"="2166",
                               "Health professionals"="2200",
                               "Medical doctors"="2210",
                               "Generalist medical practitioners"="2211",
                               "Specialist medical practitioners"="2212",
                               "Nursing and midwifery professionals"="2220",
                               "Nursing professionals"="2221",
                               "Midwifery professionals"="2222",
                               "Traditional and complementary medicine professionals"="2230",
                               "Paramedical practitioners"="2240",
                               "Veterinarians"="2250",
                               "Other health professionals"="2260",
                               "Dentists"="2261",
                               "Pharmacists"="2262",
                               "Environmental and occupational health and hygiene professionals"="2263",
                               "Physiotherapists"="2264",
                               "Dieticians and nutritionists"="2265",
                               "Audiologists and speech therapists"="2266",
                               "Optometrists and ophthalmic opticians"="2267",
                               "Health professionals not elsewhere classified"="2269",
                               "Teaching professionals"="2300",
                               "University and higher education teachers"="2310",
                               "Vocational education teachers"="2320",
                               "Secondary education teachers"="2330",
                               "Primary school and early childhood teachers"="2340",
                               "Primary school teachers"="2341",
                               "Early childhood educators"="2342",
                               "Other teaching professionals"="2350",
                               "Education methods specialists"="2351",
                               "Special needs teachers"="2352",
                               "Other language teachers"="2353",
                               "Other music teachers"="2354",
                               "Other arts teachers"="2355",
                               "Information technology trainers"="2356",
                               "Teaching professionals not elsewhere classified"="2359",
                               "Business and administration professionals"="2400",
                               "Finance professionals"="2410",
                               "Accountants"="2411",
                               "Financial and investment advisers"="2412",
                               "Financial analysts"="2413",
                               "Administration professionals"="2420",
                               "Management and organization analysts"="2421",
                               "Policy administration professionals"="2422",
                               "Personnel and careers professionals"="2423",
                               "Training and staff development professionals"="2424",
                               "Sales, marketing and public relations professionals"="2430",
                               "Advertising and marketing professionals"="2431",
                               "Public relations professionals"="2432",
                               "Technical and medical sales professionals (excluding ICT)"="2433",
                               "Information and communications technology sales professionals"="2434",
                               "Information and communications technology professionals"="2500",
                               "Software and applications developers and analysts"="2510",
                               "Systems analysts"="2511",
                               "Software developers"="2512",
                               "Web and multimedia developers"="2513",
                               "Applications programmers"="2514",
                               "Software and applications developers and analysts not elsewhere classified"="2519",
                               "Database and network professionals"="2520",
                               "Database designers and administrators"="2521",
                               "Systems administrators"="2522",
                               "Computer network professionals"="2523",
                               "Database and network professionals not elsewhere classified"="2529",
                               "Legal, social and cultural professionals"="2600",
                               "Legal professionals"="2610",
                               "Lawyers"="2611",
                               "Judges"="2612",
                               "Legal professionals not elsewhere classified"="2619",
                               "Librarians, archivists and curators"="2620",
                               "Archivists and curators"="2621",
                               "Librarians and related information professionals"="2622",
                               "Social and religious professionals"="2630",
                               "Economists"="2631",
                               "Sociologists, anthropologists and related professionals"="2632",
                               "Philosophers, historians and political scientists"="2633",
                               "Psychologists"="2634",
                               "Social work and counselling professionals"="2635",
                               "Religious professionals"="2636",
                               "Authors, journalists and linguists"="2640",
                               "Authors and related writers"="2641",
                               "Journalists"="2642",
                               "Translators, interpreters and other linguists"="2643",
                               "Creative and performing artists"="2650",
                               "Visual artists"="2651",
                               "Musicians, singers and composers"="2652",
                               "Dancers and choreographers"="2653",
                               "Film, stage and related directors and producers"="2654",
                               "Actors"="2655",
                               "Announcers on radio, television and other media"="2656",
                               "Creative and performing artists not elsewhere classified"="2659",
                               "Technicians and associate professionals"="3000",
                               "Science and engineering associate professionals"="3100",
                               "Physical and engineering science technicians"="3110",
                               "Chemical and physical science technicians"="3111",
                               "Civil engineering technicians"="3112",
                               "Electrical engineering technicians"="3113",
                               "Electronics engineering technicians"="3114",
                               "Mechanical engineering technicians"="3115",
                               "Chemical engineering technicians"="3116",
                               "Mining and metallurgical technicians"="3117",
                               "Draughtspersons"="3118",
                               "Physical and engineering science technicians not elsewhere classified"="3119",
                               "Mining, manufacturing and construction supervisors"="3120",
                               "Mining supervisors"="3121",
                               "Manufacturing supervisors"="3122",
                               "Construction supervisors"="3123",
                               "Process control technicians"="3130",
                               "Power production plant operators"="3131",
                               "Incinerator and water treatment plant operators"="3132",
                               "Chemical processing plant controllers"="3133",
                               "Petroleum and natural gas refining plant operators"="3134",
                               "Metal production process controllers"="3135",
                               "Process control technicians not elsewhere classified"="3139",
                               "Life science technicians and related associate professionals"="3140",
                               "Life science technicians (excluding medical)"="3141",
                               "Agricultural technicians"="3142",
                               "Forestry technicians"="3143",
                               "Ship and aircraft controllers and technicians"="3150",
                               "Ships' engineers"="3151",
                               "Ships' deck officers and pilots"="3152",
                               "Aircraft pilots and related associate professionals"="3153",
                               "Air traffic controllers"="3154",
                               "Air traffic safety electronics technicians"="3155",
                               "Health associate professionals"="3200",
                               "Medical and pharmaceutical technicians"="3210",
                               "Medical imaging and therapeutic equipment technicians"="3211",
                               "Medical and pathology laboratory technicians"="3212",
                               "Pharmaceutical technicians and assistants"="3213",
                               "Medical and dental prosthetic technicians"="3214",
                               "Nursing and midwifery associate professionals"="3220",
                               "Nursing associate professionals"="3221",
                               "Midwifery associate professionals"="3222",
                               "Traditional and complementary medicine associate professionals"="3230",
                               "Veterinary technicians and assistants"="3240",
                               "Other health associate professionals"="3250",
                               "Dental assistants and therapists"="3251",
                               "Medical records and health information technicians"="3252",
                               "Community health workers"="3253",
                               "Dispensing opticians"="3254",
                               "Physiotherapy technicians and assistants"="3255",
                               "Medical assistants"="3256",
                               "Environmental and occupational health inspectors and associates"="3257",
                               "Ambulance workers"="3258",
                               "Health associate professionals not elsewhere classified"="3259",
                               "Business and administration associate professionals"="3300",
                               "Financial and mathematical associate professionals"="3310",
                               "Securities and finance dealers and brokers"="3311",
                               "Credit and loans officers"="3312",
                               "Accounting associate professionals"="3313",
                               "Statistical, mathematical and related associate professionals"="3314",
                               "Valuers and loss assessors"="3315",
                               "Sales and purchasing agents and brokers"="3320",
                               "Insurance representatives"="3321",
                               "Commercial sales representatives"="3322",
                               "Buyers"="3323",
                               "Trade brokers"="3324",
                               "Business services agents"="3330",
                               "Clearing and forwarding agents"="3331",
                               "Conference and event planners"="3332",
                               "Employment agents and contractors"="3333",
                               "Real estate agents and property managers"="3334",
                               "Business services agents not elsewhere classified"="3339",
                               "Administrative and specialised secretaries"="3340",
                               "Office supervisors"="3341",
                               "Legal secretaries"="3342",
                               "Administrative and executive secretaries"="3343",
                               "Medical secretaries"="3344",
                               "Regulatory government associate professionals"="3350",
                               "Customs and border inspectors"="3351",
                               "Government tax and excise officials"="3352",
                               "Government social benefits officials"="3353",
                               "Government licensing officials"="3354",
                               "Police inspectors and detectives"="3355",
                               "Regulatory government associate professionals not elsewhere classified"="3359",
                               "Legal, social, cultural and related associate professionals"="3400",
                               "Legal, social and religious associate professionals"="3410",
                               "Police inspectors and detectives"="3411",
                               "Police inspectors and detectives_duplicated_3411"="3411",
                               "Social work associate professionals"="3412",
                               "Religious associate professionals"="3413",
                               "Sports and fitness workers"="3420",
                               "Athletes and sports players"="3421",
                               "Sports coaches, instructors and officials"="3422",
                               "Fitness and recreation instructors and program leaders"="3423",
                               "Artistic, cultural and culinary associate professionals"="3430",
                               "Photographers"="3431",
                               "Interior designers and decorators"="3432",
                               "Gallery, museum and library technicians"="3433",
                               "Chefs"="3434",
                               "Other artistic and cultural associate professionals"="3435",
                               "Information and communications technicians"="3500",
                               "Information and communications technology operations and user support technicians"="3510",
                               "Information and communications technology operations technicians"="3511",
                               "Information and communications technology user support technicians"="3512",
                               "Computer network and systems technicians"="3513",
                               "Web technicians"="3514",
                               "Telecommunications and broadcasting technicians"="3520",
                               "Broadcasting and audio-visual technicians"="3521",
                               "Telecommunications engineering technicians"="3522",
                               "Clerical support workers"="4000",
                               "General and keyboard clerks"="4100",
                               "General office clerks"="4110",
                               "Secretaries (general)"="4120",
                               "Keyboard operators"="4130",
                               "Typists and word processing operators"="4131",
                               "Data entry clerks"="4132",
                               "Customer services clerks"="4200",
                               "Tellers, money collectors and related clerks"="4210",
                               "Bank tellers and related clerks"="4211",
                               "Bookmakers, croupiers and related gaming workers"="4212",
                               "Pawnbrokers and money-lenders"="4213",
                               "Debt-collectors and related workers"="4214",
                               "Client information workers"="4220",
                               "Travel consultants and clerks"="4221",
                               "Contact centre information clerks"="4222",
                               "Telephone switchboard operators"="4223",
                               "Hotel receptionists"="4224",
                               "Enquiry clerks"="4225",
                               "Receptionists (general)"="4226",
                               "Survey and market research interviewers"="4227",
                               "Client information workers not elsewhere classified"="4229",
                               "Numerical and material recording clerks"="4300",
                               "Numerical clerks"="4310",
                               "Accounting and bookkeeping clerks"="4311",
                               "Statistical, finance and insurance clerks"="4312",
                               "Payroll clerks"="4313",
                               "Material-recording and transport clerks"="4320",
                               "Stock clerks"="4321",
                               "Production clerks"="4322",
                               "Transport clerks"="4323",
                               "Other clerical support workers"="4400",
                               "Other clerical support workers_duplicated_4410"="4410",
                               "Library clerks"="4411",
                               "Mail carriers and sorting clerks"="4412",
                               "Coding, proof-reading and related clerks"="4413",
                               "Scribes and related workers"="4414",
                               "Filing and copying clerks"="4415",
                               "Personnel clerks"="4416",
                               "Clerical support workers not elsewhere classified"="4419",
                               "Service and sales workers"="5000",
                               "Personal service workers"="5100",
                               "Travel attendants, conductors and guides"="5110",
                               "Travel attendants and travel stewards"="5111",
                               "Transport conductors"="5112",
                               "Travel guides"="5113",
                               "Cooks"="5120",
                               "Waiters and bartenders"="5130",
                               "Waiters"="5131",
                               "Bartenders"="5132",
                               "Hairdressers, beauticians and related workers"="5140",
                               "Hairdressers"="5141",
                               "Beauticians and related workers"="5142",
                               "Building and housekeeping supervisors"="5150",
                               "Cleaning and housekeeping supervisors in offices, hotels and other establishments"="5151",
                               "Domestic housekeepers"="5152",
                               "Building caretakers"="5153",
                               "Other personal services workers"="5160",
                               "Astrologers, fortune-tellers and related workers"="5161",
                               "Companions and valets"="5162",
                               "Undertakers and embalmers"="5163",
                               "Pet groomers and animal care workers"="5164",
                               "Driving instructors"="5165",
                               "Personal services workers not elsewhere classified"="5169",
                               "Sales workers"="5200",
                               "Street and market salespersons"="5210",
                               "Stall and market salespersons"="5211",
                               "Street food salespersons"="5212",
                               "Shop salespersons"="5220",
                               "Shop keepers"="5221",
                               "Shop supervisors"="5222",
                               "Shop sales assistants"="5223",
                               "Cashiers and ticket clerks"="5230",
                               "Other sales workers"="5240",
                               "Fashion and other models"="5241",
                               "Sales demonstrators"="5242",
                               "Door to door salespersons"="5243",
                               "Contact centre salespersons"="5244",
                               "Service station attendants"="5245",
                               "Food service counter attendants"="5246",
                               "Sales workers not elsewhere classified"="5249",
                               "Personal care workers"="5300",
                               "Child care workers and teachers' aides"="5310",
                               "Child care workers"="5311",
                               "Teachers' aides"="5312",
                               "Personal care workers in health services"="5320",
                               "Health care assistants"="5321",
                               "Home-based personal care workers"="5322",
                               "Personal care workers in health services not elsewhere classified"="5329",
                               "Protective services workers"="5400",
                               "Protective services workers_duplicated_5410"="5410",
                               "Fire-fighters"="5411",
                               "Police officers"="5412",
                               "Prison guards"="5413",
                               "Security guards"="5414",
                               "Protective services workers not elsewhere classified"="5419",
                               "Skilled agricultural, forestry and fishery workers"="6000",
                               "Market-oriented skilled agricultural workers"="6100",
                               "Market gardeners and crop growers"="6110",
                               "Field crop and vegetable growers"="6111",
                               "Tree and shrub crop growers"="6112",
                               "Gardeners, horticultural and nursery growers"="6113",
                               "Mixed crop growers"="6114",
                               "Animal producers"="6120",
                               "Livestock and dairy producers"="6121",
                               "Poultry producers"="6122",
                               "Apiarists and sericulturists"="6123",
                               "Animal producers not elsewhere classified"="6129",
                               "Mixed crop and animal producers"="6130",
                               "Market-oriented skilled forestry, fishery and hunting workers"="6200",
                               "Forestry and related workers"="6210",
                               "Fishery workers, hunters and trappers"="6220",
                               "Aquaculture workers"="6221",
                               "Inland and coastal waters fishery workers"="6222",
                               "Deep-sea fishery workers"="6223",
                               "Hunters and trappers"="6224",
                               "Subsistence farmers, fishers, hunters and gatherers"="6300",
                               "Subsistence crop farmers"="6310",
                               "Subsistence livestock farmers"="6320",
                               "Subsistence mixed crop and livestock farmers"="6330",
                               "Subsistence fishers, hunters, trappers and gatherers"="6340",
                               "Craft and related trades workers"="7000",
                               "Building and related trades workers, excluding electricians"="7100",
                               "Building frame and related trades workers"="7110",
                               "House builders"="7111",
                               "Bricklayers and related workers"="7112",
                               "Stonemasons, stone cutters, splitters and carvers"="7113",
                               "Concrete placers, concrete finishers and related workers"="7114",
                               "Carpenters and joiners"="7115",
                               "Building frame and related trades workers not elsewhere classified"="7119",
                               "Building finishers and related trades workers"="7120",
                               "Roofers"="7121",
                               "Floor layers and tile setters"="7122",
                               "Plasterers"="7123",
                               "Insulation workers"="7124",
                               "Glaziers"="7125",
                               "Plumbers and pipe fitters"="7126",
                               "Air conditioning and refrigeration mechanics"="7127",
                               "Painters, building structure cleaners and related trades workers"="7130",
                               "Painters and related workers"="7131",
                               "Spray painters and varnishers"="7132",
                               "Building structure cleaners"="7133",
                               "Metal, machinery and related trades workers"="7200",
                               "Sheet and structural metal workers, moulders and welders, and related workers"="7210",
                               "Metal moulders and coremakers"="7211",
                               "Welders and flamecutters"="7212",
                               "Sheet-metal workers"="7213",
                               "Structural-metal preparers and erectors"="7214",
                               "Riggers and cable splicers"="7215",
                               "Blacksmiths, toolmakers and related trades workers"="7220",
                               "Blacksmiths, hammersmiths and forging press workers"="7221",
                               "Toolmakers and related workers"="7222",
                               "Metal working machine tool setters and operators"="7223",
                               "Metal polishers, wheel grinders and tool sharpeners"="7224",
                               "Machinery mechanics and repairers"="7230",
                               "Motor vehicle mechanics and repairers"="7231",
                               "Aircraft engine mechanics and repairers"="7232",
                               "Agricultural and industrial machinery mechanics and repairers"="7233",
                               "Bicycle and related repairers"="7234",
                               "Handicraft and printing workers"="7300",
                               "Handicraft workers"="7310",
                               "Precision-instrument makers and repairers"="7311",
                               "Musical instrument makers and tuners"="7312",
                               "Jewellery and precious-metal workers"="7313",
                               "Potters and related workers"="7314",
                               "Glass makers, cutters, grinders and finishers"="7315",
                               "Sign writers, decorative painters, engravers and etchers"="7316",
                               "Handicraft workers in wood, basketry and related materials"="7317",
                               "Handicraft workers in textile, leather and related materials"="7318",
                               "Handicraft workers not elsewhere classified"="7319",
                               "Printing trades workers"="7320",
                               "Pre-press technicians"="7321",
                               "Printers"="7322",
                               "Print finishing and binding workers"="7323",
                               "Electrical and electronic trades workers"="7400",
                               "Electrical equipment installers and repairers"="7410",
                               "Building and related electricians"="7411",
                               "Electrical mechanics and fitters"="7412",
                               "Electrical line installers and repairers"="7413",
                               "Electronics and telecommunications installers and repairers"="7420",
                               "Electronics mechanics and servicers"="7421",
                               "Information and communications technology installers and servicers"="7422",
                               "Food processing, wood working, garment and other craft and related trades workers"="7500",
                               "Food processing and related trades workers"="7510",
                               "Butchers, fishmongers and related food preparers"="7511",
                               "Bakers, pastry-cooks and confectionery makers"="7512",
                               "Dairy-products makers"="7513",
                               "Fruit, vegetable and related preservers"="7514",
                               "Food and beverage tasters and graders"="7515",
                               "Tobacco preparers and tobacco products makers"="7516",
                               "Wood treaters, cabinet-makers and related trades workers"="7520",
                               "Wood treaters"="7521",
                               "Cabinet-makers and related workers"="7522",
                               "Woodworking-machine tool setters and operators"="7523",
                               "Garment and related trades workers"="7530",
                               "Tailors, dressmakers, furriers and hatters"="7531",
                               "Garment and related pattern-makers and cutters"="7532",
                               "Sewing, embroidery and related workers"="7533",
                               "Upholsterers and related workers"="7534",
                               "Pelt dressers, tanners and fellmongers"="7535",
                               "Shoemakers and related workers"="7536",
                               "Other craft and related workers"="7540",
                               "Underwater divers"="7541",
                               "Shotfirers and blasters"="7542",
                               "Product graders and testers (excluding foods and beverages)"="7543",
                               "Fumigators and other pest and weed controllers"="7544",
                               "Craft and related workers not elsewhere classified"="7549",
                               "Plant and machine operators, and assemblers"="8000",
                               "Stationary plant and machine operators"="8100",
                               "Mining and mineral processing plant operators"="8110",
                               "Miners and quarriers"="8111",
                               "Mineral and stone processing plant operators"="8112",
                               "Well drillers and borers and related workers"="8113",
                               "Cement, stone and other mineral products machine operators"="8114",
                               "Metal processing and finishing plant operators"="8120",
                               "Metal processing plant operators"="8121",
                               "Metal finishing, plating and coating machine operators"="8122",
                               "Chemical and photographic products plant and machine operators"="8130",
                               "Chemical products plant and machine operators"="8131",
                               "Photographic products machine operators"="8132",
                               "Rubber, plastic and paper products machine operators"="8140",
                               "Rubber products machine operators"="8141",
                               "Plastic products machine operators"="8142",
                               "Paper products machine operators"="8143",
                               "Textile, fur and leather products machine operators"="8150",
                               "Fibre preparing, spinning and winding machine operators"="8151",
                               "Weaving and knitting machine operators"="8152",
                               "Sewing machine operators"="8153",
                               "Bleaching, dyeing and fabric cleaning machine operators"="8154",
                               "Fur and leather preparing machine operators"="8155",
                               "Shoemaking and related machine operators"="8156",
                               "Laundry machine operators"="8157",
                               "Textile, fur and leather products machine operators not elsewhere classified"="8159",
                               "Food and related products machine operators"="8160",
                               "Wood processing and papermaking plant operators"="8170",
                               "Pulp and papermaking plant operators"="8171",
                               "Wood processing plant operators"="8172",
                               "Other stationary plant and machine operators"="8180",
                               "Glass and ceramics plant operators"="8181",
                               "Steam engine and boiler operators"="8182",
                               "Packing, bottling and labelling machine operators"="8183",
                               "Stationary plant and machine operators not elsewhere classified"="8189",
                               "Assemblers"="8200",
                               "Assemblers_duplicated_8210"="8210",
                               "Mechanical machinery assemblers"="8211",
                               "Electrical and electronic equipment assemblers"="8212",
                               "Assemblers not elsewhere classified"="8219",
                               "Drivers and mobile plant operators"="8300",
                               "Locomotive engine drivers and related workers"="8310",
                               "Locomotive engine drivers"="8311",
                               "Railway brake, signal and switch operators"="8312",
                               "Car, van and motorcycle drivers"="8320",
                               "Motorcycle drivers"="8321",
                               "Car, taxi and van drivers"="8322",
                               "Heavy truck and bus drivers"="8330",
                               "Bus and tram drivers"="8331",
                               "Heavy truck and lorry drivers"="8332",
                               "Mobile plant operators"="8340",
                               "Mobile farm and forestry plant operators"="8341",
                               "Earthmoving and related plant operators"="8342",
                               "Crane, hoist and related plant operators"="8343",
                               "Lifting truck operators"="8344",
                               "Ships' deck crews and related workers"="8350",
                               "Elementary occupations"="9000",
                               "Cleaners and helpers"="9100",
                               "Domestic, hotel and office cleaners and helpers"="9110",
                               "Domestic cleaners and helpers"="9111",
                               "Cleaners and helpers in offices, hotels and other establishments"="9112",
                               "Vehicle, window, laundry and other hand cleaning workers"="9120",
                               "Hand launderers and pressers"="9121",
                               "Vehicle cleaners"="9122",
                               "Window cleaners"="9123",
                               "Other cleaning workers"="9129",
                               "Agricultural, forestry and fishery labourers"="9200",
                               "Agricultural, forestry and fishery labourers_duplicated_9210"="9210",
                               "Crop farm labourers"="9211",
                               "Livestock farm labourers"="9212",
                               "Mixed crop and livestock farm labourers"="9213",
                               "Garden and horticultural labourers"="9214",
                               "Forestry labourers"="9215",
                               "Fishery and aquaculture labourers"="9216",
                               "Labourers in mining, construction, manufacturing and transport"="9300",
                               "Mining and construction labourers"="9310",
                               "Mining and quarrying labourers"="9311",
                               "Civil engineering labourers"="9312",
                               "Building construction labourers"="9313",
                               "Manufacturing labourers"="9320",
                               "Hand packers"="9321",
                               "Manufacturing labourers not elsewhere classified"="9329",
                               "Transport and storage labourers"="9330",
                               "Hand and pedal vehicle drivers"="9331",
                               "Drivers of animal-drawn vehicles and machinery"="9332",
                               "Freight handlers"="9333",
                               "Shelf fillers"="9334",
                               "Food preparation assistants"="9400",
                               "Food preparation assistants_duplicated_9410"="9410",
                               "Fast food preparers"="9411",
                               "Kitchen helpers"="9412",
                               "Street and related sales and service workers"="9500",
                               "Street and related service workers"="9510",
                               "Street vendors (excluding food)"="9520",
                               "Refuse workers and other elementary workers"="9600",
                               "Refuse workers"="9610",
                               "Garbage and recycling collectors"="9611",
                               "Refuse sorters"="9612",
                               "Sweepers and related labourers"="9613",
                               "Other elementary workers"="9620",
                               "Messengers, package deliverers and luggage porters"="9621",
                               "Odd job persons"="9622",
                               "Meter readers and vending-machine collectors"="9623",
                               "Water and firewood collectors"="9624",
                               "Elementary workers not elsewhere classified"="9629",
                               "Not applicable"="66666",
                               "Refusal"="77777",
                               "Don't know"="88888",
                               "No answer"="99999")) # Numeric ISCO-08 codes as a factor

ESS2016$isco08codes<-as.numeric(paste(ESS2016$isco08codes)) # Numeric ISCO-08 codes
ESS2016$isco08codes[which(ESS2016$isco08codes>=66666)]=NA

# Convert ISCO-08 codes to ISCO-88 codes

library(ISCO08ConveRsions)

ESS2016$isco88codes<-numeric(length(ESS2016$isco08codes))

for (i in 1:length(ESS2016$isco88codes)){
  if (is.na(ESS2016$isco08codes[i])){
    ESS2016$isco88codes[i]=NA}
  else{
    ESS2016$isco88codes[i]<-try(isco08toisco88(paste(ESS2016$isco08codes[i])),silent=T) # Use of try to ignore errors
  }}

ESS2016$isco88codes[which(ESS2016$isco88codes=="Error in isco08toisco88(paste(ESS2016$isco08codes[i])) : \n  Invalid ISCO08-Code\n")]<-NA
# This is to remove the few error cases. They`re classified as NAs

ESS2016$isco88codes<-as.numeric(paste(ESS2016$isco88codes)) # And we turn the vector back to numeric


# Classify individuals in the occupational group:

ESS2016$occugroup<-factor(nrow(ESS2016),levels=c("CA","MSF","BC","SCP","LSF"))

ESS2016$occugroup[which(ESS2016$isco88codes<2500 & 
                          ESS2016$emplrel=="Self-employed")]="CA"
ESS2016$occugroup[which(ESS2016$isco88codes>=2100 & ESS2016$isco88codes<2200)]="CA"
ESS2016$occugroup[which(ESS2016$isco88codes>=1100 & ESS2016$isco88codes<1300)]="CA"
ESS2016$occugroup[which(ESS2016$isco88codes>=1300 & ESS2016$isco88codes<1400)]="CA" 

ESS2016$occugroup[which(ESS2016$isco88codes>=2500 & 
                          ESS2016$emplrel=="Self-employed")]="MSF" 
ESS2016$occugroup[which(ESS2016$isco88codes>=3100 & ESS2016$isco88codes<3200)]="MSF" 
ESS2016$occugroup[which(ESS2016$isco88codes>=4100 & ESS2016$isco88codes<4300)]="MSF" 

ESS2016$occugroup[which(ESS2016$isco88codes>=7100 & ESS2016$isco88codes<7500)]="BC" 
ESS2016$occugroup[which(ESS2016$isco88codes>=6100 & ESS2016$isco88codes<6200)]="BC" 
ESS2016$occugroup[which(ESS2016$isco88codes>=8100 & ESS2016$isco88codes<8400)]="BC" 
ESS2016$occugroup[which(ESS2016$isco88codes>=9200 & ESS2016$isco88codes<9400)]="BC" 

ESS2016$occugroup[which(ESS2016$isco88codes>=2200 & ESS2016$isco88codes<2500)]="SCP" 
ESS2016$occugroup[which(ESS2016$isco88codes>=3200 & ESS2016$isco88codes<3500)]="SCP" 

ESS2016$occugroup[which(ESS2016$isco88codes>=5100 & ESS2016$isco88codes<5300)]="LSF" 
ESS2016$occugroup[which(ESS2016$isco88codes>=9100 & ESS2016$isco88codes<9200)]="LSF" 

ESS2016$occugroup[is.na(ESS2016$isco88codes)]=NA  

### Getting the final groups (country + gender + age + occupation)

ESS2016$SH_group<-factor(nrow(ESS2016),levels=c("LSF young women","LSF young men",
                                                "LSF old women","LSF old men","SCP young women","SCP young men","SCP old women",
                                                "SCP old men","BC young women","BC young men","BC old women","BC old men",
                                                "MSF young women","MSF young men","MSF old women","MSF old men","CA young women",
                                                "CA young men","CA old women","CA old men"))

ESS2016$SH_group[which(ESS2016$occugroup=="LSF" & ESS2016$gndr=="Female" & 
                         ESS2016$agea<40)]="LSF young women"
ESS2016$SH_group[which(ESS2016$occugroup=="LSF" & ESS2016$gndr=="Male" & 
                         ESS2016$agea<40)]="LSF young men"
ESS2016$SH_group[which(ESS2016$occugroup=="LSF" & ESS2016$gndr=="Female" & 
                         ESS2016$agea>=40)]="LSF old women"
ESS2016$SH_group[which(ESS2016$occugroup=="LSF" & ESS2016$gndr=="Male" & 
                         ESS2016$agea>=40)]="LSF old men"

ESS2016$SH_group[which(ESS2016$occugroup=="SCP" & ESS2016$gndr=="Female" & 
                         ESS2016$agea<40)]="SCP young women"
ESS2016$SH_group[which(ESS2016$occugroup=="SCP" & ESS2016$gndr=="Male" & 
                         ESS2016$agea<40)]="SCP young men"
ESS2016$SH_group[which(ESS2016$occugroup=="SCP" & ESS2016$gndr=="Female" & 
                         ESS2016$agea>=40)]="SCP old women"
ESS2016$SH_group[which(ESS2016$occugroup=="SCP" & ESS2016$gndr=="Male" & 
                         ESS2016$agea>=40)]="SCP old men"

ESS2016$SH_group[which(ESS2016$occugroup=="BC" & ESS2016$gndr=="Female" & 
                         ESS2016$agea<40)]="BC young women"
ESS2016$SH_group[which(ESS2016$occugroup=="BC" & ESS2016$gndr=="Male" & 
                         ESS2016$agea<40)]="BC young men"
ESS2016$SH_group[which(ESS2016$occugroup=="BC" & ESS2016$gndr=="Female" & 
                         ESS2016$agea>=40)]="BC old women"
ESS2016$SH_group[which(ESS2016$occugroup=="BC" & ESS2016$gndr=="Male" & 
                         ESS2016$agea>=40)]="BC old men"

ESS2016$SH_group[which(ESS2016$occugroup=="MSF" & ESS2016$gndr=="Female" & 
                         ESS2016$agea<40)]="MSF young women"
ESS2016$SH_group[which(ESS2016$occugroup=="MSF" & ESS2016$gndr=="Male" & 
                         ESS2016$agea<40)]="MSF young men"
ESS2016$SH_group[which(ESS2016$occugroup=="MSF" & ESS2016$gndr=="Female" & 
                         ESS2016$agea>=40)]="MSF old women"
ESS2016$SH_group[which(ESS2016$occugroup=="MSF" & ESS2016$gndr=="Male" & 
                         ESS2016$agea>=40)]="MSF old men"

ESS2016$SH_group[which(ESS2016$occugroup=="CA" & ESS2016$gndr=="Female" & 
                         ESS2016$agea<40)]="CA young women"
ESS2016$SH_group[which(ESS2016$occugroup=="CA" & ESS2016$gndr=="Male" & 
                         ESS2016$agea<40)]="CA young men"
ESS2016$SH_group[which(ESS2016$occugroup=="CA" & ESS2016$gndr=="Female" & 
                         ESS2016$agea>=40)]="CA old women"
ESS2016$SH_group[which(ESS2016$occugroup=="CA" & ESS2016$gndr=="Male" & 
                         ESS2016$agea>=40)]="CA old men"

## 4.2) ASSIGN GROUP RATES FOR COUNTRIES AND REGIMES FROM LMV_VALUES (FROM H. SCHWANDER'S REPOSITORY)


lmr_rates16<-read.csv("lmv_values_2016.csv",sep=",")
colnames(lmr_rates16)=c("Mean.Outsiderness","Austria","Belgium","Switzerland","Denmark",
                        "Spain","Finland","France","Greece","Ireland","Italy","Netherlands",
                        "Norway","Portugal","Sweden","United Kingdom","All countries")
lmr_rates16$Germany<-c(1.566463,0.352951,1.203658,0.11387,0.735009,0.22682,0.196483,
-0.519565,1.382507,0.329147,1.492428,-0.069445,0.608649,-0.086018,0.434387,-0.509703,
-0.131955,-0.652807,-0.035613,-0.864988)

lmr_rates16$Mean.Outsiderness<-as.factor(lmr_rates16$Mean.Outsiderness)

ESS2016$lmr_SH<-numeric(nrow(ESS2016))

for (i in 1:nrow(ESS2016)){
  if(ESS2016$cntry[i]=="Czechia" | ESS2016$cntry[i]=="Estonia" | ESS2016$cntry[i]=="Hungary" |
     ESS2016$cntry[i]=="Poland" | ESS2016$cntry[i]=="Slovenia" | is.na(ESS2016$cntry[i])){
     ESS2016$lmr_SH[i]<-NA}
  else{
  if (!is.na(ESS2016$SH_group[i])){
    ESS2016$lmr_SH[i]<-lmr_rates16[which(lmr_rates16$Mean.Outsiderness==ESS2016$SH_group[i]),
                                   which(colnames(lmr_rates16)==ESS2016$cntry[i])]}
  else{
    ESS2016$lmr_SH[i]<-NA
  }}
}

# Occupational unemployment rate measure Rehm (2005, 2009)

#load("RehmUnemploymentRates16.RData")

# RehmUnemploymentRates16$Rates[which(is.na(RehmUnemploymentRates16$Rates))]=9999

ESS2016$unemployed<-numeric(nrow(ESS2016))
ESS2016$unemployed[which(ESS2016$uempla=="Marked" | ESS2016$uempli=="Marked")]=1
ESS2016$unemployed[which(ESS2016$uempla=="Not marked" & ESS2016$uempli=="Not marked")]=0

ESS2016$isco08_2d<-floor(ESS2016$isco08codes/100)
ESS2016$isco08_2d<-as.factor(paste(ESS2016$isco08_2d))
ESS2016$isco08_2d<-recode_factor(ESS2016$isco08_2d,"0"="00","1"="01","2"="02","3"="03","10"="11","20"="21","30"="31",
                                 "40"="41","50"="51","60"="61","70"="71","80"="81","90"="91")
ESS2016$isco08_2d[which(ESS2016$isco08_2d=="NA")]=NA;ESS2016$isco08_2d<-droplevels(ESS2016$isco08_2d)

# ESS2016$Rehm_unemployment_rates<-numeric(nrow(ESS2016))
# for (i in 1:nrow(ESS2016)){
#   if (!is.na(ESS2016$isco08_2d[i])){
#     ESS2016$Rehm_unemployment_rates[i]<-RehmUnemploymentRates16[which(RehmUnemploymentRates16$cntry==ESS2016$cntry[i] &
#                                                                         RehmUnemploymentRates16$ISCO08==ESS2016$isco08_2d[i]),"Rates"]}
#   else{
#     ESS2016$Rehm_unemployment_rates[i]<-NA
#   }
# }

#ESS2016$Rehm_unemployment_rates[which(ESS2016$Rehm_unemployment_rates==9999)]=NA

ESS2016$lmr_Rehm<-numeric(nrow(ESS2016))
for (i in 1:nrow(ESS2016)){
  if (!is.na(ESS2016$isco08_2d[i])){
    ESS2016$lmr_Rehm[i]<-weighted.mean(ESS2016$unemployed[which(ESS2016$isco08_2d==ESS2016$isco08_2d[i] & 
                                                                         ESS2016$gndr==ESS2016$gndr[i] &
                                                                         ESS2016$cntry==ESS2016$cntry[i])],
                                              w=ESS2016$anweight[which(ESS2016$isco08_2d==ESS2016$isco08_2d[i] & 
                                                                         ESS2016$gndr==ESS2016$gndr[i] &
                                                                        ESS2016$cntry==ESS2016$cntry[i])],
                                              na.rm=T)
  }
  else{
    ESS2016$lmr_Rehm[i]<-NA
  }
}

ESS2016$lmr_Rehm<-ESS2016$lmr_Rehm*100


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

# Union membership
library(plyr)

ESS2016$mbtru<-factor(ESS2016$mbtru,levels=c("No","Yes, currently","Yes, previously","Refusal","Don't know","No answer"))
ESS2016$mbtru_dum<-revalue(ESS2016$mbtru,
                           c("No"="No","Yes, currently"="Yes","Yes, previously"="Yes","Refusal"=NA,"Don't know"=NA,"No answer"=NA))
# We will use this dummy to replicate S&H's (2013) model


# Church attendance
ESS2016$rlgatnd<-factor(ESS2016$rlgatnd,levels=c("Never","Less often","Only on special holy days","At least once a month",
                                                 "Once a week","More than once a week","Every day","Refusal","Don't know","No answer"))
ESS2016$rlgatnd<-revalue(ESS2016$rlgatnd,c("Refusal"=NA,"Don't know"=NA,"No answer"=NA))
ESS2016$rlgatnd_num<-as.numeric(ESS2016$rlgatnd) # We will use this numeric to replicate S&H's (2013) model 

# Egalitarian values
ESS2016$ipeqopt<-factor(ESS2016$ipeqopt,
                       levels=c("Not like me at all","Not like me","A little like me","Somewhat like me","Like me","Very much like me",
                                "Refusal","Don't know","No answer"))
ESS2016$ipeqopt[which(ESS2016$ipeqopt=="Refusal")]=NA
ESS2016$ipeqopt[which(ESS2016$ipeqopt=="Don't know")]=NA
ESS2016$ipeqopt[which(ESS2016$ipeqopt=="No answer")]=NA
ESS2016$ipeqopt<-factor(ESS2016$ipeqopt,levels=c("Strongly against","Against","In favour","Strongly in favour"),
                       ordered=T)

ESS2016$ipeqopt_num<-as.numeric(ESS2016$ipeqopt)# A numeric version for linear models

# Support for equal gay rights (proxy for cultural liberalism)
ESS2016$freehms<-factor(ESS2016$freehms,
                        levels=c("Disagree strongly","Disagree","Neither agree nor disagree","Agree","Agree strongly",
                                 "Refusal","Don't know","No answer"))
ESS2016$freehms[which(ESS2016$freehms=="Refusal")]=NA
ESS2016$freehms[which(ESS2016$freehms=="Don't know")]=NA
ESS2016$freehms[which(ESS2016$freehms=="No answer")]=NA
ESS2016$freehms<-factor(ESS2016$freehms,levels=c("Disagree strongly","Disagree","Neither agree nor disagree","Agree","Agree strongly"),
                        ordered=T)

ESS2016$freehms_num<-as.numeric(ESS2016$freehms)

# Support for women's rights to a job (proxy for cultural liberalism, has been inverted)
ESS2016$mnrgtjb<-factor(ESS2016$mnrgtjb,
                        levels=c("Agree strongly","Agree","Neither agree nor disagree","Disagree","Disagree strongly",
                                 "Refusal","Don't know","No answer"))
ESS2016$mnrgtjb[which(ESS2016$mnrgtjb=="Refusal")]=NA
ESS2016$mnrgtjb[which(ESS2016$mnrgtjb=="Don't know")]=NA
ESS2016$mnrgtjb[which(ESS2016$mnrgtjb=="No answer")]=NA
ESS2016$mnrgtjb<-factor(ESS2016$mnrgtjb,levels=c("Agree strongly","Agree","Neither agree nor disagree","Disagree","Disagree strongly"),
                        ordered=T)

ESS2016$mnrgtjb_num<-as.numeric(ESS2016$mnrgtjb)

cor(ESS2016$freehms_num,ESS2016$mnrgtjb_num,use="complete.obs")
cronbach.alpha(cbind(ESS2016$freehms_num,ESS2016$mnrgtjb_num),na.rm=T)

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