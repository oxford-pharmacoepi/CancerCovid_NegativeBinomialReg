renv::restore() # this should prompt you to install the various packages required for the study

# packages
library(here)
library(lubridate)
library(stringr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
require(foreign)
require(MASS)
library(tsibble)
library(graphics)
library(feasts)
library(magrittr)
library(epiR)
library(fmsb)
library(epitools)

load(here("2_DataPrep", "Data", "GeneralPop2017_20.RData"))
load(here("2_DataPrep", "Data", "IR.overall.RData"))
load(here("2_DataPrep", "Data", "IR.age.RData"))
load(here("2_DataPrep", "Data", "IR.gender.RData"))
load(here("2_DataPrep", "Data", "IR.ses.RData"))

# Periods-----------------
IR.overall <- IR.overall %>% mutate(Month1 =paste(1,month, year, sep ="-")) 
IR.gender <- IR.gender %>%  mutate(Month1 =paste(1,month, year, sep ="-")) 
IR.age_gender <- IR.age_gender %>%  mutate(Month1 =paste(1,month, year, sep ="-"))
IR.ses <- IR.ses %>%  mutate(Month1 =paste(1,month, year, sep ="-"))

Sys.setlocale("LC_TIME", "English")
IR.overall$Date <- NA
IR.overall$Date <- dmy(IR.overall$Month1)
IR.overall$Month1 <- NULL

IR.gender$Date <- NA
IR.gender$Date <- dmy(IR.gender$Month1)
IR.gender$Month1 <- NULL

IR.age_gender$Date <- NA
IR.age_gender$Date <- dmy(IR.age_gender$Month1)
IR.age_gender$Month1 <- NULL

IR.ses$Date <- NA
IR.ses$Date <- dmy(IR.ses$Month1)
IR.ses$Month1 <- NULL


IR.overall$covid <- as.factor(IR.overall$covid)
IR.overall$covid <-relevel(IR.overall$covid, "Pre-COVID")

IR.gender$covid <- as.factor(IR.gender$covid)
IR.gender$covid <-relevel(IR.gender$covid, "Pre-COVID")

IR.age_gender$covid <- as.factor(IR.age_gender$covid)
IR.age_gender$covid <-relevel(IR.age_gender$covid, "Pre-COVID")

IR.ses$covid <- as.factor(IR.ses$covid)
IR.ses$covid <-relevel(IR.ses$covid, "Pre-COVID")


#### INCIDENCE RATES-----------------
## IR  by study period stratified by age, sex and SES

overall <-IR.overall%>% group_by(covid, outcome) %>% summarise( events_t = sum(events),pmar = sum(months),)
gender <-IR.age_gender %>% group_by(covid, outcome,gender, age_gr2) %>% summarise( events_t = sum(events),pmar = sum(months),)
ses <- IR.ses %>% group_by(covid, outcome,medea) %>% summarise( events_t = sum(events),pmar = sum(months),) %>%drop_na(medea)

ir <- rbind(overall, gender,ses)%>% arrange(covid, outcome)%>% relocate(gender, .after = outcome)%>%
  relocate(age_gr2, .after = gender)%>%
  relocate(medea, .after = age_gr2)
ir1 <-as.matrix(ir[,6:7])
ci <- round(epi.conf(ir1, ctype = "inc.rate", method = "exact", N = 100000, design = 1, 
               conf.level = 0.95) * 100000,1)

ir_ci <- cbind(ir, ci)
ir_ci <- ir_ci %>% 
  mutate(ir = paste0(paste(est),"(", paste(lower), " to ", paste(upper), ")"))%>%
  dplyr::select(covid, outcome, gender, age_gr2, medea, events_t, pmar, ir)%>%
  arrange(covid, outcome)

write.csv(ir_ci, "Summary of observed data/incidence_rates_rev.csv")
rm(ci,gender, ir, ir_ci, ir1, overall,ses)



overall.post <-IR.overall%>% 
  filter(months.since.start >=29)%>%
  group_by(outcome) %>% summarise( events_t = sum(events),pmar = sum(months),)
age.gender.post <-IR.age_gender%>% 
  filter(months.since.start >=29)%>%
  group_by(outcome,age_gr2,gender) %>% summarise( events_t = sum(events),pmar = sum(months),)
ses.post <-IR.ses%>% 
  filter(months.since.start >=29)%>%
  group_by(outcome,medea) %>% summarise( events_t = sum(events),pmar = sum(months),)%>%drop_na(medea)

ir_post <- bind_rows(overall.post, age.gender.post,ses.post)%>% arrange( outcome)%>% relocate(gender, .after = outcome)%>%
  relocate(age_gr2, .after = gender)%>%
  relocate(medea, .after = age_gr2)
ir1 <-as.matrix(ir_post[,5:6])
ci <- round(epi.conf(ir1, ctype = "inc.rate", method = "exact", N = 100000, design = 1, 
               conf.level = 0.95) * 100000,1)

ir_ci <- cbind(ir_post, ci)
ir.post_ci <- ir_ci %>% 
  mutate(ir = paste0(paste(est)," (", paste(lower), " to ", paste(upper), ")"))%>%
  mutate(covid="Post-lockdown")%>%
  dplyr::select(covid,outcome, gender, age_gr2, medea, events_t, pmar, ir)%>%
  arrange(covid, outcome)

write.csv(ir.post_ci, "Summary of observed data/incidence_rates.post_rev.csv")




#### INCIDENCE RATE RATIOS: OVERALL-----------------
IR <- IR.overall

#Seleccionem periodes d'interès
#03-2020 al 03-2021 pandemia
#03-2018 al 03-2019 referencia
#per establir relació posem mateixos periodes covid independentment de si son ref o no
IR <- IR %>% filter(months.since.start<=13|months.since.start>=25)
IR$covid[which((IR$months.since.start >= 1)& (IR$months.since.start <= 4))] <-"Lockdown"
IR$covid[which((IR$months.since.start >= 5)& (IR$months.since.start <= 7))] <-"Post-lockdown1"
IR$covid[which((IR$months.since.start >=8) & (IR$months.since.start <= 10))] <-"Post-lockdown2"
IR$covid[which((IR$months.since.start >= 11) & (IR$months.since.start <= 13))] <-"Post-lockdown3"

IRR <- list()
IRR_Ref <-list()
periods<- IR%>% dplyr::select("covid")%>%distinct()%>%pull()
year <- IR%>% dplyr::select("year")%>% distinct()%>%pull()
outcome <-IR%>% dplyr::select("outcome")%>% distinct()%>%pull()
n <-0 #numero estratificacions

for (y in 1:length(outcome)){
  working.outcome <- outcome[y]
  for(z in 1:length(periods)){ 
    working.period <- periods[z]
    working.data <- IR %>% 
      filter(outcome==working.outcome)%>%
      filter((covid==working.period)) %>%
      mutate(ref=if_else(months.since.start < 25,0,1))%>% #zero indica ref
      group_by(ref)%>% #no funcionara per ultim trimestre
      summarise( events_t = sum(events),pmar = sum(months))%>%
      mutate(period = paste(working.period))%>%
      mutate(outcome= paste(working.outcome))
    
    events <- c(working.data%>%dplyr::select(events_t)%>%pull())
    pt <- c(working.data%>%dplyr::select(pmar)%>%pull())
    vector <- c(events, pt)
    rateratios <-rateratio(vector)
    
    IRR[[paste0(working.period, working.outcome)]]<- working.data %>%
      filter(period == working.period)%>%
      # filter(outcome == working.outcome)%>%
      filter(ref==1)%>%
      mutate(IR = events_t/pmar * 100000) %>%
      mutate(IRR=round(rateratios$measure[2],2)) %>%
      mutate(IRR_low =round(rateratios$measure[2,2],2)) %>%
      mutate(IRR_upp =round(rateratios$measure[2,3],2))
    
  }
}

IRR.overall <- bind_rows(IRR)
IRR.overall <- IRR.overall %>% mutate(IRR = paste0(paste(IRR)," (", paste(IRR_low), " to ", paste(IRR_upp), ")")) %>%
  dplyr::select(ref:IRR)

#### INCIDENCE RATE RATIOS: AGE & GENDER-----------------
IR <- IR.age_gender
#Seleccionem periodes d'interès
#03-2020 al 03-2021 pandemia
#03-2018 al 03-2019 referencia
#per establir relació posem mateixos periodes covid independentment de si son ref o no
IR <- IR %>% filter(months.since.start<=13|months.since.start>=25)
IR$covid[which((IR$months.since.start >= 1)& (IR$months.since.start <= 4))] <-"Lockdown"
IR$covid[which((IR$months.since.start >= 5)& (IR$months.since.start <= 7))] <-"Post-lockdown1"
IR$covid[which((IR$months.since.start >=8) & (IR$months.since.start <= 10))] <-"Post-lockdown2"
IR$covid[which((IR$months.since.start >= 11) & (IR$months.since.start <= 13))] <-"Post-lockdown3"

IRR.age_gender <- list()
periods<- IR%>% dplyr::select("covid")%>% distinct()%>%pull()
year <- IR%>% dplyr::select("year")%>% distinct()%>%pull()
outcome <-IR%>% dplyr::select("outcome")%>% distinct()%>%pull()
strata.age <- as.character(IR%>% dplyr::select("age_gr2") %>% drop_na()%>%distinct()%>%pull() )
strata.gender <- as.character(IR%>% ungroup()%>%dplyr::select("gender") %>% drop_na()%>%distinct()%>%pull() )
strata <- c(strata.age,strata.gender)
comb <- expand.grid(strata.age, strata.gender)

n <-0#numero estratificacions NO CAL EN AQUEST CAS

for (y in 1:length(outcome)){
  working.outcome <- outcome[y]
  for(z in 1:length(periods)){ 
    working.period <- periods[z]
    for(i in 1:NROW(comb)){
      
      working.strata <- paste(comb$Var1, comb$Var2)[i]
      working.age <- comb$Var1[i]
      working.gender<- comb$Var2[i]
      
      
      working.data <- IR %>% 
        filter(outcome==working.outcome)%>%
        filter((covid==working.period)) %>%
        filter((age_gr2==working.age)) %>%
        filter((gender==working.gender)) %>%
        mutate(ref=if_else(months.since.start < 25,0,1))%>% #zero indica ref
        group_by(ref)%>% #no funcionara per ultim trimestre
        summarise( events_t = sum(events),pmar = sum(months))%>%
        mutate(period = paste(working.period))%>%
        mutate(outcome= paste(working.outcome))%>%
        mutate(strata = paste(working.strata))
      
      events <- c(working.data%>%dplyr::select(events_t)%>%pull())
      pt <- c(working.data%>%dplyr::select(pmar)%>%pull())
      vector <- c(events, pt)
      rateratios <-rateratio(vector)
      
      
      IRR.age_gender[[paste0(working.period, working.outcome, working.strata)]]<- working.data %>%
        filter(period == working.period)%>%
        # filter(outcome == working.outcome)%>%
        filter(ref==1)%>%
        mutate(IR = events_t/pmar * 100000) %>%
        mutate(IRR=round(rateratios$measure[2],2)) %>%
        mutate(IRR_low =round(rateratios$measure[2,2],2)) %>%
        mutate(IRR_upp =round(rateratios$measure[2,3],2))
    }
  }
}

IRR.age_gender <- bind_rows(IRR.age_gender)
IRR.age_gender <- IRR.age_gender %>% mutate(IRR = paste0(paste(IRR)," (", paste(IRR_low), " to ", paste(IRR_upp), ")")) %>%
  dplyr::select(ref:IRR)

#### INCIDENCE RATE RATIOS: GENDER----------------
IR <- IR.gender
#Seleccionem periodes d'interès
#03-2020 al 03-2021 pandemia
#03-2018 al 03-2019 referencia
#per establir relació posem mateixos periodes covid independentment de si son ref o no
IR <- IR %>% filter(months.since.start<=13|months.since.start>=25)
IR$covid[which((IR$months.since.start >= 1)& (IR$months.since.start <= 4))] <-"Lockdown"
IR$covid[which((IR$months.since.start >= 5)& (IR$months.since.start <= 7))] <-"Post-lockdown1"
IR$covid[which((IR$months.since.start >=8) & (IR$months.since.start <= 10))] <-"Post-lockdown2"
IR$covid[which((IR$months.since.start >= 11) & (IR$months.since.start <= 13))] <-"Post-lockdown3"

IRR.gender <- list()
IRR_Ref <-list()
periods<- IR%>% dplyr::select("covid")%>% distinct()%>%pull()
year <- IR%>% dplyr::select("year")%>% distinct()%>%pull()
outcome <-IR%>% dplyr::select("outcome")%>% distinct()%>%pull()
strata <- as.character(IR%>% dplyr::select("gender") %>% drop_na()%>%distinct()%>%pull() )

n <-0 #numero estratificacions

for (y in 1:length(outcome)){
  working.outcome <- outcome[y]
  for(z in 1:length(periods)){ 
    working.period <- periods[z]
    for(i in 1:length(strata)){
      working.strata <- strata[i] 
      
      working.data <- IR %>% 
        filter(outcome==working.outcome)%>%
        filter((covid==working.period)) %>%
        filter((gender==working.strata)) %>%
        mutate(ref=if_else(months.since.start < 25,0,1))%>% #zero indica ref
        group_by(ref)%>% #no funcionara per ultim trimestre
        summarise( events_t = sum(events),pmar = sum(months))%>%
        mutate(period = paste(working.period))%>%
        mutate(outcome= paste(working.outcome))%>%
        mutate(gender = paste(working.strata))
      
      events <- c(working.data%>%dplyr::select(events_t)%>%pull())
      pt <- c(working.data%>%dplyr::select(pmar)%>%pull())
      vector <- c(events, pt)
      rateratios <-rateratio(vector)
      
      IRR.gender[[paste0(working.period, working.outcome, working.strata)]]<- working.data %>%
        filter(period == working.period)%>%
        # filter(outcome == working.outcome)%>%
        filter(ref==1)%>%
        mutate(IR = events_t/pmar * 100000) %>%
        mutate(IRR=round(rateratios$measure[2],2)) %>%
        mutate(IRR_low =round(rateratios$measure[2,2],2)) %>%
        mutate(IRR_upp =round(rateratios$measure[2,3],2))
    }
  }
}

IRR.gender <- bind_rows(IRR.gender)
IRR.gender <- IRR.gender %>% mutate(IRR = paste0(paste(IRR)," (", paste(IRR_low), " to ", paste(IRR_upp), ")")) %>%
  dplyr::select(ref:IRR)



#### INCIDENCE RATE RATIOS: SES-----------------
IR.ses<- IR.ses[which(IR.ses$medea!="Missing"),]
IR <- IR.ses
#Seleccionem periodes d'interès
#03-2020 al 03-2021 pandemia
#03-2018 al 03-2019 referencia
#per establir relació posem mateixos periodes covid independentment de si son ref o no
IR <- IR %>% filter(months.since.start<=13|months.since.start>=25)
IR$covid[which((IR$months.since.start >= 1)& (IR$months.since.start <= 4))] <-"Lockdown"
IR$covid[which((IR$months.since.start >= 5)& (IR$months.since.start <= 7))] <-"Post-lockdown1"
IR$covid[which((IR$months.since.start >=8) & (IR$months.since.start <= 10))] <-"Post-lockdown2"
IR$covid[which((IR$months.since.start >= 11) & (IR$months.since.start <= 13))] <-"Post-lockdown3"

IRR.ses <- list()
IRR_Ref <-list()
periods<- IR%>% dplyr::select("covid")%>% distinct()%>%pull()
year <- IR%>% dplyr::select("year")%>% distinct()%>%pull()
outcome <-IR%>% dplyr::select("outcome")%>% distinct()%>%pull()
strata <- as.character(IR%>% dplyr::select("medea") %>% drop_na()%>%distinct()%>%pull() )

n <-0 #numero estratificacions

for (y in 1:length(outcome)){
  working.outcome <- outcome[y]
  for(z in 1:length(periods)){ 
    working.period <- periods[z]
    for(i in 1:length(strata)){
      working.strata <- strata[i] 
      
      working.data <- IR %>% 
        filter(outcome==working.outcome)%>%
        filter((covid==working.period)) %>%
        filter((medea==working.strata)) %>%
        mutate(ref=if_else(months.since.start < 25,0,1))%>% #zero indica ref
        group_by(ref)%>% #no funcionara per ultim trimestre
        summarise( events_t = sum(events),pmar = sum(months))%>%
        mutate(period = paste(working.period))%>%
        mutate(outcome= paste(working.outcome))%>%
        mutate(medea = paste(working.strata))
      
        events <- c(working.data%>%dplyr::select(events_t)%>%pull())
        vector <- c(events, pt)
        rateratios <-rateratio(vector)
        
     
      IRR.ses[[paste0(working.period, working.outcome, working.strata)]]<- working.data %>%
        filter(period == working.period)%>%
        # filter(outcome == working.outcome)%>%
        filter(ref==1)%>%
        mutate(IR = events_t/pmar * 100000) %>%
        mutate(IRR=round(rateratios$measure[2],2)) %>%
        mutate(IRR_low =round(rateratios$measure[2,2],2)) %>%
        mutate(IRR_upp =round(rateratios$measure[2,3],2))
    }
  }
}

IRR.ses <- bind_rows(IRR.ses)
IRR.ses <- IRR.ses %>% mutate(IRR = paste0(paste(IRR)," (", paste(IRR_low), " to ", paste(IRR_upp), ")")) %>%
  dplyr::select(ref:IRR)



#### Save IRR
write.csv(IRR.overall, "Summary of observed data/irr_overall_rev.csv")
write.csv(IRR.gender, "Summary of observed data/irr_gender_rev.csv")
write.csv(IRR.age_gender, "Summary of observed data/irr_age.gender_rev.csv")
write.csv(IRR.ses, "Summary of observed data/irr_ses_rev.csv")
