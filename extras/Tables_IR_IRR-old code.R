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

load(here("1_DataPrep", "Data", "GeneralPop2018_22.RData"))

# Periods-----------------
IR.overall <- inc_data_pred_final %>% mutate(Month1 =paste(1,month, year, sep ="-")) %>% filter(denominator_cohort_id ==1)
IR.sex <- inc_data_pred_final %>% mutate(Month1 =paste(1,month, year, sep ="-")) %>% filter(denominator_sex !="Both") %>% filter(denominator_age_group =="0;150")
IR.age_sex <- inc_data_pred_final %>% mutate(Month1 =paste(1,month, year, sep ="-")) %>% filter(denominator_sex !="Both")
#IR.ses <- IR.ses %>%  mutate(Month1 =paste(1,month, year, sep ="-"))

Sys.setlocale("LC_TIME", "English")
IR.overall$Date <- NA
IR.overall$Date <- dmy(IR.overall$Month1)
IR.overall$Month1 <- NULL

IR.sex$Date <- NA
IR.sex$Date <- dmy(IR.sex$Month1)
IR.sex$Month1 <- NULL

IR.age_sex$Date <- NA
IR.age_sex$Date <- dmy(IR.age_sex$Month1)
IR.age_sex$Month1 <- NULL

IR.ses$Date <- NA
IR.ses$Date <- dmy(IR.ses$Month1)
IR.ses$Month1 <- NULL


IR.overall$covid <- as.factor(IR.overall$covid)
IR.overall$covid <-relevel(IR.overall$covid, "Pre-COVID")

IR.sex$covid <- as.factor(IR.sex$covid)
IR.sex$covid <-relevel(IR.sex$covid, "Pre-COVID")

IR.age_sex$covid <- as.factor(IR.age_sex$covid)
IR.age_sex$covid <-relevel(IR.age_sex$covid, "Pre-COVID")

IR.ses$covid <- as.factor(IR.ses$covid)
IR.ses$covid <-relevel(IR.ses$covid, "Pre-COVID")


#### INCIDENCE RATES-----------------
## IR  by study period stratified by age, sex and SES

overall <-IR.overall%>% group_by(covid, outcome) %>% summarise( events_t = sum(events),person_months_at_risk = sum(months),)
sex <-IR.age_sex %>% group_by(covid, outcome, denominator_sex, denominator_age_group) %>% summarise( events_t = sum(events),person_months_at_risk = sum(months),)
#ses <- IR.ses %>% group_by(covid, outcome,medea) %>% summarise( events_t = sum(events),person_months_at_risk = sum(months),) %>%drop_na(medea)

ir <- rbind(overall, sex)%>% arrange(covid, outcome)%>% relocate(denominator_sex, .after = outcome)%>%
  relocate(denominator_age_group, .after = denominator_sex)
  
ir1 <-as.matrix(ir[,5:6])
ci <- round(epi.conf(ir1, ctype = "inc.rate", method = "exact", N = 100000, design = 1, 
               conf.level = 0.95) * 100000,1)

ir_ci <- cbind(ir, ci)
ir_ci <- ir_ci %>% 
  mutate(ir = paste0(paste(est),"(", paste(lower), " to ", paste(upper), ")"))%>%
  dplyr::select(covid, outcome, denominator_sex, denominator_age_group, events_t, person_months_at_risk, ir)%>%
  arrange(covid, outcome)


write.csv(ir_ci, file=here("3_DataSummary", "Summary of observed data_incidence_rates.csv"))
save(ir_ci, file=here("3_DataSummary", "Summary of observed data_incidence_rates.RData"))

rm(ci,sex, ir, ir_ci, ir1, overall)



overall.post <-IR.overall%>% 
  filter(months.since.start >=31)%>%
  group_by(outcome) %>% summarise( events_t = sum(events),person_months_at_risk = sum(months),)
age.sex.post <-IR.age_sex%>% 
  filter(months.since.start >=31)%>%
  group_by(outcome,denominator_age_group,denominator_sex) %>% summarise( events_t = sum(events),person_months_at_risk = sum(months),)
#ses.post <-IR.ses%>% 
#  filter(months.since.start >=31)%>%
#  group_by(outcome,medea) %>% summarise( events_t = sum(events),person_months_at_risk = sum(months),)%>%drop_na(medea)

ir_post <- bind_rows(overall.post, age.sex.post)%>% arrange( outcome)%>% relocate(denominator_sex, .after = outcome)%>%
  relocate(denominator_age_group, .after = denominator_sex) #%>%
 # relocate(medea, .after = age_gr2)
ir1 <-as.matrix(ir_post[,4:5])
ci <- round(epi.conf(ir1, ctype = "inc.rate", method = "exact", N = 100000, design = 1, 
               conf.level = 0.95) * 100000,1)

ir_ci <- cbind(ir_post, ci)
ir.post_ci <- ir_ci %>% 
  mutate(ir = paste0(paste(est)," (", paste(lower), " to ", paste(upper), ")"))%>%
  mutate(covid="Post-lockdown")%>%
  dplyr::select(covid,outcome, denominator_sex, denominator_age_group, events_t, person_months_at_risk, ir)%>%
  arrange(covid, outcome)



write.csv(ir.post_ci, file=here("3_DataSummary", "Summary of observed data_incidence_rates_post_lockdown.csv"))
save(ir.post_ci, file=here("3_DataSummary", "Summary of observed data_incidence_rates_post_lockdown.RData"))

# upto here



#### INCIDENCE RATE RATIOS: OVERALL----------------- THE IRR CALCULATE THE RELATIVE CHANGE IN THE INCIDENCE COMPARED TO A COMPARATOR GROUP
# HERE WE COMPARE THE INCIDENCE RATE IN EACH OF THE STUDY PERIODS COMPARED TO THE TIME PERIOD BEFORE LOCKDOWN
# what i can do is calculate this from the new incidence over 3 time period calculations.

IR <- IR.overall

# Select periods of interest

IR <- IR %>% filter(months.since.start<=43|months.since.start>=48)
IR$covid[which((IR$months.since.start >= 27)& (IR$months.since.start <= 30))] <-"Lockdown"
IR$covid[which((IR$months.since.start >= 31)& (IR$months.since.start <= 34))] <-"Post-lockdown1"
IR$covid[which((IR$months.since.start >=35) & (IR$months.since.start <= 36))] <-"Second lockdown"
IR$covid[which((IR$months.since.start >= 37) & (IR$months.since.start <= 38))] <-"Third lockdown"
IR$covid[which((IR$months.since.start >= 39) & (IR$months.since.start <= 42))] <-"Easing of restrictions"
IR$covid[which((IR$months.since.start >= 43))] <-"Legal restrictions removed"

IRR <- list()
IRR_Ref <-list()
periods<- IR%>% dplyr::select("covid")%>%distinct()%>%pull()
year <- IR%>% dplyr::select("year")%>% distinct()%>%pull()
outcome <-IR%>% dplyr::select("outcome")%>% distinct()%>%pull()
n <-0 # number of stratifications

for (y in 1:length(outcome)){
  working.outcome <- outcome[y]
  for(z in 1:length(periods)){ 
    working.period <- periods[z]
    working.data <- IR %>% 
      filter(outcome==working.outcome)%>%
      filter((covid==working.period)) %>%
      mutate(ref=if_else(months.since.start < 48,26,27))%>% # 26 indicates reference OF PRE-COVID
      group_by(ref)%>% #no function for final time period
      summarise( events_t = sum(events),person_months_at_risk = sum(months))%>%
      mutate(period = paste(working.period))%>%
      mutate(outcome= paste(working.outcome))
    
    events <- c(working.data%>%dplyr::select(events_t)%>%pull())
    pt <- c(working.data%>%dplyr::select(person_months_at_risk)%>%pull())
    vector <- c(events, pt)
    rateratios <-rateratio(vector)
    
    IRR[[paste0(working.period, working.outcome)]]<- working.data %>%
      filter(period == working.period)%>%
      # filter(outcome == working.outcome)%>%
      filter(ref==0)%>%
      mutate(IR = events_t/person_months_at_risk * 100000) %>%
      mutate(IRR=round(rateratios$measure[2],2)) %>%
      mutate(IRR_low =round(rateratios$measure[2,2],2)) %>%
      mutate(IRR_upp =round(rateratios$measure[2,3],2))
    
  }
}

IRR.overall <- bind_rows(IRR)
IRR.overall <- IRR.overall %>% mutate(IRR = paste0(paste(IRR)," (", paste(IRR_low), " to ", paste(IRR_upp), ")")) %>%
  dplyr::select(ref:IRR)

#### INCIDENCE RATE RATIOS: AGE & GENDER-----------------
IR <- IR.age_sex
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
IR <- IR.sex
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




###### MARTA'S EXTRA CODE
if (is.na(rateratios[[i]])){
  extract1 =  "NA"
  
} else {
  temp_data = rateratios[[i]]$measure
  extract1 =get_IR_df_function(temp_data, list_names[i])
}

list_names <- c(outer(names_cohort_id,outcome,paste,sep="_"))
for (i in 1:length(list_names)){
  
  temp_data = rateratios[[i]]
  extraction_list[[i]] <- temp_data
  
}

# EXTRACT THE CANCER LISTS#

extract <- list()
list_names <- names(rateratios)
for (i in 1:length(list_names)){
  
  temp_data = rateratios[[i]][[2]]
  extract[[i]] <- temp_data
  
}

# or 


