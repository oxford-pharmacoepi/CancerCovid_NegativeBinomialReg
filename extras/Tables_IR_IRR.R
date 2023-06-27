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
library(flextable)

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


#### INCIDENCE RATES TABLES FOR PAPER --------------------------------------- ##
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


# add combined periods post-lockdown
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


# JOIN ALL PERIODS WITH POST-COVID

ir_ci_pre_post <- rbind(ir_ci, ir.post_ci)

write.csv(ir_ci_pre_post, file=here("3_DataSummary", "Summary of observed data_incidence_rates_pre_post_lockdown.csv"))
save(ir_ci_pre_post, file=here("3_DataSummary", "Summary of observed data_incidence_rates_pre_post_lockdown.RData"))

# Change table structure to remove events and person months, and pivot the covid categories
ir_ci_pre_post_pivot <- ir_ci_pre_post %>% dplyr::select(c(-events_t, -person_months_at_risk)) %>% tidyr::pivot_wider(names_from = covid, values_from = ir) 


ir_ci_pre_post_pivot <- ir_ci_pre_post_pivot[, c(1, 2, 3, 8, 6, 11, 7, 9, 10, 4, 5)]
ir_ci_pre_post_pivot <- ir_ci_pre_post_pivot %>% rename(Sex = denominator_sex, "Age Group" = denominator_age_group,
                                                        "Pre-COVID (Jan 2017-Feb 2020)" = "Pre-COVID", 
                                                        "Lockdown (March 2020-June 2020)" = "Lockdown",
                                                        "Post-lockdown (July 2020-Dec 2021)" = "Post-lockdown", 
                                                        "Post-lockdown 1 (July 2020-Dec 2021)" = "Post-lockdown1",
                                                        "Second lockdown (Nov 2020-Dec 2020)" = "Second lockdown", 
                                                        "Third lockdown (Jan 2021-Feb 2021)" = "Third lockdown",
                                                        "Easing of restrictions (March 2021-June 2021" = "Easing of restrictions", 
                                                        "Legal restrictions removed (July 2021-Dec 2021)"= "Legal restrictions removed")


Pretty_observed_IR_results_table <- flextable(ir_ci_pre_post_pivot) %>% theme_vanilla() %>% 
  set_caption(caption = "Incidence rates of cancer diagnoses in each of the time periods, stratified by overall, age and sex") %>% 
  width(width = 1.4) 

save(ir_ci_pre_post_pivot, file=here("3_DataSummary", "Summary of observed data_incidence_rates_ir_ci_pre_post_pivot.RData"))
write.csv(ir_ci_pre_post_pivot, file=here("3_DataSummary", "Summary of observed data_incidence_rates_ir_ci_pre_post_pivot.csv"))

save_as_docx('Pretty_observed_IR_results_table' = Pretty_observed_IR_results_table, path=here("3_DataSummary", "Summary of observed incidence rates.docx"))



# Save a version of the observed IR table for each cancer separately

# Breast

Breast_observed_IR_results_table <- ir_ci_pre_post_pivot %>% filter(outcome=="Breast")
Pretty_breast_observed_IR_results_table <- flextable(Breast_observed_IR_results_table) %>% theme_vanilla() %>% 
  set_caption(caption = "Incidence rates of breast cancer diagnoses in each of the time periods, stratified by overall, age and sex") %>% 
  width(width = 1.4) 

save(Breast_observed_IR_results_table, file=here("3_DataSummary", "Summary of observed breast_incidence_rates.RData"))
write.csv(Breast_observed_IR_results_table, file=here("3_DataSummary", "Summary of observed breast_incidence_rates.csv"))

save_as_docx('Pretty_breast_observed_IR_results_table' = Pretty_breast_observed_IR_results_table, path=here("3_DataSummary", "Summary of observed incidence rates-breast.docx"))


# Colorectal

Colorectal_observed_IR_results_table <- ir_ci_pre_post_pivot %>% filter(outcome=="Colorectal")
Pretty_colorectal_observed_IR_results_table <- flextable(Colorectal_observed_IR_results_table) %>% theme_vanilla() %>% 
  set_caption(caption = "Incidence rates of colorectal cancer diagnoses in each of the time periods, stratified by overall, age and sex") %>% 
  width(width = 1.4) 

save(Colorectal_observed_IR_results_table, file=here("3_DataSummary", "Summary of observed colorectal_incidence_rates.RData"))
write.csv(Colorectal_observed_IR_results_table, file=here("3_DataSummary", "Summary of observed colorectal_incidence_rates.csv"))

save_as_docx('Pretty_colorectal_observed_IR_results_table' = Pretty_colorectal_observed_IR_results_table, path=here("3_DataSummary", "Summary of observed incidence rates-colorectal.docx"))



# Lung

Lung_observed_IR_results_table <- ir_ci_pre_post_pivot %>% filter(outcome=="Lung")
Pretty_lung_observed_IR_results_table <- flextable(Lung_observed_IR_results_table) %>% theme_vanilla() %>% 
  set_caption(caption = "Incidence rates of lung cancer diagnoses in each of the time periods, stratified by overall, age and sex") %>% 
  width(width = 1.4) 

save(Lung_observed_IR_results_table, file=here("3_DataSummary", "Summary of observed lung_incidence_rates.RData"))
write.csv(Lung_observed_IR_results_table, file=here("3_DataSummary", "Summary of observed lung_incidence_rates.csv"))

save_as_docx('Pretty_lung_observed_IR_results_table' = Pretty_lung_observed_IR_results_table, path=here("3_DataSummary", "Summary of observed incidence rates-lung.docx"))




# Prostate

Prostate_observed_IR_results_table <- ir_ci_pre_post_pivot %>% filter(outcome=="Prostate")
Pretty_prostate_observed_IR_results_table <- flextable(Prostate_observed_IR_results_table) %>% theme_vanilla() %>% 
  set_caption(caption = "Incidence rates of prostate cancer diagnoses in each of the time periods, stratified by overall, age and sex") %>% 
  width(width = 1.4) 

save(Prostate_observed_IR_results_table, file=here("3_DataSummary", "Summary of observed prostate_incidence_rates.RData"))
write.csv(Prostate_observed_IR_results_table, file=here("3_DataSummary", "Summary of observed prostate_incidence_rates.csv"))

save_as_docx('Pretty_prostate_observed_IR_results_table' = Pretty_prostate_observed_IR_results_table, path=here("3_DataSummary", "Summary of observed incidence rates-prostate.docx"))


#### INCIDENCE RATE RATIOS: OVERALL----------------- THE IRR CALCULATE THE RELATIVE CHANGE IN THE INCIDENCE COMPARED TO A COMPARATOR GROUP
# HERE WE COMPARE THE INCIDENCE RATE IN EACH OF THE STUDY PERIODS COMPARED TO THE TIME PERIOD BEFORE LOCKDOWN

IR <- IR.overall

IRR <- list()
IRR_Ref <-list()
periods<- IR%>% dplyr::select("covid")%>%distinct()%>%pull()
year <- IR%>% dplyr::select("year")%>% distinct()%>%pull()
outcome <-IR%>% dplyr::select("outcome")%>% distinct()%>%pull()
#n <-0 # number of stratifications

events <- 1
pt<- 2

test <- c(events, pt)

for (y in 1:length(outcome)){
  working.outcome <- outcome[y]
  for(z in 1:length(periods)){ 
    working.period <- periods[z]
    working.data <- IR %>% 
      filter(outcome==working.outcome)%>%
      filter(covid==working.period) %>%
      mutate(ref=if_else(months.since.start < 27,0,1))%>% # 26 indicates reference OF PRE-COVID
      group_by(ref)%>% #no function for final time period
      summarise( events_t = sum(events),person_months_at_risk = sum(months))%>%
      mutate(period = paste(working.period))%>%
      mutate(outcome= paste(working.outcome))
    
    events <- c(working.data%>%dplyr::select(events_t)%>%pull())
    pt <- c(working.data%>%dplyr::select(person_months_at_risk)%>%pull())

    vector <- c(events, pt)
    
    rateratios <-rateratio(vector, y=NULL) # this bit throws an error of nrow(x) object x not found which is why i haven't run this
    }
  }
    
    IRR[[paste0(working.period, working.outcome)]]<- working.data %>%
      filter(period == working.period)%>%
     filter(outcome == working.outcome)%>%
      filter(ref==1)
      mutate(IR = events_t/person_months_at_risk * 100000) #%>%
     mutate(IRR=round(rateratios$measure[2],2)) %>%
      mutate(IRR_low =round(rateratios$measure[2,2],2)) %>%
      mutate(IRR_upp =round(rateratios$measure[2,3],2))
    


IRR.overall <- bind_rows(IRR)
IRR.overall <- IRR.overall %>% mutate(IRR = paste0(paste(IRR)," (", paste(IRR_low), " to ", paste(IRR_upp), ")")) %>%
  dplyr::select(ref:IRR)

