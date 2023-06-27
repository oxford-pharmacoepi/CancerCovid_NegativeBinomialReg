# ============================================================================ #
#     Create tables with incidence incidence rate ratios compared to the       #
#                             pre-covid reference                              #
#                            for cancer diagnoses                              #
#                                                                              #
#                              Nicola Barclay                                  #
#                                22-05-2023                                    #
# ============================================================================ #

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
library(data.table)



# ============ CALCULATE IRR FOR EACH CANCER OVER PERIODS ==================== #

# ======================== AGE SEX STRATIFICATION = ========================== #  


load("~/GitHub/CancerCovid_NegativeBinomialReg/3_DataSummary/inc_data_final.RData")

# This code calculates the IRR for each of the cancers over a loop
# loops over each period of interest
# and loops over age and sex categories

# the reason why there is no data for denominator cohort id 5,6,8 is that the results were obscured for all these categories
IR.overall <- inc_data_final %>% filter(  denominator_cohort_id == 9 |denominator_cohort_id == 11|denominator_cohort_id == 12|
                                          denominator_cohort_id == 14|denominator_cohort_id == 15|denominator_cohort_id == 17|
                                          denominator_cohort_id == 18)

# CREATE A NEW COLUMN OF Age and Sex groups
IR.overall <- IR.overall %>% mutate(Age_sex = case_when(grepl("9", denominator_cohort_id) ~ "Female; 20-39",
                                                                                    grepl("11", denominator_cohort_id) ~ "Male; 40-59",
                                                                                    grepl("12", denominator_cohort_id) ~ "Female; 40-59",
                                                                                    grepl("14", denominator_cohort_id) ~ "Male; 60-79",
                                                                                    grepl("15", denominator_cohort_id) ~ "Female; 60-79",
                                                                                    grepl("17", denominator_cohort_id) ~ "Male; 80-150",
                                                                                    grepl("18", denominator_cohort_id) ~ "Female; 80-150"))



names_cohort_id = names(table(IR.overall$Age_sex))
number_cohort_id = length(names_cohort_id)

IR <- IR.overall

periods<- IR%>% dplyr::select("covid")%>%distinct()%>%pull()
outcome <-IR%>% dplyr::select("outcome")%>% distinct()%>%pull()
rateratios <- list()

for (id in names_cohort_id){
  for (y in 1:length(outcome)){
    working.outcome <- outcome[y]
    vector <- NULL # a vector to place the values from the loop
    for(z in 1:length(periods)){
      working.period <- periods[z]
      working.data <- IR %>%
        filter(Age_sex==id)%>%
        filter(outcome==working.outcome)%>%
        filter(covid==working.period) %>%
        mutate(ref=if_else(months.since.start < 39,0,1))%>% # 38 indicates reference OF PRE-COVID
        group_by(ref)%>% 
        summarise( events_t = sum(events),person_months_at_risk = sum(months))%>%
        mutate(periods = paste(working.period))%>%
        mutate(outcome= paste(working.outcome))
      
      events <- c(working.data%>%dplyr::select(events_t)%>%pull())
      pt <- c(working.data%>%dplyr::select(person_months_at_risk)%>%pull())
      
      vector <- vector %>%
        union_all(
          tibble(
            events = events, person_time = pt, period = periods[z]
          )
        )
      
    }
    count <- paste(id, outcome[y], sep = ";")
    if (dim(vector)[1] > 1) {
      rateratios[[count]] <- rateratio(as.matrix(vector[,1:2], y=NULL))$measure %>% bind_cols(vector[,3])
    }
            
  }
}

rateratios_table <- bind_rows(rateratios, .id = "age_sex")

save(rateratios, file=here::here("3_DataSummary", "rateratios_to_check.RData"))
write.csv(rateratios_table, file=here::here("3_DataSummary", "IRR_table_cancer_age_sex_looped_to_check.csv"))

# LAYOUT THE TABLE HOW YOU WANT IT

rate_ratios_table_formatted <- rateratios_table %>%  mutate_if(is.numeric, round, digits=2)

# combine cis with the estimate
rate_ratios_table_formatted <- rate_ratios_table_formatted %>% mutate(estimate = paste0(paste(estimate)," (", paste(lower), " to ", paste(upper), ")")) 
  
# CREATE A NEW COLUMN OF CANCER TYPES
rate_ratios_table_formatted <- rate_ratios_table_formatted %>% mutate(Cancer = case_when(grepl("Colorectal", age_sex) ~ "Colorectal",
                                                                                   grepl("Lung", age_sex) ~ "Lung",
                                                                                   grepl("Prostate", age_sex) ~ "Prostate",
                                                                                   grepl("Breast", age_sex) ~ "Breast"))

# CREATE A NEW COLUMN OF Age groups
rate_ratios_table_formatted <- rate_ratios_table_formatted %>% mutate("Age Group" = case_when(grepl("20-39", age_sex) ~ "20-39",
                                                                                grepl("40-59", age_sex) ~ "40-59",
                                                                                grepl("60-79", age_sex) ~ "60-79",
                                                                                grepl("80-150", age_sex) ~ "80-150"))


# CREATE A NEW COLUMN OF Sex groups
rate_ratios_table_formatted <- rate_ratios_table_formatted %>% mutate(Sex = case_when(grepl("Fe", age_sex) ~ "Female",
                                                                                      grepl("Male", age_sex) ~ "Male"))

  # remove superfluous columns of cis
rate_ratios_table_formatted <- rate_ratios_table_formatted[-c(3,4)]

# Re-order columns 
rate_ratios_table_formatted <- rate_ratios_table_formatted[c(4,6,5,2,3)]

# pivot
rate_ratios_table_formatted <- rate_ratios_table_formatted %>% pivot_wider(names_from = period, values_from = estimate)



#### Save IRR
write.csv(rate_ratios_table_formatted, file=here::here("3_DataSummary", "IRR_table_cancer_age_sex_looped_formatted.csv"))
save(rate_ratios_table_formatted, file=here::here("3_DataSummary", "IRR_table_cancer_age_sex_looped_formatted.RData"))

#### Make pretty table for breast cancer
Pretty_IRR_table_breast_cancer_age_sex <- rate_ratios_table_formatted %>% filter(Cancer == "Breast")

Pretty_IRR_table_breast_cancer_age_sex <- flextable(Pretty_IRR_table_breast_cancer_age_sex) %>% theme_vanilla() %>% 
  set_caption(caption = "Incidence rate ratios of breast cancer over the lockdown periods compared to pre-COVID period, stratified by age and sex") %>% 
  width(width = 1.4) 

save_as_docx('Pretty_IRR_table_breast_age_sex' = Pretty_IRR_table_breast_cancer_age_sex, path=here("3_DataSummary", "Pretty_IRR_table_breast_cancer_age_sex.docx"))



#### Make pretty table for colorectal cancer
Pretty_IRR_table_colorectal_cancer_age_sex <- rate_ratios_table_formatted %>% filter(Cancer == "Colorectal")

Pretty_IRR_table_colorectal_cancer_age_sex <- flextable(Pretty_IRR_table_colorectal_cancer_age_sex) %>% theme_vanilla() %>% 
  set_caption(caption = "Incidence rate ratios of colorectal cancer over the lockdown periods compared to pre-COVID period, stratified by age and sex") %>% 
  width(width = 1.4) 

save_as_docx('Pretty_IRR_table_colorectal_age_sex' = Pretty_IRR_table_colorectal_cancer_age_sex, path=here("3_DataSummary", "Pretty_IRR_table_colorectal_cancer_age_sex.docx"))



#### Make pretty table for lung cancer
Pretty_IRR_table_lung_cancer_age_sex <- rate_ratios_table_formatted %>% filter(Cancer == "Lung")

Pretty_IRR_table_lung_cancer_age_sex <- flextable(Pretty_IRR_table_lung_cancer_age_sex) %>% theme_vanilla() %>% 
  set_caption(caption = "Incidence rate ratios of lung cancer over the lockdown periods compared to pre-COVID period, stratified by age and sex") %>% 
  width(width = 1.4) 

save_as_docx('Pretty_IRR_table_lung_age_sex' = Pretty_IRR_table_lung_cancer_age_sex, path=here("3_DataSummary", "Pretty_IRR_table_lung_cancer_age_sex.docx"))



#### Make pretty table for prostate cancer
Pretty_IRR_table_prostate_cancer_age_sex <- rate_ratios_table_formatted %>% filter(Cancer == "Prostate")

Pretty_IRR_table_prostate_cancer_age_sex <- flextable(Pretty_IRR_table_prostate_cancer_age_sex) %>% theme_vanilla() %>% 
  set_caption(caption = "Incidence rate ratios of prostate cancer over the lockdown periods compared to pre-COVID period, stratified by age and sex") %>% 
  width(width = 1.4) 

save_as_docx('Pretty_IRR_table_prostate_age_sex' = Pretty_IRR_table_prostate_cancer_age_sex, path=here("3_DataSummary", "Pretty_IRR_table_prostate_cancer_age_sex.docx"))
