# ============================================================================ #
#     Create tables with incidence incidence rate ratios compared to the       #
#                             pre-covid reference                              #
#                            for cancer diagnoses                              #
#                                                                              #
#                              Nicola Barclay                                  #
#                                18-04-2023                                    #
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
#rateratios <- vector("list",length(outcome)*number_cohort_id); names(rateratios) = outer(names_cohort_id,outcome,paste,sep="_")


# THERE IS A PROBLEM WITH THE OUTPUT OOF THIS. THERE ARE LISTS OF FEMALES WITH PROSTATE CANCER, AND ALL THE RATIOS i'VE
# CHECKED ARE NOT ACCURATE
# CONSIDER RUNNING THESE LOOPSSEPSARATELY BY CANCER

# it is possible that it is not calculating the row 1 (pre-covid) correctly. I checked the counts in pre-covid period manually as 
# 797 for denomiator cohort id 19 for colorectal, but in the rate ratios it calcs it as 360. so there is something wrong with the 
# sum of events by the loop below. check the other periods

# it also calculates estimates for females with prostate cancer so something is definitely wrong
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


# show table of age sex groups - note that these are the cohorts that did not have results obscured in the data prep phase.
age_sex_table <- inc_data_final %>% dplyr::select(denominator_age_group, denominator_sex, denominator_cohort_id) %>% 
                group_by(denominator_age_group, denominator_sex, denominator_cohort_id) %>% tally()

# note that there are some categories that produce no rate ratios because at least one element of the row is missing 
# these ones will need running manually. These are:

# 9_Colorectal
# 11_Colorectal
# 12_Colorectal
# 9_Lung
# 14_Lung
# 11_Prostate
# 15_Prostate
# 12_Breast
# 17_Breast

# remove all the NA lists from the rateratios list - there are some categories of 
# age and sex that didn't run becuase of the presence of any empty cells across periods
# as mentioned above
rateratios <- rateratios[!is.na(rateratios)]



# EXTRACT THE CANCER LISTS APPLYING MY FUNCTION SIMULTANEOUSLY - 
# run the function first:

# FUNCTION TO EXTRACT ALL THE IRR AND CIS FROM ALL OF THE LISTS EDITED FOR STRATIFICATION 

get_IR_df_function <- function(yourrateratiosname, title){
  
  get_IR_df <- as.data.frame(yourrateratiosname[[2]])
  get_IR_df <- get_IR_df %>%  mutate_if(is.numeric, round, digits=2)
  
  # add a column to indicate the covid period
  get_IR_df <- cbind(periods, get_IR_df)
  
  # combine cis with the estimate
  get_IR_df <- get_IR_df %>% mutate(estimate = paste0(paste(estimate)," (", paste(lower), " to ", paste(upper), ")")) 
  
  # remove superfluous columns of cis
  get_IR_df <- get_IR_df[-c(3,4)]
  
  # transpose the table to have column headings as covid periods
  get_IR_df_t <- transpose(get_IR_df)
  #redefine row and column names
  colnames(get_IR_df_t) <- colnames(periods)
  names(get_IR_df_t) <- get_IR_df_t[1,]
  get_IR_df_t <- get_IR_df_t[-1,]
  rownames(get_IR_df_t) <- paste(title)
  return(get_IR_df_t)
}

# create the loop to run the function over all elements of the list - returns error 
# because the third list doesn't have data for all periods (for 17_colorectal - 0nly 6 periods) so it stops.
# Therefore, you have to remove the elements of the rate ratio list that don't have complete periods (17 colorectal)
# and calculate that one manually


# running the code again I now get row 12 as only having 6 periods, for female prostae!
rateratios_subset <- rateratios[-12]

list_names <- names(rateratios_subset)
extract_with_function <- list()

for (i in 1:length(list_names)){
  
  temp_data = rateratios_subset[[i]]
  extract_with_function[[i]] <-get_IR_df_function(temp_data, list_names[i])
  
} 


# BIND ALL THE DATA TABLES THAT ARE NOW IN the extract_with_function LIST
IRR_table_cancer_age_sex <- Reduce(full_join,extract_with_function)

# REMOVE PRE-covid COLUMN
IRR_table_cancer_age_sex <- IRR_table_cancer_age_sex[-1]
# CONVERT THE ROWNAMES TO A NORMAL DATA COLUMN
IRR_table_cancer_age_sex <- tibble::rownames_to_column(IRR_table_cancer_age_sex, "Cancer Age and Sex")

# ADD A COLUMN OF THE LIST NAMES
IRR_table_cancer_age_sex <- cbind (list_names, IRR_table_cancer_age_sex)

# CREATE A NEW COLUMN OF CANCER TYPES
IRR_table_cancer_age_sex <- IRR_table_cancer_age_sex %>% mutate(Cancer = case_when(grepl("Colorectal", list_names) ~ "Colorectal",
                                                                                   grepl("Lung", list_names) ~ "Lung",
                                                                                   grepl("Prostate", list_names) ~ "Prostate",
                                                                                   grepl("Breast", list_names) ~ "Breast"))


# CREATE A NEW COLUMN OF Age and Sex groups
IRR_table_cancer_age_sex <- IRR_table_cancer_age_sex %>% mutate(Age_sex = case_when(grepl("9", list_names) ~ "Female; 20-39",
                                                                                   grepl("11", list_names) ~ "Male; 40-59",
                                                                                   grepl("12", list_names) ~ "Female; 40-59",
                                                                                   grepl("14", list_names) ~ "Male; 60-79",
                                                                                   grepl("15", list_names) ~ "Female; 60-79",
                                                                                   grepl("17", list_names) ~ "Male; 80-150",
                                                                                   grepl("18", list_names) ~ "Female; 80-150"))



# CREATE A NEW COLUMN OF Age and Sex groups
IRR_table_cancer_age_sex <- IRR_table_cancer_age_sex %>% mutate(Age = case_when(grepl("20-39", Age_sex) ~ "20-39",
                                                                                    grepl("40-59", Age_sex) ~ "40-59",
                                                                                    grepl("60-79", Age_sex) ~ "60-79",
                                                                                    grepl("80-150", Age_sex) ~ "80-150"))


# CREATE A NEW COLUMN OF Age and Sex groups
IRR_table_cancer_age_sex <- IRR_table_cancer_age_sex %>% mutate(Sex = case_when(grepl("Ma", Age_sex) ~ "Male",
                                                                                    grepl("Fe", Age_sex) ~ "Female"))

# REORDER
IRR_table_cancer_age_sex <- IRR_table_cancer_age_sex[c(1, 9, 10, 12, 11, 3, 4, 5, 6, 7, 8)]
IRR_table_cancer_age_sex <- arrange(IRR_table_cancer_age_sex, Cancer, Sex, Age)

#### Save IRR
write.csv(IRR_table_cancer_age_sex, file=here::here("3_DataSummary", "IRR_table_cancer_age_sex_looped.csv"))
save(IRR_table_cancer_age_sex, file=here::here("3_DataSummary", "IRR_table_cancer_age_sex_looped.RData"))

#### Make pretty table
Pretty_IRR_table_cancer_age_sex <- flextable(IRR_table_cancer_age_sex) %>% theme_vanilla() %>% 
  set_caption(caption = "Incidence rate ratios of cancers over the lockdown periods compared to pre-COVID period, stratified by age and sex") %>% 
  width(width = 1.4) 

save_as_docx('Pretty_IRR_table_cancer_age_sex' = Pretty_IRR_table_cancer_age_sex, path=here("3_DataSummary", "Pretty_IRR_table_cancer_age_sex.docx"))

