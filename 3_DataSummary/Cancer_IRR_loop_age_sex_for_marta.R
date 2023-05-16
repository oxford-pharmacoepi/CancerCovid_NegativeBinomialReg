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




# ============ CALCULATE IRR FOR EACH CANCER OVER PERIODS ==================== #

# ============ OVERALL (NO AGE SEX STRATIFICATION ) ========================== #  

# THIS WORKS FINE :-)

load("~/GitHub/CancerCovid_NegativeBinomialReg/3_DataSummary/inc_data_final.RData")

# This code calculates the IRR for each of the cancers
# separately, but loops over each period of interest
IR.overall <- inc_data_final %>% filter(  denominator_cohort_id == 9 |denominator_cohort_id == 11|denominator_cohort_id == 12|
                                          denominator_cohort_id == 14|denominator_cohort_id == 15|denominator_cohort_id == 17|
                                          denominator_cohort_id == 18)



names_cohort_id = names(table(IR.overall$denominator_cohort_id))
number_cohort_id = length(names_cohort_id)

IR <- IR.overall


periods<- IR%>% dplyr::select("covid")%>%distinct()%>%pull()
outcome <-IR%>% dplyr::select("outcome")%>% distinct()%>%pull()
rateratios <- vector("list",length(outcome)*number_cohort_id); names(rateratios) = outer(names_cohort_id,outcome,paste,sep="_")

count = 1
for (id in names_cohort_id){
  for (y in 1:length(outcome)){
    working.outcome <- outcome[y]
    vector <- data.frame(a=c(),b=c()) # a vector to place the values from the loop
    for(z in 1:length(periods)){
      working.period <- periods[z]
      working.data <- IR %>%
        filter(denominator_cohort_id==id)%>%
        filter(outcome==working.outcome)%>%
        filter(covid==working.period) %>%
        mutate(ref=if_else(months.since.start < 39,0,1))%>% # 38 indicates reference OF PRE-COVID
        group_by(ref)%>% #no function for final time period
        summarise( events_t = sum(events),person_months_at_risk = sum(months))%>%
        mutate(periods = paste(working.period))%>%
        mutate(outcome= paste(working.outcome))
      
      events <- c(working.data%>%dplyr::select(events_t)%>%pull())
      pt <- c(working.data%>%dplyr::select(person_months_at_risk)%>%pull())
      
      vector <- rbind(vector,c(events, pt))
      
      
    }
    ifelse(dim(vector)[1] > 1,
           rateratios[[count]] <-rateratio(as.matrix(vector, y=NULL)), # this bit of the code says that if there is a result in vector, give us the rate ratio. If no result, give us NA
           rateratios[[count]] <- NA)
    count = count + 1
  }
}

# show table of age sex groups
age_sex_table <- inc_data_final %>% dplyr::select(denominator_age_group, denominator_sex, denominator_cohort_id)
age_sex_table_2 <- age_sex_table %>% group_by(denominator_age_group, denominator_sex, denominator_cohort_id) %>% tally()


# remove all the NA lists from the rateratios list
rateratios <- rateratios[!is.na(rateratios)]

# EXTRACT THE CANCER LISTS#

extract <- list()
list_names <- names(rateratios)
for (i in 1:length(list_names)){
  
  temp_data = rateratios[[i]][[2]]
  extract[[i]] <- temp_data
  
}


# EXTRACT THE CANCER LISTS APPLYING MY FUNCTION SIMULTANEOUSLY - returns error about character
list_names <- names(rateratios)
y=1
for (i in 1:length(list_names)){
  
  temp_data = rateratios[[i]][[2]]
  extract1 <-get_IR_df_function(temp_data, list_names[i])
  y = y+1
} 
  


# apply the function after extracting the elements of the list
get_IR_df <- data.frame(matrix("", ncol = 1, nrow = 7))  

for (i in 1:length(extract)){

get_IR_df[i] <- as.data.frame(extract[i])
get_IR_df[i] <- get_IR_df[i] %>%  mutate_if(is.numeric, round, digits=2)
}



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

##################

rateratios_breast_f_2039 <- rateratios$`9_Breast`
rateratios_breast_f_4059 <- rateratios$`12_Breast`
rateratios_breast_f_6079 <- rateratios$`15_Breast`
rateratios_breast_f_80150 <- rateratios$`18_Breast`
rateratios_colorectal_f_2039 <- rateratios$`9_Breast`


rateratios_colorectal <- rateratios$Colorectal
rateratios_lung <- rateratios$Lung
rateratios_prostate <- rateratios$Prostate


# FUNCTION TO EXTRACT ALL THE IRR AND CIS FROM ALL OF THE LISTS EDITED FOR STRATIFICATION 

get_IR_df_function <- function(yourrateratiosname, title){
  
  get_IR_df <- as.data.frame(yourrateratiosname[[y]])
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







################################################################################

# FUNCTION TO EXTRACT ALL THE IRR AND CIS FROM ALL OF THE LISTS ORIGINAL 

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

# RUN THE FUNCTION FOR EACH OF THE RATERATIO LISTS



IRR_Breast <-  get_IR_df_function(rateratios_breast, "Breast")
IRR_Colorectal <-  get_IR_df_function(rateratios_colorectal, "Colorectal")
IRR_Lung <- get_IR_df_function(rateratios_lung, "Lung") 
IRR_Prostate <-  get_IR_df_function(rateratios_prostate, "Prostate")



# JOIN THE TABLES
IRR_table_cancer <- rbind(IRR_Breast, IRR_Colorectal, IRR_Lung, IRR_Prostate)
# REMOVE PRE-covid COLUMN
IRR_table_cancer <- IRR_table_cancer[-1]
# CONVERT THE ROWNAMES TO A NORMAL DATA COLUMN
IRR_table_cancer <- tibble::rownames_to_column(IRR_table_cancer, "Cancer")


#### Save IRR
write.csv(IRR_table_cancer, file=here::here("3_DataSummary", "IRR_table_cancer_looped.csv"))
save(IRR_table_cancer, file=here::here("3_DataSummary", "IRR_table_cancer_looped.RData"))

#### Make pretty table
Pretty_IRR_table_cancer <- flextable(IRR_table_cancer) %>% theme_vanilla() %>% 
  set_caption(caption = "Incidence rate ratios of cancers ober the lockdown periods compared to pre-COVID period") %>% 
  width(width = 1.4) 

save_as_docx('Pretty_IRR_table_cancer' = Pretty_IRR_table_cancer, path=here("3_DataSummary", "Pretty_IRR_table_cancer.docx"))


# ================ STRATIFIED BY AGE AND SEX  ================================ #


# This code calculates the IRR for each of the cancers stratified by age and sex
# loops over each period of interest
# I CAN'T GET THIS TO WORK :-(

IR.as <- inc_data_final

IRR.age.sex <- list() # i think i need to make an empty list to put the results in
# but maybe this should go inside the function?

periods<- IR%>% dplyr::select("covid")%>%distinct()%>%pull()
outcome <-IR%>% dplyr::select("outcome")%>% distinct()%>%pull()
age.strata <- IR.as %>% dplyr::select("denominator_age_group")%>% distinct()%>%pull() 
sex.strata <- IR.as %>% dplyr::select("denominator_sex")%>% distinct()%>%pull() 

strata <- c(age.strata, sex.strata)
comb <- expand.grid(age.strata, sex.strata)


rateratios.as <- vector("list",length(outcome)); names(rateratios.as) = outcome  
# should this somehow accomodate the age.sex strata?
# 

for (y in 1:length(outcome)){
  working.outcome <- outcome[y]
  vector <- data.frame(a=c(),b=c()) # a vector to place the values from the loop
  for(z in 1:length(periods)){ 
    working.period <- periods[z]
      for(i in 1:NROW(comb)){ # this bit should loop over the age sex strata but i'm not sure how to get it to put it somewhere useful
      
      working.strata <- paste(comb$Var1, comb$Var2)[i]
      working.age <- comb$Var1[i]
      working.gender<- comb$Var2[i]
      
    working.data <- IR %>% 
      filter(outcome==working.outcome)%>%
      filter(covid==working.period) %>%
      filter((denominator_age_group==working.age)) %>%
      filter((denominator_sex==working.gender)) %>%
      mutate(ref=if_else(months.since.start < 39,0,1))%>% # 38 indicates reference OF PRE-COVID
      group_by(ref)%>% #no function for final time period
      summarise( events_t = sum(events),person_months_at_risk = sum(months))%>%
      mutate(periods = paste(working.period))%>%
      mutate(outcome= paste(working.outcome))
    
    events <- c(working.data%>%dplyr::select(events_t)%>%pull())
    pt <- c(working.data%>%dplyr::select(person_months_at_risk)%>%pull())
    
    vector <- rbind(vector,c(events, pt))
    

  }
  ifelse(dim(vector)[1] > 1,
         rateratios.as[[y]] <-rateratio(as.matrix(vector, y=NULL)), # this bit of the code says that if there is a result in vector, give us the rate ratio. If no result, give us NA
         rateratios.as[[y]] <- NA)
  }
  # this part berta had included to put the results in but it doesn't quite work 
  IRR.age.sex[[paste0(working.period, working.outcome, working.strata)]]<- working.data %>%
    filter(period == working.period)%>%
    filter(outcome == working.outcome)%>%
    filter(ref==1)%>%
    mutate(IR = events_t/pmar * 100000) %>%
    mutate(IRR=round(rateratios.as$measure[2],2)) %>%
    mutate(IRR_low =round(rateratios.as$measure[2,2],2)) %>%
    mutate(IRR_upp =round(rateratios.as$measure[2,3],2))

  }

# EXTRACT THE CANCER LISTS - need to edit to add the age sex strata
rateratios_breast <- rateratios$Breast
rateratios_colorectal <- rateratios$Colorectal
rateratios_lung <- rateratios$Lung
rateratios_prostate <- rateratios$Prostate


# RUN THE FUNCTION FOR EACH OF THE RATERATIO LISTS - edit all below to add age sex strata



IRR_Breast <-  get_IR_df_function(rateratios_breast, "Breast")
IRR_Colorectal <-  get_IR_df_function(rateratios_colorectal, "Colorectal")
IRR_Lung <- get_IR_df_function(rateratios_lung, "Lung") 
IRR_Prostate <-  get_IR_df_function(rateratios_prostate, "Prostate")



# JOIN THE TABLES
IRR_table_cancer <- rbind(IRR_Breast, IRR_Colorectal, IRR_Lung, IRR_Prostate)
# REMOVE PRE-covid COLUMN
IRR_table_cancer <- IRR_table_cancer[-1]
# CONVERT THE ROWNAMES TO A NORMAL DATA COLUMN
IRR_table_cancer <- tibble::rownames_to_column(IRR_table_cancer, "Cancer")


#### Save IRR
write.csv(IRR_table_cancer, file=here::here("3_DataSummary", "IRR_table_cancer_looped.csv"))
save(IRR_table_cancer, file=here::here("3_DataSummary", "IRR_table_cancer_looped.RData"))

#### Make pretty table
Pretty_IRR_table_cancer <- flextable(IRR_table_cancer) %>% theme_vanilla() %>% 
  set_caption(caption = "Incidence rate ratios of cancers ober the lockdown periods compared to pre-COVID period") %>% 
  width(width = 1.4) 

save_as_docx('Pretty_IRR_table_cancer' = Pretty_IRR_table_cancer, path=here("3_DataSummary", "Pretty_IRR_table_cancer.docx"))



