# ============================================================================ #
#     Create tables with incidence incidence rate ratios compared to the       #
#                             pre-covid reference                              #
#                    for screening and diagnostic tests                        #
#                                                                              #
#                              Nicola Barclay                                  #
#                                18-04-2023                                    #
# ============================================================================ #

# Load the scleaned screening test data object which is from the csv file of 
# incidence results from the IncPrev package ----

load("~/GitHub/CancerCovid_NegativeBinomialReg/1_DataPrep/Data/ScreeningTests_DataPrep.RData")
IR.overall_screen <- screening_inc_data_final  %>% filter(denominator_cohort_id ==1)

IR <- IR.overall_screen

periods<- IR%>% dplyr::select("covid")%>%distinct()%>%pull()
outcome <-IR%>% dplyr::select("outcome")%>% distinct()%>%pull()

# This code calculates the IRR for each of the screening and diagnostic tests
# separately, but loops over each period of interest


# 1. working loop for one outcome - breast cancer referrals
IR_breast_cancer_referrals <- IR.overall_screen %>% filter(IR.overall_screen$outcome =="Breast Cancer Referrals")
vector <- data.frame(a=c(),b=c())

for (z in 1:length(periods)){ 
  working.period <- periods[z]
  working.data <- IR_breast_cancer_referrals %>% 
    filter(covid==working.period) %>%
    mutate(ref=if_else(months.since.start < 39,0,1))%>% # 38 indicates reference OF PRE-COVID
    group_by(ref)%>% #no function for final time period
    summarise( events_t = sum(events),person_months_at_risk = sum(months))%>%
    mutate(periods = paste(working.period))
  
  events <- c(working.data%>%dplyr::select(events_t)%>%pull())
  pt <- c(working.data%>%dplyr::select(person_months_at_risk)%>%pull())
  
  vector <- rbind(vector,c(events, pt))
}
rateratios_breast_referrals <-rateratio(as.matrix(vector, y=NULL))



# 2. working loop for one outcome - bronchoscopy - this doesnt work as there is only data pre-covid for this outcome
IR_Bronchoscopy <- IR.overall_screen %>% filter(IR.overall_screen$outcome =="Bronchoscopy")
vector <- data.frame(a=c(),b=c())

for (z in 1:length(periods)){ 
  working.period <- periods[z]
  working.data <- IR_Bronchoscopy %>% 
    filter(covid==working.period) %>%
    mutate(ref=if_else(months.since.start < 39,0,1))%>% # 38 indicates reference OF PRE-COVID
    group_by(ref)%>% #no function for final time period
    summarise( events_t = sum(events),person_months_at_risk = sum(months))%>%
    mutate(periods = paste(working.period))
  
  events <- c(working.data%>%dplyr::select(events_t)%>%pull())
  pt <- c(working.data%>%dplyr::select(person_months_at_risk)%>%pull())
  
  vector <- rbind(vector,c(events, pt))
}
rateratios_bronch <-rateratio(as.matrix(vector, y=NULL))




# 3. working loop for one outcome - Biopsy of Breast
IR_BB <- IR.overall_screen %>% filter(IR.overall_screen$outcome =="Biopsy of Breast")
vector <- data.frame(a=c(),b=c())

for (z in 1:length(periods)){ 
  working.period <- periods[z]
  working.data <- IR_BB %>% 
    filter(covid==working.period) %>%
    mutate(ref=if_else(months.since.start < 39,0,1))%>% # 38 indicates reference OF PRE-COVID
    group_by(ref)%>% #no function for final time period
    summarise( events_t = sum(events),person_months_at_risk = sum(months))%>%
    mutate(periods = paste(working.period))
  
  events <- c(working.data%>%dplyr::select(events_t)%>%pull())
  pt <- c(working.data%>%dplyr::select(person_months_at_risk)%>%pull())
  
  vector <- rbind(vector,c(events, pt))
}
rateratios_biopsy_breast <-rateratio(as.matrix(vector, y=NULL))



# 4. working loop for one outcome - colonoscopy
IR_Col <- IR.overall_screen %>% filter(IR.overall_screen$outcome =="Colonoscopy")
vector <- data.frame(a=c(),b=c())

for (z in 1:length(periods)){ 
  working.period <- periods[z]
  working.data <- IR_Col %>% 
    filter(covid==working.period) %>%
    mutate(ref=if_else(months.since.start < 39,0,1))%>% # 38 indicates reference OF PRE-COVID
    group_by(ref)%>% #no function for final time period
    summarise( events_t = sum(events),person_months_at_risk = sum(months))%>%
    mutate(periods = paste(working.period))
  
  events <- c(working.data%>%dplyr::select(events_t)%>%pull())
  pt <- c(working.data%>%dplyr::select(person_months_at_risk)%>%pull())
  
  vector <- rbind(vector,c(events, pt))
}
rateratios_colon <-rateratio(as.matrix(vector, y=NULL))



# 5. working loop for one outcome - Diagnostic Procedures Of Chest
IR_DC <- IR.overall_screen %>% filter(IR.overall_screen$outcome =="Diagnostic Procedures Of Chest")
vector <- data.frame(a=c(),b=c())

for (z in 1:length(periods)){ 
  working.period <- periods[z]
  working.data <- IR_DC %>% 
    filter(covid==working.period) %>%
    mutate(ref=if_else(months.since.start < 39,0,1))%>% # 38 indicates reference OF PRE-COVID
    group_by(ref)%>% #no function for final time period
    summarise( events_t = sum(events),person_months_at_risk = sum(months))%>%
    mutate(periods = paste(working.period))
  
  events <- c(working.data%>%dplyr::select(events_t)%>%pull())
  pt <- c(working.data%>%dplyr::select(person_months_at_risk)%>%pull())
  
  vector <- rbind(vector,c(events, pt))
}
rateratios_chest <-rateratio(as.matrix(vector, y=NULL))



# 6. working loop for one outcome - Excision Of Breast
IR_EB <- IR.overall_screen %>% filter(IR.overall_screen$outcome =="Excision Of Breast")
vector <- data.frame(a=c(),b=c())

for (z in 1:length(periods)){ 
  working.period <- periods[z]
  working.data <- IR_EB %>% 
    filter(covid==working.period) %>%
    mutate(ref=if_else(months.since.start < 39,0,1))%>% # 38 indicates reference OF PRE-COVID
    group_by(ref)%>% #no function for final time period
    summarise( events_t = sum(events),person_months_at_risk = sum(months))%>%
    mutate(periods = paste(working.period))
  
  events <- c(working.data%>%dplyr::select(events_t)%>%pull())
  pt <- c(working.data%>%dplyr::select(person_months_at_risk)%>%pull())
  
  vector <- rbind(vector,c(events, pt))
}
rateratios_excision_breast <-rateratio(as.matrix(vector, y=NULL))


# 7. working loop for one outcome - Lung Cancer Referrals
IR_LC <- IR.overall_screen %>% filter(IR.overall_screen$outcome =="Lung Cancer Referrals")
vector <- data.frame(a=c(),b=c())

for (z in 1:length(periods)){ 
  working.period <- periods[z]
  working.data <- IR_LC %>% 
    filter(covid==working.period) %>%
    mutate(ref=if_else(months.since.start < 39,0,1))%>% # 38 indicates reference OF PRE-COVID
    group_by(ref)%>% #no function for final time period
    summarise( events_t = sum(events),person_months_at_risk = sum(months))%>%
    mutate(periods = paste(working.period))
  
  events <- c(working.data%>%dplyr::select(events_t)%>%pull())
  pt <- c(working.data%>%dplyr::select(person_months_at_risk)%>%pull())
  
  vector <- rbind(vector,c(events, pt))
}
rateratios_lung_referrals <-rateratio(as.matrix(vector, y=NULL))




# 8. working loop for one outcome - Mammograms
IR_mam <- IR.overall_screen %>% filter(IR.overall_screen$outcome =="Mammograms")
vector <- data.frame(a=c(),b=c())

for (z in 1:length(periods)){ 
  working.period <- periods[z]
  working.data <- IR_mam %>% 
    filter(covid==working.period) %>%
    mutate(ref=if_else(months.since.start < 39,0,1))%>% # 38 indicates reference OF PRE-COVID
    group_by(ref)%>% #no function for final time period
    summarise( events_t = sum(events),person_months_at_risk = sum(months))%>%
    mutate(periods = paste(working.period))
  
  events <- c(working.data%>%dplyr::select(events_t)%>%pull())
  pt <- c(working.data%>%dplyr::select(person_months_at_risk)%>%pull())
  
  vector <- rbind(vector,c(events, pt))
}
rateratios_mammograms <-rateratio(as.matrix(vector, y=NULL))



# 9. working loop for one outcome - Prostate Specific Antigen Test
IR_psa <- IR.overall_screen %>% filter(IR.overall_screen$outcome =="Prostate Specific Antigen Test")
vector <- data.frame(a=c(),b=c())

for (z in 1:length(periods)){ 
  working.period <- periods[z]
  working.data <- IR_psa %>% 
    filter(covid==working.period) %>%
    mutate(ref=if_else(months.since.start < 39,0,1))%>% # 38 indicates reference OF PRE-COVID
    group_by(ref)%>% #no function for final time period
    summarise( events_t = sum(events),person_months_at_risk = sum(months))%>%
    mutate(periods = paste(working.period))
  
  events <- c(working.data%>%dplyr::select(events_t)%>%pull())
  pt <- c(working.data%>%dplyr::select(person_months_at_risk)%>%pull())
  
  vector <- rbind(vector,c(events, pt))
}
rateratios_psa <-rateratio(as.matrix(vector, y=NULL))




# 10. working loop for one outcome - Seen in Breast Clinic
IR_sbc <- IR.overall_screen %>% filter(IR.overall_screen$outcome =="Seen in Breast Clinic")
vector <- data.frame(a=c(),b=c())

for (z in 1:length(periods)){ 
  working.period <- periods[z]
  working.data <- IR_sbc %>% 
    filter(covid==working.period) %>%
    mutate(ref=if_else(months.since.start < 39,0,1))%>% # 38 indicates reference OF PRE-COVID
    group_by(ref)%>% #no function for final time period
    summarise( events_t = sum(events),person_months_at_risk = sum(months))%>%
    mutate(periods = paste(working.period))
  
  events <- c(working.data%>%dplyr::select(events_t)%>%pull())
  pt <- c(working.data%>%dplyr::select(person_months_at_risk)%>%pull())
  
  vector <- rbind(vector,c(events, pt))
}
rateratios_seen_breast_clinic <-rateratio(as.matrix(vector, y=NULL))



# 11. working loop for one outcome - Seen Breast Surgeon
IR_sbs <- IR.overall_screen %>% filter(IR.overall_screen$outcome =="Seen Breast Surgeon")
vector <- data.frame(a=c(),b=c())

for (z in 1:length(periods)){ 
  working.period <- periods[z]
  working.data <- IR_sbs %>% 
    filter(covid==working.period) %>%
    mutate(ref=if_else(months.since.start < 39,0,1))%>% # 38 indicates reference OF PRE-COVID
    group_by(ref)%>% #no function for final time period
    summarise( events_t = sum(events),person_months_at_risk = sum(months))%>%
    mutate(periods = paste(working.period))
  
  events <- c(working.data%>%dplyr::select(events_t)%>%pull())
  pt <- c(working.data%>%dplyr::select(person_months_at_risk)%>%pull())
  
  vector <- rbind(vector,c(events, pt))
}
rateratios_seen_breast_surgeon <-rateratio(as.matrix(vector, y=NULL))


# 12. working loop for one outcome - Sigmoidoscopy
IR_sig <- IR.overall_screen %>% filter(IR.overall_screen$outcome =="Sigmoidoscopy")
vector <- data.frame(a=c(),b=c())

for (z in 1:length(periods)){ 
  working.period <- periods[z]
  working.data <- IR_sig %>% 
    filter(covid==working.period) %>%
    mutate(ref=if_else(months.since.start < 39,0,1))%>% # 38 indicates reference OF PRE-COVID
    group_by(ref)%>% #no function for final time period
    summarise( events_t = sum(events),person_months_at_risk = sum(months))%>%
    mutate(periods = paste(working.period))
  
  events <- c(working.data%>%dplyr::select(events_t)%>%pull())
  pt <- c(working.data%>%dplyr::select(person_months_at_risk)%>%pull())
  
  vector <- rbind(vector,c(events, pt))
}
rateratios_Sigmoidoscopy <-rateratio(as.matrix(vector, y=NULL))


# 13 working loop for one outcome - Biopsy Of Prostate
IR_bp <- IR.overall_screen %>% filter(IR.overall_screen$outcome =="Biopsy Of Prostate")
vector <- data.frame(a=c(),b=c())

for (z in 1:length(periods)){ 
  working.period <- periods[z]
  working.data <- IR_bp %>% 
    filter(covid==working.period) %>%
    mutate(ref=if_else(months.since.start < 39,0,1))%>% # 38 indicates reference OF PRE-COVID
    group_by(ref)%>% #no function for final time period
    summarise( events_t = sum(events),person_months_at_risk = sum(months))%>%
    mutate(periods = paste(working.period))
  
  events <- c(working.data%>%dplyr::select(events_t)%>%pull())
  pt <- c(working.data%>%dplyr::select(person_months_at_risk)%>%pull())
  
  vector <- rbind(vector,c(events, pt))
}
rateratios_biopsy_prostate <-rateratio(as.matrix(vector, y=NULL))


# 14. working loop for one outcome - Bowel Cancer Screening Prog
IR_bcsp <- IR.overall_screen %>% filter(IR.overall_screen$outcome =="Bowel Cancer Screening Prog")
vector <- data.frame(a=c(),b=c())

for (z in 1:length(periods)){ 
  working.period <- periods[z]
  working.data <- IR_bcsp %>% 
    filter(covid==working.period) %>%
    mutate(ref=if_else(months.since.start < 39,0,1))%>% # 38 indicates reference OF PRE-COVID
    group_by(ref)%>% #no function for final time period
    summarise( events_t = sum(events),person_months_at_risk = sum(months))%>%
    mutate(periods = paste(working.period))
  
  events <- c(working.data%>%dplyr::select(events_t)%>%pull())
  pt <- c(working.data%>%dplyr::select(person_months_at_risk)%>%pull())
  
  vector <- rbind(vector,c(events, pt))
}
rateratios_bowel_cancer_screen <-rateratio(as.matrix(vector, y=NULL))


################################################################################

# FUNCTION TO EXTRACT ALL THE IRR AND CIS FROM ALL OF THE LISTS 

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

IRR_BCR <-  get_IR_df_function(rateratios_breast_referrals, "Breast Cancer Referrals")
IRR_BB <-  get_IR_df_function(rateratios_biopsy_breast, "Biopsy of Breast")
IRR_Bronch <- get_IR_df_function(rateratios_bronch, "Bronchoscopy") #  doesnt work as data incomplete
IRR_Col <-  get_IR_df_function(rateratios_colon, "Colonoscopy")
IRR_DPC <-  get_IR_df_function(rateratios_chest, "Diagnostic Procedures of Chest")
IRR_EB <-  get_IR_df_function(rateratios_excision_breast, "Excision of Breast")
IRR_LCR <-  get_IR_df_function(rateratios_lung_referrals, "Lung Cancer Referrals")
IRR_Mam <-  get_IR_df_function(rateratios_mammograms, "Mammograms")
IRR_PSA <-  get_IR_df_function(rateratios_psa, "PSA")
IRR_SBC <-  get_IR_df_function(rateratios_seen_breast_clinic, "Seen in Breast Clinic")
IRR_SBS <-  get_IR_df_function(rateratios_seen_breast_surgeon, "Seen by Breast Surgeon")
IRR_SIG <-  get_IR_df_function(rateratios_Sigmoidoscopy, "Sigmoidoscopy")
IRR_BP <-  get_IR_df_function(rateratios_biopsy_prostate, "Biopsy of Prostate")
IRR_BCSP <-  get_IR_df_function(rateratios_bowel_cancer_screen, "Bowel Cancer Screening Prog") #  doesnt work as data incomplete


# JOIN THE TABLES
IRR_table_screening_tests <- rbind(IRR_BCR, IRR_BB, IRR_Col, IRR_DPC, IRR_EB, IRR_LCR, IRR_Mam, IRR_PSA, IRR_SBC, IRR_SBS, IRR_SIG, IRR_BP)
# REMOVE PRE-covid COLUMN
IRR_table_screening_tests <- IRR_table_screening_tests[-1]
# CONVERT THE ROWNAMES TO A NORMAL DATA COLUMN
IRR_table_screening_tests <- tibble::rownames_to_column(IRR_table_screening_tests, "Screening/Diagnostic Test")


#### Save IRR
write.csv(IRR_table_screening_tests, file=here("3_DataSummary", "IRR_table_screening_tests.csv"))
save(IRR_table_screening_tests, file=here("3_DataSummary", "IRR_table_screening_tests.RData"))

#### Make pretty table
Pretty_IRR_table_screening_tests <- flextable(IRR_table_screening_tests) %>% theme_vanilla() %>% 
  set_caption(caption = "Incidence rate ratios of screening and diagnostic tests compared to pre-COVID period") %>% 
  width(width = 1.4) 

save_as_docx('Pretty_IRR_table_screening_tests' = Pretty_IRR_table_screening_tests, path=here("3_DataSummary", "Pretty_IRR_table_screening_tests.docx"))

