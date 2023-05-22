# ============================================================================ #
#     Create tables with incidence incidence rate ratios compared to the       #
#                             pre-covid reference                              #
#                            for cancer diagnoses                              #
#                                                                              #
#                              Nicola Barclay                                  #
#                                18-04-2023                                    #
# ============================================================================ #

load("C:/Users/martapm/Downloads/inc_data_final.RData")
library(dplyr)
library(epitools)
# ============ CALCULATE IRR FOR EACH CANCER OVER PERIODS ==================== #

# ============ OVERALL (NO AGE SEX STRATIFICATION ) ========================== #  

# This code calculates the IRR for each of the cancers
# separately, but loops over each period of interest
# THIS WORKS FINE :-)

IR.overall <- inc_data_final %>% filter(denominator_cohort_id ==1)

IR <- IR.overall

periods<- IR%>% dplyr::select("covid")%>%distinct()%>%pull()
outcome <-IR%>% dplyr::select("outcome")%>% distinct()%>%pull()
rateratios <- vector("list",length(outcome)); names(rateratios) = outcome

for (y in 1:length(outcome)){
  working.outcome <- outcome[y]
  vector <- data.frame(a=c(),b=c()) # a vector to place the values from the loop
  for(z in 1:length(periods)){ 
    working.period <- periods[z]
    working.data <- IR %>% 
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
         rateratios[[y]] <-rateratio(as.matrix(vector, y=NULL)), # this bit of the code says that if there is a result in vector, give us the rate ratio. If no result, give us NA
         rateratios[[y]] <- NA)
} 

# EXTRACT THE CANCER LISTS
rateratios_breast <- rateratios$Breast
rateratios_colorectal <- rateratios$Colorectal
rateratios_lung <- rateratios$Lung
rateratios_prostate <- rateratios$Prostate



# ============ (AGE SEX STRATIFICATION by Marta) ========================== #  

IR.overall <- inc_data_final %>% filter(denominator_cohort_id == 9 |denominator_cohort_id == 11|denominator_cohort_id == 12|
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
## EXTRACT THE CANCER LISTS ## MPM: you will need to redo the data extraction using the following rateratios names:
names(rateratios)

#rateratios_breast <- rateratios$Breast
#rateratios_colorectal <- rateratios$Colorectal
#rateratios_lung <- rateratios$Lung
#rateratios_prostate <- rateratios$Prostate




################################################################################

## FUNCTION TO EXTRACT ALL THE IRR AND CIS FROM ALL OF THE LISTS  ## MPM: I hadn't modifyed any of it.

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


