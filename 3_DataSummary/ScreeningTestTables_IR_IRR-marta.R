# load the data  ----
load("~/GitHub/CancerCovid_NegativeBinomialReg/1_DataPrep/Data/ScreeningTests_DataPrep.RData") #Nicola's wd
#load("~/R_projects_folder/nicola_cancer_project/ScreeningTests_DataPrep.RData") #Marta's wd

#load libraries
library(plyr)
library(dplyr)    #%>% filters, etc
library(epitools) #rateratio


# filter overal for all age and sex groups
IR.overall <- screening_inc_data_final %>% filter(denominator_cohort_id ==1)

# rename data for ease in code
IR <- IR.overall


# loop for calculating IRR from events (events) and number of person months 
# (months) for each covid time period (covid x 7, with pre-covid as the ref) and for each screening/diagnostic
# test outcome (outcome x 14)

periods<- IR%>% dplyr::select("covid")%>%distinct()%>%pull()
outcome <-IR%>% dplyr::select("outcome")%>% distinct()%>%pull()
#IRR <- list() # a list to store all the output later
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

# Berta then had this bit of code to fill in the IRR list with the output from working.data. This was originally inside the above loop.
    
    IRR[[paste0(working.period, working.outcome)]]<- working.data %>%
      filter(period == working.period)%>%
      filter(outcome == working.outcome)%>%
      filter(ref==1)%>%
      mutate(IR = events_t/pmar * 100000) %>%
      mutate(IRR=round(rateratios$measure[2],2)) %>%
      mutate(IRR_low =round(rateratios$measure[2,2],2)) %>%
      mutate(IRR_upp =round(rateratios$measure[2,3],2))
    
