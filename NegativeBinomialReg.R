# ============================================================================ #
#                         NEGATIVE BINOMIAL REGRESSION                         #
#                            FOR CANCER/COVID STUDY                            #
#                                Nicola Barclay                                #
#                                 17-01-2023                                   #
# ============================================================================ #

# packages -----
library(DatabaseConnector)
library(here)
library(lubridate)
library(stringr)
library(ggplot2)
library(DBI)
library(dbplyr)
library(dplyr)
library(tidyr)
library(tableone)
library(scales)
library(forcats)
library(epiR)
library(RPostgreSQL)
library(foreign)
library(MASS)
library(tsibble)
library(graphics)
library(feasts)
library(magrittr)
library(ciTools)
library(readr)

# Load data - incidence estimates derived from IncPrev package
IR.overall <- read_csv("Data/incidence_estimates_cdmgold202007.csv")
View(IR.overall)

# Name the outcomes in the dataset
IR.overall <- IR.overall %>% mutate(outcome = case_when(outcome_cohort_id == 1 ~ "Breast",
                                                       outcome_cohort_id == 2 ~ "Colorectal",
                                                       outcome_cohort_id == 3 ~ "Lung",
                                                       outcome_cohort_id == 4 ~ "Prostate"))
# Create a variable of person_months
IR.overall <- IR.overall %>% mutate(person_months = (IR.overall$person_days/30.44))

# only include estimates computed by month
IR.overall <- IR.overall %>% filter(analysis_interval == "months")

# number of months in the study
n.months<-lubridate::interval(ymd(start.date),ymd(end.date)) %/% months(1)

###Validation:
##Fit to 01/03/2017- 01/03/2019
##Forecast 01/03/2019 - 01/03/2020


outcomes_to_fit<- IR.overall %>% dplyr::select("outcome")%>% distinct()%>%pull()

###### Validation Overall: 

models.overall_validation <- list()
models.overall_validation.fit <- list()
models.overall_pred <- list()

months.since.start <- IR.overall$incidence_start_date 





end_mod <- 24 #month.since.start= Feb 2019
end_pred <- 36 #month.since.start= Feb 2020

for(j in 1:length(outcomes_to_fit)){
  
  # Negative Binomial
  working.nb <- glm.nb(n_events ~ as.factor(month)+months.since.start,data=IR.overall%>%  
                         filter(outcome==outcomes_to_fit[j]) %>% filter(months.since.start<= end_mod))
  models.overall_validation[[paste0("m.","overall",outcomes_to_fit[j], ".nb")]] <- working.nb
  
  pred <-predict(working.nb, newdata=IR.overall %>% filter(months.since.start<= end_pred)  %>%
                   filter(outcome==outcomes_to_fit[j]),type="response", se.fit = TRUE, interval= "prediction", level=0.95)
  
  models.overall_pred[[paste0("m.","overall",outcomes_to_fit[j], ".nb")]] <- cbind(IR.overall %>% filter(months.since.start<= end_pred) %>% filter(outcome==outcomes_to_fit[j]),
                                                                                   data.frame(est=as.character(pred$fit), model="nb")) %>%  add_pi(working.nb, names = c("lwr", "upr"), alpha = 0.05)
  
}


val_overall <- bind_rows(models.overall_pred) %>% mutate(ir_pred =pred*100000/months, lwr_pred=lwr*100000/months, upr_pred=upr*100000/months) %>%
  mutate_if(is.numeric, ~round(., 1)) %>% dplyr::select(-c(model, est))
#save(val_overall, file=here("WorkingData", "Validation_model.RData"))
