# ============================================================================ #
#                         NEGATIVE BINOMIAL REGRESSION                         #
#                            FOR CANCER/COVID STUDY                            #
#                               VALIDATION MODEL                               #
#                                Nicola Barclay                                #
#                                 16-02-2023                                   #
# THIS SCRIPT CONTAINS OLD CODE THAT BROKE - FOR REFERENCE                     #
# ============================================================================ #


# packages -----
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
library(ciTools)
library(faraway)
library(ggpubr)


###### Observed vs expected (Negative Binomial Regression models)
###Forecast:
##Fit to 01/01/2017- 01/02/2019
##Forecast 01/03/2019 - 01/03/2020

load(here("1_DataPrep", "Data", "GeneralPop2017_20.RData"))


IR.overall <- inc_data_final %>% mutate(Month1 =paste(1,month, year, sep ="-")) %>% filter(denominator_cohort_id ==1)
IR.age_gender <- inc_data_final %>%  mutate(Month1 =paste(1,month, year, sep ="-"))  %>% filter(denominator_sex !="Both")


#IR.ses <- IR.ses %>% mutate(Month1 =paste(1,month, year, sep ="-"))
outcomes_to_fit<- inc_data_final %>% dplyr::select("outcome")%>% distinct()%>%pull()



# check that there are records in each of the combinations of outcome, age and sex
save_counts <- count(IR.age_gender, outcome, denominator_age_group, denominator_sex, name = "Freq") %>% print(n=Inf)


#### Overall----------
Sys.setlocale("LC_TIME", "English")
IR.overall$Date <- NA
IR.overall$Date <- dmy(IR.overall$Month1)
IR.overall$Month1 <- NULL
IR.age_gender$Date <- NA
IR.age_gender$Date <- dmy(IR.age_gender$Month1)
IR.age_gender$Month1 <- NULL

#IR.ses$Date <- NA
#IR.ses$Date <- dmy(IR.ses$Month1)
#IR.ses$Month1 <- NULL

###### Validation Overall: 
models.overall_validation <- list()
models.overall_validation.fit <- list()
models.overall_pred <- list()

end_mod <- 26 #month.since.start= Feb 2019
end_pred <- 38 #month.since.start= Feb 2020

# THIS RUNS, BUT COMES UP WITH THE FOLLOWING WARNING:

#Warning messages:
#1: In theta.ml(Y, mu, sum(w), w, limit = control$maxit, trace = control$trace >  ... :
#                iteration limit reached
#
# IT IS POSSIBLE THAT THIS IS BECAUSE THE NEGATIVE BINOMIAL METHOD IS BEST WHEN DATA ARE OVERDISPERSED, MY DATA ARE OVERDISPERSED.
# SO NEGATIVE BINOMIAL IS APPROPRIATE. THE OUTCOME HAS 152 ROWS AS WE ONLY PREDICT UP TO FEB 2020

for(j in 1:length(outcomes_to_fit)){
  
  # Negative Binomial
  working.nb <- glm.nb(events ~ as.factor(month)+months.since.start,data=IR.overall%>%  
                         filter(outcome==outcomes_to_fit[j]) %>% filter(months.since.start<= end_mod))
  models.overall_validation[[paste0("m.","overall",outcomes_to_fit[j], ".nb")]] <- working.nb
  
  pred <-predict(working.nb, newdata=IR.overall %>% filter(months.since.start<= end_pred)  %>%
                   filter(outcome==outcomes_to_fit[j]),type="response", se.fit = TRUE, interval= "prediction", level=0.95)
  
  models.overall_pred[[paste0("m.","overall",outcomes_to_fit[j], ".nb")]] <- cbind(IR.overall %>% filter(months.since.start<= end_pred) %>% filter(outcome==outcomes_to_fit[j]),
                                                                                   data.frame(est=as.character(pred$fit), model="nb")) %>%  add_pi(working.nb, names = c("lwr", "upr"), alpha = 0.05)
  
}


val_overall <- bind_rows(models.overall_pred) %>% mutate(ir_pred =pred*100000/months, lwr_pred=lwr*100000/months, upr_pred=upr*100000/months) %>%
  mutate_if(is.numeric, ~round(., 1)) %>% dplyr::select(-c(model, est))
save(val_overall, file=here("4_Results", db.name,  "Validation","Validation_model.RData"))


# with poisson instead of negative binomial
for(j in 1:length(outcomes_to_fit)){
  
  # Negative Binomial
  working.p <- glm(events ~ as.factor(month)+months.since.start,data=IR.overall%>%  
                         filter(outcome==outcomes_to_fit[j]) %>% filter(months.since.start<= end_mod), family=poisson)
  models.overall_validation[[paste0("m.","overall",outcomes_to_fit[j], ".nb")]] <- working.p
  
  pred <-predict(working.p, newdata=IR.overall %>% filter(months.since.start<= end_pred)  %>%
                   filter(outcome==outcomes_to_fit[j]),type="response", se.fit = TRUE, interval= "prediction", level=0.95)
  
  models.overall_pred[[paste0("m.","overall",outcomes_to_fit[j], ".nb")]] <- cbind(IR.overall %>% filter(months.since.start<= end_pred) %>% filter(outcome==outcomes_to_fit[j]),
                                                                                   data.frame(est=as.character(pred$fit), model="poisson")) %>%  add_pi(working.p, names = c("lwr", "upr"), alpha = 0.05)
  
}




val_overall_p <- bind_rows(models.overall_pred) %>% mutate(ir_pred =pred*100000/months, lwr_pred=lwr*100000/months, upr_pred=upr*100000/months) %>%
  mutate_if(is.numeric, ~round(., 1)) %>% dplyr::select(-c(model, est))
save(val_overall_p, file=here("4_Results", db.name,  "Validation","Validation_model.RData"))


rm(IR.overall, working.nb, models.overall_pred, models.overall_validation, models.overall_validation.fit)


###### Validation by age-gender: THIS DOES NOT WORK AS THERE ARE SOME CATEOGRIES THAT HAVE NO ROWS

age_to_fit <- IR.age_gender%>%  ungroup() %>%dplyr::select("denominator_age_group")%>% distinct()%>%pull()
gender_to_fit <-  IR.age_gender %>% ungroup() %>%dplyr::select("denominator_sex")%>% distinct()%>%pull()
models.age_gender <- list()
models.age_gender_pred <- list()

end_mod <- 26 #month.since.start= Feb 2019
end_pred <- 38 #month.since.start= Feb 2020


## - CHECK THIS - FOR SOME REASON THIS ONLY RUNS FOR BREAST CANCER, FEMALES, AGE 80-150, AND DOES NOT CREATE PREDICTIONS FOR THE OTHERS
# I THINK THIS IS BECAUSE THERE ARE SOME CANCERS THAT ARE ONLY IN MALES AND SOME ONLY IN FEMALES, SO THERE ARE SOME LEVELS WHERE SEX ONLY HAS ONE FACTOR WITH DATA
# AND SO THE MODEL DOES NOT RUN


for(j in 1:length(outcomes_to_fit)){
  for(i in 1:length(age_to_fit)){
    for(y in 1:length(gender_to_fit)){
      if (nrow(IR.age_gender%>% filter(denominator_sex==gender_to_fit[y]) %>% 
               filter(denominator_age_group==age_to_fit[i])%>% filter(outcome==outcomes_to_fit[j])) <1){ next }
          # this should filter out combinations where there is no data and skip to the next iteration, but it doesn't 
      
        working.nb <- glm.nb(events ~ as.factor(month), months.since.start,data=IR.age_gender%>% filter(months.since.start<= end_mod) %>% filter(denominator_sex==gender_to_fit[y]) %>% 
                             filter(denominator_age_group==age_to_fit[i])%>% filter(outcome==outcomes_to_fit[j]))


    models.age_gender[[paste0("m.",age_to_fit[i],gender_to_fit[y],".nb")]]  <- working.nb
      pred <-predict(working.nb, newdata=IR.age_gender%>% filter(months.since.start<= end_pred) %>% filter(denominator_sex==gender_to_fit[y]) %>% filter(denominator_age_group==age_to_fit[i])%>%
                       filter(outcome==outcomes_to_fit[j]),type="response", se.fit = TRUE, interval= "prediction", level=0.95)
      
      models.age_gender_pred[[paste0("m.",age_to_fit[i],gender_to_fit[y],outcomes_to_fit[j], ".nb")]] <- cbind(IR.age_gender %>% filter(months.since.start<= end_pred) %>% 
                                                                                                                 filter(outcome==outcomes_to_fit[j])%>% filter(denominator_sex==gender_to_fit[y]) %>% 
                                                                                                                 filter(denominator_age_group==age_to_fit[i]), data.frame(est=as.character(pred$fit), model="nb")) %>%  
        add_pi(working.nb, names = c("lwr", "upr"), alpha = 0.05) %>%
        mutate(ir_pred=pred*100000/months, lwr_pred=lwr*100000/months, upr_pred=upr*100000/months) %>%
        mutate(red = (100*(events-pred)/pred))
    }
  }
}


val_age_gender <- bind_rows(models.age_gender_pred) %>% mutate(ir_pred =pred*100000/months, lwr_pred=lwr*100000/months, upr_pred=upr*100000/months) %>% 
  mutate_if(is.numeric, ~round(., 1)) %>% dplyr::select(-c(model, est))
save(val_age_gender, file=here("4_Results", db.name,  "Validation", "Validation_age_gender.RData"))



# as a poisson


for(j in 1:length(outcomes_to_fit)){
  for(i in 1:length(age_to_fit)){
    for(y in 1:length(gender_to_fit)){
      if (nrow(IR.age_gender%>% filter(denominator_sex==gender_to_fit[y]) %>% 
               filter(denominator_age_group==age_to_fit[i])%>% filter(outcome==outcomes_to_fit[j])) <1){ next }
      # this should filter out combinations where there is no data and skip to the next iteration, but it doesn't 
      
      working.p <- glm(events ~ as.factor(month), months.since.start,data=IR.age_gender%>% filter(months.since.start<= end_mod) %>% filter(denominator_sex==gender_to_fit[y]) %>% 
                             filter(denominator_age_group==age_to_fit[i])%>% filter(outcome==outcomes_to_fit[j]), family="poisson")
      
      
      models.age_gender[[paste0("m.",age_to_fit[i],gender_to_fit[y],".nb")]]  <- working.p
      pred <-predict(working.p, newdata=IR.age_gender%>% filter(months.since.start<= end_pred) %>% filter(denominator_sex==gender_to_fit[y]) %>% filter(denominator_age_group==age_to_fit[i])%>%
                       filter(outcome==outcomes_to_fit[j]),type="response", se.fit = TRUE, interval= "prediction", level=0.95)
      
      models.age_gender_pred[[paste0("m.",age_to_fit[i],gender_to_fit[y],outcomes_to_fit[j], ".nb")]] <- cbind(IR.age_gender %>% filter(months.since.start<= end_pred) %>% 
                                                                                                                 filter(outcome==outcomes_to_fit[j])%>% filter(denominator_sex==gender_to_fit[y]) %>% 
                                                                                                                 filter(denominator_age_group==age_to_fit[i]), data.frame(est=as.character(pred$fit), model="poisson")) %>%  
        add_pi(working.p, names = c("lwr", "upr"), alpha = 0.05) %>%
        mutate(ir_pred=pred*100000/months, lwr_pred=lwr*100000/months, upr_pred=upr*100000/months) %>%
        mutate(red = (100*(events-pred)/pred))
    }
  }
}


val_age_gender <- bind_rows(models.age_gender_pred) %>% mutate(ir_pred =pred*100000/months, lwr_pred=lwr*100000/months, upr_pred=upr*100000/months) %>% 
  mutate_if(is.numeric, ~round(., 1)) %>% dplyr::select(-c(model, est))
save(val_age_gender, file=here("4_Results", db.name,  "Validation", "Validation_age_gender.RData"))



rm(IR.age_gender, models.age_gender,models.age_gender_pred, pred,working.nb, age_to_fit, gender_to_fit, i, j, y )



##### validation by age and sex with combined categories

# Create a dataset with age and sex categories combined

IR.age_gender_cat <- IR.age_gender %>% dplyr::mutate(age_sex_cat = case_when(denominator_cohort_id == 3 ~ "Female 0-150",
                                                                             denominator_cohort_id == 6 ~ "Female 0-19",
                                                                             denominator_cohort_id == 9 ~ "Female 20-39",
                                                                             denominator_cohort_id == 12 ~ "Female 40-59",
                                                                             denominator_cohort_id == 15 ~ "Female 60-79",
                                                                             denominator_cohort_id == 18 ~ "Female 80-150",
                                                                             denominator_cohort_id == 2 ~ "Male 0-150",
                                                                             denominator_cohort_id == 5 ~ "Male 0-19",
                                                                             denominator_cohort_id == 8 ~ "Male 20-39",
                                                                             denominator_cohort_id == 11 ~ "Male 40-59",
                                                                             denominator_cohort_id == 14 ~ "Male 60-79",
                                                                             denominator_cohort_id == 17 ~ "Male 80-150"))


# Create a dataset with age and sex categories combined

IR.age_female_cat <- IR.age_female %>% dplyr::mutate(age_sex_cat = case_when(denominator_cohort_id == 3 ~ "Female 0-150",
                                                                             denominator_cohort_id == 6 ~ "Female 0-19",
                                                                             denominator_cohort_id == 9 ~ "Female 20-39",
                                                                             denominator_cohort_id == 12 ~ "Female 40-59",
                                                                             denominator_cohort_id == 15 ~ "Female 60-79",
                                                                             denominator_cohort_id == 18 ~ "Female 80-150"))
###### Validation by age and sex - INITIAL SET UP  


IR.age_male <- inc_data_final %>%  mutate(Month1 =paste(1,month, year, sep ="-"))  %>% filter(denominator_sex =="Male")
IR.age_female <- inc_data_final %>%  mutate(Month1 =paste(1,month, year, sep ="-"))  %>% filter(denominator_sex =="Female")

outcomes_to_fit_male<-c("Lung","Colorectal","Prostate")
outcomes_to_fit_female<-c("Breast","Lung","Colorectal")

IR.age_male$Date <- NA
IR.age_male$Date <- dmy(IR.age_male$Month1)
IR.age_male$Month1 <- NULL
IR.age_female$Date <- NA
IR.age_female$Date <- dmy(IR.age_female$Month1)
IR.age_female$Month1 <- NULL

age_to_fit_male <- IR.age_male %>%  ungroup() %>%dplyr::select("denominator_age_group")%>% distinct()%>%pull()
models.age_male <- list()
models.age_male_pred <- list()

end_mod <- 26 #month.since.start= Feb 2019
end_pred <- 38 #month.since.start= Feb 2020


# FOR MALES ONLY - THIS RUNS FINE

for(j in 1:length(outcomes_to_fit_male)){
  for(i in 1:length(age_to_fit_male)){
      working.nb <- glm.nb(events ~ as.factor(month)+months.since.start,data=IR.age_male%>% filter(months.since.start<= end_mod) %>% 
                             filter(denominator_age_group==age_to_fit_male[i])%>% filter(outcome==outcomes_to_fit_male[j]))
      
      models.age_male[[paste0("m.",age_to_fit_male[i],".nb")]]  <- working.nb
      pred <-predict(working.nb, newdata=IR.age_male %>% filter(months.since.start<= end_pred) %>% filter(denominator_age_group==age_to_fit_male[i])%>%
                       filter(outcome==outcomes_to_fit_male[j]),type="response", se.fit = TRUE, interval= "prediction", level=0.95)
      
      models.age_male_pred[[paste0("m.",age_to_fit_male[i],outcomes_to_fit_male[j], ".nb")]] <- cbind(IR.age_male %>% filter(months.since.start<= end_pred) %>% 
                                                                                                                 filter(outcome==outcomes_to_fit_male[j])%>%  
                                                                                                                 filter(denominator_age_group==age_to_fit_male[i]), data.frame(est=as.character(pred$fit), model="nb")) %>%  
        add_pi(working.nb, names = c("lwr", "upr"), alpha = 0.05) %>%
        mutate(ir_pred=pred*100000/months, lwr_pred=lwr*100000/months, upr_pred=upr*100000/months) %>%
        mutate(red = (100*(events-pred)/pred))
    }
  }

val_age_male <- bind_rows(models.age_male_pred) %>% mutate(ir_pred =pred*100000/months, lwr_pred=lwr*100000/months, upr_pred=upr*100000/months) %>% 
  mutate_if(is.numeric, ~round(., 1)) %>% dplyr::select(-c(model, est))
save(val_age_male, file=here("4_Results", db.name,  "Validation", "Validation_age_male.RData"))

rm(IR.age_male, models.age_male,models.age_male_pred, pred,working.nb, age_to_fit_male)




###### Validation by age- FOR FEMALES ONLY - THIS DOES NOT RUN COLORECTAL CANCER

age_to_fit_female <- IR.age_female %>%  ungroup() %>%dplyr::select("denominator_age_group")%>% distinct()%>%pull()
models.age_female <- list()
models.age_female_pred <- list()

end_mod <- 26 #month.since.start= Feb 2019
end_pred <- 38 #month.since.start= Feb 2020


for(j in 1:length(outcomes_to_fit_female)){
  for(i in 1:length(age_to_fit_female)){
    
   if (nrow(IR.age_female %>%  filter(denominator_age_group==age_to_fit_female[i])%>% filter(outcome==outcomes_to_fit[j])) <1){ next } # this works to filter out categories with no rows
    
    working.nb <- glm.nb(events ~ as.factor(month)+months.since.start,data=IR.age_female%>% filter(months.since.start<= end_mod) %>% 
                           filter(denominator_age_group==age_to_fit_female[i])%>% filter(outcome==outcomes_to_fit_female[j]))
    
    if (working.nb$th.warn == "iteration limit reached") {
     working.nb <- glm(events~as.factor(month)+months.since.start,data=IR.age_female%>% filter(months.since.start<= end_mod) %>% 
    filter(denominator_age_group==age_to_fit_female[i])%>% filter(outcome==outcomes_to_fit_female[j]), family=poisson)
    } # this should make the model default to a poisson model if the data are not overdispersed in one of the category combinations, which is what is evident from the means/vars of some categories
    
        models.age_female[[paste0("m.",age_to_fit_female[i],".nb")]]  <- working.nb
    pred <-predict(working.nb, newdata=IR.age_female %>% filter(months.since.start<= end_pred) %>% filter(denominator_age_group==age_to_fit_female[i])%>%
                     filter(outcome==outcomes_to_fit_female[j]),type="response", se.fit = TRUE, interval= "prediction", level=0.95)
    
    models.age_female_pred[[paste0("m.",age_to_fit_female[i],outcomes_to_fit_female[j], ".nb")]] <- cbind(IR.age_female %>% filter(months.since.start<= end_pred) %>% 
                                                                                                      filter(outcome==outcomes_to_fit_female[j])%>%  
                                                                                                      filter(denominator_age_group==age_to_fit_female[i]), data.frame(est=as.character(pred$fit), model="nb")) %>%  
      add_pi(working.nb, names = c("lwr", "upr"), alpha = 0.05) %>%
      mutate(ir_pred=pred*100000/months, lwr_pred=lwr*100000/months, upr_pred=upr*100000/months) %>%
      mutate(red = (100*(events-pred)/pred))
  }
}

val_age_female <- bind_rows(models.age_female_pred) %>% mutate(ir_pred =pred*100000/months, lwr_pred=lwr*100000/months, upr_pred=upr*100000/months) %>% 
  mutate_if(is.numeric, ~round(., 1)) %>% dplyr::select(-c(model, est))
save(val_age_female, file=here("4_Results", db.name,  "Validation", "Validation_age_female.RData"))

rm(IR.age_female, models.age_female,models.age_female_pred, pred,working.nb, age_to_fit_female, i, j)


# run this separately by each cancer

# Breast

age_to_fit_female <- IR.age_female %>%  ungroup() %>%dplyr::select("denominator_age_group")%>% distinct()%>%pull()
models.age_female <- list()
models.age_female_pred <- list()



for(i in 1:length(age_to_fit_female)){
  
  working.nb <- glm.nb(events ~ as.factor(month)+months.since.start,data=IR.age_female%>% filter(months.since.start<= end_mod) %>% 
                         filter(denominator_age_group==age_to_fit_female[i])%>% filter(outcome=="Breast"))
  
  models.age_female[[paste0("m.",age_to_fit_female[i],".nb")]]  <- working.nb
  pred <-predict(working.nb, newdata=IR.age_female %>% filter(months.since.start<= end_pred) %>% filter(denominator_age_group==age_to_fit_female[i])%>%
                   filter(outcome=="Breast"),type="response", se.fit = TRUE, interval= "prediction", level=0.95)
  
  models.age_female_pred[[paste0("m.",age_to_fit_female[i],"Breast", ".nb")]] <- cbind(IR.age_female %>% filter(months.since.start<= end_pred) %>% 
                                                                                         filter(outcome=="Breast")%>%  
                                                                                         filter(denominator_age_group==age_to_fit_female[i]), data.frame(est=as.character(pred$fit), model="nb")) %>%  
    add_pi(working.nb, names = c("lwr", "upr"), alpha = 0.05) %>%
    mutate(ir_pred=pred*100000/months, lwr_pred=lwr*100000/months, upr_pred=upr*100000/months) %>%
    mutate(red = (100*(events-pred)/pred))
}

val_age_female_breast <- bind_rows(models.age_female_pred) %>% mutate(ir_pred =pred*100000/months, lwr_pred=lwr*100000/months, upr_pred=upr*100000/months) %>% 
  mutate_if(is.numeric, ~round(., 1)) %>% dplyr::select(-c(model, est))

rm(models.age_female,models.age_female_pred, pred,working.nb, age_to_fit_female, i)

# Colorectal

age_to_fit_female <- IR.age_female %>%  ungroup() %>%dplyr::select("denominator_age_group")%>% distinct()%>%pull()
models.age_female <- list()
models.age_female_pred <- list()

end_mod <- 26 #month.since.start= Feb 2019
end_pred <- 38 #month.since.start= Feb 2020


  for(i in 1:length(age_to_fit_female)){
    
      working.nb <- glm.nb(events ~ as.factor(month)+months.since.start,data=IR.age_female%>% filter(months.since.start<= end_mod) %>% 
                           filter(denominator_age_group==age_to_fit_female[i])%>% filter(outcome=="Colorectal"))
    
    models.age_female[[paste0("m.",age_to_fit_female[i],".nb")]]  <- working.nb
    pred <-predict(working.nb, newdata=IR.age_female %>% filter(months.since.start<= end_pred) %>% filter(denominator_age_group==age_to_fit_female[i])%>%
                     filter(outcome=="Colorectal"),type="response", se.fit = TRUE, interval= "prediction", level=0.95)
    
    models.age_female_pred[[paste0("m.",age_to_fit_female[i],"Colorectal", ".nb")]] <- cbind(IR.age_female %>% filter(months.since.start<= end_pred) %>% 
                                                                                                            filter(outcome=="Colorectal")%>%  
                                                                                                            filter(denominator_age_group==age_to_fit_female[i]), data.frame(est=as.character(pred$fit), model="nb")) %>%  
      add_pi(working.nb, names = c("lwr", "upr"), alpha = 0.05) %>%
      mutate(ir_pred=pred*100000/months, lwr_pred=lwr*100000/months, upr_pred=upr*100000/months) %>%
      mutate(red = (100*(events-pred)/pred))
  }


val_age_female_colorectal <- bind_rows(models.age_female_pred) %>% mutate(ir_pred =pred*100000/months, lwr_pred=lwr*100000/months, upr_pred=upr*100000/months) %>% 
  mutate_if(is.numeric, ~round(., 1)) %>% dplyr::select(-c(model, est))

rm(models.age_female,models.age_female_pred, pred,working.nb, age_to_fit_female, i)



# Lung

age_to_fit_female <- IR.age_female %>%  ungroup() %>%dplyr::select("denominator_age_group")%>% distinct()%>%pull()
models.age_female <- list()
models.age_female_pred <- list()

for(i in 1:length(age_to_fit_female)){
  
  working.nb <- glm.nb(events ~ as.factor(month)+months.since.start,data=IR.age_female%>% filter(months.since.start<= end_mod) %>% 
                         filter(denominator_age_group==age_to_fit_female[i])%>% filter(outcome=="Lung"))
  
  models.age_female[[paste0("m.",age_to_fit_female[i],".nb")]]  <- working.nb
  pred <-predict(working.nb, newdata=IR.age_female %>% filter(months.since.start<= end_pred) %>% filter(denominator_age_group==age_to_fit_female[i])%>%
                   filter(outcome=="Lung"),type="response", se.fit = TRUE, interval= "prediction", level=0.95)
  
  models.age_female_pred[[paste0("m.",age_to_fit_female[i],"Lung", ".nb")]] <- cbind(IR.age_female %>% filter(months.since.start<= end_pred) %>% 
                                                                                         filter(outcome=="Lung")%>%  
                                                                                         filter(denominator_age_group==age_to_fit_female[i]), data.frame(est=as.character(pred$fit), model="nb")) %>%  
    add_pi(working.nb, names = c("lwr", "upr"), alpha = 0.05) %>%
    mutate(ir_pred=pred*100000/months, lwr_pred=lwr*100000/months, upr_pred=upr*100000/months) %>%
    mutate(red = (100*(events-pred)/pred))
}

val_age_female_lung <- bind_rows(models.age_female_pred) %>% mutate(ir_pred =pred*100000/months, lwr_pred=lwr*100000/months, upr_pred=upr*100000/months) %>% 
  mutate_if(is.numeric, ~round(., 1)) %>% dplyr::select(-c(model, est))

rm(models.age_female,models.age_female_pred, pred,working.nb, age_to_fit_female, i)


# combine the rows from the 3 cancer outputs for females

val_age_female <- rbind(val_age_female_breast, val_age_female_colorectal, val_age_female_lung)


save(val_age_female, file=here("4_Results", db.name,  "Validation", "Validation_age_female.RData"))

rm(IR.age_female, models.age_female,models.age_female_pred, pred,working.nb, age_to_fit_female)





###### Validation by SES: run this when we have the data
ses_to_fit <- IR.ses %>%  ungroup() %>%dplyr::select("medea")%>% distinct()%>%pull()
models.ses_pred <- list()
models.ses <- list()
end_mod <- 26 #month.since.start= Feb 2019
end_pred <- 38 #month.since.start= Feb 2020

for(j in 1:length(outcomes_to_fit)){
  for(i in 1:(length(ses_to_fit))){
    working.nb <- glm.nb(events ~ as.factor(month)+months.since.start,data=IR.ses%>% filter(months.since.start<= end_mod) %>% filter(medea==ses_to_fit[i]) %>% 
                           filter(outcome==outcomes_to_fit[j]))
    models.ses[[paste0("m.",ses_to_fit[i],".nb")]]  <- working.nb
    pred <-predict(working.nb, newdata=IR.ses%>% filter(months.since.start<= end_pred) %>% filter(medea==ses_to_fit[i])%>%
                     filter(outcome==outcomes_to_fit[j]),type="response", se.fit = TRUE, interval= "prediction", level=0.95)
    
    models.ses_pred[[paste0("m.",ses_to_fit[i],outcomes_to_fit[j], ".nb")]] <- cbind(IR.ses %>% filter(months.since.start<= end_pred) %>% filter(outcome==outcomes_to_fit[j])%>% 
                                                                                       filter(medea==ses_to_fit[i]), data.frame(est=as.character(pred$fit), model="nb")) %>%  
      add_pi(working.nb, names = c("lwr", "upr"), alpha = 0.05) %>%
      mutate(ir_pred=pred*100000/months, lwr_pred=lwr*100000/months, upr_pred=upr*100000/months) %>%
      mutate(red = (100*(events-pred)/pred))
  }
}


val_ses <- bind_rows(models.ses_pred) %>% mutate(ir_pred =pred*100000/months, lwr_pred=lwr*100000/months, upr_pred=upr*100000/months)%>% 
  mutate_if(is.numeric, ~round(., 1)) %>% dplyr::select(-c(model, est))
save(val_ses, file=here("4_Results", db.name,  "Validation", "Validation_ses.RData"))

rm(IR.ses, models.ses,models.ses_pred, pred,working.nb, ses_to_fit,
   end_mod,end_pred, i,j,outcomes_to_fit,y)




##### PLOTS--------

# overall
# Breast

overall_Breast <- val_overall %>% filter(outcome=="Breast")%>%
  ggplot()+
  geom_point(aes(Date,ir_m, colour= "Observed"))+
  geom_line(aes(Date,ir_m,colour= "Observed"))+
  
  geom_point(aes(Date,ir_pred,colour= "Expected"))+
  geom_line(aes(Date,ir_pred,colour= "Expected"))+
  geom_ribbon(aes(ymin = lwr_pred,ymax = upr_pred, x=Date),  fill = "blue", alpha = 0.1)+
  scale_color_manual(name= "", values=c(Observed="red", Expected="blue"))+
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 month", limits= c(as.Date("2017-01-01"),as.Date("2020-03-01")),expand=c(0.005,0.005))+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

overall_Breast <- overall_Breast + 
  theme(axis.text.x = element_text(angle=90), 
        axis.title.y = element_text(size = 9),    
        plot.margin=grid::unit(c(1,1,0,1), "cm"))+
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-01"))),linetype=2, color="black")+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("")#+
#ggtitle("Expected vs. Observed incidence rates for breast cancer from validation model")

# Colorectal
overall_Colorectal <- val_overall %>% filter(outcome=="Colorectal")%>%
  ggplot()+
  geom_point(aes(Date,ir_m, colour= "Observed"))+
  geom_line(aes(Date,ir_m,colour= "Observed"))+
  
  geom_point(aes(Date,ir_pred,colour= "Expected"))+
  geom_line(aes(Date,ir_pred,colour= "Expected"))+
  geom_ribbon(aes(ymin = lwr_pred,ymax = upr_pred, x=Date),  fill = "blue", alpha = 0.1)+
  scale_color_manual(name= "", values=c(Observed="red", Expected="blue"))+
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 month", limits= c(as.Date("2017-01-01"),as.Date("2020-03-01")),expand=c(0.005,0.005))+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

overall_Colorectal <- overall_Colorectal + 
  theme(axis.text.x = element_text(angle=90), 
        axis.title.y = element_text(size = 9),    
        plot.margin=grid::unit(c(1,1,0,1), "cm"))+
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-01"))),linetype=2, color="black")+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("")#+
#ggtitle("Expected vs. Observed incidence rates for colorectal cancer from validation model")


# Lung
overall_Lung <- val_overall %>% filter(outcome=="Lung")%>%
  ggplot()+
  geom_point(aes(Date,ir_m, colour= "Observed"))+
  geom_line(aes(Date,ir_m,colour= "Observed"))+
  
  geom_point(aes(Date,ir_pred,colour= "Expected"))+
  geom_line(aes(Date,ir_pred,colour= "Expected"))+
  geom_ribbon(aes(ymin = lwr_pred,ymax = upr_pred, x=Date),  fill = "blue", alpha = 0.1)+
  scale_color_manual(name= "", values=c(Observed="red", Expected="blue"))+
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 month", limits= c(as.Date("2017-01-01"),as.Date("2020-03-01")),expand=c(0.005,0.005))+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

overall_Lung <- overall_Lung + 
  theme(axis.text.x = element_text(angle=90), 
        axis.title.y = element_text(size = 9),    
        plot.margin=grid::unit(c(1,1,0,1), "cm"))+
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-01"))),linetype=2, color="black")+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("")#+
#ggtitle("Expected vs. Obsereved incidence rates for Lung cancer from validation model")

# Prostate
overall_Prostate <- val_overall %>% filter(outcome=="Prostate")%>%
  ggplot()+
  geom_point(aes(Date,ir_m, colour= "Observed"))+
  geom_line(aes(Date,ir_m,colour= "Observed"))+
  
  geom_point(aes(Date,ir_pred,colour= "Expected"))+
  geom_line(aes(Date,ir_pred,colour= "Expected"))+
  geom_ribbon(aes(ymin = lwr_pred,ymax = upr_pred, x=Date),  fill = "blue", alpha = 0.1)+
  scale_color_manual(name= "", values=c(Observed="red", Expected="blue"))+
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 month", limits= c(as.Date("2017-01-01"),as.Date("2020-03-01")),expand=c(0.005,0.005))+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

overall_Prostate <- overall_Prostate + 
  theme(axis.text.x = element_text(angle=90), 
        axis.title.y = element_text(size = 9),    
        plot.margin=grid::unit(c(1,1,0,1), "cm"))+
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-01"))),linetype=2, color="black")+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("")#+
#ggtitle("Expected vs. Observed incidence rates for prostate cancer from validation model")

figure_Validation_overall<-ggarrange(overall_Breast, overall_Colorectal, overall_Lung, overall_Prostate, 
                          align="hv", ncol=2, nrow=2,
                          labels = c("A) Breast Cancer", "B) Colorectal Cancer", "C) Lung Cancer", "D) Prostate Cancer"),font.label = list(size = 12),
                          hjust = c(-0.25,-0.25),
                          common.legend=TRUE, legend="right" )



## Plots stratified by Age and gender
# Breast #  this returns all NAs so something is not right here with the class of the variables
val_age_female$denominator_sex <- factor(val_age_female$denominator_sex, levels=rev(levels(val_age_female$denominator_sex)))
levels(val_age_female$denominator_sex) <- c("Female", "Male", "Both")
levels(val_age_female$denominator_age_group) <- c("40;59", "60;79", "0;150", "80;150")

age_female_Breast <- val_age_female  %>% 
  filter(outcome=="Breast") %>% 
  ggplot()+
  facet_wrap(val_age_female$denominator_age_group,scales="free")+
  geom_point(aes(Date,ir_m, colour= "Observed"))+
  geom_line(aes(Date,ir_m,colour= "Observed"))+
  
  geom_point(aes(Date,ir_pred,colour= "Expected"))+
  geom_line(aes(Date,ir_pred,colour= "Expected"))+
  geom_ribbon(aes(ymin = lwr_pred,ymax = upr_pred, x=Date),  fill = "blue", alpha = 0.1)+
  scale_color_manual(name= "", values=c(Observed="red", Expected="blue"))+
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "4 month")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )



age_female_Breast <-age_female_Breast+ 
  theme(axis.text.x = element_text(angle=90),
        axis.title.y = element_text(size = 9),
        plot.margin=grid::unit(c(1,0.5,0,1), "cm") )+
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-01"))),linetype=2, color="black")+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("")



## ADD ALL OTHER PLOTS FOR THE OTHER CANCERS STRATIFIED BY AGE AND SEX HERE TOO WHEN THE ABOVE IS CORRECT

# Males
#Colorectal

val_age_male$denominator_sex <- factor(val_age_male$denominator_sex, levels=rev(levels(val_age_male$denominator_sex)))
levels(val_age_male$denominator_sex) <- c("Female", "Male", "Both")
levels(val_age_male$denominator_age_group) <- c("40;59", "60;79", "0;150", "80;150")

val_age_male$denominator_age_group <- as.factor(val_age_male$denominator_age_group)

age_male_colorectal <- val_age_male  %>% 
  filter(outcome=="Colorectal") %>% 
  ggplot()+
  facet_grid(col = vars(val_age_male$denominator_age_group))+
  geom_point(aes(Date,ir_m, colour= "Observed"))+
  geom_line(aes(Date,ir_m,colour= "Observed"))+
  
  geom_point(aes(Date,ir_pred,colour= "Expected"))+
  geom_line(aes(Date,ir_pred,colour= "Expected"))+
  geom_ribbon(aes(ymin = lwr_pred,ymax = upr_pred, x=Date),  fill = "blue", alpha = 0.1)+
  scale_color_manual(name= "", values=c(Observed="red", Expected="blue"))+
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "4 month")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )

age_male_colorectal <-age_male_colorectal+ 
  theme(axis.text.x = element_text(angle=90),
        axis.title.y = element_text(size = 9),
        plot.margin=grid::unit(c(1,0.5,0,1), "cm") )+
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-01"))),linetype=2, color="black")+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("")

age_male_colorectal




# Males
# Lung

val_age_male$denominator_sex <- factor(val_age_male$denominator_sex, levels=rev(levels(val_age_male$denominator_sex)))
levels(val_age_male$denominator_sex) <- c("Female", "Male", "Both")
levels(val_age_male$denominator_age_group) <- c("40;59", "60;79", "0;150", "80;150")

val_age_male$denominator_age_group <- as.factor(val_age_male$denominator_age_group)

age_male_lung <- val_age_male  %>% 
  filter(outcome=="Lung") %>% 
  ggplot()+
  facet_grid(col = vars(val_age_male$denominator_age_group))+
  geom_point(aes(Date,ir_m, colour= "Observed"))+
  geom_line(aes(Date,ir_m,colour= "Observed"))+
  
  geom_point(aes(Date,ir_pred,colour= "Expected"))+
  geom_line(aes(Date,ir_pred,colour= "Expected"))+
  geom_ribbon(aes(ymin = lwr_pred,ymax = upr_pred, x=Date),  fill = "blue", alpha = 0.1)+
  scale_color_manual(name= "", values=c(Observed="red", Expected="blue"))+
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "4 month")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )

age_male_lung <-age_male_lung+ 
  theme(axis.text.x = element_text(angle=90),
        axis.title.y = element_text(size = 9),
        plot.margin=grid::unit(c(1,0.5,0,1), "cm") )+
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-01"))),linetype=2, color="black")+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("")

age_male_lung




figure_age_gender <-ggarrange(age_gender_Breast, age_gender_Colorectal, age_gender_Lung, age_gender_Prostate,
                              align="hv", ncol=2, nrow=2,
                              labels = c("A) Breast Cancer", "B) Colorectal Cancer", "C) Lung Cancer", "D) Prostate Cancer"),font.label = list(size = 12),
                              hjust = c(-0.25,-0.25),
                              common.legend=TRUE, legend="right" )



# add this when we have the SES data
# SES
prediction_ses$medea <- factor(prediction_ses$medea, levels=c("U1", "U2", "U3", "U4", "U5", "R"))
plot_ses_Breast <- prediction_ses%>%
  filter(outcome=="Breast") %>% filter(months.since.start >=13)%>%
  ggplot()+
  facet_grid(.~medea,scales="free")+
  geom_point(aes(Date,ir_m, colour= "Observed"))+
  geom_line(aes(Date,ir_m,colour= "Observed"))+
  
  geom_point(aes(Date,ir_pred,colour= "Expected"))+
  geom_line(aes(Date,ir_pred,colour= "Expected"))+
  geom_ribbon(aes(ymin = lwr_pred,ymax = upr_pred, x=Date),  fill = "blue", alpha = 0.1)+
  scale_color_manual(name= "", values=c(Observed="red", Expected="blue"))+
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "4 month")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )


plot_ses_Breast <- plot_ses_Breast+
  theme(axis.text.x=element_text(angle=90, hjust=1),
        axis.title.y = element_text(size = 9),
        plot.margin=grid::unit(c(1,0.5,0,1), "cm") )+
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-01"))),
             linetype=2, color="black")+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("")

# ADD ALL OTHER PLOTS FOR THE OTHER CANCERS STRATIFIED BY SES HERE TOO WHEN THE ABOVE IS CORRECT

figure_ses <-ggarrange(plot_ses_Breast, plot_ses_Colorectal, plot_ses_Lung, plot_ses_Prostate,
                       align="hv", ncol=1, nrow=2,
                       labels = c("A) Breast Cancer", "B) Colorectal Cancer", "C) Lung Cancer", "D) Prostate Cancer"),font.label = list(size = 12),
                       hjust = c(-0.25,-0.25),
                       common.legend=TRUE, legend="right" )


# Save
ggsave(here("4_Results", db.name, "Plots", "Figure_1_validation_overall.jpg"), figure_Validation_overall, dpi=300, scale = 2, width = 12, height = 9)
ggsave(here("4_Results", db.name, "Plots", "Figure_2_age_gender.jpg"), figure_age_gender, dpi=300, scale = 2)
ggsave(here("4_Results", db.name, "Plots", "Figure_3_ses.jpg"), figure_ses, dpi=300, scale = 2)


rm(figure_overall, figure_age_gender,figure_ses, prediction_age.gender,predicion_overall, predicition_ses, 
   end_mod, j, outcomes_to_fit)
