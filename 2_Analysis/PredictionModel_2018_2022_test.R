# ============================================================================ #
#                         NEGATIVE BINOMIAL REGRESSION                         #
#                            FOR CANCER/COVID STUDY                            #
#                               PREDICTION MODEL                               #
#                                Nicola Barclay                                #
#                                 17-01-2023                                   #
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
##Fit to 01/01/2018- 01/02/2020
##Forecast 01/03/2020 - 01/01/2022

load(here("1_DataPrep", "Data", "GeneralPop2018_22.RData"))


IR.overall <- inc_data_pred_final %>% mutate(Month1 =paste(1,month, year, sep ="-")) %>% filter(denominator_cohort_id ==1)

# remove data from Dec 2021 to see if the model still over predicts the last iteration
IR.overall <- IR.overall %>% filter(IR.overall$month_year != "12/2021")


IR.age_gender <- inc_data_pred_final %>%  mutate(Month1 =paste(1,month, year, sep ="-"))
#IR.ses <- IR.ses %>% mutate(Month1 =paste(1,month, year, sep ="-")) 
outcomes_to_fit<- inc_data_pred_final %>% dplyr::select("outcome")%>% distinct()%>%pull()


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


models <- list()
models_pred <- list()
models_period<- list()
models_total <- list()
models_post <- list()

IR.overall$covid2 <- NA
IR.overall$covid2[which(IR.overall$covid =="Pre-COVID")] <- "Pre-COVID"
IR.overall$covid2[which(IR.overall$covid =="Lockdown")] <- "Lockdown"
IR.overall$covid2[which(IR.overall$months.since.start >=31)] <- "Post-COVID"

end_mod <- 26 #month.since.start= Feb 2020 assuming start date of 2018

# filter out data from 2017 as I am using data from 2018 for this modelling 
IR.overall <- IR.overall %>% filter(IR.overall$months.since.start>=1)

for(j in 1:length(outcomes_to_fit)){
  
  working.nb <- glm.nb(events ~ as.factor(month)+months.since.start,data=IR.overall%>% filter(months.since.start<= end_mod) %>%
                         filter(outcome==outcomes_to_fit[j]))
  models[[paste0("m.",".nb")]]  <- working.nb
  pred <-predict(working.nb, newdata=IR.overall%>% 
                   filter(outcome==outcomes_to_fit[j]),type="response", se.fit = TRUE, interval= "prediction", level=0.95)
  models_pred[[paste0("m.",outcomes_to_fit[j], ".nb")]] <- cbind(IR.overall %>%  filter(outcome==outcomes_to_fit[j]), data.frame(est=as.character(pred$fit), model="nb")) %>%  
    add_pi(working.nb, names = c("lwr", "upr"), alpha = 0.05)  %>%  
    mutate(ir_pred=pred*100000/months, lwr_pred=lwr*100000/months, upr_pred=upr*100000/months) %>%
    mutate(red = (100*(pred-events)/pred)) %>% # this is the proportion of events reduced in the predictive modelling
    mutate(red_lwr = (100*(lwr-events)/lwr))%>%
    mutate(red_upr = (100*(upr-events)/upr))
  models_period[[paste0("m.",outcomes_to_fit[j], ".nb")]] <-models_pred[[paste0("m.",outcomes_to_fit[j], ".nb")]] %>% 
    group_by(covid, outcome)%>%filter(covid != "Pre-COVID")%>% 
    summarise(events_t= sum(events), pred_t= sum(pred), lwr_t =sum(lwr), upr_t=sum(upr)) %>%
    mutate(red = (100*(pred_t-events_t)/pred_t)) %>%
    mutate(red_lwr = (100*(lwr_t-events_t)/lwr_t))%>%
    mutate(red_upr = (100*(upr_t-events_t)/upr_t))
  models_total[[paste0("m.",outcomes_to_fit[j], ".nb")]] <-  models_pred[[paste0("m.",outcomes_to_fit[j], ".nb")]] %>% 
    filter(covid != "Pre-COVID")%>% group_by(outcome)%>%
    summarise(events_t= sum(events), pred_t= sum(pred), lwr_t =sum(lwr), upr_t=sum(upr)) %>%
    mutate(covid="Total")%>%
    mutate(red = (100*(pred_t-events_t)/pred_t)) %>%
    mutate(red_lwr = (100*(lwr_t-events_t)/lwr_t))%>%
    mutate(red_upr = (100*(upr_t-events_t)/upr_t))
  
  models_post[[paste0("m.",outcomes_to_fit[j], ".nb")]] <-  models_pred[[paste0("m.",outcomes_to_fit[j], ".nb")]]%>% 
    filter(covid2 == "Post-COVID")%>% group_by(outcome)%>%
    summarise(events_t= sum(events), pred_t= sum(pred), lwr_t =sum(lwr), upr_t=sum(upr)) %>%
    mutate(covid="Post")%>% 
    mutate(red = (100*(pred_t-events_t)/pred_t)) %>%
    mutate(red_lwr = (100*(lwr_t-events_t)/lwr_t))%>%
    mutate(red_upr = (100*(upr_t-events_t)/upr_t))
  
}


prediction_overall<- bind_rows(models_pred)  %>% mutate_if(is.numeric, ~round(., 1))
predicton_overall_periods<- as.data.frame(rbind(bind_rows(models_period), bind_rows(models_total), bind_rows(models_post))) %>% arrange(outcome, covid) %>%
  mutate(red = round(red,1), pred_t=round(pred_t, 0), red_lwr =round(red_lwr,1), red_upr=round(red_upr,1))
tab <- predicton_overall_periods %>%  mutate(pred = paste0(paste(pred_t)," (", paste(lwr_t), " to ", paste(upr_t), ")")) %>%
  mutate(red_perc = paste0(paste(red)," (", paste(red_lwr)," to ", paste(red_upr), ")")) %>% dplyr::select(covid, outcome, pred, events_t, red_perc)
tab_red <- tab %>%
  dplyr::select(covid, outcome,red_perc)%>%
  spread(key=covid,value=red_perc) %>% 
  mutate(value = "Red_perc")  %>% # reduction percentage
  relocate("Post", .after= "Legal restrictions removed")
tab_dif <- predicton_overall_periods %>%
  mutate(dif=pred_t-events_t)%>%
  mutate(dif_lwr= lwr_t-events_t)%>%
  mutate(dif_upr = upr_t-events_t)%>%
  mutate(under_dx = paste0(paste(dif)," (", paste(dif_lwr)," to ", paste(dif_upr), ")"))%>%
  dplyr::select(covid, outcome,under_dx) %>%
  spread(key=covid,value=under_dx) %>% 
  mutate(value = "Underdx") %>% # under diagnosed
  relocate("Post", .after= "Legal restrictions removed")

tab1 <-rbind(tab_red, tab_dif)

write.csv(tab1, file=here("4_Results", db.name, "Modelling", "overall_table_remove_dec_2021.csv"))
write.csv(tab, file=here("4_Results", db.name, "Modelling", "overall_red_remove_dec_2021.csv"))
save(prediction_overall, file=here("4_Results", db.name,  "Modelling", "Prediction_Overall_2018-2022_removing_dec_2021.RData"))
rm(IR.overall,models, models_period, models_post, models_pred, models_total, pred,
   prediction_overall_periods, tab, working.nb, predicton_overall_periods)



####Age and gender------- RUN THIS WHEN WE HAVE THE FULL DATASET. IT MIGHT BE THAT THE TRIPLE LOOP WON'T RUN AND
# EACH SEX AND CANCER WILL NEED TO BE RUN SEPARATELY
age_to_fit <- IR.age_gender%>%  ungroup() %>%dplyr::select("denominator_age_group")%>% distinct()%>%pull()
gender_to_fit <-  IR.age_gender %>% ungroup() %>%dplyr::select("denominator_sex")%>% distinct()%>%pull()
models.age_gender <- list()
models.age_gender_pred <- list()
period.age_gender <- list()
models.age_gender_period <- list()
models.age_gender_total <- list()
models.age_gender_post <- list()
IR.age_gender$covid2 <- NA
IR.age_gender$covid2[which(IR.age_gender$covid =="Pre-COVID")] <- "Pre-COVID"
IR.age_gender$covid2[which(IR.age_gender$covid =="Lockdown")] <- "Lockdown"
IR.age_gender$covid2[which(IR.age_gender$months.since.start >=31)] <- "Post-COVID"

end_mod <- 26 #month.since.start= Feb 2020 assuming start date in Jan 2018


## FOR SOME REASON THIS ONLY RUNS FOR LUNG CANCER AND DOES NOT CREATE PREDICTIONS FOR THE OTHERS
for(j in 1:length(outcomes_to_fit)){
  for(i in 1:length(age_to_fit)){
    for(y in 1:length(gender_to_fit)){
      working.nb <- glm.nb(events ~ as.factor(month)+months.since.start,data=IR.age_gender%>% filter(months.since.start<= end_mod) %>% filter(denominator_sex==gender_to_fit[y]) %>% 
                             filter(denominator_age_group==age_to_fit[i])%>% filter(outcome==outcomes_to_fit[j]))
      models.age_gender[[paste0("m.",age_to_fit[i],gender_to_fit[y],".nb")]]  <- working.nb
      pred <-predict(working.nb, newdata=IR.age_gender%>%  filter(denominator_sex==gender_to_fit[y]) %>% filter(denominator_age_group==age_to_fit[i])%>%
                       filter(outcome==outcomes_to_fit[j]),type="response", se.fit = TRUE, interval= "prediction", level=0.95)
      
      models.age_gender_pred[[paste0("m.",age_to_fit[i],gender_to_fit[y],outcomes_to_fit[j], ".nb")]] <- cbind(IR.age_gender %>%  filter(outcome==outcomes_to_fit[j])%>% filter(denominator_sex==gender_to_fit[y]) %>% 
                                                                                                                 filter(denominator_age_group==age_to_fit[i]), data.frame(est=as.character(pred$fit), model="nb")) %>%  
        add_pi(working.nb, names = c("lwr", "upr"), alpha = 0.05) %>%
        mutate(ir_pred=pred*100000/months, lwr_pred=lwr*100000/months, upr_pred=upr*100000/months)
      
      models.age_gender_period[[paste0("m.",age_to_fit[i],gender_to_fit[y],outcomes_to_fit[j], ".nb")]] <-models.age_gender_pred[[paste0("m.",age_to_fit[i],gender_to_fit[y],outcomes_to_fit[j], ".nb")]] %>% 
        group_by(covid, outcome, denominator_sex, denominator_age_group)%>%filter(covid != "Pre-COVID")%>% 
        summarise(events_t= sum(events), pred_t= sum(pred), lwr_t =sum(lwr), upr_t=sum(upr)) %>%
        mutate(red = (100*(pred_t-events_t)/pred_t)) %>%
        mutate(red_lwr = (100*(lwr_t-events_t)/lwr_t))%>%
        mutate(red_upr = (100*(upr_t-events_t)/upr_t))
      
      models.age_gender_total[[paste0("m.",age_to_fit[i],gender_to_fit[y],outcomes_to_fit[j], ".nb")]] <-  models.age_gender_pred[[paste0("m.",age_to_fit[i],gender_to_fit[y],outcomes_to_fit[j], ".nb")]]%>% 
        filter(covid != "Pre-COVID")%>% group_by(outcome, denominator_sex, denominator_age_group)%>%
        summarise(events_t= sum(events), pred_t= sum(pred), lwr_t =sum(lwr), upr_t=sum(upr)) %>%
        mutate(covid="Total")%>%
        mutate(red = (100*(pred_t-events_t)/pred_t)) %>%
        mutate(red_lwr = (100*(lwr_t-events_t)/lwr_t))%>%
        mutate(red_upr = (100*(upr_t-events_t)/upr_t))
      
      models.age_gender_post[[paste0("m.",age_to_fit[i],gender_to_fit[y],outcomes_to_fit[j], ".nb")]] <-  models.age_gender_pred[[paste0("m.",age_to_fit[i],gender_to_fit[y],outcomes_to_fit[j], ".nb")]]%>% 
        filter(covid2 == "Post-COVID")%>% group_by(outcome, denominator_sex, denominator_age_group)%>%
        summarise(events_t= sum(events), pred_t= sum(pred), lwr_t =sum(lwr), upr_t=sum(upr)) %>%
        mutate(covid="Post")%>%
        mutate(red = (100*(pred_t-events_t)/pred_t)) %>%
        mutate(red_lwr = (100*(lwr_t-events_t)/lwr_t))%>%
        mutate(red_upr = (100*(upr_t-events_t)/upr_t))
      
      
    }
  }
}


prediction_age.gender <- bind_rows(models.age_gender_pred)%>% mutate_if(is.numeric, ~round(., 1)) 
predicton_periods_age_gender <- as.data.frame(rbind(bind_rows(models.age_gender_period), bind_rows(models.age_gender_total),bind_rows(models.age_gender_post)))%>% arrange(outcome, covid) %>%
  mutate(red = round(red,1), pred_t=round(pred_t, 0),red_lwr = round(red_lwr,1), red_upr = round(red_upr,1))

tab <- predicton_periods_age_gender %>% arrange(denominator_sex)%>%  mutate(pred = paste0(paste(pred_t),"(", paste(lwr_t, upr_t), ")")) %>%
  mutate(red_perc = paste0(paste(red)," (", paste(red_lwr), " to ", paste(red_upr), ")")) %>% dplyr::select(covid, outcome, denominator_sex, denominator_age_group, pred, events_t, red_perc)
tab_red <- tab %>%
  dplyr::select(covid, denominator_age_group, denominator_sex, outcome,red_perc)%>%
  spread(key=covid,value=red_perc) %>%
  arrange(outcome,denominator_sex)%>%
  mutate(value="Red_perd") #%>%
  #relocate("Post", .after= "Post-lockdown3")

tab_dif <- predicton_periods_age_gender %>%
  mutate(dif=pred_t-events_t)%>%
  mutate(dif_lwr= lwr_t-events_t)%>%
  mutate(dif_upr = upr_t-events_t)%>%
  mutate(under_dx = paste0(paste(dif)," (", paste(dif_lwr)," to ", paste(dif_upr), ")"))%>%
  dplyr::select(covid, denominator_sex, denominator_age_group, outcome,under_dx)%>%
  spread(key=covid,value=under_dx) %>% 
  arrange(outcome,denominator_sex)%>%
  mutate(value = "Underdx") #%>%
  #relocate("Post", .after= "Post-lockdown3")

tab_age.gender <- rbind(tab_red, tab_dif)


tab_gender0 <- predicton_periods_age_gender %>% group_by(covid, denominator_sex, outcome) %>% 
  summarise(events_t_g = sum(events_t),pred_t2=sum(pred_t), lwr_t2=sum(lwr_t), upr_t2=sum(upr_t),
            red_t = (100*(pred_t2-events_t_g)/pred_t2), red_lwr =(100*(lwr_t2-events_t_g)/lwr_t2), red_upr=(100*(upr_t2-events_t_g)/upr_t2)) %>% 
  arrange(outcome, covid) %>%
  mutate(red_t = round(red_t,1), pred_t2=round(pred_t2, 0),red_lwr = round(red_lwr,1), red_upr = round(red_upr,1))

tab_gender<- tab_gender0%>%
  arrange(denominator_sex)%>%  mutate(pred_t2 = paste0(paste(pred_t2)," (", paste(lwr_t2), " to ", paste(upr_t2), ")")) %>%
  mutate(red_t = paste0(paste(red_t)," (", paste(red_lwr), " to ", paste(red_upr), ")")) %>% 
  dplyr::select(covid,denominator_sex, outcome,red_t)%>%
  spread(key=covid,value=red_t) %>%
  arrange(outcome,denominator_sex)%>%
  mutate(value="Red_perd") #%>%
  #relocate("Post", .after= "Post-lockdown3")


tab_dif <- tab_gender0 %>%
  mutate(dif=pred_t2-events_t_g)%>%
  mutate(dif_lwr= lwr_t2-events_t_g)%>%
  mutate(dif_upr = upr_t2-events_t_g)%>%
  mutate(under_dx = paste0(paste(dif)," (", paste(dif_lwr)," to ", paste(dif_upr), ")"))%>%
  dplyr::select(covid, denominator_sex, outcome,under_dx)%>%
  spread(key=covid,value=under_dx) %>% 
  arrange(outcome,denominator_sex)%>%
  mutate(value = "Underdx") #%>%
  #relocate("Post", .after= "Post-lockdown3")

tab_gender <-rbind(tab_gender, tab_dif)


write.csv(tab_age.gender, file=here("4_Results", db.name, "Modelling", "age_gender_red_table.csv"))
write.csv(tab_gender, file=here("4_Results", db.name, "Modelling", "gender_red_table.csv"))
write.csv(tab_age.gender, file=here("4_Results", db.name, "Modelling", "age_gender_red_table.csv"))
write.csv(tab, file=here("4_Results", db.name, "Modelling", "age.gender_red.csv"))
write.csv(tab_gender0, file=here("4_Results", db.name, "Modelling", "gender_red.csv"))
save(prediction_age.gender, file=here("4_Results", db.name,  "Modelling", "Prediction_age.gender.RData"))



rm(IR.age_gender, models.age_gender, models.age_gender_period, models.age_gender_pred,
   models.age_gender_post, models.age_gender_total, prediction_periods_age_gender, tab, tab2,
   working.nb, age_to_fit,gender_to_fit, period.age_gender,pred,predicton_periods_age_gender, prediction_periods_age_gender)

####SES-------- run this when we have the data
ses_to_fit <- IR.ses %>%  ungroup() %>%dplyr::select("medea")%>% distinct()%>%pull() 
models.ses <- list()
models.ses_pred <- list()
models.ses_period <- list()
models.ses_total <- list()
models.ses_post <- list()

IR.ses$covid2 <- NA
IR.ses$covid2[which(IR.ses$covid =="Pre-COVID")] <- "Pre-COVID"
IR.ses$covid2[which(IR.ses$covid =="Lockdown")] <- "Lockdown"
IR.ses$covid2[which(IR.ses$months.since.start >=31)] <- "Post-COVID"

end_mod <- 26 #month.since.start= Feb 2020
IR.ses$medea <- as.factor(IR.ses$medea)
for(j in 1:length(outcomes_to_fit)){
  for(i in 1:(length(ses_to_fit)-1)){
    working.nb <- glm.nb(events ~ as.factor(month)+months.since.start,data=IR.ses%>% filter(months.since.start<= end_mod) %>% filter(medea==ses_to_fit[i]) %>% 
                           filter(outcome==outcomes_to_fit[j]))
    
    
    models.ses[[paste0("m.",ses_to_fit[i],".nb")]]  <- working.nb
    pred <-predict(working.nb, newdata=IR.ses%>%  filter(medea==ses_to_fit[i]) %>% 
                     filter(outcome==outcomes_to_fit[j]),type="response", se.fit = TRUE, interval= "prediction", level=0.95)
    
    models.ses_pred[[paste0("m.",ses_to_fit[i],outcomes_to_fit[j], ".nb")]] <- cbind(IR.ses %>%  filter(outcome==outcomes_to_fit[j])%>% filter(medea==ses_to_fit[i]),
                                                                                     data.frame(est=as.character(pred$fit), model="nb")) %>% 
      add_pi(working.nb, names = c("lwr", "upr"), alpha = 0.05) %>%
      mutate(ir_pred=pred*100000/months, lwr_pred=lwr*100000/months, upr_pred=upr*100000/months)%>%
      mutate(red = (100*(pred-events)/pred)) %>%
      mutate(red_lwr = (100*(lwr-events)/lwr))%>%
      mutate(upr_lwr = (100*(upr-events)/upr))
    
    
    models.ses_period[[paste0("m.",ses_to_fit[i],outcomes_to_fit[j], ".nb")]] <- models.ses_pred[[paste0("m.",ses_to_fit[i],outcomes_to_fit[j], ".nb")]] %>% 
      group_by(covid, outcome, medea)%>%filter(covid != "Pre-COVID")%>% 
      summarise(events_t= sum(events), pred_t= sum(pred),lwr_t =sum(lwr), upr_t=sum(upr)) %>%
      mutate(red = (100*(pred_t-events_t)/pred_t)) %>%
      mutate(red_lwr = (100*(lwr_t-events_t)/lwr_t))%>%
      mutate(red_upr = (100*(upr_t-events_t)/upr_t))
    models.ses_total[[paste0("m.",ses_to_fit[i],outcomes_to_fit[j], ".nb")]] <- models.ses_pred[[paste0("m.",ses_to_fit[i],outcomes_to_fit[j], ".nb")]] %>% 
      filter(covid != "Pre-COVID")%>% group_by(outcome,medea)%>%
      summarise(events_t= sum(events), pred_t= sum(pred), lwr_t =sum(lwr), upr_t=sum(upr)) %>%
      mutate(covid="Total")%>%
      mutate(red = (100*(pred_t-events_t)/pred_t)) %>%
      mutate(red_lwr = (100*(lwr_t-events_t)/lwr_t))%>%
      mutate(red_upr = (100*(upr_t-events_t)/upr_t))   
    
    models.ses_post[[paste0("m.",ses_to_fit[i],outcomes_to_fit[j], ".nb")]] <-  models.ses_pred[[paste0("m.",ses_to_fit[i],outcomes_to_fit[j], ".nb")]]%>% 
      filter(covid2 == "Post-COVID")%>% group_by(outcome,medea)%>%
      summarise(events_t= sum(events), pred_t= sum(pred), lwr_t =sum(lwr), upr_t=sum(upr)) %>%
      mutate(covid="Post")%>%
      mutate(red = (100*(pred_t-events_t)/pred_t)) %>%
      mutate(red_lwr = (100*(lwr_t-events_t)/lwr_t))%>%
      mutate(red_upr = (100*(upr_t-events_t)/upr_t))
    
  }
}

prediction_ses <- bind_rows(models.ses_pred) %>% mutate_if(is.numeric, ~round(., 1))
predicton_periods_ses <- as.data.frame(rbind(bind_rows(models.ses_period), bind_rows(models.ses_total), bind_rows(models.ses_post)))%>% arrange(outcome, covid) %>%
  mutate(red = round(red,1), pred_t=round(pred_t, 0), red_lwr =round(red_lwr,1), red_upr=round(red_upr,1))
tab <- predicton_periods_ses %>%  mutate(pred = paste0(paste(pred_t)," (", paste(lwr_t), " to ", paste(upr_t), ")")) %>%
  mutate(red_perc = paste0(paste(red)," (", paste(red_lwr), " to ", paste(red_upr), ")")) %>% dplyr::select(covid, outcome, medea, pred, events_t, red_perc)
tab$medea <- ordered(tab$medea, levels =c("U1", "U2", "U3", "U4", "U5", "R"))
tab_red <- tab %>%
  dplyr::select(covid,medea, outcome,red_perc)%>%
  spread(key=covid,value=red_perc) %>%
  arrange(outcome,medea)%>%
  mutate(value="Red_perc")%>%
  relocate("Post", .after= "Post-lockdown3")

tab_dif <- predicton_periods_ses %>%
  mutate(dif=pred_t-events_t)%>%
  mutate(dif_lwr= lwr_t-events_t)%>%
  mutate(dif_upr = upr_t-events_t)%>%
  mutate(under_dx = paste0(paste(dif)," (", paste(dif_lwr)," to ", paste(dif_upr), ")"))%>%
  dplyr::select(covid,medea, outcome,under_dx)%>%
  spread(key=covid,value=under_dx) %>% 
  arrange(outcome,medea)%>%
  mutate(value = "Underdx")%>%
  relocate("Post", .after= "Post-lockdown3")

tab_ses <- rbind(tab_red, tab_dif)

write.csv(predicton_periods_ses, "Results/Modelling/prediction_periods_ses_red.csv")
write.csv(tab_ses, "Results/Modelling/ses_red_table.csv")
save(prediction_ses, file=here("4_Results", db.name,  "Modelling", "Prediction_ses.RData"))

rm(IR.ses,models.ses, models.ses_period, models.ses_post, models.ses_pred, models.ses_total, pred,
   predicton_periods_ses, tab, working.nb, ses_to_fit, outcomes_to_fit, end_mod, i, j, y)

###TABLE--------- run this when we have the full data
tab1 <- tab1 %>% mutate(strata="overall")
tab_gender <-tab_gender %>%rename(strata=denominator_sex)
tab_age.gender1 <-tab_age.gender%>%mutate(strata=paste0(denominator_age_group, ",", denominator_sex))%>%
  dplyr::select(-denominator_age_group, -denominator_sex)
tab_ses <-tab_ses %>%rename(strata=medea)

table <- rbind(tab1, tab_gender,tab_age.gender1, tab_ses) %>% arrange(outcome)
table_undx <- table %>%filter(value=="Underdx")%>%
  rename(Lockdown_undx="Lockdown")%>%
  rename(PL1_undx="Post-lockdown1")%>%
  rename(PL2_undx="Post-lockdown2")%>%
  rename(PL3_undx="Post-lockdown3")%>%
  rename(PL_undx="Post")%>%
  dplyr::select(-Total, -value)

table_red <- table %>%filter(value!="Underdx")%>%
  rename(Lockdown_red="Lockdown")%>%
  rename(PL1_red="Post-lockdown1")%>%
  rename(PL2_red="Post-lockdown2")%>%
  rename(PL3_red="Post-lockdown3")%>%
  rename(PL_red="Post")%>%
  dplyr::select(-Total, -value)

table <- merge(table_undx, table_red)
table1 <- table[, c("outcome", "strata", "Lockdown_undx","Lockdown_red",
                    "PL1_undx",  "PL1_red", 
                    "PL2_undx", "PL2_red",
                    "PL3_undx","PL3_red",
                    "PL_undx",  "PL_red" )]
table1$strata <- factor(table1$strata, levels=c(
 "overall", "Female","Male", "18-34,Female","35-64,Female",">=65,Female",
 "18-34,Male","35-64,Male",">=65,Male",
 "U1", "U2", "U3", "U4", "U5", "R"
))

table1 <- table1 %>%arrange(outcome, strata)
write.csv(table1, "4_Results", db.name, "Modelling", "Table_Results.rev.csv")

##### PLOTS--------

# overall
# Breast

overall_prediction_Breast <- prediction_overall %>% filter(outcome=="Breast")%>%
  ggplot()+
  geom_point(aes(Date,ir_m, colour= "Observed"))+
  geom_line(aes(Date,ir_m,colour= "Observed"))+
  
  geom_point(aes(Date,ir_pred,colour= "Expected"))+
  geom_line(aes(Date,ir_pred,colour= "Expected"))+
  geom_ribbon(aes(ymin = lwr_pred,ymax = upr_pred, x=Date),  fill = "blue", alpha = 0.1)+
  scale_color_manual(name= "", values=c(Observed="red", Expected="blue"))+
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 month", limits= c(as.Date("2018-01-01"),as.Date("2022-06-01")),expand=c(0.005,0.005))+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

overall_prediction_Breast <- overall_prediction_Breast + 
  theme(axis.text.x = element_text(angle=90), 
        axis.title.y = element_text(size = 9),    
        plot.margin=grid::unit(c(1,1,0,1), "cm"))+
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-01"))),linetype=2, color="black")+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("")+
  ggtitle("Incidence rates for breast cancer before and after COVID-19 lockdown")

# Colorectal
overall_prediction_Colorectal <- prediction_overall %>% filter(outcome=="Colorectal")%>%
  ggplot()+
  geom_point(aes(Date,ir_m, colour= "Observed"))+
  geom_line(aes(Date,ir_m,colour= "Observed"))+
  
  geom_point(aes(Date,ir_pred,colour= "Expected"))+
  geom_line(aes(Date,ir_pred,colour= "Expected"))+
  geom_ribbon(aes(ymin = lwr_pred,ymax = upr_pred, x=Date),  fill = "blue", alpha = 0.1)+
  scale_color_manual(name= "", values=c(Observed="red", Expected="blue"))+
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 month", limits= c(as.Date("2018-01-01"),as.Date("2022-06-01")),expand=c(0.005,0.005))+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

overall_prediction_Colorectal <- overall_prediction_Colorectal + 
  theme(axis.text.x = element_text(angle=90), 
        axis.title.y = element_text(size = 9),    
        plot.margin=grid::unit(c(1,1,0,1), "cm"))+
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-01"))),linetype=2, color="black")+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("")+
  ggtitle("Incidence rates for colorectal cancer before and after COVID-19 lockdown")


# Lung
overall_prediction_Lung <- prediction_overall %>% filter(outcome=="Lung")%>%
  ggplot()+
  geom_point(aes(Date,ir_m, colour= "Observed"))+
  geom_line(aes(Date,ir_m,colour= "Observed"))+
  
  geom_point(aes(Date,ir_pred,colour= "Expected"))+
  geom_line(aes(Date,ir_pred,colour= "Expected"))+
  geom_ribbon(aes(ymin = lwr_pred,ymax = upr_pred, x=Date),  fill = "blue", alpha = 0.1)+
  scale_color_manual(name= "", values=c(Observed="red", Expected="blue"))+
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 month", limits= c(as.Date("2018-01-01"),as.Date("2022-06-01")),expand=c(0.005,0.005))+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

overall_prediction_Lung <- overall_prediction_Lung + 
  theme(axis.text.x = element_text(angle=90), 
        axis.title.y = element_text(size = 9),    
        plot.margin=grid::unit(c(1,1,0,1), "cm"))+
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-01"))),linetype=2, color="black")+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("")+
  ggtitle("Incidence rates for Lung cancer before and after COVID-19 lockdown")

# Prostate
overall_prediction_Prostate <- prediction_overall %>% filter(outcome=="Prostate")%>%
  ggplot()+
  geom_point(aes(Date,ir_m, colour= "Observed"))+
  geom_line(aes(Date,ir_m,colour= "Observed"))+
  
  geom_point(aes(Date,ir_pred,colour= "Expected"))+
  geom_line(aes(Date,ir_pred,colour= "Expected"))+
  geom_ribbon(aes(ymin = lwr_pred,ymax = upr_pred, x=Date),  fill = "blue", alpha = 0.1)+
  scale_color_manual(name= "", values=c(Observed="red", Expected="blue"))+
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 month", limits= c(as.Date("2018-01-01"),as.Date("2022-06-01")),expand=c(0.005,0.005))+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

overall_prediction_Prostate <- overall_prediction_Prostate + 
  theme(axis.text.x = element_text(angle=90), 
        axis.title.y = element_text(size = 9),    
        plot.margin=grid::unit(c(1,1,0,1), "cm"))+
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-01"))),linetype=2, color="black")+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("")+
  ggtitle("Incidence rates for prostate cancer before and after COVID-19 lockdown")

figure_prediction_overall<-ggarrange(overall_prediction_Breast, overall_prediction_Colorectal, overall_prediction_Lung, overall_prediction_Prostate, 
                    align="hv", ncol=2, nrow=2,
                    labels = c("A) Breast Cancer", "B) Colorectal Cancer", "C) Lung Cancer", "D) Prostate Cancer"),font.label = list(size = 12),
                    hjust = c(-0.25,-0.25),
                    common.legend=TRUE, legend="right" )



# Age and gender
# Breast
prediction_age.gender$denominator_sex <- factor(prediction_age.gender$denominator_sex, levels=rev(levels(prediction_age.gender$denominator_sex)))
levels(prediction_age.gender$denominator_sex) <- c("Female", "Male", "Both")
levels(prediction_age.gender$denominator_age_group) <- c("40;59", "60;79", "0;150", "80;150")

age_gender_Breast <- prediction_age.gender  %>% 
  filter(outcome=="Breast") %>% 
  ggplot()+
 facet_grid(denominator_sex~denominator_age_group,scales="free")+
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



age_gender_Breast <-age_gender_Breast+ 
  theme(axis.text.x = element_text(angle=90),
        axis.title.y = element_text(size = 9),
        plot.margin=grid::unit(c(1,0.5,0,1), "cm") )+
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "4 month")+
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-01"))),linetype=2, color="black")+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("")



## ADD ALL OTHER PLOTS FOR THE OTHER CANCERS STRATIFIED BY AGE AND SEX HERE TOO




figure_age_gender <-ggarrange(age_gender_AD, age_gender_MD,
                              align="hv", ncol=1, nrow=2,
                              labels = c("A) Anxiety disorders", "B) Depressive disorders"),font.label = list(size = 12),
                              hjust = c(-0.25,-0.25),
                              common.legend=TRUE, legend="right" )




# add this when we have the SES data
# SES
prediction_ses$medea <- factor(prediction_ses$medea, levels=c("U1", "U2", "U3", "U4", "U5", "R"))
plot_ses_AD <- prediction_ses%>%
  filter(outcome=="AnxietyDisorders") %>% filter(months.since.start >=13)%>%
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


plot_ses_AD <- plot_ses_AD+
  theme(axis.text.x=element_text(angle=90, hjust=1),
        axis.title.y = element_text(size = 9),
        plot.margin=grid::unit(c(1,0.5,0,1), "cm") )+
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-01"))),
             linetype=2, color="black")+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("")

plot_ses_MD <- prediction_ses%>%filter(outcome!="AnxietyDisorders") %>% filter(months.since.start >=13)%>%
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


plot_ses_MD <- plot_ses_MD+
  theme(axis.text.x=element_text(angle=90, hjust=1),
        axis.title.y = element_text(size = 9),
        plot.margin=grid::unit(c(1,0.5,0,1), "cm"))+
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-01"))),
             linetype=2, color="black")+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("")



figure_ses <-ggarrange(plot_ses_AD, plot_ses_MD,
                       align="hv", ncol=1, nrow=2,
                       labels = c("A) Anxiety disorders", "B) Depressive disorders"),font.label = list(size = 12),
                       hjust = c(-0.25,-0.25),
                       common.legend=TRUE, legend="right" )



# Save

ggsave(here("4_Results", db.name, "Plots", "Figure_1_prediction_overall_remove_dec_2021.tiff"), figure_prediction_overall, dpi=300, scale = 2)
ggsave(here("4_Results", db.name, "Plots", "Figure_1_prediction_overall_remove_dec_2021.jpg"), figure_prediction_overall, dpi=300, scale = 2.5)




here("4_Results", db.name,  "Plots", ggsave("Figure1.tiff", figure_prediction_overall,dpi=300))
here("4_Results", db.name,  "Plots", ggsave("Figure2.tiff",figure_age_gender,dpi=300))
here("4_Results", db.name,  "Plots", ggsave("Figure3.tiff", figure_ses,dpi=300))

here("4_Results", db.name,  "Plots", ggsave("Figure1_jpg.jpg", figure_prediction_overall,dpi=300))
here("4_Results", db.name,  "Plots", ggsave("Figure2_jpg.jpg", figure_age_gender,dpi=300))

here("4_Results", db.name,  "Plots", ggsave("Figure3_jpg.jpg", figure_ses,dpi=300))
rm(figure_prediction_overall, figure_age_gender,figure_ses, prediction_age.gender,predicion_overall, predicition_ses, 
   end_mod, j, outcomes_to_fit)