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


###### Observed vs expected (Negative Binomial Regression models)
###Forecast:
##Fit to 01/03/2018- 01/02/2020
##Forecast 01/03/2020 - 01/03/2021

load(here("DataPrep/March2018_March2021/IR.overall.rev.RData"))
load(here("DataPrep/March2018_March2021/IR.age_gender.rev.RData"))
load(here("DataPrep/March2018_March2021/IR.ses.rev.RData"))


IR.overall <- IR.overall %>% mutate(Month1 =paste(1,month, year, sep ="-")) %>% filter(outcome =="AnxietyDisorders"| outcome=="MajorDepressiveDisorder")
IR.age_gender <- IR.age_gender %>%  mutate(Month1 =paste(1,month, year, sep ="-")) %>% filter(outcome =="AnxietyDisorders"| outcome=="MajorDepressiveDisorder")
IR.ses <- IR.ses %>% mutate(Month1 =paste(1,month, year, sep ="-")) %>% filter(outcome =="AnxietyDisorders"| outcome=="MajorDepressiveDisorder")
outcomes_to_fit<- IR.overall %>% dplyr::select("outcome")%>% distinct()%>%pull()


#### Overall----------
Sys.setlocale("LC_TIME", "English")
IR.overall$Date <- NA
IR.overall$Date <- dmy(IR.overall$Month1)
IR.overall$Month1 <- NULL
IR.age_gender$Date <- NA
IR.age_gender$Date <- dmy(IR.age_gender$Month1)
IR.age_gender$Month1 <- NULL
IR.ses$Date <- NA
IR.ses$Date <- dmy(IR.ses$Month1)
IR.ses$Month1 <- NULL


models <- list()
models_pred <- list()
models_period<- list()
models_total <- list()
models_post <- list()

IR.overall$covid2 <- NA
IR.overall$covid2[which(IR.overall$covid =="Pre-COVID")] <- "Pre-COVID"
IR.overall$covid2[which(IR.overall$covid =="Lockdown")] <- "Lockdown"
IR.overall$covid2[which(IR.overall$months.since.start >=29)] <- "Post-COVID"

end_mod <- 24 #month.since.start= Feb 2020


for(j in 1:length(outcomes_to_fit)){
  
  working.nb <- glm.nb(events ~ as.factor(month)+months.since.start,data=IR.overall%>% filter(months.since.start<= end_mod) %>%
                         filter(outcome==outcomes_to_fit[j]))
  models[[paste0("m.",".nb")]]  <- working.nb
  pred <-predict(working.nb, newdata=IR.overall%>% 
                   filter(outcome==outcomes_to_fit[j]),type="response", se.fit = TRUE, interval= "prediction", level=0.95)
  models_pred[[paste0("m.",outcomes_to_fit[j], ".nb")]] <- cbind(IR.overall %>%  filter(outcome==outcomes_to_fit[j]), data.frame(est=as.character(pred$fit), model="nb")) %>%  
    add_pi(working.nb, names = c("lwr", "upr"), alpha = 0.05)  %>%  
    mutate(ir_pred=pred*100000/months, lwr_pred=lwr*100000/months, upr_pred=upr*100000/months) %>%
    mutate(red = (100*(pred-events)/pred)) %>%
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
  mutate(value = "Red_perc")%>%
  relocate("Post", .after= "Post-lockdown3")
tab_dif <- predicton_overall_periods %>%
  mutate(dif=pred_t-events_t)%>%
  mutate(dif_lwr= lwr_t-events_t)%>%
  mutate(dif_upr = upr_t-events_t)%>%
  mutate(under_dx = paste0(paste(dif)," (", paste(dif_lwr)," to ", paste(dif_upr), ")"))%>%
  dplyr::select(covid, outcome,under_dx)%>%
  spread(key=covid,value=under_dx) %>% 
  mutate(value = "Underdx")%>%
  relocate("Post", .after= "Post-lockdown3")

tab1 <-rbind(tab_red, tab_dif)

write.csv(tab1, "Modelling/Results/overall_table.rev.csv")
write.csv(tab, "Modelling/Results/overall_red.rev.csv")
save(prediction_overall, file=here("WorkingData", "Prediction_Overall_2021.rev.RData"))
rm(IR.overall,models, models_period, models_post, models_pred, models_total, pred,
   prediction_overall_periods, tab, working.nb, predicton_overall_periods)



####Age and gender-------
age_to_fit <- IR.age_gender %>%  ungroup() %>%dplyr::select("age_gr2")%>% distinct()%>%pull()
gender_to_fit <-  IR.age_gender %>% ungroup() %>%dplyr::select("gender")%>% distinct()%>%pull()
models.age_gender <- list()
models.age_gender_pred <- list()
period.age_gender <- list()
models.age_gender_period <- list()
models.age_gender_total <- list()
models.age_gender_post <- list()
IR.age_gender$covid2 <- NA
IR.age_gender$covid2[which(IR.age_gender$covid =="Pre-COVID")] <- "Pre-COVID"
IR.age_gender$covid2[which(IR.age_gender$covid =="Lockdown")] <- "Lockdown"
IR.age_gender$covid2[which(IR.age_gender$months.since.start >=29)] <- "Post-COVID"
end_mod <- 24 #month.since.start= Feb 2020
for(j in 1:length(outcomes_to_fit)){
  for(i in 1:length(age_to_fit)){
    for(y in 1:length(gender_to_fit)){
      working.nb <- glm.nb(events ~ as.factor(month)+months.since.start,data=IR.age_gender%>% filter(months.since.start<= end_mod) %>% filter(gender==gender_to_fit[y]) %>% 
                             filter(age_gr2==age_to_fit[i])%>% filter(outcome==outcomes_to_fit[j]))
      models.age_gender[[paste0("m.",age_to_fit[i],gender_to_fit[y],".nb")]]  <- working.nb
      pred <-predict(working.nb, newdata=IR.age_gender%>%  filter(gender==gender_to_fit[y]) %>% filter(age_gr2==age_to_fit[i])%>%
                       filter(outcome==outcomes_to_fit[j]),type="response", se.fit = TRUE, interval= "prediction", level=0.95)
      
      models.age_gender_pred[[paste0("m.",age_to_fit[i],gender_to_fit[y],outcomes_to_fit[j], ".nb")]] <- cbind(IR.age_gender %>%  filter(outcome==outcomes_to_fit[j])%>% filter(gender==gender_to_fit[y]) %>% 
                                                                                                                 filter(age_gr2==age_to_fit[i]), data.frame(est=as.character(pred$fit), model="nb")) %>%  
        add_pi(working.nb, names = c("lwr", "upr"), alpha = 0.05) %>%
        mutate(ir_pred=pred*100000/months, lwr_pred=lwr*100000/months, upr_pred=upr*100000/months)
      
      models.age_gender_period[[paste0("m.",age_to_fit[i],gender_to_fit[y],outcomes_to_fit[j], ".nb")]] <-models.age_gender_pred[[paste0("m.",age_to_fit[i],gender_to_fit[y],outcomes_to_fit[j], ".nb")]] %>% 
        group_by(covid, outcome, gender, age_gr2)%>%filter(covid != "Pre-COVID")%>% 
        summarise(events_t= sum(events), pred_t= sum(pred), lwr_t =sum(lwr), upr_t=sum(upr)) %>%
        mutate(red = (100*(pred_t-events_t)/pred_t)) %>%
        mutate(red_lwr = (100*(lwr_t-events_t)/lwr_t))%>%
        mutate(red_upr = (100*(upr_t-events_t)/upr_t))
      
      models.age_gender_total[[paste0("m.",age_to_fit[i],gender_to_fit[y],outcomes_to_fit[j], ".nb")]] <-  models.age_gender_pred[[paste0("m.",age_to_fit[i],gender_to_fit[y],outcomes_to_fit[j], ".nb")]]%>% 
        filter(covid != "Pre-COVID")%>% group_by(outcome,gender, age_gr2)%>%
        summarise(events_t= sum(events), pred_t= sum(pred), lwr_t =sum(lwr), upr_t=sum(upr)) %>%
        mutate(covid="Total")%>%
        mutate(red = (100*(pred_t-events_t)/pred_t)) %>%
        mutate(red_lwr = (100*(lwr_t-events_t)/lwr_t))%>%
        mutate(red_upr = (100*(upr_t-events_t)/upr_t))
      
      models.age_gender_post[[paste0("m.",age_to_fit[i],gender_to_fit[y],outcomes_to_fit[j], ".nb")]] <-  models.age_gender_pred[[paste0("m.",age_to_fit[i],gender_to_fit[y],outcomes_to_fit[j], ".nb")]]%>% 
        filter(covid2 == "Post-COVID")%>% group_by(outcome,gender, age_gr2)%>%
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

tab <- predicton_periods_age_gender %>% arrange(gender)%>%  mutate(pred = paste0(paste(pred_t),"(", paste(lwr_t, upr_t), ")")) %>%
  mutate(red_perc = paste0(paste(red)," (", paste(red_lwr), " to ", paste(red_upr), ")")) %>% dplyr::select(covid, outcome, gender, age_gr2,pred, events_t, red_perc)
tab_red <- tab %>%
  dplyr::select(covid,age_gr2, gender, outcome,red_perc)%>%
  spread(key=covid,value=red_perc) %>%
  arrange(outcome,gender)%>%
  mutate(value="Red_perd")%>%
  relocate("Post", .after= "Post-lockdown3")

tab_dif <- predicton_periods_age_gender %>%
  mutate(dif=pred_t-events_t)%>%
  mutate(dif_lwr= lwr_t-events_t)%>%
  mutate(dif_upr = upr_t-events_t)%>%
  mutate(under_dx = paste0(paste(dif)," (", paste(dif_lwr)," to ", paste(dif_upr), ")"))%>%
  dplyr::select(covid, age_gr2,gender, outcome,under_dx)%>%
  spread(key=covid,value=under_dx) %>% 
  arrange(outcome,gender)%>%
  mutate(value = "Underdx")%>%
  relocate("Post", .after= "Post-lockdown3")

tab_age.gender <- rbind(tab_red, tab_dif)


tab_gender0 <- predicton_periods_age_gender %>% group_by(covid, gender,outcome) %>% 
  summarise(events_t_g = sum(events_t),pred_t2=sum(pred_t), lwr_t2=sum(lwr_t), upr_t2=sum(upr_t),
            red_t = (100*(pred_t2-events_t_g)/pred_t2), red_lwr =(100*(lwr_t2-events_t_g)/lwr_t2), red_upr=(100*(upr_t2-events_t_g)/upr_t2)) %>% 
  arrange(outcome, covid) %>%
  mutate(red_t = round(red_t,1), pred_t2=round(pred_t2, 0),red_lwr = round(red_lwr,1), red_upr = round(red_upr,1))

tab_gender<- tab_gender0%>%
  arrange(gender)%>%  mutate(pred_t2 = paste0(paste(pred_t2)," (", paste(lwr_t2), " to ", paste(upr_t2), ")")) %>%
  mutate(red_t = paste0(paste(red_t)," (", paste(red_lwr), " to ", paste(red_upr), ")")) %>% 
  dplyr::select(covid,gender, outcome,red_t)%>%
  spread(key=covid,value=red_t) %>%
  arrange(outcome,gender)%>%
  mutate(value="Red_perd")%>%
  relocate("Post", .after= "Post-lockdown3")


tab_dif <- tab_gender0 %>%
  mutate(dif=pred_t2-events_t_g)%>%
  mutate(dif_lwr= lwr_t2-events_t_g)%>%
  mutate(dif_upr = upr_t2-events_t_g)%>%
  mutate(under_dx = paste0(paste(dif)," (", paste(dif_lwr)," to ", paste(dif_upr), ")"))%>%
  dplyr::select(covid, gender, outcome,under_dx)%>%
  spread(key=covid,value=under_dx) %>% 
  arrange(outcome,gender)%>%
  mutate(value = "Underdx")%>%
  relocate("Post", .after= "Post-lockdown3")

tab_gender <-rbind(tab_gender, tab_dif)


write.csv(tab_age.gender, "Modelling/Results/age_gender_red_table.rev.csv")
write.csv(tab_gender, "Modelling/Results/gender_red_table.rev.csv")
write.csv(tab_age.gender, "Modelling/Results/age_gender_red_table.rev.csv")
write.csv(tab, "Modelling/Results/age.gender_red.rev.csv")
write.csv(tab_gender0, "Modelling/Results/gender_red.rev.csv")
save(prediction_age.gender, file=here("WorkingData", "Prediction_age.gender.rev.RData"))



rm(IR.age_gender, models.age_gender, models.age_gender_period, models.age_gender_pred,
   models.age_gender_post, models.age_gender_total, prediction_periods_age_gender, tab, tab2,
   working.nb, age_to_fit,gender_to_fit, period.age_gender,pred,predicton_periods_age_gender, prediction_periods_age_gender)

####SES--------
ses_to_fit <- IR.ses %>%  ungroup() %>%dplyr::select("medea")%>% distinct()%>%pull() 
models.ses <- list()
models.ses_pred <- list()
models.ses_period <- list()
models.ses_total <- list()
models.ses_post <- list()

IR.ses$covid2 <- NA
IR.ses$covid2[which(IR.ses$covid =="Pre-COVID")] <- "Pre-COVID"
IR.ses$covid2[which(IR.ses$covid =="Lockdown")] <- "Lockdown"
IR.ses$covid2[which(IR.ses$months.since.start >=29)] <- "Post-COVID"

end_mod <- 24 #month.since.start= Feb 2020
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

write.csv(predicton_periods_ses, "Modelling/Results/prediction_periods_ses_red.rev.csv")
write.csv(tab_ses, "Modelling/Results/ses_red_table.rev.csv")
save(prediction_ses, file=here("WorkingData", "Prediction_ses_rev.RData"))

rm(IR.ses,models.ses, models.ses_period, models.ses_post, models.ses_pred, models.ses_total, pred,
   predicton_periods_ses, tab, working.nb, ses_to_fit, outcomes_to_fit, end_mod, i, j, y)

###TABLE---------
tab1 <- tab1 %>% mutate(strata="overall")
tab_gender <-tab_gender %>%rename(strata=gender)
tab_age.gender1 <-tab_age.gender%>%mutate(strata=paste0(age_gr2, ",", gender))%>%
  dplyr::select(-age_gr2, -gender)
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
write.csv(table1, "Modelling/Results/Table_Results.rev.csv")
##### PLOTS--------

# overall
overall_AD <- prediction_overall %>% filter(outcome=="AnxietyDisorders")%>%
  ggplot()+
  geom_point(aes(Date,ir_m, colour= "Observed"))+
  geom_line(aes(Date,ir_m,colour= "Observed"))+
  
  geom_point(aes(Date,ir_pred,colour= "Expected"))+
  geom_line(aes(Date,ir_pred,colour= "Expected"))+
  geom_ribbon(aes(ymin = lwr_pred,ymax = upr_pred, x=Date),  fill = "blue", alpha = 0.1)+
  scale_color_manual(name= "", values=c(Observed="red", Expected="blue"))+
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 month", limits= c(as.Date("2018-03-01"),as.Date("2021-03-01")),expand=c(0.005,0.005))+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

overall_AD <- overall_AD + 
  theme(axis.text.x = element_text(angle=90), 
        axis.title.y = element_text(size = 9),    
        plot.margin=grid::unit(c(1,1,0,1), "cm"))+
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-01"))),linetype=2, color="black")+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("")

overall_MD <- prediction_overall %>% filter(outcome!="AnxietyDisorders")%>%
  ggplot()+
  geom_point(aes(Date,ir_m, colour= "Observed"))+
  geom_line(aes(Date,ir_m,colour= "Observed"))+
  
  geom_point(aes(Date,ir_pred,colour= "Expected"))+
  geom_line(aes(Date,ir_pred,colour= "Expected"))+
  geom_ribbon(aes(ymin = lwr_pred,ymax = upr_pred, x=Date),  fill = "blue", alpha = 0.1)+
  scale_color_manual(name= "", values=c(Observed="red", Expected="blue"))+
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 month", limits= c(as.Date("2018-03-01"),as.Date("2021-03-01")),expand=c(0.005,0.005))+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

overall_MD <- overall_MD + 
  theme(axis.text.x = element_text(angle=90), 
        axis.title.y = element_text(size = 9),    
        plot.margin=grid::unit(c(0,1,0,1), "cm"))+
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-01"))),linetype=2, color="black")+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("")


figure_overall<-ggarrange(overall_AD, overall_MD, 
                    align="hv", ncol=1, nrow=2,
                    labels = c("A) Anxiety disorders", "B) Depressive disorders"),font.label = list(size = 12),
                    hjust = c(-0.25,-0.25),
                    common.legend=TRUE, legend="right" )
rm(overall_AD, overall_MD)


# Age and gender
prediction_age.gender$gender <- factor(prediction_age.gender$gender, levels=rev(levels(prediction_age.gender$gender)))
levels(prediction_age.gender$gender) <- c("Women", "Men")
levels(prediction_age.gender$age_gr2) <- c("18-34 y", "35-64 y", ">=65 y")
age_gender_AD <- prediction_age.gender  %>% 
  filter(outcome=="AnxietyDisorders") %>% filter(months.since.start >= 13)%>%
  ggplot()+
  facet_grid(gender~age_gr2,scales="free")+
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



age_gender_AD <-age_gender_AD+ 
  theme(axis.text.x = element_text(angle=90),
        axis.title.y = element_text(size = 9),
        plot.margin=grid::unit(c(1,0.5,0,1), "cm") )+
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "4 month")+
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-01"))),linetype=2, color="black")+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("")


age_gender_MD <- prediction_age.gender  %>% 
  filter(outcome!="AnxietyDisorders") %>% filter(months.since.start >= 13)%>%
  ggplot()+
  facet_grid(gender~age_gr2,scales="free")+
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
        panel.grid.minor = element_blank())




age_gender_MD<-age_gender_MD + 
  theme(axis.text.x = element_text(angle=90),
        axis.title.y = element_text(size = 9),
        plot.margin=grid::unit(c(1,0.5,0,1), "cm") )+
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-01"))),linetype=2, color="black")+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("")

figure_age_gender <-ggarrange(age_gender_AD, age_gender_MD,
                              align="hv", ncol=1, nrow=2,
                              labels = c("A) Anxiety disorders", "B) Depressive disorders"),font.label = list(size = 12),
                              hjust = c(-0.25,-0.25),
                              common.legend=TRUE, legend="right" )

rm(age_gender_AD, age_gender_MD)



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

rm(plot_ses_AD,plot_ses_MD)

# Save
ggsave("Figures/Figure1.rev.tiff", figure_overall,dpi=300)
ggsave("Figures/Figure2.rev.tiff",figure_age_gender,dpi=300)
ggsave("Figures/Figure3.rev.tiff", figure_ses,dpi=300)

ggsave("Figures/Figure1.rev_jpg.jpg", figure_overall,dpi=300)
ggsave("Figures/Figure2.rev_jpg.jpg", figure_age_gender,dpi=300)

ggsave("Figures/Figure3.rev_jpg.jpg", figure_ses,dpi=300)
rm(figure_overall, figure_age_gender,figure_ses, prediction_age.gender,predicion_overall, predicition_ses, 
   end_mod, j, outcomes_to_fit)