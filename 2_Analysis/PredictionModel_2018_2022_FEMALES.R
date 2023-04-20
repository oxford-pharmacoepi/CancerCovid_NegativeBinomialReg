###### Observed vs expected (Negative Binomial Regression models)
###Forecast:
##Fit to 01/01/2018- 01/02/2020
##Forecast 01/03/2020 - 01/01/2022

load(here("1_DataPrep", "Data", "GeneralPop2018_22.RData"))


## SET UP FOR RUNNING PREDICTION MODEL FOR femaleS SEPARATELY

IR.age_female <- inc_data_pred_final %>%  mutate(Month1 =paste(1,month, year, sep ="-"))  %>% filter(denominator_sex =="Female")

# TO RULE OUT DEC 2021 DATA FILTER THE FOLLOWING. THIS IS BECAUSE THERE IS INCOMPLETE DATA IN THAT MONTH AND THE PREDICTION IS INACCURATE.
IR.age_female  <- IR.age_female %>%  filter(month_year !="12/2021")

# filter out data from 2017 as I am using data from 2018 for this modelling 

IR.age_female <- IR.age_female %>% filter(IR.age_female$months.since.start>=1)


outcomes_to_fit_female<-c("Breast","Lung","Colorectal")


IR.age_female$Date <- NA
IR.age_female$Date <- dmy(IR.age_female$Month1)
IR.age_female$Month1 <- NULL



age_to_fit_female <- IR.age_female %>%  ungroup() %>%dplyr::select("denominator_age_group")%>% distinct()%>%pull()
models.age_female.fit <- list()
models.age_female_pred <- list()

period.age_female <- list()
models.age_female_period <- list()
models.age_female_total <- list()
models.age_female_post <- list()
IR.age_female$covid2 <- NA
IR.age_female$covid2[which(IR.age_female$covid =="Pre-COVID")] <- "Pre-COVID"
IR.age_female$covid2[which(IR.age_female$covid =="Lockdown")] <- "Lockdown"
IR.age_female$covid2[which(IR.age_female$months.since.start >=31)] <- "Post-COVID"

end_mod <- 26 #month.since.start= Feb 2020 assuming start date in Jan 2018

## FOR FEMALES ONLY - this does not run altogether, so will have to separate out the cancers and age groups

# Breast - this works through all the age groups fine.

age_to_fit_female <- IR.age_female %>%  ungroup() %>%dplyr::select("denominator_age_group")%>% distinct()%>%pull()
models.age_female_breast.fit <- list()
models.age_female_pred <- list()


  for(i in 1:length(age_to_fit_female)){
      working.nb <- glm.nb(events ~ as.factor(month)+months.since.start,data=IR.age_female%>% filter(months.since.start<= end_mod) %>% 
                             filter(denominator_age_group==age_to_fit_female[i])%>% filter(outcome=="Breast"))
      models.age_female_breast.fit[[paste0("m.",age_to_fit_female[i],".nb")]]  <- working.nb
      pred <-predict(working.nb, newdata=IR.age_female %>% filter(denominator_age_group==age_to_fit_female[i])%>%
                       filter(outcome=="Breast"),type="response", se.fit = TRUE, interval= "prediction", level=0.95)
      
      models.age_female_pred[[paste0("m.",age_to_fit_female[i],"Breast", ".nb")]] <- cbind(IR.age_female %>%  filter(outcome=="Breast") %>% 
                                                                                                                 filter(denominator_age_group==age_to_fit_female[i]), data.frame(est=as.character(pred$fit), model="nb")) %>%  
        add_pi(working.nb, names = c("lwr", "upr"), alpha = 0.05) %>%
        mutate(ir_pred=pred*100000/months, lwr_pred=lwr*100000/months, upr_pred=upr*100000/months)
      
      models.age_female_period[[paste0("m.",age_to_fit_female[i],"Breast", ".nb")]] <-models.age_female_pred[[paste0("m.",age_to_fit_female[i],"Breast", ".nb")]] %>% 
        group_by(covid, denominator_sex, outcome, denominator_age_group)%>% filter(covid != "Pre-COVID")%>% 
        summarise(events_t= sum(events), pred_t= sum(pred), lwr_t =sum(lwr), upr_t=sum(upr)) %>%
        mutate(red = (100*(pred_t-events_t)/pred_t)) %>%
        mutate(red_lwr = (100*(lwr_t-events_t)/lwr_t))%>%
        mutate(red_upr = (100*(upr_t-events_t)/upr_t))
      
      models.age_female_total[[paste0("m.",age_to_fit_female[i],"Breast", ".nb")]] <-  models.age_female_pred[[paste0("m.",age_to_fit_female[i],"Breast", ".nb")]]%>% 
        filter(covid != "Pre-COVID")%>% group_by(outcome, denominator_sex, denominator_age_group)%>%
        summarise(events_t= sum(events), pred_t= sum(pred), lwr_t =sum(lwr), upr_t=sum(upr)) %>%
        mutate(covid="Total")%>%
        mutate(red = (100*(pred_t-events_t)/pred_t)) %>%
        mutate(red_lwr = (100*(lwr_t-events_t)/lwr_t))%>%
        mutate(red_upr = (100*(upr_t-events_t)/upr_t))
      
      models.age_female_post[[paste0("m.",age_to_fit_female[i],"Breast", ".nb")]] <-  models.age_female_pred[[paste0("m.",age_to_fit_female[i],"Breast", ".nb")]]%>% 
        filter(covid2 == "Post-COVID")%>% group_by(outcome, denominator_sex, denominator_age_group)%>%
        summarise(events_t= sum(events), pred_t= sum(pred), lwr_t =sum(lwr), upr_t=sum(upr)) %>%
        mutate(covid="Post")%>%
        mutate(red = (100*(pred_t-events_t)/pred_t)) %>%
        mutate(red_lwr = (100*(lwr_t-events_t)/lwr_t))%>%
        mutate(red_upr = (100*(upr_t-events_t)/upr_t))
      
  }



prediction_age.female_breast <- bind_rows(models.age_female_pred)%>% mutate_if(is.numeric, ~round(., 1)) 
predicton_periods_age_female_breast <- as.data.frame(rbind(bind_rows(models.age_female_period), bind_rows(models.age_female_total),bind_rows(models.age_female_post)))%>% arrange(outcome, covid) %>%
  mutate(red = round(red,1), pred_t=round(pred_t, 0),red_lwr = round(red_lwr,1), red_upr = round(red_upr,1))

tab_breast <- predicton_periods_age_female_breast %>% mutate(pred = paste0(paste(pred_t),"(", paste(lwr_t, upr_t), ")")) %>%
  mutate(red_perc = paste0(paste(red)," (", paste(red_lwr), " to ", paste(red_upr), ")")) %>% dplyr::select(covid, outcome, denominator_sex, denominator_age_group, pred, events_t, red_perc)
tab_red_breast <- tab_breast %>%
  dplyr::select(covid, denominator_sex, denominator_age_group, outcome,red_perc)%>%
  spread(key=covid,value=red_perc) %>%
  arrange(outcome)%>%
  mutate(value="Red_perc") %>%
  relocate("Easing of restrictions", .after= "Third lockdown") %>%
  relocate("Legal restrictions removed", .after= "Easing of restrictions") %>%
  relocate("Post", .after= "Legal restrictions removed") 

tab_dif_breast <- predicton_periods_age_female_breast %>%
  mutate(dif=pred_t-events_t)%>%
  mutate(dif_lwr= lwr_t-events_t)%>%
  mutate(dif_upr = upr_t-events_t)%>%
  mutate(under_dx = paste0(paste(dif)," (", paste(dif_lwr)," to ", paste(dif_upr), ")"))%>%
  dplyr::select(covid, denominator_sex, denominator_age_group, outcome,under_dx)%>%
  spread(key=covid,value=under_dx) %>% 
  arrange(outcome)%>%
  mutate(value = "Underdx") %>%
  relocate("Easing of restrictions", .after= "Third lockdown") %>%
  relocate("Legal restrictions removed", .after= "Easing of restrictions") %>%
  relocate("Post", .after= "Legal restrictions removed") 

tab_age.female_breast <- rbind(tab_red_breast, tab_dif_breast)


tab_female0_breast <- predicton_periods_age_female_breast %>% group_by(denominator_sex, covid, outcome) %>% 
  summarise(events_t_g = sum(events_t),pred_t2=sum(pred_t), lwr_t2=sum(lwr_t), upr_t2=sum(upr_t),
            red_t = (100*(pred_t2-events_t_g)/pred_t2), red_lwr =(100*(lwr_t2-events_t_g)/lwr_t2), red_upr=(100*(upr_t2-events_t_g)/upr_t2)) %>% 
  arrange(outcome, covid) %>%
  mutate(red_t = round(red_t,1), pred_t2=round(pred_t2, 0),red_lwr = round(red_lwr,1), red_upr = round(red_upr,1))

tab_female_breast<- tab_female0_breast%>%
  mutate(pred_t2 = paste0(paste(pred_t2)," (", paste(lwr_t2), " to ", paste(upr_t2), ")")) %>%
  mutate(red_t = paste0(paste(red_t)," (", paste(red_lwr), " to ", paste(red_upr), ")")) %>% 
  dplyr::select(denominator_sex, covid, outcome,red_t)%>%
  spread(key=covid,value=red_t) %>%
  arrange(outcome)%>%
  mutate(value="Red_perc") %>%
  relocate("Easing of restrictions", .after= "Third lockdown") %>%
  relocate("Legal restrictions removed", .after= "Easing of restrictions") %>%
  relocate("Post", .after= "Legal restrictions removed") 


tab_dif_breast <- tab_female0_breast %>%
  mutate(dif=pred_t2-events_t_g)%>%
  mutate(dif_lwr= lwr_t2-events_t_g)%>%
  mutate(dif_upr = upr_t2-events_t_g)%>%
  mutate(under_dx = paste0(paste(dif)," (", paste(dif_lwr)," to ", paste(dif_upr), ")"))%>%
  dplyr::select(denominator_sex, covid, outcome,under_dx)%>%
  spread(key=covid,value=under_dx) %>% 
  arrange(outcome)%>%
  mutate(value = "Underdx") %>%
  relocate("Easing of restrictions", .after= "Third lockdown") %>%
  relocate("Legal restrictions removed", .after= "Easing of restrictions") %>%
  relocate("Post", .after= "Legal restrictions removed") 

tab_female_breast <-rbind(tab_female_breast, tab_dif_breast)


write.csv(tab_age.female_breast, file=here("4_Results", db.name, "Modelling", "age_female_red_table_breast.csv"))
write.csv(tab_female_breast, file=here("4_Results", db.name, "Modelling", "female_red_table_breast.csv"))
write.csv(tab_age.female_breast, file=here("4_Results", db.name, "Modelling", "age_female_red_table_breast.csv"))
write.csv(tab_breast, file=here("4_Results", db.name, "Modelling", "age.female_red_breast.csv"))
write.csv(tab_female0_breast, file=here("4_Results", db.name, "Modelling", "female_red_breast.csv"))
save(prediction_age.female_breast, file=here("4_Results", db.name,  "Modelling", "Prediction_age.female_breast.RData"))
save(models.age_female_breast.fit, file=here("4_Results", db.name,  "Modelling", "Pred_models.age_female_breast.fit.RData"))
save(IR.age_female, file=here("4_Results", db.name,  "Modelling", "IR.age_female.RData"))


rm(models.age_female_period, models.age_female_pred,
   models.age_female_post, models.age_female_total, 
   working.nb, age_to_fit_female, period.age_female,pred,i)



# Colorectal - this works through all the age groups fine.


#  - check that there are records in each of the combinations of outcome, age and sex. If there are some combinations with no rows, then may need to run these separately
save_counts <- count(IR.age_female, outcome, denominator_age_group, denominator_sex, name = "Freq") %>% print(n=Inf)

# only include age groups where there is more than one month of data (note that age 20-39 only has one month of colorectal cases n=5)
age_to_fit_female_c <- c("0;150","80;150","60;79","40;59")
models.age_female_colorectal.fit <- list()
models.age_female_pred <- list()
period.age_female <- list()
models.age_female_period <- list()
models.age_female_total <- list()
models.age_female_post <- list()


for(i in 1:length(age_to_fit_female_c)){
  working.nb <- glm.nb(events ~ as.factor(month)+months.since.start,data=IR.age_female%>% filter(months.since.start<= end_mod) %>% 
                         filter(denominator_age_group==age_to_fit_female_c[i])%>% filter(outcome=="Colorectal"))
  models.age_female_colorectal.fit[[paste0("m.",age_to_fit_female_c[i],".nb")]]  <- working.nb
  pred <-predict(working.nb, newdata=IR.age_female %>% filter(denominator_age_group==age_to_fit_female_c[i])%>%
                   filter(outcome=="Colorectal"),type="response", se.fit = TRUE, interval= "prediction", level=0.95)
  
  models.age_female_pred[[paste0("m.",age_to_fit_female_c[i],"Colorectal", ".nb")]] <- cbind(IR.age_female %>%  filter(outcome=="Colorectal") %>% 
                                                                                         filter(denominator_age_group==age_to_fit_female_c[i]), data.frame(est=as.character(pred$fit), model="nb")) %>%  
    add_pi(working.nb, names = c("lwr", "upr"), alpha = 0.05) %>%
    mutate(ir_pred=pred*100000/months, lwr_pred=lwr*100000/months, upr_pred=upr*100000/months)
  
  models.age_female_period[[paste0("m.",age_to_fit_female_c[i],"Colorectal", ".nb")]] <-models.age_female_pred[[paste0("m.",age_to_fit_female_c[i],"Colorectal", ".nb")]] %>% 
    group_by(denominator_sex, covid, outcome, denominator_age_group)%>% filter(covid != "Pre-COVID")%>% 
    summarise(events_t= sum(events), pred_t= sum(pred), lwr_t =sum(lwr), upr_t=sum(upr)) %>%
    mutate(red = (100*(pred_t-events_t)/pred_t)) %>%
    mutate(red_lwr = (100*(lwr_t-events_t)/lwr_t))%>%
    mutate(red_upr = (100*(upr_t-events_t)/upr_t))
  
  models.age_female_total[[paste0("m.",age_to_fit_female_c[i],"Colorectal", ".nb")]] <-  models.age_female_pred[[paste0("m.",age_to_fit_female_c[i],"Colorectal", ".nb")]]%>% 
    filter(covid != "Pre-COVID")%>% group_by(outcome, denominator_sex, denominator_age_group)%>%
    summarise(events_t= sum(events), pred_t= sum(pred), lwr_t =sum(lwr), upr_t=sum(upr)) %>%
    mutate(covid="Total")%>%
    mutate(red = (100*(pred_t-events_t)/pred_t)) %>%
    mutate(red_lwr = (100*(lwr_t-events_t)/lwr_t))%>%
    mutate(red_upr = (100*(upr_t-events_t)/upr_t))
  
  models.age_female_post[[paste0("m.",age_to_fit_female_c[i],"Colorectal", ".nb")]] <-  models.age_female_pred[[paste0("m.",age_to_fit_female_c[i],"Colorectal", ".nb")]]%>% 
    filter(covid2 == "Post-COVID")%>% group_by(outcome, denominator_sex, denominator_age_group)%>%
    summarise(events_t= sum(events), pred_t= sum(pred), lwr_t =sum(lwr), upr_t=sum(upr)) %>%
    mutate(covid="Post")%>%
    mutate(red = (100*(pred_t-events_t)/pred_t)) %>%
    mutate(red_lwr = (100*(lwr_t-events_t)/lwr_t))%>%
    mutate(red_upr = (100*(upr_t-events_t)/upr_t))
  
}



prediction_age.female_colorectal <- bind_rows(models.age_female_pred)%>% mutate_if(is.numeric, ~round(., 1)) 
predicton_periods_age_female_colorectal <- as.data.frame(rbind(bind_rows(models.age_female_period), bind_rows(models.age_female_total),bind_rows(models.age_female_post)))%>% arrange(outcome, covid) %>%
  mutate(red = round(red,1), pred_t=round(pred_t, 0),red_lwr = round(red_lwr,1), red_upr = round(red_upr,1))

tab_colorectal <- predicton_periods_age_female_colorectal %>% mutate(pred = paste0(paste(pred_t),"(", paste(lwr_t, upr_t), ")")) %>%
  mutate(red_perc = paste0(paste(red)," (", paste(red_lwr), " to ", paste(red_upr), ")")) %>% dplyr::select(covid,denominator_sex, outcome, denominator_age_group, pred, events_t, red_perc)
tab_red_colorectal <- tab_colorectal %>%
  dplyr::select(covid, denominator_sex, denominator_age_group, outcome,red_perc)%>%
  spread(key=covid,value=red_perc) %>%
  arrange(outcome)%>%
  mutate(value="Red_perc") %>%
  relocate("Easing of restrictions", .after= "Third lockdown") %>%
  relocate("Legal restrictions removed", .after= "Easing of restrictions") %>%
  relocate("Post", .after= "Legal restrictions removed") 

tab_dif_colorectal <- predicton_periods_age_female_colorectal %>%
  mutate(dif=pred_t-events_t)%>%
  mutate(dif_lwr= lwr_t-events_t)%>%
  mutate(dif_upr = upr_t-events_t)%>%
  mutate(under_dx = paste0(paste(dif)," (", paste(dif_lwr)," to ", paste(dif_upr), ")"))%>%
  dplyr::select(covid, denominator_sex, denominator_age_group, outcome,under_dx)%>%
  spread(key=covid,value=under_dx) %>% 
  arrange(outcome)%>%
  mutate(value = "Underdx") %>%
  relocate("Easing of restrictions", .after= "Third lockdown") %>%
  relocate("Legal restrictions removed", .after= "Easing of restrictions") %>%
  relocate("Post", .after= "Legal restrictions removed") 

tab_age.female_colorectal <- rbind(tab_red_colorectal, tab_dif_colorectal)


tab_female0_colorectal <- predicton_periods_age_female_colorectal %>% group_by(covid, denominator_sex, outcome) %>% 
  summarise(events_t_g = sum(events_t),pred_t2=sum(pred_t), lwr_t2=sum(lwr_t), upr_t2=sum(upr_t),
            red_t = (100*(pred_t2-events_t_g)/pred_t2), red_lwr =(100*(lwr_t2-events_t_g)/lwr_t2), red_upr=(100*(upr_t2-events_t_g)/upr_t2)) %>% 
  arrange(outcome, denominator_sex,  covid) %>%
  mutate(red_t = round(red_t,1), pred_t2=round(pred_t2, 0),red_lwr = round(red_lwr,1), red_upr = round(red_upr,1))

tab_female_colorectal<- tab_female0_colorectal%>%
  mutate(pred_t2 = paste0(paste(pred_t2)," (", paste(lwr_t2), " to ", paste(upr_t2), ")")) %>%
  mutate(red_t = paste0(paste(red_t)," (", paste(red_lwr), " to ", paste(red_upr), ")")) %>% 
  dplyr::select(covid, denominator_sex, outcome,red_t)%>%
  spread(key=covid,value=red_t) %>%
  arrange(outcome)%>%
  mutate(value="Red_perc") %>%
  relocate("Easing of restrictions", .after= "Third lockdown") %>%
  relocate("Legal restrictions removed", .after= "Easing of restrictions") %>%
  relocate("Post", .after= "Legal restrictions removed") 


tab_dif_colorectal <- tab_female0_colorectal %>%
  mutate(dif=pred_t2-events_t_g)%>%
  mutate(dif_lwr= lwr_t2-events_t_g)%>%
  mutate(dif_upr = upr_t2-events_t_g)%>%
  mutate(under_dx = paste0(paste(dif)," (", paste(dif_lwr)," to ", paste(dif_upr), ")"))%>%
  dplyr::select(covid, denominator_sex,  outcome,under_dx)%>%
  spread(key=covid,value=under_dx) %>% 
  arrange(outcome)%>%
  mutate(value = "Underdx") %>%
  relocate("Easing of restrictions", .after= "Third lockdown") %>%
  relocate("Legal restrictions removed", .after= "Easing of restrictions") %>%
  relocate("Post", .after= "Legal restrictions removed") 

tab_female_colorectal <-rbind(tab_female_colorectal, tab_dif_colorectal)


write.csv(tab_age.female_colorectal, file=here("4_Results", db.name, "Modelling", "age_female_red_table_colorectal.csv"))
write.csv(tab_female_colorectal, file=here("4_Results", db.name, "Modelling", "female_red_table_colorectal.csv"))
write.csv(tab_age.female_colorectal, file=here("4_Results", db.name, "Modelling", "age_female_red_table_colorectal.csv"))
write.csv(tab_colorectal, file=here("4_Results", db.name, "Modelling", "age.female_red_colorectal.csv"))
write.csv(tab_female0_colorectal, file=here("4_Results", db.name, "Modelling", "female_red_colorectal.csv"))
save(prediction_age.female_colorectal, file=here("4_Results", db.name,  "Modelling", "Prediction_age.female_colorectal.RData"))
save(models.age_female_colorectal.fit, file=here("4_Results", db.name,  "Modelling", "Pred_models.age_female_colorectal.fit.RData"))


rm(models.age_female_period, models.age_female_pred,
   models.age_female_post, models.age_female_total, 
   working.nb, age_to_fit_female, period.age_female,pred,i)

# Lung


#  - check that there are records in each of the combinations of outcome, age and sex. If there are some combinations with no rows, then may need to run these separately
save_counts <- count(IR.age_female, outcome, denominator_age_group, denominator_sex, name = "Freq") %>% print(n=Inf)

# only include age groups where there is more than one month of data (note that age 20-39 only has 0 cases)
age_to_fit_female_L <- c("0;150","40;59","60;79","80;150")
models.age_female_lung.fit <- list()
models.age_female_pred <- list()
period.age_female <- list()
models.age_female_period <- list()
models.age_female_total <- list()
models.age_female_post <- list()


for(i in 1:length(age_to_fit_female_L)){
  working.nb <- glm.nb(events ~ as.factor(month)+months.since.start,data=IR.age_female%>% filter(months.since.start<= end_mod) %>% 
                         filter(denominator_age_group==age_to_fit_female_L[i])%>% filter(outcome=="Lung"))
  models.age_female_lung.fit[[paste0("m.",age_to_fit_female_L[i],".nb")]]  <- working.nb
  pred <-predict(working.nb, newdata=IR.age_female %>% filter(denominator_age_group==age_to_fit_female_L[i])%>%
                   filter(outcome=="Lung"),type="response", se.fit = TRUE, interval= "prediction", level=0.95)
  
  models.age_female_pred[[paste0("m.",age_to_fit_female_L[i],"Lung", ".nb")]] <- cbind(IR.age_female %>%  filter(outcome=="Lung") %>% 
                                                                                               filter(denominator_age_group==age_to_fit_female_L[i]), data.frame(est=as.character(pred$fit), model="nb")) %>%  
    add_pi(working.nb, names = c("lwr", "upr"), alpha = 0.05) %>%
    mutate(ir_pred=pred*100000/months, lwr_pred=lwr*100000/months, upr_pred=upr*100000/months)
  
  models.age_female_period[[paste0("m.",age_to_fit_female_L[i],"Lung", ".nb")]] <-models.age_female_pred[[paste0("m.",age_to_fit_female_L[i],"Lung", ".nb")]] %>% 
    group_by(covid, outcome, denominator_sex, denominator_age_group)%>% filter(covid != "Pre-COVID")%>% 
    summarise(events_t= sum(events), pred_t= sum(pred), lwr_t =sum(lwr), upr_t=sum(upr)) %>%
    mutate(red = (100*(pred_t-events_t)/pred_t)) %>%
    mutate(red_lwr = (100*(lwr_t-events_t)/lwr_t))%>%
    mutate(red_upr = (100*(upr_t-events_t)/upr_t))
  
  models.age_female_total[[paste0("m.",age_to_fit_female_L[i],"Lung", ".nb")]] <-  models.age_female_pred[[paste0("m.",age_to_fit_female_L[i],"Lung", ".nb")]]%>% 
    filter(covid != "Pre-COVID")%>% group_by(outcome, denominator_sex, denominator_age_group)%>%
    summarise(events_t= sum(events), pred_t= sum(pred), lwr_t =sum(lwr), upr_t=sum(upr)) %>%
    mutate(covid="Total")%>%
    mutate(red = (100*(pred_t-events_t)/pred_t)) %>%
    mutate(red_lwr = (100*(lwr_t-events_t)/lwr_t))%>%
    mutate(red_upr = (100*(upr_t-events_t)/upr_t))
  
  models.age_female_post[[paste0("m.",age_to_fit_female_L[i],"Lung", ".nb")]] <-  models.age_female_pred[[paste0("m.",age_to_fit_female_L[i],"Lung", ".nb")]]%>% 
    filter(covid2 == "Post-COVID")%>% group_by(outcome, denominator_sex, denominator_age_group)%>%
    summarise(events_t= sum(events), pred_t= sum(pred), lwr_t =sum(lwr), upr_t=sum(upr)) %>%
    mutate(covid="Post")%>%
    mutate(red = (100*(pred_t-events_t)/pred_t)) %>%
    mutate(red_lwr = (100*(lwr_t-events_t)/lwr_t))%>%
    mutate(red_upr = (100*(upr_t-events_t)/upr_t))
  
}



prediction_age.female_lung <- bind_rows(models.age_female_pred)%>% mutate_if(is.numeric, ~round(., 1)) 
predicton_periods_age_female_lung <- as.data.frame(rbind(bind_rows(models.age_female_period), bind_rows(models.age_female_total),bind_rows(models.age_female_post)))%>% arrange(outcome, denominator_sex, covid) %>%
  mutate(red = round(red,1), pred_t=round(pred_t, 0),red_lwr = round(red_lwr,1), red_upr = round(red_upr,1))

tab_lung <- predicton_periods_age_female_lung %>% mutate(pred = paste0(paste(pred_t),"(", paste(lwr_t, upr_t), ")")) %>%
  mutate(red_perc = paste0(paste(red)," (", paste(red_lwr), " to ", paste(red_upr), ")")) %>% dplyr::select(covid, denominator_sex, outcome, denominator_age_group, pred, events_t, red_perc)
tab_red_lung <- tab_lung %>%
  dplyr::select(covid, denominator_sex, denominator_age_group, outcome,red_perc)%>%
  spread(key=covid,value=red_perc) %>%
  arrange(outcome)%>%
  mutate(value="Red_perc") %>%
  relocate("Easing of restrictions", .after= "Third lockdown") %>%
  relocate("Legal restrictions removed", .after= "Easing of restrictions") %>%
  relocate("Post", .after= "Legal restrictions removed") 

tab_dif_lung <- predicton_periods_age_female_lung %>%
  mutate(dif=pred_t-events_t)%>%
  mutate(dif_lwr= lwr_t-events_t)%>%
  mutate(dif_upr = upr_t-events_t)%>%
  mutate(under_dx = paste0(paste(dif)," (", paste(dif_lwr)," to ", paste(dif_upr), ")"))%>%
  dplyr::select(covid, denominator_sex, denominator_age_group, outcome,under_dx)%>%
  spread(key=covid,value=under_dx) %>% 
  arrange(outcome)%>%
  mutate(value = "Underdx") %>%
  relocate("Easing of restrictions", .after= "Third lockdown") %>%
  relocate("Legal restrictions removed", .after= "Easing of restrictions") %>%
  relocate("Post", .after= "Legal restrictions removed") 

tab_age.female_lung <- rbind(tab_red_lung, tab_dif_lung)


tab_female0_lung <- predicton_periods_age_female_lung %>% group_by(covid, denominator_sex, outcome) %>% 
  summarise(events_t_g = sum(events_t),pred_t2=sum(pred_t), lwr_t2=sum(lwr_t), upr_t2=sum(upr_t),
            red_t = (100*(pred_t2-events_t_g)/pred_t2), red_lwr =(100*(lwr_t2-events_t_g)/lwr_t2), red_upr=(100*(upr_t2-events_t_g)/upr_t2)) %>% 
  arrange(outcome, covid) %>%
  mutate(red_t = round(red_t,1), pred_t2=round(pred_t2, 0),red_lwr = round(red_lwr,1), red_upr = round(red_upr,1))

tab_female_lung<- tab_female0_lung%>%
  mutate(pred_t2 = paste0(paste(pred_t2)," (", paste(lwr_t2), " to ", paste(upr_t2), ")")) %>%
  mutate(red_t = paste0(paste(red_t)," (", paste(red_lwr), " to ", paste(red_upr), ")")) %>% 
  dplyr::select(covid, denominator_sex, outcome,red_t)%>%
  spread(key=covid,value=red_t) %>%
  arrange(outcome)%>%
  mutate(value="Red_perc") %>%
  relocate("Easing of restrictions", .after= "Third lockdown") %>%
  relocate("Legal restrictions removed", .after= "Easing of restrictions") %>%
  relocate("Post", .after= "Legal restrictions removed") 


tab_dif_lung <- tab_female0_lung %>%
  mutate(dif=pred_t2-events_t_g)%>%
  mutate(dif_lwr= lwr_t2-events_t_g)%>%
  mutate(dif_upr = upr_t2-events_t_g)%>%
  mutate(under_dx = paste0(paste(dif)," (", paste(dif_lwr)," to ", paste(dif_upr), ")"))%>%
  dplyr::select(covid, denominator_sex, outcome,under_dx)%>%
  spread(key=covid,value=under_dx) %>% 
  arrange(outcome)%>%
  mutate(value = "Underdx") %>%
  relocate("Easing of restrictions", .after= "Third lockdown") %>%
  relocate("Legal restrictions removed", .after= "Easing of restrictions") %>%
  relocate("Post", .after= "Legal restrictions removed") 

tab_female_lung <-rbind(tab_female_lung, tab_dif_lung)


write.csv(tab_age.female_lung, file=here("4_Results", db.name, "Modelling", "age_female_red_table_lung.csv"))
write.csv(tab_female_lung, file=here("4_Results", db.name, "Modelling", "female_red_table_lung.csv"))
write.csv(tab_age.female_lung, file=here("4_Results", db.name, "Modelling", "age_female_red_table_lung.csv"))
write.csv(tab_lung, file=here("4_Results", db.name, "Modelling", "age.female_red_lung.csv"))
write.csv(tab_female0_lung, file=here("4_Results", db.name, "Modelling", "female_red_lung.csv"))
save(prediction_age.female_lung, file=here("4_Results", db.name,  "Modelling", "Prediction_age.female_lung.RData"))
save(models.age_female_lung.fit, file=here("4_Results", db.name,  "Modelling", "Pred_models.age_female_lung.fit.RData"))


rm(models.age_female_period, models.age_female_pred,
   models.age_female_post, models.age_female_total, 
   working.nb, age_to_fit_female, period.age_female,pred,i)

# combine the rows from the 3 cancer outputs for females and save all relevant files

tab_age_female <- rbind(tab_age.female_breast, tab_age.female_colorectal, tab_age.female_lung)
tab_female <- rbind(tab_female_breast, tab_female_colorectal, tab_female_lung)


save(tab_age_female, tab_female, file=here("4_Results", db.name,  "Modelling", "Modelling_age_female_nb.RData"))
write.csv(tab_female, file=here("4_Results", db.name,  "Modelling","Modelling_female_nb.csv"))
write.csv(tab_age_female, file=here("4_Results", db.name,  "Modelling","Modelling_age_female_nb.csv"))

rm(IR.age_female, models.age_female,models.age_female_pred, pred,working.nb, age_to_fit_female, i)


# combine the rows from the all male and female outputs stratified by age and save all relevant files
tab_sex <- rbind(tab_female, tab_male)
tab_age_sex <- rbind(tab_age_female, tab_age.male)

save(tab_age_sex, file=here("4_Results", db.name,  "Modelling", "Modelling_age_sex_nb.RData"))
write.csv(tab_age_sex, file=here("4_Results", db.name,  "Modelling","Modelling_age_sex_nb.csv"))

save(tab_sex, file=here("4_Results", db.name,  "Modelling", "Modelling_sex_nb.RData"))
write.csv(tab_sex, file=here("4_Results", db.name,  "Modelling","Modelling_sex_nb.csv"))


# combine all the prediction data frames for the 3 cancers for females and then for males and females

prediction_age.sex <- rbind(prediction_age.female_breast, prediction_age.female_colorectal, prediction_age.female_lung, prediction_age.male)
save(prediction_age.sex, file=here("4_Results", db.name,  "Modelling", "Prediction_age_sex.RData"))
write.csv(prediction_age.sex, file=here("4_Results", db.name,  "Modelling","Prediction_age_sex.csv"))


## ================ PLOTS BY AGE AND SEX ================================= ##


# Age and sex
# Breast
#prediction_age.gender$denominator_sex <- factor(prediction_age.gender$denominator_sex, levels=rev(levels(prediction_age.gender$denominator_sex)))
#levels(prediction_age.gender$denominator_sex) <- c("Female", "Male")
#levels(prediction_age.gender$denominator_age_group) <- c("20;39", "40;59", "60;79", "0;150", "80;150")

age_sex_Breast <- prediction_age.sex  %>% 
  filter(outcome=="Breast") %>% 
  ggplot()+
  facet_grid(denominator_age_group~denominator_sex,scales="free")+
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

age_sex_Breast <-age_sex_Breast+ 
  theme(axis.text.x = element_text(angle=90),
        axis.title.y = element_text(size = 9),
        plot.margin=grid::unit(c(1,0.5,0,1), "cm") )+
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "4 month")+
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-01"))),linetype=2, color="black")+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("")

age_sex_Breast


# Colorectal

age_sex_Colorectal <- prediction_age.sex  %>% 
  filter(outcome=="Colorectal") %>% 
  ggplot()+
  facet_grid(denominator_age_group~denominator_sex,scales="free")+
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

age_sex_Colorectal <-age_sex_Colorectal+ 
  theme(axis.text.x = element_text(angle=90),
        axis.title.y = element_text(size = 9),
        plot.margin=grid::unit(c(1,0.5,0,1), "cm") )+
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "4 month")+
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-01"))),linetype=2, color="black")+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("")

age_sex_Colorectal



# Lung

age_sex_Lung <- prediction_age.sex  %>% 
  filter(outcome=="Lung") %>% 
  ggplot()+
  facet_grid(denominator_age_group~denominator_sex,scales="free")+
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

age_sex_Lung <-age_sex_Lung+ 
  theme(axis.text.x = element_text(angle=90),
        axis.title.y = element_text(size = 9),
        plot.margin=grid::unit(c(1,0.5,0,1), "cm") )+
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "4 month")+
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-01"))),linetype=2, color="black")+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("")

age_sex_Lung



# Prostate

age_sex_Prostate <- prediction_age.sex  %>% 
  filter(outcome=="Prostate") %>% 
  ggplot()+
  facet_grid(denominator_age_group~denominator_sex,scales="free")+
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

age_sex_Prostate <-age_sex_Prostate+ 
  theme(axis.text.x = element_text(angle=90),
        axis.title.y = element_text(size = 9),
        plot.margin=grid::unit(c(1,0.5,0,1), "cm") )+
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "4 month")+
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-01"))),linetype=2, color="black")+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("")

age_sex_Prostate

## COMBINE PLOTS

figure_modelling_age_sex <-ggarrange(age_sex_Breast, age_sex_Colorectal, age_sex_Lung, age_sex_Prostate,
                                        align="hv", ncol=2, nrow=2,
                                        labels = c("A) Breast Cancer", "B) Colorectal Cancer", "C) Lung Cancer", "D) Prostate Cancer"),font.label = list(size = 12),
                                        hjust = c(-0.25,-0.25),
                                        common.legend=TRUE, legend="right" )

figure_modelling_age_sex


ggsave(here("4_Results", db.name, "Plots", "Figure_2_modelling_age_sex.jpg"), figure_modelling_age_sex, dpi=300, scale = 1.25,  width = 16, height = 10)
ggsave(here("4_Results", db.name, "Plots", "Figure_2_modelling_age_sex.tiff"), figure_modelling_age_sex, dpi=300, scale = 1.25,  width = 16, height = 10)


# Save

ggsave(here("4_Results", db.name, "Plots", "Figure_2_prediction_age_sex.tiff"), figure_modelling_age_sex, dpi=300, scale = 2)
ggsave(here("4_Results", db.name, "Plots", "Figure_2_prediction_age_sex.jpg"), figure_modelling_age_sex, dpi=300, scale = 2.5)

rm(figure_prediction_overall, figure_age_gender,figure_ses, prediction_age.gender,predicion_overall, predicition_ses, 
   end_mod, j, outcomes_to_fit)
 





## ================ PLOTS BY AGE AND SEX ================================= ##
## ================ EDITED FOR PAPER     ================================= ##

# Age and sex
# Breast
#prediction_age.gender$denominator_sex <- factor(prediction_age.gender$denominator_sex, levels=rev(levels(prediction_age.gender$denominator_sex)))
#levels(prediction_age.gender$denominator_sex) <- c("Female", "Male")
#levels(prediction_age.gender$denominator_age_group) <- c("20;39", "40;59", "60;79", "0;150", "80;150")

age_sex_Breast <- prediction_age.sex  %>% 
  filter(outcome=="Breast") %>% 
  ggplot()+
  facet_grid(denominator_age_group~denominator_sex,scales="free")+
  geom_point(aes(Date,ir_m, colour= "Observed"))+
  geom_line(aes(Date,ir_m,colour= "Observed"))+
  theme(strip.text.x = element_text(size = 16))+
  theme(strip.text.y = element_text(size = 16))+
  
  geom_point(aes(Date,ir_pred,colour= "Expected"))+
  geom_line(aes(Date,ir_pred,colour= "Expected"))+
  geom_ribbon(aes(ymin = lwr_pred,ymax = upr_pred, x=Date),  fill = "blue", alpha = 0.1)+
  scale_color_manual(name= "", values=c(Observed="red", Expected="blue"))+
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month", limits= c(as.Date("2020-03-01"),as.Date("2021-11-01")),expand=c(0.005,0.005))+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )

age_sex_Breast <-age_sex_Breast+ 
  theme(axis.text.x = element_text(angle=90),
        axis.title.y = element_text(size = 14),
        plot.margin=grid::unit(c(1,0.5,0,1), "cm"),
        legend.text = element_text(size = 14))+
  
  #scale_x_date(date_labels = "%b %Y", date_breaks = "4 month")+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("")

age_sex_Breast


# Colorectal

age_sex_Colorectal <- prediction_age.sex  %>% 
  filter(outcome=="Colorectal") %>% 
  ggplot()+
  facet_grid(denominator_age_group~denominator_sex,scales="free")+
  geom_point(aes(Date,ir_m, colour= "Observed"))+
  geom_line(aes(Date,ir_m,colour= "Observed"))+
  theme(strip.text.x = element_text(size = 16))+
  theme(strip.text.y = element_text(size = 16))+
  
  geom_point(aes(Date,ir_pred,colour= "Expected"))+
  geom_line(aes(Date,ir_pred,colour= "Expected"))+
  geom_ribbon(aes(ymin = lwr_pred,ymax = upr_pred, x=Date),  fill = "blue", alpha = 0.1)+
  scale_color_manual(name= "", values=c(Observed="red", Expected="blue"))+
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month", limits= c(as.Date("2020-03-01"),as.Date("2021-11-01")),expand=c(0.005,0.005))+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )

age_sex_Colorectal <-age_sex_Colorectal+ 
  theme(axis.text.x = element_text(angle=90),
        axis.title.y = element_text(size = 14),
        plot.margin=grid::unit(c(1,0.5,0,1), "cm"),
        legend.text = element_text(size = 14))+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("")

age_sex_Colorectal



# Lung

age_sex_Lung <- prediction_age.sex  %>% 
  filter(outcome=="Lung") %>% 
  ggplot()+
  facet_grid(denominator_age_group~denominator_sex,scales="free")+
  geom_point(aes(Date,ir_m, colour= "Observed"))+
  geom_line(aes(Date,ir_m,colour= "Observed"))+
  theme(strip.text.x = element_text(size = 16))+
  theme(strip.text.y = element_text(size = 16))+
  
  geom_point(aes(Date,ir_pred,colour= "Expected"))+
  geom_line(aes(Date,ir_pred,colour= "Expected"))+
  geom_ribbon(aes(ymin = lwr_pred,ymax = upr_pred, x=Date),  fill = "blue", alpha = 0.1)+
  scale_color_manual(name= "", values=c(Observed="red", Expected="blue"))+
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month", limits= c(as.Date("2020-03-01"),as.Date("2021-11-01")),expand=c(0.005,0.005))+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )

age_sex_Lung <-age_sex_Lung+ 
  theme(axis.text.x = element_text(angle=90),
        axis.title.y = element_text(size = 14),
        plot.margin=grid::unit(c(1,0.5,0,1), "cm"),
        legend.text = element_text(size = 14))+

  ylab("Incidence rate per 100,000 person-months")+
  xlab("")

age_sex_Lung



# Prostate

age_sex_Prostate <- prediction_age.sex  %>% 
  filter(outcome=="Prostate") %>% 
  ggplot()+
  facet_grid(denominator_age_group~denominator_sex,scales="free")+
  geom_point(aes(Date,ir_m, colour= "Observed"))+
  geom_line(aes(Date,ir_m,colour= "Observed"))+
  theme(strip.text.x = element_text(size = 16))+
  theme(strip.text.y = element_text(size = 16))+
  
  geom_point(aes(Date,ir_pred,colour= "Expected"))+
  geom_line(aes(Date,ir_pred,colour= "Expected"))+
  geom_ribbon(aes(ymin = lwr_pred,ymax = upr_pred, x=Date),  fill = "blue", alpha = 0.1)+
  scale_color_manual(name= "", values=c(Observed="red", Expected="blue"))+
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month", limits= c(as.Date("2020-03-01"),as.Date("2021-11-01")),expand=c(0.005,0.005))+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )

age_sex_Prostate <-age_sex_Prostate+ 
  theme(axis.text.x = element_text(angle=90),
        axis.title.y = element_text(size = 14),
        plot.margin=grid::unit(c(1,0.5,0,1), "cm"),
        legend.text = element_text(size = 14))+

  ylab("Incidence rate per 100,000 person-months")+
  xlab("")

age_sex_Prostate

## COMBINE PLOTS

figure_modelling_age_sex_march2020 <-ggarrange(age_sex_Breast, age_sex_Colorectal, age_sex_Lung, age_sex_Prostate,
                                     align="hv", ncol=2, nrow=2,
                                     labels = c("A) Breast Cancer", "B) Colorectal Cancer", "C) Lung Cancer", "D) Prostate Cancer"),font.label = list(size = 12),
                                     hjust = c(-0.25,-0.25),
                                     common.legend=TRUE, legend="right" )

figure_modelling_age_sex_march2020


ggsave(here("4_Results", db.name, "Plots", "Figure_2_modelling_age_sex_march2020.jpg"), figure_modelling_age_sex_march2020, dpi=300, scale = 1.25,  width = 16, height = 10)
ggsave(here("4_Results", db.name, "Plots", "Figure_2_modelling_age_sex_march2020.tiff"), figure_modelling_age_sex_march2020, dpi=300, scale = 1.25,  width = 16, height = 10)




rm(figure_prediction_overall, figure_age_gender,figure_ses, prediction_age.gender,predicion_overall, predicition_ses, 
   end_mod, j, outcomes_to_fit)
