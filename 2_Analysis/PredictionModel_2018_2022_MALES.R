# ============================================================================ #
#                         NEGATIVE BINOMIAL REGRESSION                         #
#                            FOR CANCER/COVID STUDY                            #
#                          PREDICTION MODEL - MALES                            #
#                                Nicola Barclay                                #
#                                 17-01-2023                                   #
# ============================================================================ #



###### Observed vs expected (Negative Binomial Regression models)
###Forecast:
##Fit to 01/01/2018- 01/02/2020
##Forecast 01/03/2020 - 01/01/2022

load(here("1_DataPrep", "Data", "GeneralPop2018_22.RData"))


## SET UP FOR RUNNING PREDICTION MODEL FOR MALES SEPARATELY

IR.age_male <- inc_data_pred_final %>%  mutate(Month1 =paste(1,month, year, sep ="-"))  %>% filter(denominator_sex =="Male")

# TO RULE OUT DEC 2021 DATA FILTER THE FOLLOWING. THIS IS BECAUSE THERE IS INCOMPLETE DATA IN THAT MONTH AND THE PREDICTION IS INACCURATE.
IR.age_male  <- IR.age_male %>%  filter(month_year !="12/2021")

# filter out data from 2017 as I am using data from 2018 for this modelling 
IR.age_male <- IR.age_male %>% filter(IR.age_male$months.since.start>=1)


outcomes_to_fit_male<-c("Lung","Colorectal","Prostate")


IR.age_male$Date <- NA
IR.age_male$Date <- dmy(IR.age_male$Month1)
IR.age_male$Month1 <- NULL


age_to_fit_male <- IR.age_male %>%  ungroup() %>%dplyr::select("denominator_age_group")%>% distinct()%>%pull()
models.age_male.fit <- list()
models.age_male_pred <- list()

period.age_male <- list()
models.age_male_period <- list()
models.age_male_total <- list()
models.age_male_post <- list()
IR.age_male$covid2 <- NA
IR.age_male$covid2[which(IR.age_male$covid =="Pre-COVID")] <- "Pre-COVID"
IR.age_male$covid2[which(IR.age_male$covid =="Lockdown")] <- "Lockdown"
IR.age_male$covid2[which(IR.age_male$months.since.start >=31)] <- "Post-COVID"


end_mod <- 26 #month.since.start= Feb 2020 assuming start date in Jan 2018

## FOR MALES ONLY


for(j in 1:length(outcomes_to_fit_male)){
  for(i in 1:length(age_to_fit_male)){
      working.nb <- glm.nb(events ~ as.factor(month)+months.since.start,data=IR.age_male%>% filter(months.since.start<= end_mod) %>% 
                             filter(denominator_age_group==age_to_fit_male[i])%>% filter(outcome==outcomes_to_fit_male[j]))
      models.age_male.fit[[paste0("m.",age_to_fit_male[i],".nb")]]  <- working.nb
      pred <-predict(working.nb, newdata=IR.age_male %>% filter(denominator_age_group==age_to_fit_male[i])%>%
                       filter(outcome==outcomes_to_fit_male[j]),type="response", se.fit = TRUE, interval= "prediction", level=0.95)
      
      models.age_male_pred[[paste0("m.",age_to_fit_male[i],outcomes_to_fit_male[j], ".nb")]] <- cbind(IR.age_male %>%  filter(outcome==outcomes_to_fit_male[j]) %>% 
                                                                                                                 filter(denominator_age_group==age_to_fit_male[i]), data.frame(est=as.character(pred$fit), model="nb")) %>%  
        add_pi(working.nb, names = c("lwr", "upr"), alpha = 0.05) %>%
        mutate(ir_pred=pred*100000/months, lwr_pred=lwr*100000/months, upr_pred=upr*100000/months)
      
      models.age_male_period[[paste0("m.",age_to_fit_male[i],outcomes_to_fit_male[j], ".nb")]] <-models.age_male_pred[[paste0("m.",age_to_fit_male[i],outcomes_to_fit_male[j], ".nb")]] %>% 
        group_by(covid, denominator_sex, outcome, denominator_age_group)%>%filter(covid != "Pre-COVID")%>% 
        summarise(events_t= sum(events), pred_t= sum(pred), lwr_t =sum(lwr), upr_t=sum(upr)) %>%
        mutate(red = (100*(pred_t-events_t)/pred_t)) %>%
        mutate(red_lwr = (100*(lwr_t-events_t)/lwr_t))%>%
        mutate(red_upr = (100*(upr_t-events_t)/upr_t))
      
      models.age_male_total[[paste0("m.",age_to_fit_male[i],outcomes_to_fit_male[j], ".nb")]] <-  models.age_male_pred[[paste0("m.",age_to_fit_male[i],outcomes_to_fit_male[j], ".nb")]]%>% 
        filter(covid != "Pre-COVID")%>% group_by(outcome, denominator_sex, denominator_age_group)%>%
        summarise(events_t= sum(events), pred_t= sum(pred), lwr_t =sum(lwr), upr_t=sum(upr)) %>%
        mutate(covid="Total")%>%
        mutate(red = (100*(pred_t-events_t)/pred_t)) %>%
        mutate(red_lwr = (100*(lwr_t-events_t)/lwr_t))%>%
        mutate(red_upr = (100*(upr_t-events_t)/upr_t))
      
      models.age_male_post[[paste0("m.",age_to_fit_male[i],outcomes_to_fit_male[j], ".nb")]] <-  models.age_male_pred[[paste0("m.",age_to_fit_male[i],outcomes_to_fit_male[j], ".nb")]]%>% 
        filter(covid2 == "Post-COVID")%>% group_by(outcome, denominator_sex, denominator_age_group)%>%
        summarise(events_t= sum(events), pred_t= sum(pred), lwr_t =sum(lwr), upr_t=sum(upr)) %>%
        mutate(covid="Post")%>%
        mutate(red = (100*(pred_t-events_t)/pred_t)) %>%
        mutate(red_lwr = (100*(lwr_t-events_t)/lwr_t))%>%
        mutate(red_upr = (100*(upr_t-events_t)/upr_t))
      
  }
}


prediction_age.male <- bind_rows(models.age_male_pred)%>% mutate_if(is.numeric, ~round(., 1)) 
predicton_periods_age_male <- as.data.frame(rbind(bind_rows(models.age_male_period), bind_rows(models.age_male_total),bind_rows(models.age_male_post)))%>% arrange(outcome, denominator_sex, covid) %>%
  mutate(red = round(red,1), pred_t=round(pred_t, 0),red_lwr = round(red_lwr,1), red_upr = round(red_upr,1))

tab <- predicton_periods_age_male %>% mutate(pred = paste0(paste(pred_t),"(", paste(lwr_t, upr_t), ")")) %>%
  mutate(red_perc = paste0(paste(red)," (", paste(red_lwr), " to ", paste(red_upr), ")")) %>% dplyr::select(covid, outcome, denominator_sex, denominator_age_group, pred, events_t, red_perc)
tab_red <- tab %>%
  dplyr::select(covid, denominator_sex, denominator_age_group, outcome,red_perc)%>%
  spread(key=covid,value=red_perc) %>%
  arrange(outcome)%>%
  mutate(value="Red_perd") %>%
  relocate("Easing of restrictions", .after= "Third lockdown") %>%
  relocate("Legal restrictions removed", .after= "Easing of restrictions") %>%
  relocate("Post", .after= "Legal restrictions removed") 

tab_dif <- predicton_periods_age_male %>%
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

tab_age.male <- rbind(tab_red, tab_dif)


tab_male0 <- predicton_periods_age_male %>% group_by(covid, denominator_sex, outcome) %>% 
  summarise(events_t_g = sum(events_t),pred_t2=sum(pred_t), lwr_t2=sum(lwr_t), upr_t2=sum(upr_t),
            red_t = (100*(pred_t2-events_t_g)/pred_t2), red_lwr =(100*(lwr_t2-events_t_g)/lwr_t2), red_upr=(100*(upr_t2-events_t_g)/upr_t2)) %>% 
  arrange(outcome, covid) %>%
  mutate(red_t = round(red_t,1), pred_t2=round(pred_t2, 0),red_lwr = round(red_lwr,1), red_upr = round(red_upr,1))

tab_male<- tab_male0%>%
  mutate(pred_t2 = paste0(paste(pred_t2)," (", paste(lwr_t2), " to ", paste(upr_t2), ")")) %>%
  mutate(red_t = paste0(paste(red_t)," (", paste(red_lwr), " to ", paste(red_upr), ")")) %>% 
  dplyr::select(covid, denominator_sex, outcome,red_t)%>%
  spread(key=covid,value=red_t) %>%
  arrange(outcome)%>%
  mutate(value="Red_perc") %>%
  relocate("Easing of restrictions", .after= "Third lockdown") %>%
  relocate("Legal restrictions removed", .after= "Easing of restrictions") %>%
  relocate("Post", .after= "Legal restrictions removed") 


tab_dif <- tab_male0 %>%
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

tab_male <-rbind(tab_male, tab_dif)


write.csv(tab_age.male, file=here("4_Results", db.name, "Modelling", "Modelling_age_male_nb.csv"))
write.csv(tab_male, file=here("4_Results", db.name, "Modelling", "Modelling_male_nb.csv.csv"))
write.csv(tab_age.male, file=here("4_Results", db.name, "Modelling", "age_male_red_table.csv"))
save(prediction_age.male, file=here("4_Results", db.name,  "Modelling", "Prediction_age.male.RData"))
save(tab_age.male, tab_male, file=here("4_Results", db.name,  "Modelling", "Modelling_age_male_nb.RData"))
save(IR.age_male, file=here("4_Results", db.name,  "Modelling", "IR.age_male.RData"))


rm(IR.age_gender, models.age_gender, models.age_gender_period, models.age_gender_pred,
   models.age_gender_post, models.age_gender_total, prediction_periods_age_gender, tab, tab2,
   working.nb, age_to_fit,gender_to_fit, period.age_gender,pred,predicton_periods_age_gender, prediction_periods_age_gender)
