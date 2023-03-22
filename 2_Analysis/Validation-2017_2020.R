# ============================================================================ #
#                         NEGATIVE BINOMIAL REGRESSION                         #
#                            FOR CANCER/COVID STUDY                            #
#                               VALIDATION MODEL                               #
#                                Nicola Barclay                                #
#                                 23-02-2023                                   #
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
##Forecast 01/03/2019 - 01/02/2020

load(here("1_DataPrep", "Data", "GeneralPop2017_20.RData"))
Sys.setlocale("LC_TIME", "English")




#### Overall----------

IR.overall <- inc_data_final %>% mutate(Month1 =paste(1,month, year, sep ="-")) %>% filter(denominator_cohort_id ==1)
IR.overall$Date <- NA
IR.overall$Date <- dmy(IR.overall$Month1)
IR.overall$Month1 <- NULL

outcomes_to_fit<- inc_data_final %>% dplyr::select("outcome")%>% distinct()%>%pull()

###### Validation Overall: 
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
# SO NEGATIVE BINOMIAL IS APPROPRIATE BUT THERE ARE SOME CATEGORY COMBINATIONS THAT ARE NOT, SO IT MIGHT RUN BETTER WITH SOME AS A POISSON.
# THE OUTCOME HAS 152 ROWS AS WE ONLY PREDICT UP TO FEB 2020

for(j in 1:length(outcomes_to_fit)){
  
  # Negative Binomial
  working.nb <- glm.nb(events ~ as.factor(month)+months.since.start,data=IR.overall%>%  
                         filter(outcome==outcomes_to_fit[j]) %>% filter(months.since.start<= end_mod))
  models.overall_validation.fit[[paste0("m.","overall",outcomes_to_fit[j], ".nb")]] <- working.nb
  
  pred <-predict(working.nb, newdata=IR.overall %>% filter(months.since.start<= end_pred)  %>%
                   filter(outcome==outcomes_to_fit[j]),type="response", se.fit = TRUE, interval= "prediction", level=0.95)
  
  models.overall_pred[[paste0("m.","overall",outcomes_to_fit[j], ".nb")]] <- cbind(IR.overall %>% filter(months.since.start<= end_pred) %>% filter(outcome==outcomes_to_fit[j]),
                                                                                   data.frame(est=as.character(pred$fit), model="nb")) %>%  add_pi(working.nb, names = c("lwr", "upr"), alpha = 0.05)
  
}

val_overall <- bind_rows(models.overall_pred) %>% mutate(ir_pred =pred*100000/months, lwr_pred=lwr*100000/months, upr_pred=upr*100000/months) %>%
  mutate_if(is.numeric, ~round(., 1)) %>% dplyr::select(-c(model, est))

save(val_overall, file=here("4_Results", db.name,  "Validation","Validation_model_overall_nb.RData"))
save(models.overall_validation.fit, file=here("4_Results", db.name,  "Validation", "Validation_model_overall_nb_fit.RData"))
write.csv(val_overall, file=here("4_Results", db.name,  "Validation","Validation_model_overall_nb.csv"))

rm(working.nb, pred, models.overall_pred, models.overall_validation.fit, j)


# overall with poisson instead of negative binomial

models.overall_validation.fit <- list()
models.overall_pred <- list()

for(j in 1:length(outcomes_to_fit)){
  
  working.p <- glm(events ~ as.factor(month)+months.since.start,data=IR.overall%>%  
                         filter(outcome==outcomes_to_fit[j]) %>% filter(months.since.start<= end_mod), family=poisson)
  models.overall_validation.fit[[paste0("m.","overall",outcomes_to_fit[j], ".p")]] <- working.p
  
  pred <-predict(working.p, newdata=IR.overall %>% filter(months.since.start<= end_pred)  %>%
                   filter(outcome==outcomes_to_fit[j]),type="response", se.fit = TRUE, interval= "prediction", level=0.95)
  
  models.overall_pred[[paste0("m.","overall",outcomes_to_fit[j], ".p")]] <- cbind(IR.overall %>% filter(months.since.start<= end_pred) %>% filter(outcome==outcomes_to_fit[j]),
                                                                                   data.frame(est=as.character(pred$fit), model="poisson")) %>%  add_pi(working.p, names = c("lwr", "upr"), alpha = 0.05)
  
}

val_overall_p <- bind_rows(models.overall_pred) %>% mutate(ir_pred =pred*100000/months, lwr_pred=lwr*100000/months, upr_pred=upr*100000/months) %>%
  mutate_if(is.numeric, ~round(., 1)) %>% dplyr::select(-c(model, est))

save(val_overall, file=here("4_Results", db.name,  "Validation","Validation_model_overall_poisson.RData"))
save(models.overall_validation.fit, file=here("4_Results", db.name,  "Validation","Fit Statistics", "Validation_model_overall_poisson_fit.RData"))
write.csv(val_overall, file=here("4_Results", db.name,  "Validation","Validation_model_overall_poisson.csv"))

rm(working.p, pred, models.overall_pred, models.overall_validation.fit, j)



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
models.age_male.fit <- list()
models.age_male_pred <- list()


age_to_fit_female <- IR.age_female %>%  ungroup() %>%dplyr::select("denominator_age_group")%>% distinct()%>%pull()
models.age_female.fit <- list()
models.age_female_pred <- list()

end_mod <- 26 #month.since.start= Feb 2019
end_pred <- 38 #month.since.start= Feb 2020


# FOR MALES ONLY - THIS RUNS FINE

for(j in 1:length(outcomes_to_fit_male)){
  for(i in 1:length(age_to_fit_male)){
      working.nb <- glm.nb(events ~ as.factor(month)+months.since.start,data=IR.age_male%>% filter(months.since.start<= end_mod) %>% 
                             filter(denominator_age_group==age_to_fit_male[i])%>% filter(outcome==outcomes_to_fit_male[j]))
      
      models.age_male.fit[[paste0("m.",age_to_fit_male[i],".nb")]]  <- working.nb
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


save(val_age_male, file=here("4_Results", db.name,  "Validation", "Validation_age_male_nb.RData"))
save(models.age_male.fit, file=here("4_Results", db.name,  "Validation",  "Validation_model_male_nb_fit.RData"))
write.csv(val_age_male, file=here("4_Results", db.name,  "Validation","Validation_age_male_nb.csv"))

rm(models.age_male.fit,models.age_male_pred, pred,working.nb, age_to_fit_male, i, j)



# FOR FEMALES ONLY - this does not run

#for(j in 1:length(outcomes_to_fit_female)){
#  for(i in 1:length(age_to_fit_female)){
#    working.nb <- glm.nb(events ~ as.factor(month)+months.since.start,data=IR.age_female%>% filter(months.since.start<= end_mod) %>% 
#                           filter(denominator_age_group==age_to_fit_female[i])%>% filter(outcome==outcomes_to_fit_female[j]))
#    
#    models.age_female.fit[[paste0("m.",age_to_fit_female[i],".nb")]]  <- working.nb
#    pred <-predict(working.nb, newdata=IR.age_female %>% filter(months.since.start<= end_pred) %>% filter(denominator_age_group==age_to_fit_female[i])%>%
#                     filter(outcome==outcomes_to_fit_female[j]),type="response", se.fit = TRUE, interval= "prediction", level=0.95)
#    
#    models.age_female_pred[[paste0("m.",age_to_fit_female[i],outcomes_to_fit_female[j], ".nb")]] <- cbind(IR.age_female %>% filter(months.since.start<= end_pred) %>% 
#                                                                                                      filter(outcome==outcomes_to_fit_female[j])%>%  
#                                                                                                      filter(denominator_age_group==age_to_fit_female[i]), data.frame(est=as.character(pred$fit), model="nb")) %>%  
#      add_pi(working.nb, names = c("lwr", "upr"), alpha = 0.05) %>%
#      mutate(ir_pred=pred*100000/months, lwr_pred=lwr*100000/months, upr_pred=upr*100000/months) %>%
#      mutate(red = (100*(events-pred)/pred))
#  }
#}

#val_age_female <- bind_rows(models.age_female_pred) %>% mutate(ir_pred =pred*100000/months, lwr_pred=lwr*100000/months, upr_pred=upr*100000/months) %>% 
#  mutate_if(is.numeric, ~round(., 1)) %>% dplyr::select(-c(model, est))#


#save(val_age_female, file=here("4_Results", db.name,  "Validation", "Validation_age_female_nb.RData"))
#save(models.age_female.fit, file=here("4_Results", db.name,  "Validation",  "Validation_model_female_nb_fit.RData"))
#write.csv(val_age_female, file=here("4_Results", db.name,  "Validation","Validation_age_female_nb.csv"))

#rm(models.age_female.fit,models.age_female_pred, pred,working.nb, age_to_fit_female, i, j)
#






###### Validation by age- FOR FEMALES ONLY 
# run this separately by each cancer as combined it did not work with the multiple category for loops

# Breast - this works through all the age groups fine.

age_to_fit_female <- IR.age_female %>%  ungroup() %>%dplyr::select("denominator_age_group")%>% distinct()%>%pull()
models.age_female_breast.fit <- list()
models.age_female_pred <- list()

for(i in 1:length(age_to_fit_female)){
  
  working.nb <- glm.nb(events ~ as.factor(month)+months.since.start,data=IR.age_female%>% filter(months.since.start<= end_mod) %>% 
                         filter(denominator_age_group==age_to_fit_female[i])%>% filter(outcome=="Breast"))
  
  models.age_female_breast.fit[[paste0("m.",age_to_fit_female[i],".nb")]]  <- working.nb
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

save(models.age_female_breast.fit, file=here("4_Results", db.name,  "Validation",  "Validation_model_female_nb_fit_breast.RData"))
save(val_age_female_breast, file=here("4_Results", db.name,  "Validation",  "val_age_female_breast.RData"))
rm(models.age_female_breast.fit,models.age_female_pred, pred,working.nb, age_to_fit_female, i)


# Colorectal - this does not work through all the ages as there are some categories with too few frequencies:

#  - check that there are records in each of the combinations of outcome, age and sex. If there are some combinations with no rows, then may need to run these separately
save_counts <- count(IR.age_female, outcome, denominator_age_group, denominator_sex, name = "Freq") %>% print(n=Inf)

age_to_fit_female_c <- c("0;150","80;150","60;79","40;59")
models.age_female_colorectal.fit <- list()
models.age_female_pred <- list()

end_mod <- 26 #month.since.start= Feb 2019
end_pred <- 38 #month.since.start= Feb 2020


  for(i in 1:length(age_to_fit_female_c)){
      working.nb <- glm.nb(events ~ as.factor(month)+months.since.start,data=IR.age_female%>% filter(months.since.start<= end_mod) %>% 
                           filter(denominator_age_group==age_to_fit_female_c[i])%>% filter(outcome=="Colorectal"))
    
    models.age_female_colorectal.fit[[paste0("m.",age_to_fit_female_c[i],".nb")]]  <- working.nb
    pred <-predict(working.nb, newdata=IR.age_female %>% filter(months.since.start<= end_pred) %>% filter(denominator_age_group==age_to_fit_female_c[i])%>%
                     filter(outcome=="Colorectal"),type="response", se.fit = TRUE, interval= "prediction", level=0.95)
    
    models.age_female_pred[[paste0("m.",age_to_fit_female_c[i],"Colorectal", ".nb")]] <- cbind(IR.age_female %>% filter(months.since.start<= end_pred) %>% 
                                                                                                            filter(outcome=="Colorectal")%>%  
                                                                                                            filter(denominator_age_group==age_to_fit_female_c[i]), data.frame(est=as.character(pred$fit), model="nb")) %>%  
      add_pi(working.nb, names = c("lwr", "upr"), alpha = 0.05) %>%
      mutate(ir_pred=pred*100000/months, lwr_pred=lwr*100000/months, upr_pred=upr*100000/months) %>%
      mutate(red = (100*(events-pred)/pred))
  }


val_age_female_colorectal <- bind_rows(models.age_female_pred) %>% mutate(ir_pred =pred*100000/months, lwr_pred=lwr*100000/months, upr_pred=upr*100000/months) %>% 
  mutate_if(is.numeric, ~round(., 1)) %>% dplyr::select(-c(model, est))

save(models.age_female_colorectal.fit, file=here("4_Results", db.name,  "Validation",  "Validation_model_female_nb_fit_colorectal.RData"))
save(val_age_female_colorectal, file=here("4_Results", db.name,  "Validation",  "val_age_female_colorectal.RData"))
rm(models.age_female_colorectal.fit, models.age_female_pred, pred,working.nb, age_to_fit_female, i)

# need to figure out what to do with the 1 row in 20;39 age group that hasn't run. This i have currently excluded but need to run this.


# Lung

age_to_fit_female_L <- c("0;150","40;59","60;79","80;150")
models.age_female_lung.fit <- list()
models.age_female_pred <- list()

for(i in 1:length(age_to_fit_female_L)){
  
  working.nb <- glm.nb(events ~ as.factor(month)+months.since.start,data=IR.age_female%>% filter(months.since.start<= end_mod) %>% 
                         filter(denominator_age_group==age_to_fit_female_L[i])%>% filter(outcome=="Lung"))
  
  models.age_female_lung.fit[[paste0("m.",age_to_fit_female_L[i],".nb")]]  <- working.nb
  pred <-predict(working.nb, newdata=IR.age_female %>% filter(months.since.start<= end_pred) %>% filter(denominator_age_group==age_to_fit_female_L[i])%>%
                   filter(outcome=="Lung"),type="response", se.fit = TRUE, interval= "prediction", level=0.95)
  
  models.age_female_pred[[paste0("m.",age_to_fit_female_L[i],"Lung", ".nb")]] <- cbind(IR.age_female %>% filter(months.since.start<= end_pred) %>% 
                                                                                         filter(outcome=="Lung")%>%  
                                                                                         filter(denominator_age_group==age_to_fit_female_L[i]), data.frame(est=as.character(pred$fit), model="nb")) %>%  
    add_pi(working.nb, names = c("lwr", "upr"), alpha = 0.05) %>%
    mutate(ir_pred=pred*100000/months, lwr_pred=lwr*100000/months, upr_pred=upr*100000/months) %>%
    mutate(red = (100*(events-pred)/pred))
}

val_age_female_lung <- bind_rows(models.age_female_pred) %>% mutate(ir_pred =pred*100000/months, lwr_pred=lwr*100000/months, upr_pred=upr*100000/months) %>% 
  mutate_if(is.numeric, ~round(., 1)) %>% dplyr::select(-c(model, est))

save(models.age_female_lung.fit, file=here("4_Results", db.name,  "Validation",  "Validation_model_female_nb_fit_lung.RData"))
save(val_age_female_lung, file=here("4_Results", db.name,  "Validation",  "val_age_female_lung.RData"))
rm(models.age_female_lung.fit,models.age_female_pred, pred,working.nb, age_to_fit_female, i)


# combine the rows from the 3 cancer outputs for females and save all relevant files

val_age_female <- rbind(val_age_female_breast, val_age_female_colorectal, val_age_female_lung)


save(val_age_female, file=here("4_Results", db.name,  "Validation", "Validation_age_female_nb.RData"))
write.csv(val_age_female, file=here("4_Results", db.name,  "Validation","Validation_age_female_nb.csv"))

rm(IR.age_female, models.age_female,models.age_female_pred, pred,working.nb, age_to_fit_female)


# combine the rows from the all male and female outputs and save all relevant files
val_age_sex <- rbind(val_age_male, val_age_female)


save(val_age_sex, file=here("4_Results", db.name,  "Validation", "Validation_age_sex_nb.RData"))
write.csv(val_age_sex, file=here("4_Results", db.name,  "Validation","Validation_age_sex_nb.csv"))


###### Validation by SES: run this when we have the data

IR.ses <- IR.ses %>% mutate(Month1 =paste(1,month, year, sep ="-"))
IR.ses$Date <- NA
IR.ses$Date <- dmy(IR.ses$Month1)
IR.ses$Month1 <- NULL
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
   end_mod,end_pred, i,j,outcomes_to_fit, i, j)




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

# Save
ggsave(here("4_Results", db.name, "Plots", "Figure_1_validation_overall.jpg"), figure_Validation_overall, dpi=300, scale = 1, width = 12, height = 9)



## PLOTS STRATIFIED BY AGE AND GENDER

# Breast # 

age_female_Breast <- val_age_sex  %>% 
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

age_female_Breast <-age_female_Breast+ 
  theme(axis.text.x = element_text(angle=90),
        axis.title.y = element_text(size = 9),
        plot.margin=grid::unit(c(1,0.5,0,1), "cm") )+
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-01"))),linetype=2, color="black")+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("")

# COLORECTAL # 

age_sex_Colorectal <- val_age_sex  %>% 
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
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-01"))),linetype=2, color="black")+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("")

# LUNG # 

age_sex_Lung <- val_age_sex  %>% 
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
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-01"))),linetype=2, color="black")+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("")


# PROSTATE # 

age_male_Prostate <- val_age_sex  %>% 
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

age_male_Prostate <-age_male_Prostate+ 
  theme(axis.text.x = element_text(angle=90),
        axis.title.y = element_text(size = 9),
        plot.margin=grid::unit(c(1,0.5,0,1), "cm") )+
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-01"))),linetype=2, color="black")+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("")


figure_age_gender <-ggarrange(age_female_Breast, age_sex_Colorectal, age_sex_Lung, age_male_Prostate,
                              align="hv", ncol=2, nrow=2,
                              labels = c("A) Breast Cancer", "B) Colorectal Cancer", "C) Lung Cancer", "D) Prostate Cancer"),font.label = list(size = 12),
                              hjust = c(-0.25,-0.25),
                              common.legend=TRUE, legend="right" )

ggsave(here("4_Results", db.name, "Plots", "Figure_2_validation_age_gender.jpg"), figure_age_gender, dpi=300, scale = 1.25,  width = 16, height = 10)


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



ggsave(here("4_Results", db.name, "Plots", "Figure_3_ses.jpg"), figure_ses, dpi=300, scale = 2)


rm(figure_overall, figure_age_gender,figure_ses, prediction_age.gender,predicion_overall, predicition_ses, 
   end_mod, j, outcomes_to_fit)
