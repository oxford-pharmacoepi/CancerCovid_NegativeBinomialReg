# ============================================================================ #
#                         NEGATIVE BINOMIAL REGRESSION                         #
#                            FOR CANCER/COVID STUDY                            #
#                          PREDICTION MODEL - OVERALL                          #
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
library(flextable)


###### Observed vs expected (Negative Binomial Regression models)
###Forecast:
##Fit to 01/01/2018- 01/02/2020
##Forecast 01/03/2020 - 01/01/2022

load(here("1_DataPrep", "Data", "GeneralPop2018_22.RData"))


IR.overall <- inc_data_pred_final %>% mutate(Month1 =paste(1,month, year, sep ="-")) %>% filter(denominator_cohort_id ==1)

# TO RULE OUT DEC 2021 DATA FILTER THE FOLLOWING. THIS IS BECAUSE THERE IS INCOMPLETE DATA IN THAT MONTH AND THE PREDICTION IS INACCURATE.
IR.overall <- IR.overall %>%  filter(month_year !="12/2021")

#IR.age_gender <- inc_data_pred_final %>%  mutate(Month1 =paste(1,month, year, sep ="-"))
#IR.ses <- IR.ses %>% mutate(Month1 =paste(1,month, year, sep ="-")) 
outcomes_to_fit<- inc_data_pred_final %>% dplyr::select("outcome")%>% distinct()%>%pull()


#### Overall----------
Sys.setlocale("LC_TIME", "English")
IR.overall$Date <- NA
IR.overall$Date <- dmy(IR.overall$Month1)
IR.overall$Month1 <- NULL

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
  mutate(value = "Red_perc")  %>%
  relocate("Easing of restrictions", .after= "Third lockdown") %>%
  relocate("Legal restrictions removed", .after= "Easing of restrictions") %>%
  relocate("Post", .after= "Legal restrictions removed") 


tab_dif <- predicton_overall_periods %>%
  mutate(dif=pred_t-events_t)%>%
  mutate(dif_lwr= lwr_t-events_t)%>%
  mutate(dif_upr = upr_t-events_t)%>%
  mutate(under_dx = paste0(paste(dif)," (", paste(dif_lwr)," to ", paste(dif_upr), ")"))%>%
  dplyr::select(covid, outcome,under_dx) %>%
  spread(key=covid,value=under_dx) %>% 
  mutate(value = "Underdx") %>%
  relocate("Easing of restrictions", .after= "Third lockdown") %>%
  relocate("Legal restrictions removed", .after= "Easing of restrictions") %>%
  relocate("Post", .after= "Legal restrictions removed") 

tab1 <-rbind(tab_red, tab_dif)

write.csv(tab1, file=here("4_Results", db.name, "Modelling", "overall_table.csv"))
write.csv(tab, file=here("4_Results", db.name, "Modelling", "overall_red.csv"))
save(prediction_overall, predicton_overall_periods, tab_red, tab_dif, tab, tab1, file=here("4_Results", db.name,  "Modelling", "Prediction_Overall_2018-2022.RData"))
save(IR.overall, file=here("4_Results", db.name,  "Modelling", "IR.overall.RData"))
rm(IR.overall,models, models_period, models_post, models_pred, models_total, pred,
   prediction_overall_periods, tab, working.nb, predicton_overall_periods)



###TABLE--------- 
tab1 <- tab1 %>% mutate(strata="overall")
tab_sex <-tab_sex %>% rename(strata=denominator_sex)
tab_age_sex <-tab_age_sex %>% mutate(strata=paste0(denominator_age_group, ",", denominator_sex))%>%
  dplyr::select(-denominator_age_group, -denominator_sex)
# tab_ses <-tab_ses %>%rename(strata=medea)

tab1_perc_red_diagnoses_formatted <- tab1 %>% arrange(outcome)
tab1_perc_red_diagnoses_formatted <- tab1_perc_red_diagnoses_formatted[c(1,10,2,8,3,4,5,6,7,9)]
names(tab1_perc_red_diagnoses_formatted)[1] = "Cancer"
names(tab1_perc_red_diagnoses_formatted)[2] = "Parameter"
names(tab1_perc_red_diagnoses_formatted)[3] = "Lockdown (March 2020-June 2020)"
names(tab1_perc_red_diagnoses_formatted)[4] = "Post-lockdown (July 2020-Dec 2021)"
names(tab1_perc_red_diagnoses_formatted)[5] = "Post-lockdown 1 (July 2020-Oct 2020)"
names(tab1_perc_red_diagnoses_formatted)[6] = "Second lockdown (Nov 2020-Dec 2020)"
names(tab1_perc_red_diagnoses_formatted)[7] = "Third lockdown (Jan 2021-Feb 2021)"
names(tab1_perc_red_diagnoses_formatted)[8] = "Easing of restrictions (March 2021-June 2021)"
names(tab1_perc_red_diagnoses_formatted)[9] = "Legal restrictions removed (July 2021-Dec 2021)"
names(tab1_perc_red_diagnoses_formatted)[10] = "Total (March 2020-Dec 2021)"
tab1_perc_red_diagnoses_formatted <- tab1_perc_red_diagnoses_formatted %>% 
                                      mutate(Parameter = case_when(Parameter == "Red_perc" ~ "Percent reduction",
                                                                  Parameter == "Underdx" ~ "N underdiagnoses"))

Pretty_modelling_results_overall_table <- flextable(tab1_perc_red_diagnoses_formatted) %>% theme_vanilla() %>% 
  set_caption(caption = "Number of cancer underdiagnoses and proportion reduced since pre-COVID, in each of the time periods") %>% 
  width(width = 1.4) 

save_as_docx('Table_Modelling_overall_results' = Pretty_modelling_results_overall_table, path=here("4_Results", db.name, "Modelling", "Table_Modelling_Results_overall_perc_red.docx"))

write.csv(tab1_perc_red_diagnoses_formatted, file=here("4_Results", db.name, "Modelling", "Table_Modelling_Overall_Results.csv"))



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
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month", limits= c(as.Date("2018-01-01"),as.Date("2021-11-01")),expand=c(0.005,0.005))+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

overall_prediction_Breast <- overall_prediction_Breast + 
  theme(axis.text.x = element_text(angle=90), 
        axis.title.y = element_text(size = 14),    
        plot.margin=grid::unit(c(1,1,0,1), "cm"),
        legend.text = element_text(size = 14))+
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-01"))),linetype=2, color="black")+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("") #+
  #ggtitle("Incidence rates for breast cancer before and after COVID-19 lockdown")

# Colorectal
overall_prediction_Colorectal <- prediction_overall %>% filter(outcome=="Colorectal")%>%
  ggplot()+
  geom_point(aes(Date,ir_m, colour= "Observed"))+
  geom_line(aes(Date,ir_m,colour= "Observed"))+
  
  geom_point(aes(Date,ir_pred,colour= "Expected"))+
  geom_line(aes(Date,ir_pred,colour= "Expected"))+
  geom_ribbon(aes(ymin = lwr_pred,ymax = upr_pred, x=Date),  fill = "blue", alpha = 0.1)+
  scale_color_manual(name= "", values=c(Observed="red", Expected="blue"))+
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month", limits= c(as.Date("2018-01-01"),as.Date("2021-11-01")),expand=c(0.005,0.005))+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

overall_prediction_Colorectal <- overall_prediction_Colorectal + 
  theme(axis.text.x = element_text(angle=90), 
        axis.title.y = element_text(size = 14),    
        plot.margin=grid::unit(c(1,1,0,1), "cm"),
        legend.text = element_text(size = 14))+
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-01"))),linetype=2, color="black")+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("")#+
  #ggtitle("Incidence rates for colorectal cancer before and after COVID-19 lockdown")


# Lung
overall_prediction_Lung <- prediction_overall %>% filter(outcome=="Lung")%>%
  ggplot()+
  geom_point(aes(Date,ir_m, colour= "Observed"))+
  geom_line(aes(Date,ir_m,colour= "Observed"))+
  
  geom_point(aes(Date,ir_pred,colour= "Expected"))+
  geom_line(aes(Date,ir_pred,colour= "Expected"))+
  geom_ribbon(aes(ymin = lwr_pred,ymax = upr_pred, x=Date),  fill = "blue", alpha = 0.1)+
  scale_color_manual(name= "", values=c(Observed="red", Expected="blue"))+
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month", limits= c(as.Date("2018-01-01"),as.Date("2021-11-01")),expand=c(0.005,0.005))+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

overall_prediction_Lung <- overall_prediction_Lung + 
  theme(axis.text.x = element_text(angle=90), 
        axis.title.y = element_text(size = 14),    
        plot.margin=grid::unit(c(1,1,0,1), "cm"),
        legend.text = element_text(size = 14))+
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-01"))),linetype=2, color="black")+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("")#+
  #ggtitle("Incidence rates for Lung cancer before and after COVID-19 lockdown")

# Prostate
overall_prediction_Prostate <- prediction_overall %>% filter(outcome=="Prostate")%>%
  ggplot()+
  geom_point(aes(Date,ir_m, colour= "Observed"))+
  geom_line(aes(Date,ir_m,colour= "Observed"))+
  
  geom_point(aes(Date,ir_pred,colour= "Expected"))+
  geom_line(aes(Date,ir_pred,colour= "Expected"))+
  geom_ribbon(aes(ymin = lwr_pred,ymax = upr_pred, x=Date),  fill = "blue", alpha = 0.1)+
  scale_color_manual(name= "", values=c(Observed="red", Expected="blue"))+
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month", limits= c(as.Date("2018-01-01"),as.Date("2021-11-01")),expand=c(0.005,0.005))+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

overall_prediction_Prostate <- overall_prediction_Prostate + 
  theme(axis.text.x = element_text(angle=90), 
        axis.title.y = element_text(size = 14),    
        plot.margin=grid::unit(c(1,1,0,1), "cm"),
        legend.text = element_text(size = 14))+
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-01"))),linetype=2, color="black")+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("")#+
  #ggtitle("Incidence rates for prostate cancer before and after COVID-19 lockdown")

figure_prediction_overall<-ggarrange(overall_prediction_Breast, overall_prediction_Colorectal, overall_prediction_Lung, overall_prediction_Prostate, 
                    align="hv", ncol=2, nrow=2,
                    labels = c("A) Breast Cancer", "B) Colorectal Cancer", "C) Lung Cancer", "D) Prostate Cancer"),font.label = list(size = 12),
                    hjust = c(-0.25,-0.25),
                    common.legend=TRUE, legend="right" )



# Save

ggsave(here("4_Results", db.name, "Plots", "Figure_1_prediction_overall.tiff"), figure_prediction_overall, dpi=600, scale = 1.25,  width = 16, height = 10)
ggsave(here("4_Results", db.name, "Plots", "Figure_1_prediction_overall.jpg"), figure_prediction_overall, dpi=600, scale = 1.25,  width = 16, height = 10)

rm(figure_prediction_overall, figure_age_gender,figure_ses, prediction_age.gender,predicion_overall, predicition_ses, 
   end_mod, j, outcomes_to_fit)




##### PLOTS - edited for paper --------

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
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month", limits= c(as.Date("2020-03-01"),as.Date("2021-11-01")),expand=c(0.005,0.005))+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

overall_prediction_Breast <- overall_prediction_Breast + 
  theme(axis.text.x = element_text(angle=90), 
        axis.title.y = element_text(size = 14),    
        plot.margin=grid::unit(c(1,1,0,1), "cm"),
        legend.text = element_text(size = 14))+
  #geom_vline(xintercept=as.numeric(as.Date(c("2020-03-01"))),linetype=2, color="black")+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("") #+
#ggtitle("Incidence rates for breast cancer before and after COVID-19 lockdown")

# Colorectal
overall_prediction_Colorectal <- prediction_overall %>% filter(outcome=="Colorectal")%>%
  ggplot()+
  geom_point(aes(Date,ir_m, colour= "Observed"))+
  geom_line(aes(Date,ir_m,colour= "Observed"))+
  
  geom_point(aes(Date,ir_pred,colour= "Expected"))+
  geom_line(aes(Date,ir_pred,colour= "Expected"))+
  geom_ribbon(aes(ymin = lwr_pred,ymax = upr_pred, x=Date),  fill = "blue", alpha = 0.1)+
  scale_color_manual(name= "", values=c(Observed="red", Expected="blue"))+
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month", limits= c(as.Date("2020-03-01"),as.Date("2021-11-01")),expand=c(0.005,0.005))+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

overall_prediction_Colorectal <- overall_prediction_Colorectal + 
  theme(axis.text.x = element_text(angle=90), 
        axis.title.y = element_text(size = 14),    
        plot.margin=grid::unit(c(1,1,0,1), "cm"),
        legend.text = element_text(size = 14))+
 # geom_vline(xintercept=as.numeric(as.Date(c("2020-03-01"))),linetype=2, color="black")+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("")#+
#ggtitle("Incidence rates for colorectal cancer before and after COVID-19 lockdown")


# Lung
overall_prediction_Lung <- prediction_overall %>% filter(outcome=="Lung")%>%
  ggplot()+
  geom_point(aes(Date,ir_m, colour= "Observed"))+
  geom_line(aes(Date,ir_m,colour= "Observed"))+
  
  geom_point(aes(Date,ir_pred,colour= "Expected"))+
  geom_line(aes(Date,ir_pred,colour= "Expected"))+
  geom_ribbon(aes(ymin = lwr_pred,ymax = upr_pred, x=Date),  fill = "blue", alpha = 0.1)+
  scale_color_manual(name= "", values=c(Observed="red", Expected="blue"))+
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month", limits= c(as.Date("2020-03-01"),as.Date("2021-11-01")),expand=c(0.005,0.005))+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

overall_prediction_Lung <- overall_prediction_Lung + 
  theme(axis.text.x = element_text(angle=90), 
        axis.title.y = element_text(size = 14),    
        plot.margin=grid::unit(c(1,1,0,1), "cm"),
        legend.text = element_text(size = 14))+
 # geom_vline(xintercept=as.numeric(as.Date(c("2020-03-01"))),linetype=2, color="black")+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("")#+
#ggtitle("Incidence rates for Lung cancer before and after COVID-19 lockdown")

# Prostate
overall_prediction_Prostate <- prediction_overall %>% filter(outcome=="Prostate")%>%
  ggplot()+
  geom_point(aes(Date,ir_m, colour= "Observed"))+
  geom_line(aes(Date,ir_m,colour= "Observed"))+
  
  geom_point(aes(Date,ir_pred,colour= "Expected"))+
  geom_line(aes(Date,ir_pred,colour= "Expected"))+
  geom_ribbon(aes(ymin = lwr_pred,ymax = upr_pred, x=Date),  fill = "blue", alpha = 0.1)+
  scale_color_manual(name= "", values=c(Observed="red", Expected="blue"))+
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month", limits= c(as.Date("2020-03-01"),as.Date("2021-11-01")),expand=c(0.005,0.005))+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

overall_prediction_Prostate <- overall_prediction_Prostate + 
  theme(axis.text.x = element_text(angle=90), 
        axis.title.y = element_text(size = 14),    
        plot.margin=grid::unit(c(1,1,0,1), "cm"),
        legend.text = element_text(size = 14))+
 # geom_vline(xintercept=as.numeric(as.Date(c("2020-03-01"))),linetype=2, color="black")+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("")#+
#ggtitle("Incidence rates for prostate cancer before and after COVID-19 lockdown")

figure_prediction_overall_march2020<-ggarrange(overall_prediction_Breast, overall_prediction_Colorectal, overall_prediction_Lung, overall_prediction_Prostate, 
                                     align="hv", ncol=2, nrow=2,
                                     labels = c("A) Breast Cancer", "B) Colorectal Cancer", "C) Lung Cancer", "D) Prostate Cancer"),font.label = list(size = 12),
                                     hjust = c(-0.25,-0.25),
                                     common.legend=TRUE, legend="right" )



# Save

ggsave(here("4_Results", db.name, "Plots", "Figure_1_prediction_overall_march2020.tiff"), figure_prediction_overall_march2020, dpi=600, scale = 1.25,  width = 16, height = 10)
ggsave(here("4_Results", db.name, "Plots", "Figure_1_prediction_overall_march2020.jpg"), figure_prediction_overall_march2020, dpi=600, scale = 1.25,  width = 16, height = 10)

rm(figure_prediction_overall, figure_age_gender,figure_ses, prediction_age.gender,predicion_overall, predicition_ses, 
   end_mod, j, outcomes_to_fit)
