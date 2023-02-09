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


###Validation:
##Fit to 01/03/2017- 01/03/2019
##Forecast 01/03/2019 - 01/03/2020

load(here("DataPrep/March2017_March2020/IR.overall.rev.RData"))
load(here("DataPrep/March2017_March2020/IR.age_gender.rev.RData"))
load(here("DataPrep/March2017_March2020/IR.ses.rev.RData"))


IR.overall <-IR.overall %>% mutate(Month1 =paste(1,month, year, sep ="-")) %>% filter(outcome =="AnxietyDisorders"| outcome=="MajorDepressiveDisorder")
Sys.setlocale("LC_TIME", "English")
IR.overall$Date <- NA
IR.overall$Date <- dmy(IR.overall$Month1)
IR.overall$Month1 <- NULL

IR.age_gender <-IR.age_gender %>% mutate(Month1 =paste(1,month, year, sep ="-")) %>% filter(outcome =="AnxietyDisorders"| outcome=="MajorDepressiveDisorder")
Sys.setlocale("LC_TIME", "English")
IR.age_gender$Date <- NA
IR.age_gender$Month1 <- NULL


IR.ses <-IR.ses %>% mutate(Month1 =paste(1,month, year, sep ="-")) %>% filter(outcome =="AnxietyDisorders"| outcome=="MajorDepressiveDisorder")
Sys.setlocale("LC_TIME", "English")
IR.ses$Date <- NA
IR.ses$Date <- dmy(IR.ses$Month1)
IR.ses$Month1 <- NULL

outcomes_to_fit<- IR.overall %>% dplyr::select("outcome")%>% distinct()%>%pull()

###### Validation Overall: 

models.overall_validation <- list()
models.overall_validation.fit <- list()
models.overall_pred <- list()

end_mod <- 24 #month.since.start= Feb 2019
end_pred <- 36 #month.since.start= Feb 2020
  
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
#save(val_overall, file=here("WorkingData", "Validation_model.RData"))


rm(IR.overall, working.nb, models.overall_pred, models.overall_validation, models.overall_validation.fit)

###### Validation by age-gender:
age_to_fit <- IR.age_gender %>%  ungroup() %>%dplyr::select("age_gr2")%>% distinct()%>%pull()
gender_to_fit <-  IR.age_gender %>% ungroup() %>%dplyr::select("gender")%>% distinct()%>%pull()
models.age_gender_pred <- list()
models.age_gender <- list()
end_mod <- 24 #month.since.start= Feb 2019
end_pred <- 36 #month.since.start= Feb 2020

for(j in 1:length(outcomes_to_fit)){
  for(i in 1:length(age_to_fit)){
    for(y in 1:length(gender_to_fit)){
      working.nb <- glm.nb(events ~ as.factor(month)+months.since.start,data=IR.age_gender%>% filter(months.since.start<= end_mod) %>% filter(gender==gender_to_fit[y]) %>% 
                         filter(age_gr2==age_to_fit[i])%>% filter(outcome==outcomes_to_fit[j]))
      models.age_gender[[paste0("m.",age_to_fit[i],gender_to_fit[y],".nb")]]  <- working.nb
      pred <-predict(working.nb, newdata=IR.age_gender%>% filter(months.since.start<= end_pred) %>% filter(gender==gender_to_fit[y]) %>% filter(age_gr2==age_to_fit[i])%>%
                   filter(outcome==outcomes_to_fit[j]),type="response", se.fit = TRUE, interval= "prediction", level=0.95)
  
      models.age_gender_pred[[paste0("m.",age_to_fit[i],gender_to_fit[y],outcomes_to_fit[j], ".nb")]] <- cbind(IR.age_gender %>% filter(months.since.start<= end_pred) %>% filter(outcome==outcomes_to_fit[j])%>% filter(gender==gender_to_fit[y]) %>% 
                                                                                                             filter(age_gr2==age_to_fit[i]), data.frame(est=as.character(pred$fit), model="nb")) %>%  
      add_pi(working.nb, names = c("lwr", "upr"), alpha = 0.05) %>%
      mutate(ir_pred=pred*100000/months, lwr_pred=lwr*100000/months, upr_pred=upr*100000/months) %>%
      mutate(red = (100*(events-pred)/pred))
    }
  }
}


val_age_gender <- bind_rows(models.age_gender_pred) %>% mutate(ir_pred =pred*100000/months, lwr_pred=lwr*100000/months, upr_pred=upr*100000/months) %>% 
  mutate_if(is.numeric, ~round(., 1)) %>% dplyr::select(-c(model, est))
#save(val_age_gender, file=here("WorkingData", "Validation_age_gender.RData"))

rm(IR.age_gender, models.age_gender,models.age_gender_pred, pred,working.nb, age_to_fit, gender_to_fit )

###### Validation by SES:
ses_to_fit <- IR.ses %>%  ungroup() %>%dplyr::select("medea")%>% distinct()%>%pull()
models.ses_pred <- list()
models.ses <- list()
end_mod <- 24 #month.since.start= Feb 2019
end_pred <- 36 #month.since.start= Feb 2020

for(j in 1:length(outcomes_to_fit)){
  for(i in 1:(length(ses_to_fit)-1)){
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
#save(val_ses, file=here("WorkingData", "Validation_ses.RData"))

rm(IR.ses, models.ses,models.ses_pred, pred,working.nb, ses_to_fit,
   end_mod,end_pred, i,j,outcomes_to_fit,y)


###Plots:

#Overall:

overall_AD <- val_overall %>% filter(outcome=="AnxietyDisorders")%>%
  ggplot()+
  geom_point(aes(Date,ir_m, colour= "Observed"))+
  geom_line(aes(Date,ir_m,colour= "Observed"))+
  
  geom_point(aes(Date,ir_pred,colour= "Expected"))+
  geom_line(aes(Date,ir_pred,colour= "Expected"))+
  geom_ribbon(aes(ymin = lwr_pred,ymax = upr_pred, x=Date),  fill = "blue", alpha = 0.1)+
  scale_color_manual(name= "", values=c(Observed="red", Expected="blue"))+
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month", limits= c(as.Date("2017-03-01"),as.Date("2020-02-01")),expand=c(0.005,0.005))+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())


overall_AD <- overall_AD + 
  theme(#axis.text.x = element_blank(),
    #axis.ticks.x = element_blank(),
    # axis.title.x = element_blank(),
    axis.text.x = element_text(angle=90), 
    axis.title.y = element_text(size = 9),    
    plot.margin=grid::unit(c(1,1,0,1), "cm"))+
  geom_vline(xintercept=as.numeric(as.Date(c("2019-03-01"))),linetype=2, color="black")+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("")

overall_MD <- val_overall %>% filter(outcome!="AnxietyDisorders")%>%
  ggplot()+
  geom_point(aes(Date,ir_m, colour= "Observed"))+
  geom_line(aes(Date,ir_m,colour= "Observed"))+
  
  geom_point(aes(Date,ir_pred,colour= "Expected"))+
  geom_line(aes(Date,ir_pred,colour= "Expected"))+
  geom_ribbon(aes(ymin = lwr_pred,ymax = upr_pred, x=Date),  fill = "blue", alpha = 0.1)+
  scale_color_manual(name= "", values=c(Observed="red", Expected="blue"))+
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month", limits= c(as.Date("2017-03-01"),as.Date("2020-02-01")),expand=c(0.005,0.005))+
  theme_bw()+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

overall_MD <- overall_MD + 
  theme(axis.text.x = element_text(angle=90), 
        axis.title.y = element_text(size = 9),
        plot.margin=grid::unit(c(0,1,0,1), "cm")
  )+ 
  geom_vline(xintercept=as.numeric(as.Date(c("2019-03-01"))),linetype=2, color="black")+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("")

sup1 <- ggarrange(overall_AD+ rremove("xlab"), overall_MD, 
                  align="hv", ncol=1, nrow=2,
                  labels = c("A) Anxiety disorders", "B) Depressive disorders"),font.label = list(size = 12),
                  hjust = c(-0.25,-0.25),
                  common.legend=TRUE, legend="right" )

rm(overall_AD, overall_MD)

# Age and gender
val_age_gender$gender <- factor(val_age_gender$gender, levels=rev(levels(val_age_gender$gender)))
levels(val_age_gender$gender) <- c("Women", "Men")
levels(val_age_gender$age_gr2) <- c("18-34 y", "35-64 y", ">=65 y")

age_gender_AD_valplot <- val_age_gender  %>% filter(outcome=="AnxietyDisorders") %>%
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



age_gender_AD_valplot <-age_gender_AD_valplot + 
  theme(axis.text.x = element_text(angle=90),
        axis.title.y = element_text(size = 9),
        plot.margin=grid::unit(c(1,0.5,0,1), "cm") )+
  
  scale_x_date(date_labels = "%b %Y", date_breaks = "4 month")+
  geom_vline(xintercept=as.numeric(as.Date(c("2019-03-01"))),linetype=2, color="black")+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("")


age_gender_MD_valplot <- val_age_gender  %>% filter(outcome!="AnxietyDisorders") %>%
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




age_gender_MD_valplot <-age_gender_MD_valplot + 
  theme(axis.text.x = element_text(angle=90),
        axis.title.y = element_text(size = 9),
        plot.margin=grid::unit(c(1,0.5,0,1), "cm") )+
  geom_vline(xintercept=as.numeric(as.Date(c("2019-03-01"))),linetype=2, color="black")+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("")

sup2 <-ggarrange(age_gender_AD_valplot, age_gender_MD_valplot,
                 align="hv", ncol=1, nrow=2,
                 labels = c("A) Anxiety disorders", "B) Depressive disorders"),font.label = list(size = 12),
                 hjust = c(-0.25,-0.25),
                 common.legend=TRUE, legend="right" )

rm(age_gender_MD_valplot, age_gender_AD_valplot)

# SES
val_ses$medea <- factor(val_ses$medea, levels=c("U1", "U2", "U3", "U4", "U5", "R"))
plot_ses_AD <- val_ses%>%filter(outcome=="AnxietyDisorders") %>%
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
  geom_vline(xintercept=as.numeric(as.Date(c("2019-03-01"))),
             linetype=2, color="black")+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("")

plot_ses_MD <- val_ses%>%filter(outcome!="AnxietyDisorders") %>%
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
  geom_vline(xintercept=as.numeric(as.Date(c("2019-03-01"))),
             linetype=2, color="black")+
  ylab("Incidence rate per 100,000 person-months")+
  xlab("")



sup3 <-ggarrange(plot_ses_AD, plot_ses_MD,
                                  align="hv", ncol=1, nrow=2,
                           labels = c("A) Anxiety disorders", "B) Depressive disorders"),font.label = list(size = 12),
                           hjust = c(-0.25,-0.25),
                                  common.legend=TRUE, legend="right" )

rm(plot_ses_AD, plot_ses_MD)

# Save
ggsave("Figures/FigureS1.rev.tiff",sup1,dpi=300)
ggsave("Figures/FigureS1.rev_jpg.jpg",sup1,dpi=300)
ggsave("Figures/FigureS2.rev.tiff",sup2,dpi=300)
ggsave("Figures/FigureS2.rev_jpg.jpg",sup2,dpi=300)
ggsave("Figures/FigureS3.rev.tiff",sup3,dpi=300)
ggsave("Figures/FigureS3.rev_jpg.jpg",sup3,dpi=300)

