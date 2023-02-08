# ============================================================================ #
#                Data preparation for negative binomial regression             #
#                           for Cancer Covid Study                             #
#                              Nicola Barclay                                  #
#                                8-02-2023                                     #
# ============================================================================ #


# Read the cancer cohort table name and the cdm databases

cohortTableExposures_db   <-  cdm[[exposure_table_name_1]]
outcome_db        <- cdm[[outcome_table_name_1]]


# dates ----
#start date
start.date<-as.Date(dmy(paste0("01-01-","2017")))
#end date 
end.date<-as.Date(dmy(paste0("31-12-","2019")))



# define the population ----

Pop<-person_db %>% 
  inner_join(cohortTableExposures_db %>% 
               dplyr::select(subject_id,cohort_start_date) %>% 
               rename("person_id"="subject_id")) %>% 
  dplyr::select(person_id,gender_concept_id, 
                year_of_birth, month_of_birth, day_of_birth,
                cohort_start_date) %>% 
  left_join(observation_period_db %>% 
              dplyr::select("person_id",  "observation_period_start_date", "observation_period_end_date")) %>% 
  collect()

exclusion_table <- tibble(N_current=nrow(Pop), exclusion_reason=NA)


# add age and gender -----
Pop$age<- NA
if(sum(is.na(Pop$day_of_birth))==0 & sum(is.na(Pop$month_of_birth))==0){
  # if we have day and month 
  Pop<-Pop %>%
    mutate(age=floor(as.numeric((ymd(cohort_start_date)-
                                   ymd(paste(year_of_birth,
                                             month_of_birth,
                                             day_of_birth, sep="-"))))/365.25))
} else { 
  Pop<-Pop %>% 
    mutate(age= year(cohort_start_date)-year_of_birth)
}


# wider age groups
Pop<-Pop %>% 
  mutate(age_gr2=ifelse(age<=34,  "18-34",
                        ifelse(age>=35 & age<=64,  "35-64",    
                               ifelse(age>=65, ">=65",
                                      NA)))) %>% 
  mutate(age_gr2= factor(age_gr2, 
                         levels = c("18-34","35-64",">=65")))

# gender
# concept_id = 8507 male
# concept_id = 8532 female

Pop<-Pop %>% 
  mutate(gender= ifelse(gender_concept_id==8507, "Male",
                        ifelse(gender_concept_id==8532, "Female", NA ))) %>% 
  mutate(gender= factor(gender, 
                        levels = c("Male", "Female")))


# if missing age or gender, drop
Pop<-Pop %>% 
  filter(!is.na(age))

Pop<-Pop %>% 
  filter(!is.na(gender))


# add prior observation time -----
Pop<-Pop %>%  
  mutate(prior_obs_days=as.numeric(difftime(cohort_start_date,
                                            observation_period_start_date,
                                            units="days"))) %>% 
  mutate(prior_obs_years=prior_obs_days/365.25)
quantile(Pop$prior_obs_days)

Pop <- Pop %>%
  filter(prior_obs_years >=1)
Pop %>% group_by(gender) %>% count(age_gr2)

exclusion_table<-rbind(exclusion_table,
                       c(nrow(Pop),
                         "Year of prior history"))

# ADD SES HERE WHEN WE HAVE LINKAGE TO MULITPLE INDEX OF DEPRIVATION DATA
#
#
#


# Lost of FUP ----
Pop_lost <- Pop %>%
  filter(observation_period_end_date<as.Date("2019-12-31"))

# save General Pop----
save(Pop, file=here("DataPrep", "General.Pop.2017_19.RData"))
write.csv(exclusion_table, file=here("DataPrep", "exclusion_table_2017_19.csv"))


#Incidence-------
Pop <- Pop[,1:14]
IR.overall<-list() # list to collect monthy overall IRs
IR.age <- list()
IR.gender <-list()
IR.age_gender<-list() # list to collect monthy IRs by age_gr2 and gender
IR.ses <-list()


## UPTO HERE


# for each outcome of interest

outcome.cohorts <- cdm[[outcome_table_name_1]]   # need to see how this can refer to each cohort definition

for(j in 1:length(outcome.cohorts$id)){      # may need to change the id here to cohort_definition_id and may need to join to additional tables
  working.outcome<-outcome.cohorts$id[j]
  working.outcome.name<-outcome.cohorts$name[j]
  
  print(paste0("- Getting ", working.outcome.name,
               " (", j, " of ", length(working.outcome.name), ")"))#CANVI???
  working.Pop<-Pop 
  # Note, by definition nobody in our exposure population has a history of an outcome
  # (as defined in atlas)
  # everyone has a year of prior history
  
  # event of interest ------
  working.outcomes<-outcome_db %>%
    filter(cohort_definition_id %in% working.outcome) %>%
    dplyr::select(subject_id, cohort_start_date) %>% 
    collect()
  
  
  # first event after index date -----
  f_u.outcome<-working.outcomes %>%  
    inner_join(working.Pop %>% 
                 dplyr::select(person_id,cohort_start_date) %>% 
                 rename("subject_id"="person_id") %>% 
                 rename("Pop_cohort_start_date"="cohort_start_date"))  %>% 
    filter(cohort_start_date> Pop_cohort_start_date) %>% 
    filter(cohort_start_date<= end.date)
  
  f_u.outcome<-f_u.outcome %>% 
    group_by(subject_id) %>%
    arrange(cohort_start_date) %>% 
    mutate(seq=1:length(subject_id)) %>% 
    filter(seq==1) %>% 
    dplyr::select(subject_id,cohort_start_date)  %>% 
    rename("f_u.outcome_date"="cohort_start_date") %>% 
    mutate(f_u.outcome=1)
  working.Pop<-working.Pop %>% 
    left_join(f_u.outcome,
              by=c("person_id"="subject_id"))
  working.Pop<-working.Pop %>% 
    mutate(f_u.outcome=ifelse(is.na(f_u.outcome),0,1))
  
  
  
  # overall TAR -----
  # censor at first of outcome, end of observation period, study end
  
  # if event, date of event
  # if no event,  censor at tar.end.date or end of observation period, whichever comes first
  working.Pop<-working.Pop %>%
    mutate(f_u.outcome_date=if_else(f_u.outcome==1,
                                    f_u.outcome_date, 
                                    if_else(observation_period_end_date < end.date,
                                            observation_period_end_date, 
                                            end.date )))
  working.Pop<-working.Pop %>% 
    mutate(f_u.outcome.days=as.numeric(difftime(f_u.outcome_date,
                                                cohort_start_date, 
                                                units="days")))
  working.Pop<-working.Pop %>% 
    mutate(f_u.outcome.months=floor(as.numeric(difftime(f_u.outcome_date,
                                                        cohort_start_date,
                                                        units="days"))/7))
  
  
  # IRs  ------
  
  # for loop to go month by month
  # get number of events
  # days contributed in that month
  
  # number of months in the study
  n.months<-lubridate::interval(ymd(start.date),ymd(end.date)) %/% months(1)
  
  for(i in 1:(n.months+1)){ 
    print(paste0("Getting month: ", i, " of ", n.months))
    
    working.month.start<-start.date+months(i-1) # date, first day of the month
    working.month.end<-start.date+months(i)-days(1) # date, last day of the month
    working.month.days<-as.numeric(difftime(start.date+months(i),working.month.start,
                                            units="days")) # number of days in the month
    #drop people who were censored prior to month
    # must have one day in the woking month
    working.month.pop<-working.Pop %>% 
      filter(f_u.outcome_date>=working.month.start) #canvi berta
    
    # number of days contributed in month of interest
    working.month.pop<-working.month.pop %>% 
      mutate(working.month.days=ifelse(f_u.outcome_date>=working.month.end,
                                       working.month.days , # all the days of the month
                                       as.numeric(difftime(f_u.outcome_date,working.month.start,
                                                           units="days")) # days that were contribute before censor   
      ))
    # overall
    IR.overall[[paste0(working.outcome.name,"_",i)]]<- working.month.pop %>% 
      summarise(n=length(person_id),
                days=sum(working.month.days),
                months = (days/30.44),
                years=(days/365.25),
                events= sum(f_u.outcome_date >= working.month.start &
                              f_u.outcome_date < working.month.end &
                              f_u.outcome==1))%>% 
      mutate(ir_m=(events/months)*100000) %>% 
      mutate(month=month(working.month.start)) %>% 
      mutate(year=year(working.month.start)) %>% 
      mutate(months.since.start=i) %>% 
      mutate(strata="overall") %>% 
      mutate(outcome=working.outcome.name) %>%
      mutate(covid = case_when(months.since.start <= 24 ~ "Pre-COVID",
                               (months.since.start >= 25)&(months.since.start <= 28)~ "Lockdown",
                               (months.since.start >= 29)&(months.since.start <= 31)~ "Post-lockdown1",
                               (months.since.start >= 32)&(months.since.start <= 34)~ "Post-lockdown2",
                               months.since.start >= 35  ~ "Post-lockdown3"))
    
    # age
    IR.age[[paste0(working.outcome.name,"_",i)]] <- working.month.pop %>% 
      group_by(age_gr2) %>% 
      summarise(n=length(person_id),
                days=sum(working.month.days),
                months = (days/30.44),
                years=(days/365.25),
                events= sum(f_u.outcome_date >= working.month.start &
                              f_u.outcome_date < working.month.end &
                              f_u.outcome==1))%>% 
      mutate(ir_m=(events/months)*100000) %>% 
      mutate(month=month(working.month.start)) %>% 
      mutate(year=year(working.month.start)) %>% 
      mutate(months.since.start=i) %>% 
      mutate(strata="age_gr2") %>% 
      mutate(outcome=working.outcome.name)%>%
      mutate(covid = case_when(months.since.start <= 24 ~ "Pre-COVID",
                               (months.since.start >= 25)&(months.since.start <= 28)~ "Lockdown",
                               (months.since.start >= 29)&(months.since.start <= 31)~ "Post-lockdown1",
                               (months.since.start >= 32)&(months.since.start <= 34)~ "Post-lockdown2",
                               months.since.start >= 35  ~ "Post-lockdown3"))
    
    # gender
    IR.gender[[paste0(working.outcome.name,"_",i)]] <- working.month.pop %>% 
      group_by(gender) %>% 
      summarise(n=length(person_id),
                days=sum(working.month.days),
                months = (days/30.44),
                years=(days/365.25),
                events= sum(f_u.outcome_date >= working.month.start &
                              f_u.outcome_date < working.month.end &
                              f_u.outcome==1))%>% 
      mutate(ir_m=(events/months)*100000) %>% 
      mutate(month=month(working.month.start)) %>% 
      mutate(year=year(working.month.start)) %>% 
      mutate(months.since.start=i) %>% 
      mutate(strata="gender") %>% 
      mutate(outcome=working.outcome.name)%>%
      mutate(covid = case_when(months.since.start <= 24 ~ "Pre-COVID",
                               (months.since.start >= 25)&(months.since.start <= 28)~ "Lockdown",
                               (months.since.start >= 29)&(months.since.start <= 31)~ "Post-lockdown1",
                               (months.since.start >= 32)&(months.since.start <= 34)~ "Post-lockdown2",
                               months.since.start >= 35  ~ "Post-lockdown3"))
    
    
    # age_gender
    IR.age_gender[[paste0(working.outcome.name,"_",i)]] <- working.month.pop %>% 
      group_by(age_gr2, gender) %>% 
      summarise(n=length(person_id),
                days=sum(working.month.days),
                months = (days/30.44),
                years=(days/365.25),
                events= sum(f_u.outcome_date >= working.month.start &
                              f_u.outcome_date < working.month.end &
                              f_u.outcome==1))%>% 
      mutate(ir_m=(events/months)*100000) %>% 
      mutate(month=month(working.month.start)) %>% 
      mutate(year=year(working.month.start)) %>% 
      mutate(months.since.start=i) %>% 
      mutate(strata="age_gr2, gender") %>% 
      mutate(outcome=working.outcome.name)%>%
      mutate(covid = case_when(months.since.start <= 24 ~ "Pre-COVID",
                               (months.since.start >= 25)&(months.since.start <= 28)~ "Lockdown",
                               (months.since.start >= 29)&(months.since.start <= 31)~ "Post-lockdown1",
                               (months.since.start >= 32)&(months.since.start <= 34)~ "Post-lockdown2",
                               months.since.start >= 35  ~ "Post-lockdown3"))
    
    
    # ses
    IR.ses[[paste0(working.outcome.name,"_",i)]] <- working.month.pop %>% 
      group_by(medea) %>% 
      summarise(n=length(person_id),
                days=sum(working.month.days),
                months = (days/30.44),
                years=(days/365.25),
                events= sum(f_u.outcome_date >= working.month.start &
                              f_u.outcome_date < working.month.end &
                              f_u.outcome==1))%>% 
      mutate(ir_m=(events/months)*100000) %>% 
      mutate(month=month(working.month.start)) %>% 
      mutate(year=year(working.month.start)) %>% 
      mutate(months.since.start=i) %>% 
      mutate(strata="medea") %>% 
      mutate(outcome=working.outcome.name)%>%
      mutate(covid = case_when(months.since.start <= 24 ~ "Pre-COVID",
                               (months.since.start >= 25)&(months.since.start <= 28)~ "Lockdown",
                               (months.since.start >= 29)&(months.since.start <= 31)~ "Post-lockdown1",
                               (months.since.start >= 32)&(months.since.start <= 34)~ "Post-lockdown2",
                               months.since.start >= 35  ~ "Post-lockdown3"))
    
    
  }
}



IR.overall<-bind_rows(IR.overall)
IR.age <- bind_rows(IR.age)
IR.gender <-bind_rows(IR.gender)
IR.age_gender<-bind_rows(IR.age_gender)
IR.ses <- bind_rows(IR.ses)

IR.overall%>% 
  ggplot()+
  facet_grid(outcome~ year, scales="free")+
  geom_point(aes(month,ir_m))+
  geom_line(aes(month,ir_m))+
  scale_y_continuous(name="Incidence rate per 100,000 person-years")+
  scale_x_continuous(breaks=seq(1,12,2))+
  theme_bw()


# save ----
save(IR.overall, file =here("DataPrep/March2017_March2020", "IR.overall.rev.RData"))
save(IR.age, file=here("DataPrep/March2017_March2020", "IR.age.rev.RData"))
save(IR.gender, file=here("DataPrep/March2017_March2020", "IR.gender.rev.RData"))
save(IR.age_gender, file =here("DataPrep/March2017_March2020", "IR.age_gender.rev.RData"))
save(IR.ses, file =here("DataPrep/March2017_March2020", "IR.ses.rev.RData"))








