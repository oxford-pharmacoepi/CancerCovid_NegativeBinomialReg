# ============================================================================ #
#     Create tables with incidence incidence rate ratios compared to the       #
#                             pre-covid reference                              #
#                            for cancer diagnoses                              #
#                                                                              #
#                              Nicola Barclay                                  #
#                                18-04-2023                                    #
# ============================================================================ #



# ===================== DATA PREP ============================================ #

# Load the scleaned screening test data object which is from the csv file of 
# incidence results from the IncPrev package ----

#inc_data <- read_csv("1_DataPrep", "Data", "incidence_estimates.csv")

inc_data <- incidence_estimates


# columns to remove from inc_data - remove all those that do not vary
inc_data <- inc_data %>% dplyr::select(c(-analysis_id, -cohort_obscured, -analysis_repeated_events, - denominator_days_prior_history,
                                         -analysis_complete_database_intervals,-analysis_min_cell_count, -denominator_strata_cohort_definition_id,
                                         -denominator_strata_cohort_name, -database_name)) %>%
  # name outcomes
  mutate(outcome = case_when(outcome_cohort_id == 1 ~ "Breast",
                             outcome_cohort_id == 2 ~ "Colorectal",
                             outcome_cohort_id == 3 ~ "Lung",
                             outcome_cohort_id == 4 ~ "Prostate")) %>%
  
  # save only data for months not years
  filter(analysis_interval == "months") %>%
  
  # save only data for with washout of 365 days
  filter(analysis_outcome_washout == "365")

exclusion_table <- tibble(N_current=nrow(inc_data), exclusion_reason=NA)

# drop data where the result is obscured
inc_data <- inc_data %>% filter(result_obscured == "FALSE")
exclusion_table<-rbind(exclusion_table,
                       c(nrow(inc_data),
                         "Result obscured"))

# create year column
inc_data <- inc_data %>% mutate(year = as.Date(inc_data$incidence_start_date, format="%d/%m/%Y")) %>%
  mutate(year = format(year, format = "%Y"))

# create month column
inc_data <- inc_data %>% mutate(month = as.Date(inc_data$incidence_start_date, format="%d/%m/%Y")) %>%
  mutate(month = format(month, format = "%m"))

# create month-year column
inc_data <- inc_data %>% mutate(month_year = as.Date(inc_data$incidence_start_date, format="%d/%m/%Y")) %>%
  mutate(month_year = format(month_year, format = "%m/%Y"))


# compute person months
inc_data <- inc_data %>% mutate(months = inc_data$person_days/30.4375) # this is the average number of days in a month

# compute incidence rate per 100,000 person months
inc_data <- inc_data %>% mutate(ir_m = ((inc_data$n_events/months)*100000))


# dates ----
#start date
start.date<-as.Date(dmy(paste0("01-01-","2017")))
start.date.month.year<- format(as.Date(start.date), "%m-%Y")

#end date 
end.date<-as.Date(dmy(paste0("01-01-","2022")))
end.date.month.year<- format(as.Date(end.date), "%m-%Y")

# number of months in the study
n.months<-lubridate::interval(ymd(start.date),ymd(end.date)) %/% months(1)



# create months since start of the study for each of the estimates to use as a time variable
# function for getting number of months since start ----------------------

months.since.start.working <- function(d) { lt <- as.POSIXlt(as.Date(d, origin = "1900-01-01"))
lt$year*12 + lt$mon}

months.since.start.function <- function(d1, d2) {months.since.start.working(d2) - months.since.start.working(d1)}


inc_data <- inc_data %>% mutate(months.since.start = 1+(months.since.start.function(start.date, inc_data$incidence_start_date)))

# show all date variables to check correct
inc_data  %>% dplyr::select(incidence_start_date, months.since.start) %>% print(n=40)


# add covid time periods for months since start
inc_data <- inc_data %>% mutate(covid = case_when(months.since.start <= 38 ~ "Pre-COVID", # start date is 01-2017, so 38 months is up to 1st March 2020
                                                  (months.since.start >= 39)&(months.since.start <= 42)~ "Lockdown", # March 2020 up to end of June
                                                  (months.since.start >= 43)&(months.since.start <= 46)~ "Post-first lockdown 1", # July to end of oct 2020
                                                  (months.since.start >= 47)&(months.since.start <= 48)~ "Second lockdown", # Nov - end of Dec 2020
                                                  (months.since.start >= 49)&(months.since.start <= 50)~ "Third lockdown", # Jan - end of feb 2021
                                                  (months.since.start >= 51)&(months.since.start <= 54)~ "Easing of restrictions", # March - end of june 2021
                                                  months.since.start >= 55  ~ "Legal restrictions removed")) #  july 2021 onwards

# show all date variables to check correct
inc_data  %>% dplyr::select(incidence_start_date, months.since.start, covid) %>% print(n=60)

# remove variables not required for analysis
colnames(inc_data)

inc_data_final <- inc_data %>% dplyr::select(n_persons, incidence_start_date, person_days, months, person_years, n_events, ir_m, month, year, months.since.start, outcome, covid,
                                             month_year, denominator_age_group, denominator_sex, denominator_cohort_id)


# rename columns in line with Berta's column names
inc_data_final <- inc_data_final %>% rename("n" = "n_persons", "days" = "person_days", "years" = "person_years", "events" = "n_events")

head(inc_data_final)

save(inc_data_final, file=here("3_DataSummary", "inc_data_final_updated.RData"))


# ============ CALCULATE IRR FOR EACH CANCER OVER PERIODS ==================== #

# ============ OVERALL (NO AGE SEX STRATIFICATION ) ========================== #


# This code calculates the IRR for each of the cancers
# separately, but loops over each period of interest

IR.overall <- inc_data_final %>% filter(denominator_cohort_id ==1)

IR <- IR.overall


periods<- IR%>% dplyr::select("covid")%>%distinct()%>%pull()
outcome <-IR%>% dplyr::select("outcome")%>% distinct()%>%pull()
#IRR <- list() # a list to store all the output later
rateratios <- vector("list",length(outcome)); names(rateratios) = outcome

for (y in 1:length(outcome)){
  working.outcome <- outcome[y]
  vector <- data.frame(a=c(),b=c()) # a vector to place the values from the loop
  for(z in 1:length(periods)){ 
    working.period <- periods[z]
    working.data <- IR %>% 
      filter(outcome==working.outcome)%>%
      filter(covid==working.period) %>%
      mutate(ref=if_else(months.since.start < 39,0,1))%>% # 38 indicates reference OF PRE-COVID
      group_by(ref)%>% #no function for final time period
      summarise( events_t = sum(events),person_months_at_risk = sum(months))%>%
      mutate(periods = paste(working.period))%>%
      mutate(outcome= paste(working.outcome))
    
    events <- c(working.data%>%dplyr::select(events_t)%>%pull())
    pt <- c(working.data%>%dplyr::select(person_months_at_risk)%>%pull())
    
    vector <- rbind(vector,c(events, pt))
    
    
  }  
  ifelse(dim(vector)[1] > 1,
         rateratios[[y]] <-rateratio(as.matrix(vector, y=NULL)), # this bit of the code says that if there is a result in vector, give us the rate ratio. If no result, give us NA
         rateratios[[y]] <- NA)
} 

# EXTRACT THE CANCER LISTS
rateratios_breast <- rateratios$Breast
rateratios_colorectal <- rateratios$Colorectal
rateratios_lung <- rateratios$Lung
rateratios_prostate <- rateratios$Prostate




################################################################################

# FUNCTION TO EXTRACT ALL THE IRR AND CIS FROM ALL OF THE LISTS 

get_IR_df_function <- function(yourrateratiosname, title){
  
  get_IR_df <- as.data.frame(yourrateratiosname[[2]])
  get_IR_df <- get_IR_df %>%  mutate_if(is.numeric, round, digits=2)
  
  # add a column to indicate the covid period
  get_IR_df <- cbind(periods, get_IR_df)
  
  # combine cis with the estimate
  get_IR_df <- get_IR_df %>% mutate(estimate = paste0(paste(estimate)," (", paste(lower), " to ", paste(upper), ")")) 
  
  # remove superfluous columns of cis
  get_IR_df <- get_IR_df[-c(3,4)]
  
  # transpose the table to have column headings as covid periods
  get_IR_df_t <- transpose(get_IR_df)
  #redefine row and column names
  colnames(get_IR_df_t) <- colnames(periods)
  names(get_IR_df_t) <- get_IR_df_t[1,]
  get_IR_df_t <- get_IR_df_t[-1,]
  rownames(get_IR_df_t) <- paste(title)
  return(get_IR_df_t)
}

# RUN THE FUNCTION FOR EACH OF THE RATERATIO LISTS



IRR_Breast <-  get_IR_df_function(rateratios_breast, "Breast")
IRR_Colorectal <-  get_IR_df_function(rateratios_colorectal, "Colorectal")
IRR_Lung <- get_IR_df_function(rateratios_lung, "Lung") 
IRR_Prostate <-  get_IR_df_function(rateratios_prostate, "Prostate")



# JOIN THE TABLES
IRR_table_cancer <- rbind(IRR_Breast, IRR_Colorectal, IRR_Lung, IRR_Prostate)
# REMOVE PRE-covid COLUMN
IRR_table_cancer <- IRR_table_cancer[-1]
# CONVERT THE ROWNAMES TO A NORMAL DATA COLUMN
IRR_table_cancer <- tibble::rownames_to_column(IRR_table_cancer, "Cancer")


#### Save IRR
write.csv(IRR_table_cancer, file=here::here("3_DataSummary", "IRR_table_cancer_looped_updated.csv"))
save(IRR_table_cancer, file=here::here("3_DataSummary", "IRR_table_cancer_looped_updated.RData"))

#### Make pretty table
Pretty_IRR_table_cancer <- flextable(IRR_table_cancer) %>% theme_vanilla() %>% 
  set_caption(caption = "Incidence rate ratios of cancers ober the lockdown periods compared to pre-COVID period") %>% 
  width(width = 1.4) 

save_as_docx('Pretty_IRR_table_cancer' = Pretty_IRR_table_cancer, path=here("3_DataSummary", "Pretty_IRR_table_cancer_updated.docx"))



# ============== CREATE FOREST PLOT OF INCIDENCE RATE RATIOS ================= #

# Format the data. First create table with the estimates and CIs in separate columns
# FUNCTION TO EXTRACT ALL THE IRR AND CIS FROM ALL OF THE LISTS 

get_IR_df_function_CIs_Sep <- function(yourrateratiosname, title){
  
  Cancer <- c(title)
  IR_CIS <- as.data.frame(yourrateratiosname[[2]])
  IR_CIS <- IR_CIS %>% mutate_if(is.numeric, round, digits=2)
  IR_CIS <-cbind(Cancer, periods, IR_CIS)

  return(IR_CIS)
}

# RUN THE FUNCTION FOR EACH OF THE RATERATIO LISTS

IRR_Breast_CIs_Sep <-  get_IR_df_function_CIs_Sep(rateratios_breast, "Breast")
IRR_Colorectal_CIs_Sep <-  get_IR_df_function_CIs_Sep(rateratios_colorectal, "Colorectal")
IRR_Lung_CIs_Sep <- get_IR_df_function_CIs_Sep(rateratios_lung, "Lung") 
IRR_Prostate_CIs_Sep <-  get_IR_df_function_CIs_Sep(rateratios_prostate, "Prostate")

# JOIN THE RATIO OUTPUTS
IRR_FOREST <- rbind(IRR_Breast_CIs_Sep, IRR_Colorectal_CIs_Sep, IRR_Lung_CIs_Sep, IRR_Prostate_CIs_Sep)

# filter out pre-covid 
IRR_FOREST <- IRR_FOREST %>% filter(periods !="Pre-COVID")

# RENAME PERIODS
IRR_FOREST <- IRR_FOREST %>% rename("Lockdown Periods" = periods) 


IRR_FOREST <- IRR_FOREST  %>%
  mutate(`Lockdown Periods` = factor(`Lockdown Periods`, levels=rev(c("Lockdown", "Post-first lockdown 1", "Second lockdown", 
                                              "Third lockdown", "Easing of restrictions", "Legal restrictions removed"))) )

# color blind palette
# The palette with grey:
#cbPalette <- c("#CC79A7", "#D55E00", "#0072B2", "#F0E442", "#009E73", "#56B4E9", "#E69F00", "#999999")

IRR_forest_cancer =
  ggplot(data=IRR_FOREST, aes(x = `Lockdown Periods`,y = estimate, ymin = lower, ymax = upper ))+
  geom_pointrange(aes(col=`Lockdown Periods`, shape=`Lockdown Periods`))+
  geom_hline(aes(fill=`Lockdown Periods`),yintercept =1, linetype=2)+
  xlab('Cancer')+ ylab("Incidence Rate Ratio (95% Confidence Interval - Pre-Pandemic as reference)")+
  geom_errorbar(aes(ymin=lower, ymax=upper,col=`Lockdown Periods`),width=0.5,cex=0.8)+ 
  facet_wrap(~Cancer,strip.position="left",nrow=4,scales = "free_y") +
  theme(plot.title=element_text(size=14,face="bold"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(size=12,face="bold"),
        axis.title=element_text(size=14,face="bold"),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        strip.text.y = element_text(hjust=0,vjust = 1,angle=180,face="bold", size=12))+
        #panel.background = element_blank(),
        #panel.grid.major = element_line(color = "grey", size = 0.2, linetype = "dashed"))+
  #guides(color=guide_legend(title="Lockdown Periods"), shape=guide_legend(title="Lockdown Periods"))+
  guides(color = guide_legend(reverse = TRUE), shape = guide_legend(reverse = TRUE))+
 # scale_fill_manual(values=cbPalette)+
  #scale_colour_manual(values=cbPalette)+
  coord_flip()


IRR_forest_cancer

# Save

ggsave(here("4_Results", db.name, "Plots", "IRR_forest_cancer_updated.tiff"), IRR_forest_cancer, dpi=600, scale = 1.3,  width = 10, height = 8)
ggsave(here("4_Results", db.name, "Plots", "IRR_forest_cancer_updated.jpg"), IRR_forest_cancer, dpi=600, scale = 1.3,  width = 10, height = 8)


# ================ STRATIFIED BY AGE AND SEX  ================================ #
# this is not complete and does not work

# This code calculates the IRR for each of the cancers stratified by age and sex
# loops over each period of interest

IR.age.sex <- inc_data_final

IR.as <- IR.age.sex

IRR.age.sex <- list()

periods<- IR%>% dplyr::select("covid")%>%distinct()%>%pull()
outcome <-IR%>% dplyr::select("outcome")%>% distinct()%>%pull()
age.strata <- IR.as %>% dplyr::select("denominator_age_group")%>% distinct()%>%pull() 
sex.strata <- IR.as %>% dplyr::select("denominator_sex")%>% distinct()%>%pull() 

strata <- c(age.strata, sex.strata)
comb <- expand.grid(age.strata, sex.strata)


rateratios.as <- vector("list",length(outcome)); names(rateratios.as) = outcome

for (y in 1:length(outcome)){
  working.outcome <- outcome[y]
  vector <- data.frame(a=c(),b=c()) # a vector to place the values from the loop
  for(z in 1:length(periods)){ 
    working.period <- periods[z]
      for(i in 1:NROW(comb)){
      
      working.strata <- paste(comb$Var1, comb$Var2)[i]
      working.age <- comb$Var1[i]
      working.gender<- comb$Var2[i]
      
    working.data <- IR %>% 
      filter(outcome==working.outcome)%>%
      filter(covid==working.period) %>%
      filter((denominator_age_group==working.age)) %>%
      filter((denominator_sex==working.gender)) %>%
      mutate(ref=if_else(months.since.start < 39,0,1))%>% # 38 indicates reference OF PRE-COVID
      group_by(ref)%>% #no function for final time period
      summarise( events_t = sum(events),person_months_at_risk = sum(months))%>%
      mutate(periods = paste(working.period))%>%
      mutate(outcome= paste(working.outcome))
    
    events <- c(working.data%>%dplyr::select(events_t)%>%pull())
    pt <- c(working.data%>%dplyr::select(person_months_at_risk)%>%pull())
    
    vector <- rbind(vector,c(events, pt))
    

  }
  ifelse(dim(vector)[1] > 1,
         rateratios.as[[y]] <-rateratio(as.matrix(vector, y=NULL)), # this bit of the code says that if there is a result in vector, give us the rate ratio. If no result, give us NA
         rateratios.as[[y]] <- NA)
  }
  
  IRR.age.sex[[paste0(working.period, working.outcome, working.strata)]]<- working.data %>%
    filter(period == working.period)%>%
    filter(outcome == working.outcome)%>%
    filter(ref==1)%>%
    mutate(IR = events_t/pmar * 100000) %>%
    mutate(IRR=round(rateratios.as$measure[2],2)) %>%
    mutate(IRR_low =round(rateratios.as$measure[2,2],2)) %>%
    mutate(IRR_upp =round(rateratios.as$measure[2,3],2))

  }

# EXTRACT THE CANCER LISTS - need to edit to add the age sex strata
rateratios_breast <- rateratios$Breast
rateratios_colorectal <- rateratios$Colorectal
rateratios_lung <- rateratios$Lung
rateratios_prostate <- rateratios$Prostate


# RUN THE FUNCTION FOR EACH OF THE RATERATIO LISTS



IRR_Breast <-  get_IR_df_function(rateratios_breast, "Breast")
IRR_Colorectal <-  get_IR_df_function(rateratios_colorectal, "Colorectal")
IRR_Lung <- get_IR_df_function(rateratios_lung, "Lung") 
IRR_Prostate <-  get_IR_df_function(rateratios_prostate, "Prostate")



# JOIN THE TABLES
IRR_table_cancer <- rbind(IRR_Breast, IRR_Colorectal, IRR_Lung, IRR_Prostate)
# REMOVE PRE-covid COLUMN
IRR_table_cancer <- IRR_table_cancer[-1]
# CONVERT THE ROWNAMES TO A NORMAL DATA COLUMN
IRR_table_cancer <- tibble::rownames_to_column(IRR_table_cancer, "Cancer")


#### Save IRR
write.csv(IRR_table_cancer, file=here::here("3_DataSummary", "IRR_table_cancer_looped.csv"))
save(IRR_table_cancer, file=here::here("3_DataSummary", "IRR_table_cancer_looped.RData"))

#### Make pretty table
Pretty_IRR_table_cancer <- flextable(IRR_table_cancer) %>% theme_vanilla() %>% 
  set_caption(caption = "Incidence rate ratios of cancers ober the lockdown periods compared to pre-COVID period") %>% 
  width(width = 1.4) 

save_as_docx('Pretty_IRR_table_cancer' = Pretty_IRR_table_cancer, path=here("3_DataSummary", "Pretty_IRR_table_cancer.docx"))



