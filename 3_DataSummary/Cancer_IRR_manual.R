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
                                                  (months.since.start >= 43)&(months.since.start <= 46)~ "Post-lockdown1", # July to end of oct 2020
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

save(inc_data_final, file=here("3_DataSummary", "inc_data_final.RData"))


# ============ CALCULATE IRR FOR EACH CANCER OVER PERIODS ==================== #

# This code calculates the IRR for each of the cancers
# separately, but loops over each period of interest

IR.overall <- inc_data_final %>% filter(denominator_cohort_id ==1)

IR <- IR.overall

periods<- IR%>% dplyr::select("covid")%>%distinct()%>%pull()
outcome <-IR%>% dplyr::select("outcome")%>% distinct()%>%pull()





# 1. working loop for one outcome - breast cancer
IR_breast_cancer <- IR %>% filter(IR$outcome =="Breast")
vector <- data.frame(a=c(),b=c())

for (z in 1:length(periods)){ 
  working.period <- periods[z]
  working.data <- IR_breast_cancer %>% 
    filter(covid==working.period) %>%
    mutate(ref=if_else(months.since.start < 39,0,1))%>% # 38 indicates reference OF PRE-COVID
    group_by(ref)%>% #no function for final time period
    summarise( events_t = sum(events),person_months_at_risk = sum(months))%>%
    mutate(periods = paste(working.period))
  
  events <- c(working.data%>%dplyr::select(events_t)%>%pull())
  pt <- c(working.data%>%dplyr::select(person_months_at_risk)%>%pull())
  
  vector <- rbind(vector,c(events, pt))
}
rateratios_breast <-rateratio(as.matrix(vector, y=NULL))



# 2. colorectal cancer
IR_colorectal_cancer <- IR %>% filter(IR$outcome =="Colorectal")
vector <- data.frame(a=c(),b=c())

for (z in 1:length(periods)){ 
  working.period <- periods[z]
  working.data <- IR_colorectal_cancer %>% 
    filter(covid==working.period) %>%
    mutate(ref=if_else(months.since.start < 39,0,1))%>% # 38 indicates reference OF PRE-COVID
    group_by(ref)%>% #no function for final time period
    summarise( events_t = sum(events),person_months_at_risk = sum(months))%>%
    mutate(periods = paste(working.period))
  
  events <- c(working.data%>%dplyr::select(events_t)%>%pull())
  pt <- c(working.data%>%dplyr::select(person_months_at_risk)%>%pull())
  
  vector <- rbind(vector,c(events, pt))
}
rateratios_colorectal <-rateratio(as.matrix(vector, y=NULL))




# 3. lung cancer
IR_lung_cancer <- IR %>% filter(IR$outcome =="Lung")
vector <- data.frame(a=c(),b=c())

for (z in 1:length(periods)){ 
  working.period <- periods[z]
  working.data <- IR_lung_cancer %>% 
    filter(covid==working.period) %>%
    mutate(ref=if_else(months.since.start < 39,0,1))%>% # 38 indicates reference OF PRE-COVID
    group_by(ref)%>% #no function for final time period
    summarise( events_t = sum(events),person_months_at_risk = sum(months))%>%
    mutate(periods = paste(working.period))
  
  events <- c(working.data%>%dplyr::select(events_t)%>%pull())
  pt <- c(working.data%>%dplyr::select(person_months_at_risk)%>%pull())
  
  vector <- rbind(vector,c(events, pt))
}
rateratios_lung <-rateratio(as.matrix(vector, y=NULL))



# 4. prostate cancer
IR_prostate_cancer <- IR %>% filter(IR$outcome =="Prostate")
vector <- data.frame(a=c(),b=c())

for (z in 1:length(periods)){ 
  working.period <- periods[z]
  working.data <- IR_prostate_cancer %>% 
    filter(covid==working.period) %>%
    mutate(ref=if_else(months.since.start < 39,0,1))%>% # 38 indicates reference OF PRE-COVID
    group_by(ref)%>% #no function for final time period
    summarise( events_t = sum(events),person_months_at_risk = sum(months))%>%
    mutate(periods = paste(working.period))
  
  events <- c(working.data%>%dplyr::select(events_t)%>%pull())
  pt <- c(working.data%>%dplyr::select(person_months_at_risk)%>%pull())
  
  vector <- rbind(vector,c(events, pt))
}
rateratios_prostate <-rateratio(as.matrix(vector, y=NULL))






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
write.csv(IRR_table_cancer, file=here("3_DataSummary", "IRR_table_cancer.csv"))
save(IRR_table_cancer, file=here("3_DataSummary", "IRR_table_cancer.RData"))

#### Make pretty table
Pretty_IRR_table_cancer <- flextable(IRR_table_cancer) %>% theme_vanilla() %>% 
  set_caption(caption = "Incidence rate ratios of cancers ober the lockdown periods compared to pre-COVID period") %>% 
  width(width = 1.4) 

save_as_docx('Pretty_IRR_table_cancer' = Pretty_IRR_table_cancer, path=here("3_DataSummary", "Pretty_IRR_table_cancer.docx"))

