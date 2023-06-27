# ============================================================================ #
#                Data preparation for negative binomial regression             #
#                           for Cancer Covid Study                             #
#                              Nicola Barclay                                  #
#                                8-02-2023                                     #
# THIS IMPORTS CSV FILE FROM INCPREV PACKAGE AND PREPARES IT FOR ANALYSIS      #
# ============================================================================ #


# Read the csv file of incidence results from the IncPrev package ----
# This creates the data for use in validating the model using all pre-covid data
# Jan 2017 to March 2020

#inc_data <- readr::read_csv("1_DataPrep", "Data", input_data)

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
end.date<-as.Date(dmy(paste0("01-03-","2020")))
end.date.month.year<- format(as.Date(end.date), "%m-%Y")

# number of months in the study
n.months<-lubridate::interval(ymd(start.date),ymd(end.date)) %/% months(1)



#  To account for possible seasonality and linear trends, we will fit calendar month as a categorical 
#  variable and time as a continuous variable. The number of months since the start of the study is considered as 
#  the unit of measurement for time

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


# rename columns 
inc_data_final <- inc_data_final %>% rename("n" = "n_persons", "days" = "person_days", "years" = "person_years", "events" = "n_events")

head(inc_data_final)



# save General Pop----
save(inc_data_final, file = here("1_DataPrep", "Data", "GeneralPop2017_20.RData"))

write.csv(exclusion_table, file=here("1_DataPrep", "exclusion_table_2017_20.csv"))

