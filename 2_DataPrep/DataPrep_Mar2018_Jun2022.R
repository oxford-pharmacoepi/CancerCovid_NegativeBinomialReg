# ============================================================================ #
#                Data preparation for negative binomial regression             #
#                           for Cancer Covid Study                             #
#                              Nicola Barclay                                  #
#                                8-02-2023                                     #
# THIS IMPORTS CSV FILE FROM INCPREV PACKAGE AND PREPARES IT FOR ANALYSIS      #
# ============================================================================ #


# Read the csv file of incidence results from the IncPrev package ----
# This creates the data for use in modelling using data from Jan 2018 and extrapolating
# Jan 2019 to end of data availability at June 2022

# inc_data <- read.csv(here(file = "2_DataPrep", "Data", "incidence_estimates_cancers.csv"))
inc_data <- read_csv("2_DataPrep/Data/incidence_estimates_cancers.csv")


# columns to remove from inc_data
inc_data <- inc_data %>% dplyr::select(c(-analysis_id, -cohort_obscured, -analysis_outcome_washout, - denominator_days_prior_history,
                                         -analysis_complete_database_intervals,-analysis_min_cell_count, -denominator_strata_cohort_definition_id,
                                       -denominator_strata_cohort_name, -database_name)) %>%
                                       # name outcomes
                                         mutate(outcome = case_when(outcome_cohort_id == 1 ~ "Breast",
                                                                               outcome_cohort_id == 2 ~ "Colorectal",
                                                                               outcome_cohort_id == 3 ~ "Lung",
                                                                               outcome_cohort_id == 4 ~ "Prostate")) %>%

                                       # save only data for months not years
                                         filter(analysis_interval == "months")

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
start.date<-as.Date(dmy(paste0("01-01-","2018")))
start.date.month.year<- format(as.Date(start.date), "%m-%Y")

#end date 
end.date<-as.Date(dmy(paste0("01-06-","2022")))
end.date.month.year<- format(as.Date(end.date), "%m-%Y")

# number of months in the study
n.months<-lubridate::interval(ymd(start.date),ymd(end.date)) %/% months(1)


# create months since start of the study for each of the estimates

inc_data <- inc_data %>% mutate(date_formatted = as.Date(dmy(inc_data$incidence_start_date)))
class(inc_data$date_formatted)



# test function for getting number of months since start ----------------------

months.since.start.working <- function(d) { lt <- as.POSIXlt(as.Date(d, origin = "1900-01-01"))
                        lt$year*12 + lt$mon}

months.since.start.function <- function(d1, d2) {months.since.start.working(d2) - months.since.start.working(d1)}


inc_data <- inc_data %>% mutate(months.since.start = 1+(months.since.start.function(start.date, inc_data$date_formatted)))

# show all date variables to check correct
inc_data  %>% dplyr::select(incidence_start_date, date_formatted, months.since.start) %>% print(n=40)


# add covid time periods for months since start
inc_data <- inc_data %>% mutate(covid = case_when(months.since.start <= 26 ~ "Pre-COVID", # start date is 01-2017, so 38 months is up to 1st March 2020
                                                  (months.since.start >= 27)&(months.since.start <= 30)~ "Lockdown", # March 2020 up to end of June
                                                  (months.since.start >= 31)&(months.since.start <= 34)~ "Post-lockdown1", # July to end of oct 2020
                                                  (months.since.start >= 35)&(months.since.start <= 36)~ "Second lockdown", # Nov - end of Dec 2020
                                                  (months.since.start >= 37)&(months.since.start <= 38)~ "Third lockdown", # Jan - end of feb 2021
                                                  (months.since.start >= 39)&(months.since.start <= 42)~ "Easing of restrictions", # March - end of june 2021
                                                  months.since.start >= 43  ~ "Legal restrictions removed")) #  july 2021 onwards

# show all date variables to check correct
inc_data  %>% dplyr::select(incidence_start_date, date_formatted, months.since.start, covid) %>% print(n=40)

# remove variables not required for analysis
colnames(inc_data)

inc_data_final <- inc_data %>% dplyr::select(n_persons, person_days, months, person_years, n_events, ir_m, month, year, months.since.start, outcome, covid,
                                             month_year, date_formatted, denominator_age_group, denominator_sex, denominator_cohort_id)


# rename columns in line with Berta's column names
inc_data_final <- inc_data_final %>% rename("n" = "n_persons", "days" = "person_days", "years" = "person_years", "events" = "n_events")

head(inc_data_final)



# ADD SES HERE WHEN WE HAVE LINKAGE TO MULITPLE INDEX OF DEPRIVATION DATA
#
#
#



# save General Pop----
save(inc_data_final, file = here("2_DataPrep", "Data", "GeneralPop2018_22.RData"))

write.csv(exclusion_table, file=here("2_DataPrep", "exclusion_table_2018_22.csv"))



# example plot  
inc_yrs_plot <- inc_data_final %>%
  filter(denominator_cohort_id == 18) %>%
  ggplot(aes(x = date_formatted, y=ir_m,
                            color=outcome, group=outcome)) +
  geom_point() + geom_line() +
  scale_y_continuous(limits = c(0, 100)) +
  ggtitle("Incidence Rates of Cancer in Years Before and After COVID-19 Lockdown") +
  labs(colour = "Cancer", x="Time" , y="Incidence per 100000 person-months") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_vline(xintercept=as.numeric(as.Date(c("2020-03-23"))),linetype=2, color="red")

inc_yrs_plot
