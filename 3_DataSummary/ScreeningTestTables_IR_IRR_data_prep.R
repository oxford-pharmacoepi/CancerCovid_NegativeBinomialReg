# ============================================================================ #
#     Create tables with incidence expressed as per 100,000 person months      #
#                    for screening and diagnostic tests                        #
#                                                                              #
#                              Nicola Barclay                                  #
#                                16-05-2023                                    #
# ============================================================================ #

# Read the csv file of incidence results from the IncPrev package ----

screening_inc_data <- read_csv("1_DataPrep/Data/screening_incidence_estimates_UPDATED.csv")

# columns to remove from screening_inc_data - remove all those that do not vary
screening_inc_data <- screening_inc_data %>% dplyr::select(c(-analysis_id, -cohort_obscured, -analysis_repeated_events, - denominator_days_prior_history,
                                         -analysis_complete_database_intervals,-analysis_min_cell_count, -denominator_strata_cohort_definition_id,
                                         -denominator_strata_cohort_name, -cdm_name)) %>%
  # name outcomes
  mutate(outcome = case_when(outcome_cohort_name == "BiopsyOfBreast" ~ "Biopsy of Breast",
                             outcome_cohort_name == "BreastCancerReferrals" ~ "Breast Cancer Referrals",
                             outcome_cohort_name == "ExcisionOfBreast" ~ "Excision Of Breast",
                             outcome_cohort_name == "Mammograms" ~ "Mammograms",
                             outcome_cohort_name == "SeenBreastClinic" ~ "Seen in Breast Clinic",
                             outcome_cohort_name == "SeenBreastSurgeon" ~ "Seen Breast Surgeon",
                             outcome_cohort_name == "BowelCancerScreeningProg" ~ "Bowel Cancer Screening Prog",
                             outcome_cohort_name == "Colonoscopy" ~ "Colonoscopy",
                             outcome_cohort_name == "ColorectalCancerReferrals" ~ "Colorectal Cancer Referrals",
                             outcome_cohort_name == "QuantitativeFaecalImmunochemicalTests" ~ "Quantitative Faecal Immunochemical Tests",
                             outcome_cohort_name == "Sigmoidoscopy" ~ "Sigmoidoscopy",
                             outcome_cohort_name == "Bronchoscopy" ~ "Bronchoscopy",
                             outcome_cohort_name == "DiagnosticProceduresOfChest" ~ "Diagnostic Procedures Of Chest",
                             outcome_cohort_name == "LungCancerReferrals" ~ "Lung Cancer Referrals",
                             outcome_cohort_name == "BiopsyOfProstate" ~ "Biopsy Of Prostate",
                             outcome_cohort_name == "ProstateSpecificAntigenTest" ~ "Prostate Specific Antigen Test")) %>%
  
  # save only data for months not years
  filter(analysis_interval == "months") 
  
  
exclusion_table <- tibble(N_current=nrow(screening_inc_data), exclusion_reason=NA)

# drop data where the result is obscured
screening_inc_data <- screening_inc_data %>% filter(result_obscured == "FALSE")
exclusion_table<-rbind(exclusion_table,
                       c(nrow(screening_inc_data),
                         "Result obscured"))

# create year column
screening_inc_data <- screening_inc_data %>% mutate(year = as.Date(screening_inc_data$incidence_start_date, format="%d/%m/%Y")) %>%
  mutate(year = format(year, format = "%Y"))

# create month column
screening_inc_data <- screening_inc_data %>% mutate(month = as.Date(screening_inc_data$incidence_start_date, format="%d/%m/%Y")) %>%
  mutate(month = format(month, format = "%m"))

# create month-year column
screening_inc_data <- screening_inc_data %>% mutate(month_year = as.Date(screening_inc_data$incidence_start_date, format="%d/%m/%Y")) 

#%>%   mutate(month_year = format(month_year, format = "%m/%Y")) %>% as.POSIXlt(as.Date(screening_inc_data$month_year))


# compute person months
screening_inc_data <- screening_inc_data %>% mutate(months = screening_inc_data$person_days/30.4375) # this is the average number of days in a month


# compute incidence rate per 100,000 person months
screening_inc_data <- screening_inc_data %>% mutate(ir_m = ((screening_inc_data$n_events/months)*100000))


# dates ----
#start date
start.date<-as.Date(dmy(paste0("01-01-","2017")))
start.date.month.year<- format(as.Date(start.date), "%m-%Y")

#end date 
end.date<-as.Date(dmy(paste0("01-01-","2022")))
end.date.month.year<- format(as.Date(end.date), "%m-%Y")

# number of months in the study
n.months<-lubridate::interval(ymd(start.date),ymd(end.date)) %/% months(1)



#  To account for possible seasonality and linear trends, we will fit calendar month as a categorical 
#  variable and time as a continuous variable. The number of months since the start of the study is considered as 
#  the unit of measurement for time

# create months since start of the study for each of the estimates to use as a time variable
# function for getting number of months since start ----------------------

months.since.start.working <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"))
lt$year*12 + lt$mon }

months.since.start.function <- function(d1, d2) {months.since.start.working(d2) - months.since.start.working(d1)}


screening_inc_data <- screening_inc_data %>% mutate(months.since.start = 1+(months.since.start.function(start.date, screening_inc_data$month_year)))

# show all date variables to check correct
screening_inc_data  %>% dplyr::select(incidence_start_date, months.since.start) %>% print(n=60)


# add covid time periods for months since start
screening_inc_data <- screening_inc_data %>% mutate(covid = case_when(months.since.start <= 38 ~ "Pre-COVID", # start date is 01-2017, so 38 months is up to 1st March 2020
                                                  (months.since.start >= 39)&(months.since.start <= 42)~ "Lockdown", # March 2020 up to end of June
                                                  (months.since.start >= 43)&(months.since.start <= 46)~ "Post-first lockdown 1", # July to end of oct 2020
                                                  (months.since.start >= 47)&(months.since.start <= 48)~ "Second lockdown", # Nov - end of Dec 2020
                                                  (months.since.start >= 49)&(months.since.start <= 50)~ "Third lockdown", # Jan - end of feb 2021
                                                  (months.since.start >= 51)&(months.since.start <= 54)~ "Easing of restrictions", # March - end of june 2021
                                                  months.since.start >= 55  ~ "Legal restrictions removed")) #  july 2021 onwards

# show all date variables to check correct
screening_inc_data  %>% dplyr::select(incidence_start_date, months.since.start, covid) %>% print(n=60)

# remove variables not required for analysis
colnames(screening_inc_data)

screening_inc_data_final <- screening_inc_data %>% dplyr::select(n_persons, incidence_start_date, person_days, months, person_years, n_events, ir_m, month, year, months.since.start, outcome, covid,
                                             denominator_age_group, denominator_sex, denominator_cohort_id)


# rename columns in line with Berta's column names
screening_inc_data_final <- screening_inc_data_final %>% rename("n" = "n_persons", "days" = "person_days", "years" = "person_years", "events" = "n_events")

head(screening_inc_data_final)



# save screening tests data----
save(screening_inc_data_final, file = here("1_DataPrep", "Data", "ScreeningTests_DataPrep_updated.RData"))



# Periods-----------------
IR.overall <- screening_inc_data_final %>% mutate(Month1 =paste(1,month, year, sep ="-")) %>% filter(denominator_cohort_id ==1)

Sys.setlocale("LC_TIME", "English")
IR.overall$Date <- NA
IR.overall$Date <- dmy(IR.overall$Month1)
IR.overall$Month1 <- NULL

IR.overall$covid <- as.factor(IR.overall$covid)
IR.overall$covid <-relevel(IR.overall$covid, "Pre-COVID")


#### INCIDENCE RATES TABLES FOR PAPER --------------------------------------- ##

#This gives you all the rates calculated in each of the time periods
overall <-IR.overall%>% group_by(covid, outcome) %>% summarise( events_t = sum(events),person_months_at_risk = sum(months),)

ir <- rbind(overall)%>% arrange(covid, outcome)

ir1 <-as.matrix(ir[,3:4])
ci <- round(epi.conf(ir1, ctype = "inc.rate", method = "exact", N = 100000, design = 1, 
                     conf.level = 0.95) * 100000,1)

ir_ci <- cbind(ir, ci)
ir_ci <- ir_ci %>% 
  mutate(ir = paste0(paste(est),"(", paste(lower), " to ", paste(upper), ")"))%>%
  dplyr::select(covid, outcome, events_t, person_months_at_risk, ir)%>%
  arrange(covid, outcome)


write.csv(ir_ci, file=here("3_DataSummary", db.name, "IR_IRR_DataSummaryResults", "Summary of observed data_incidence_rates_screening_tests.csv"))
save(ir_ci, file=here("3_DataSummary", db.name, "IR_IRR_DataSummaryResults", "Summary of observed data_incidence_rates_screening_tests.RData"))

rm(ci, ir, ir_ci, ir1, overall)


# add combined periods post-lockdown - this gives you all the IR calculated anytime after lockdown.These are not averaged but caluclated
overall.post <-IR.overall%>% 
  filter(months.since.start >=43)%>%
  group_by(outcome) %>% summarise( events_t = sum(events),person_months_at_risk = sum(months),)

ir_post <- bind_rows(overall.post)%>% arrange(outcome)
ir2 <-as.matrix(ir_post[,2:3])
ci2 <- round(epi.conf(ir2, ctype = "inc.rate", method = "exact", N = 100000, design = 1, 
                     conf.level = 0.95) * 100000,1)

ir_ci2 <- cbind(ir_post, ci2)
ir.post_ci <- ir_ci2 %>% 
  mutate(ir = paste0(paste(est)," (", paste(lower), " to ", paste(upper), ")"))%>%
  mutate(covid="Post-lockdown")%>%
  dplyr::select(covid,outcome,  events_t, person_months_at_risk, ir)%>%
  arrange(covid, outcome)


write.csv(ir.post_ci, file=here("3_DataSummary", db.name, "IR_IRR_DataSummaryResults", "Summary of observed data_incidence_rates_post_lockdown_screening_tests.csv"))
save(ir.post_ci, file=here("3_DataSummary", db.name, "IR_IRR_DataSummaryResults", "Summary of observed data_incidence_rates_post_lockdown_screening_tests.RData"))


# JOIN ALL PERIODS WITH POST-COVID

ir_ci_pre_post <- rbind(ir_ci, ir.post_ci)

write.csv(ir_ci_pre_post, file=here("3_DataSummary", db.name, "IR_IRR_DataSummaryResults", "Summary of observed data_incidence_rates_pre_post_lockdown_screening_tests.csv"))
save(ir_ci_pre_post, file=here("3_DataSummary", db.name, "IR_IRR_DataSummaryResults", "Summary of observed data_incidence_rates_pre_post_lockdown_screening_tests.RData"))

# Change table structure to remove events and person months, and pivot the covid categories
ir_ci_pre_post_pivot <- ir_ci_pre_post %>% dplyr::select(c(-events_t, -person_months_at_risk)) %>% tidyr::pivot_wider(names_from = covid, values_from = ir) 


ir_ci_pre_post_pivot <- ir_ci_pre_post_pivot[c(2,4,8,10,12,13,3,6,14,5,7,9,1,11), c(1, 2, 5, 9, 6, 7, 8, 3,4)]
ir_ci_pre_post_pivot <- ir_ci_pre_post_pivot %>% rename("Pre-COVID (Jan 2017-Feb 2020)" = "Pre-COVID", 
                                                        "Lockdown (March 2020-June 2020)" = "Lockdown",
                                                        "Post-lockdown (July 2020-Dec 2021)" = "Post-lockdown", 
                                                        "Post-first lockdown 1 (July 2020-Oct 2020)" = "Post-first lockdown 1",
                                                        "Second lockdown (Nov 2020-Dec 2020)" = "Second lockdown", 
                                                        "Third lockdown (Jan 2021-Feb 2021)" = "Third lockdown",
                                                        "Easing of restrictions (March 2021-June 2021" = "Easing of restrictions", 
                                                        "Legal restrictions removed (July 2021-Dec 2021)"= "Legal restrictions removed")


Pretty_observed_IR_results_table_screening <- flextable(ir_ci_pre_post_pivot) %>% theme_vanilla() %>% 
  set_caption(caption = "Incidence rates of screening and diagnostic tests in each of the time periods") %>% 
  width(width = 1.4) 

save(ir_ci_pre_post_pivot, file=here("3_DataSummary", db.name, "IR_IRR_DataSummaryResults", "Summary of observed data_incidence_rates_ir_ci_pre_post_pivot_screening_tests.RData"))
write.csv(ir_ci_pre_post_pivot, file=here("3_DataSummary", db.name, "IR_IRR_DataSummaryResults", "Summary of observed data_incidence_rates_ir_ci_pre_post_pivot_screening_tests.csv"))

save_as_docx('Pretty_observed_IR_results_table' = Pretty_observed_IR_results_table_screening, path=here("3_DataSummary", "Summary of observed incidence rates for screening tests.docx"))

