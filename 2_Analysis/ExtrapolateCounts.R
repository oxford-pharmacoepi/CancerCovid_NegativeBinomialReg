# Script for extrapolating the number of counts underdiagnosed to the general population
# CPRD GOLD covers 4.63% of the UK population
# Multiply the counts by 21.59 (100/4.63) to get the extrapolated counts

# get the csv file with the missed diagnosis and percent reduction counts from
# the predictive modelling results

missed_diagnoses <- read.csv(file=here("4_Results", db.name, "Modelling", "Table_Modelling_age_sex_Results.csv"))

# filter only rows of N underdiagnoses
missed_diagnoses <- missed_diagnoses %>% filter(missed_diagnoses$Parameter == "N underdiagnoses")

# get all column names for the df
periods <- colnames(missed_diagnoses) 
periods <- periods[-c(1:4)]

# Remove patentheses and content from the columns and add to the df
Lockdown_extrapolated <-   gsub("\\s*\\([^\\)]+\\)","",as.character(missed_diagnoses$Lockdown..March.2020.June.2020.))
Post_lockdown_extrapolated <-   gsub("\\s*\\([^\\)]+\\)","",as.character(missed_diagnoses$Post.lockdown..July.2020.Dec.2021.))
Post_lockdown1_extrapolated <-   gsub("\\s*\\([^\\)]+\\)","",as.character(missed_diagnoses$Post.lockdown.1..July.2020.Oct.2020.))
Second_lockdown_extrapolated <-   gsub("\\s*\\([^\\)]+\\)","",as.character(missed_diagnoses$Second.lockdown..Nov.2020.Dec.2020.))
Third_lockdown_extrapolated <-   gsub("\\s*\\([^\\)]+\\)","",as.character(missed_diagnoses$Third.lockdown..Jan.2021.Feb.2021.))
Easing_restrictions_extrapolated <-   gsub("\\s*\\([^\\)]+\\)","",as.character(missed_diagnoses$Easing.of.restrictions..March.2021.June.2021.))
Legal_restrictions_removed_extrapolated <-   gsub("\\s*\\([^\\)]+\\)","",as.character(missed_diagnoses$Legal.restrictions.removed..July.2021.Dec.2021.))
Total_extrapolated <-   gsub("\\s*\\([^\\)]+\\)","",as.character(missed_diagnoses$Total..March.2020.Dec.2021.))

missed_diagnoses_add <- cbind(missed_diagnoses, Lockdown_extrapolated, Post_lockdown_extrapolated, Post_lockdown1_extrapolated, Second_lockdown_extrapolated,
                              Third_lockdown_extrapolated, Easing_restrictions_extrapolated, Legal_restrictions_removed_extrapolated, Total_extrapolated)

# Remove superfluous columns
missed_diagnoses_extrapolated <- missed_diagnoses_add[-c(5:12)]

# multiply columns by 21.59 to extrapolate counts to total population of UK
missed_diagnoses_extrapolated <-
