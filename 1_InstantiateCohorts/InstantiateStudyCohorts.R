# ============================================================================ #
#                 INSTANTIATE COHORTS FOR CANCER/COVID STUDY                   #
#                                Nicola Barclay                                #
#                                 18-01-2023                                   #
# ============================================================================ #





# ============================================================================ #
#               1.  GENERAL POPULATION DENOMINATOR FROM 2017                   #
# ============================================================================ #


# instantiate general population exposure cohorts
info(logger, "- getting general population exposure cohorts")

exposure_cohorts_1 <- readCohortSet(here::here("1_InstantiateCohorts","ExposureCohorts"))




cdm <- generateCohortSet(cdm = cdm, 
                         cohortSet = exposure_cohorts_1,
                         cohortTableName = exposure_table_name_1,
                         overwrite = TRUE
)

cdm$exposure_table_name_1 %>% group_by(cohort_definition_id) %>% tally() %>% collect() 

info(logger, "- got general population exposure cohorts")



# ============================================================================ #
#                       2.  CANCER DIAGNOSES AS OUTCOMES                       #
# ============================================================================ #


# instantiate cancer outcome cohorts
info(logger, "- getting cancer outcomes")

outcome_cohorts_1 <- readCohortSet(here::here("1_InstantiateCohorts","OutcomeCohorts"))




cdm <- generateCohortSet(cdm = cdm, 
                         cohortSet = outcome_cohorts_1,
                         cohortTableName = outcome_table_name_1,
                         overwrite = TRUE
)

cdm$outcome_table_name_1 %>% group_by(cohort_definition_id) %>% tally() %>% collect() 

info(logger, "- got cancer outcomes")



