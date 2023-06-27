# output files ----
if (!file.exists(output.folder1)){
  dir.create(output.folder1, recursive = TRUE)}

if (!file.exists(output.folder2)){
  dir.create(output.folder2, recursive = TRUE)}

if (!file.exists(output.folder3)){
  dir.create(output.folder3, recursive = TRUE)}

if (!file.exists(output.folder4)){
  dir.create(output.folder4, recursive = TRUE)}


start<-Sys.time()

# start log ----
log_file <- paste0(output.folder, "/log.txt")
logger <- create.logger()
logfile(logger) <- log_file
level(logger) <- "INFO"

# Run data preparation ----
info(logger, 'RUNNING DATA PREPARATION')
source(here("1_DataPrep","DataPrep_Jan2017_Mar2020.R"))
source(here("1_DataPrep","DataPrep_Mar2018_Jan2022.R"))
info(logger, 'DATA PREPARATION RAN')

# Run validation ----
info(logger, 'RUNNING VALIDATION BASED ON 2017-2019 DATA')
source(here("2_Analysis","Validation-2017_2020.R"))
info(logger, 'VALIDATION BASED ON 2017-2019 DATA RAN')

# Run prediction model ----
info(logger, 'RUNNING PREDICTION MODEL')
source(here("2_Analysis","PredictionModel_2018_2022.R"))
source(here("2_Analysis","PredictionModel_2018_2022_MALES.R"))
source(here("2_Analysis","PredictionModel_2018_2022_FEMALES.R"))
info(logger, 'PREDICTION MODEL RAN')

# Run extrapolated counts ----
info(logger, 'RUNNING EXTRAPOLATION COUNTS')
source(here("2_Analysis","ExtrapolateCounts.R"))
info(logger, 'EXTRAPOLATION COUNTS RAN')

# Run incidence rate ratios of observed data for cancers ----
info(logger, 'RUNNING IRR TABLES AND FIGURES OF CANCERS')
source(here("3_DataSummary","Cancer_IRR_loop_overall.R"))
source(here("3_DataSummary","Cancer_IRR_loop_age_sex_final.R"))
info(logger, 'IRR TABLES AND FIGURES OF CANCERS RAN')

# Run incidence rate ratios of observed data for screening tests ----
info(logger, 'RUNNING IRR TABLES AND FIGURES OF SCREENING TESTS')
source(here("3_DataSummary","ScreeningTestTables_IR_IRR_data_prep.R"))
source(here("3_DataSummary","ScreeningTestTables_IRR_manual.R"))
info(logger, 'IRR TABLES AND FIGURES OF SCREENING TESTS RAN')

print("Done!")
print("-- If all has worked, there should now be .csv files, data objects and 
      tables in the corresponding results folders for each population to share")
print("-- Thank you for running the study!")
Sys.time()-start
readLines(log_file)