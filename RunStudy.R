# output files ----
if (!file.exists(output.folder1)){
  dir.create(output.folder1, recursive = TRUE)}

if (!file.exists(output.folder2)){
  dir.create(output.folder2, recursive = TRUE)}

if (!file.exists(output.folder3)){
  dir.create(output.folder3, recursive = TRUE)}

if (!file.exists(output.folder4)){
  dir.create(output.folder4, recursive = TRUE)}


# table names----
exposure_table_name_1 <- paste0(outcome_table_stem,"_denominator") # this is the data for the cancer outcomes after lockdown
outcome_table_name_1 <- paste0(outcome_table_stem,"_cancers") # this is the data for the cancer outcomes before lockdown

start<-Sys.time()

# start log ----
log_file <- paste0(output.folder, "/log.txt")
logger <- create.logger()
logfile(logger) <- log_file
level(logger) <- "INFO"

# instantiate study cohorts ----
info(logger, 'INSTANTIATING STUDY COHORTS')
source(here("1_InstantiateCohorts","InstantiateStudyCohorts.R"))
info(logger, 'GOT STUDY COHORTS')


# Run data preparation ----
info(logger, 'RUNNING DATA PREPARATION')
source(here("2_DataPrep","DataPrep.R"))
info(logger, 'DATA PREPARATION RAN')

# Run validation ----
info(logger, 'RUNNING VALIDATION BASED ON 2017-2019 DATA')
source(here("3_Analysis","Validation.R"))
info(logger, 'VALIDATION BASED ON 2017-2019 DATA RAN')

# Run prediction model ----
info(logger, 'RUNNING PREDICTION MODEL')
source(here("3_Analysis","PredictionModel.R"))
info(logger, 'PREDICTION MODEL RAN')

print("Done!")
print("-- If all has worked, there should now be .csv files, data objects and 
      tables in the corresponding results folders for each popualtion to share")
print("-- Thank you for running the study!")
Sys.time()-start
readLines(log_file)