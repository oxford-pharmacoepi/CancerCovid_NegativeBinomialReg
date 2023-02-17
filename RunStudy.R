# output files ----
if (!file.exists(output.folder1)){
  dir.create(output.folder1, recursive = TRUE)}

if (!file.exists(output.folder2)){
  dir.create(output.folder2, recursive = TRUE)}

if (!file.exists(output.folder3)){
  dir.create(output.folder3, recursive = TRUE)}


start<-Sys.time()

# start log ----
log_file <- paste0(output.folder, "/log.txt")
logger <- create.logger()
logfile(logger) <- log_file
level(logger) <- "INFO"

# Run data preparation ----
info(logger, 'RUNNING DATA PREPARATION')
source(here("1_DataPrep","DataPrep.R"))
info(logger, 'DATA PREPARATION RAN')

# Run validation ----
info(logger, 'RUNNING VALIDATION BASED ON 2017-2019 DATA')
source(here("2_Analysis","Validation.R"))
info(logger, 'VALIDATION BASED ON 2017-2019 DATA RAN')

# Run prediction model ----
info(logger, 'RUNNING PREDICTION MODEL')
source(here("2_Analysis","PredictionModel.R"))
info(logger, 'PREDICTION MODEL RAN')

print("Done!")
print("-- If all has worked, there should now be .csv files, data objects and 
      tables in the corresponding results folders for each popualtion to share")
print("-- Thank you for running the study!")
Sys.time()-start
readLines(log_file)