# ============================================================================ #
#          CODE TO RUN FOR CANCER/COVID NEGATIVE BINOMIAL REGRESSION           #
#                 USING DATA FROM 2017 TO 2019 TO PREDICT CANCER               # 
#                     INCIDENCE RATES IN 2020 TO 2022                          #
#                                Nicola Barclay                                #
#                                 01-02-2023                                   #
#                                                                              #
#             THIS SHOULD BE THE ONLY FILE YOU NEED TO INTERACT WITH           #
#                                                                              #
# ============================================================================ #



## ----------------------------- LOAD PACKAGES ------------------------------ ##

# load r packages
library(DatabaseConnector)
library(here)
library(lubridate)
library(stringr)
library(ggplot2)
library(DBI)
library(dbplyr)
library(dplyr)
library(tidyr)
library(tableone)
library(scales)
library(forcats)
library(epiR)
library(RPostgreSQL)
library(foreign)
library(MASS)
library(tsibble)
library(graphics)
library(feasts)
library(magrittr)
library(ciTools)
library(readr)


# database metadata and connection details -----
# The name/ acronym for the database
db.name<-"..."

# Set output folder locations -----
# the paths to the folders where the results from this analysis will be saved
output.folder1<-here("Results", db.name, "Breast")
output.folder2<-here("Results", db.name, "Colorectal")
output.folder3<-here("Results", db.name, "Lung")
output.folder4<-here("Results", db.name, "Prostate")

# Specify databaseConnector connection details -----
# database connection details
# connect to database
user        <-  "..."
password    <-  "..."
port        <-  "..." 
host        <-  "..." 
server_dbi  <-  "..."
server      <-  "..."

# Specify cdm_reference via DBI connection details -----
# In this study we also use the DBI package to connect to the database
# set up the dbConnect details below (see https://dbi.r-dbi.org/articles/dbi for more details)
# you may need to install another package for this (although RPostgres is included with renv in case you are using postgres)
# see here for details: https://odyosg.github.io/CDMConnector/articles/DBI_connection_examples.html
db <- dbConnect("...",
                dbname = server_dbi,
                port = port,
                host = host, 
                user = user, 
                password = password)

# Set database details -----
# The name of the schema that contains the OMOP CDM with patient-level data
cdm_database_schema<-"..."

# The name of the schema that contains the vocabularies 
# (often this will be the same as cdm_database_schema)
vocabulary_database_schema<-cdm_database_schema

# The name of the schema where results tables will be created 
results_database_schema<-"..."

# Name of outcome and strata tables in the result table where the outcome and strata cohorts will be stored
# Note, if there is an existing table in your results schema with the same names
# it will be overwritten 

outcome_table_stem<-"..."


# create cdm reference ----
cdm <- CDMConnector::cdm_from_con(con = db, 
                                  cdm_schema = cdm_database_schema,
                                  write_schema = results_database_schema)


# to check whether the DBI connection is correct, 
# running the next line should give you a count of your person table
cdm$person %>% 
  tally()

# Databases that will be used       
person_db             <- cdm$person
visit_occurrence_db   <- cdm$visit_occurrence
condition_occurrence_db <- cdm$condition_occurrence
concept_db            <- cdm$concept
care_site_db          <- cdm$care_site
location_db           <- cdm$location
death_db              <- cdm$death
observation_period_db <- cdm$observation_period
procedure_occurrence_db >- cdm$procedure_occurrence

# Run the study ------
source(here("RunStudy.R"))
# after the study is run you should have a zip folder in your output folder to share