# ============================================================================ #
#          CODE TO RUN FOR CANCER/COVID NEGATIVE BINOMIAL REGRESSION           #
#                 USING DATA FROM 2017 TO PREDICT CANCER                       # 
#                     INCIDENCE RATES IN 2020 TO 2022                          #
#                                Nicola Barclay                                #
#                                 01-02-2023                                   #
#                                                                              #
#             THIS SHOULD BE THE ONLY FILE YOU NEED TO INTERACT WITH           #
#                                                                              #
# ============================================================================ #



## ----------------------------- LOAD PACKAGES ------------------------------ ##

# load r packages
library(here)
library(lubridate)
library(stringr)
library(ggplot2)
library(dbplyr)
library(dplyr)
library(tidyr)
library(tableone)
library(scales)
library(forcats)
library(epiR)
library(foreign)
library(MASS)
library(tsibble)
library(graphics)
library(feasts)
library(magrittr)
library(ciTools)
library(readr)
library(log4r)
library(ggpubr)



# database metadata and connection details -----
# The name/ acronym for the database
db.name<-"..."

# input file name exported from IncidencePrevalence package - this should be one .csv file
# and should be inserted here: "1_DataPrep", "Data")
input_data <- "... .csv"

# Set output folder locations -----
# the paths to the folders where the results from this analysis will be saved
output.folder1<-here("4_Results", db.name, "Validation")
output.folder2<-here("4_Results", db.name, "Modelling")
output.folder3<-here("4_Results", db.name, "Plots")
output.folder4<-here("3_DataSummary", db.name, "IR_IRR_DataSummaryResults")


# Run the study ------
source(here("RunStudy.R"))
# after the study is run you should have a zip folder in your output folder to share