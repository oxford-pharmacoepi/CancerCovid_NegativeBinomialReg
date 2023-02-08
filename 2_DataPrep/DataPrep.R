# ============================================================================ #
#                Data preparation for negative binomial regression             #
#                           for Cancer Covid Study                             #
#                              Nicola Barclay                                  #
#                                8-02-2023                                     #
# ============================================================================ #


# Read the cancer cohort table name and the cdm databases
outcome_db        <- cdm[[outcome_table_name_1]]



# dates ----
#start date
start.date<-as.Date(dmy(paste0("01-03-","2017")))
#end date 
end.date<-as.Date(dmy(paste0("31-03-","2020")))