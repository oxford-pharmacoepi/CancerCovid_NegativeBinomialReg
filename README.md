# NegativeBinomialReg
This is exploratory code to run a negative binomial regression of incidence rates of cancer diagnoses for the Cncer Covid OPTIMA project.

The code firstly prepares your data. YOur data should be one .csv file exported from the IncidencePrevalence package containing the incidence estimates for your study, inserted into the 'Data' folder in this project, located in 1_DataPrep.

Second, the code runs a negative binomial validation modelling, using data from Jan 2017 to Feb 2019 to forecast incidence rates from March 2019 to March 2020.

Third, the code runs a prediction model using data from Jan 2018 to Feb 2020 to forecast incidence rates from March 2020 to the end of data availability.