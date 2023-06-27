# Negative Binomial Regression Time Series Modelling: 
## Incidence of cancer screening/diagnostic tests/referrals and cancer diagnoses over the COVID lockdown 

## Introduction
This code utilises the output of the incidence rates analysis of cancer screening/diagnostic tests/referrals and cancer diagnoses over the COVID lockdown periods for the Cancer/Covid OPTIMA project. This code performs negative binomial regression modelling to model the observed vs. expected incidence rates. Here we focus on breast, colorectal, lung and prostate cancer, and associated screening/diagnostic tests/referrals in their diagnostic pathways.

The code firstly prepares your data. Your data should be one .csv file exported from the IncidencePrevalence package containing the incidence estimates for cancer diagnoses, and one for screening/diagnostic tests, inserted into the 'Data' folder in this project, located in 1_DataPrep.

Second, the code runs a negative binomial validation modelling, using data from Jan 2017 to Feb 2019 to forecast incidence rates from March 2019 to March 2020.

Third, the code runs a prediction model using data from Jan 2018 to Feb 2020 to forecast incidence rates from March 2020 to the end of data availability, and produces tables and figures of these estimates. 

Fourth, the code produces tables and figures of incidence rates and incidence rate ratios from pre-lockdown, across extended lockdown periods.

These analyses are featured in the following paper:

Barclay, N.L., Pineda Moncusi, M., Jödicke, A. M., Prieto-Alhambra, D., Raventós, B., Newby, D., Delmestri, A., Man, W-Y., Chen, X., & Català, M. (in preparation). Changes in Incidence of Breast, Colorectal, Lung and Prostate Cancer, and Screening and Diagnostic Tests, Before, During and After the UK National COVID-19 Lockdown: A Retrospective Cohort Study Using UK Primary Care Health Records

## Running the analyses
1) Download this entire repository (you can download as a zip folder using Code -> Download ZIP, or you can use GitHub Desktop). 
2) Open the project <i>NegativeBinomialReg.Rproj</i> in RStudio (when inside the project, you will see its name on the top-right of your RStudio session)
3) Open and work though the <i>CodeToRun.R</i> file which should be the only file that you need to interact with. Run the lines in the file, adding your database specific information and so on (see comments within the file for instructions). The last line of this file will run the study <i>(source(here("RunStudy.R"))</i>.     
4) After running you should then have a zip folder with results to share in your home directory.

## Associated repositories and documents
The paper (Barclay et al., in prep) contains analyses from other github repositories which can be found here:

https://github.com/oxford-pharmacoepi/CancerCovid_CohortDiagnostics

https://github.com/oxford-pharmacoepi/CancerCovid_Characterisations

https://github.com/oxford-pharmacoepi/CancerCovid_IncidencePrevalence


In addition, a shiny app through which to view additional cohort diagnostics will be added.
