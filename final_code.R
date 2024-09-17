#########################################################
## Libraries
#########################################################
library(tidyverse)
library(ggthemes)
library(scales)
library(knitr)
library(kableExtra)
library(RColorBrewer)
library(rstudioapi)


#########################################################
## Data Read
#########################################################
## Set the working directory to path of saved R script
setwd(getActiveDocumentContext()$path %>% 
        str_remove_all(pattern = "Claim Fraud Detection\\.R"))

## Reading in all the files
raw_claims_data <- read_csv("Claim_Fraud_data__Claims.csv")
raw_assessment_data <- read_csv("Claim_Fraud_data_Assessment.csv")
raw_driver_data <- read_csv("Claim_Fraud_data_Driver.csv")
raw_policy_data <- read_csv("Claim_Fraud_data_Policy.csv")
raw_vehicle_data <- read_csv("Claim_Fraud_data_Vehicle.csv")


#########################################################
## Exploratory Data Analysis
#########################################################
## Just trying to view each dataset directly
raw_claims_data
raw_assessment_data
raw_driver_data
raw_policy_data
raw_vehicle_data
















