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
        str_remove_all(pattern = "final_code\\.R"))

## Reading in all the files
raw_claims_data <- read_csv("Claim_Fraud_data__Claims.csv")
raw_assessment_data <- read_csv("Claim_Fraud_data_Assessment.csv")
raw_driver_data <- read_csv("Claim_Fraud_data_Driver.csv")
raw_policy_data <- read_csv("Claim_Fraud_data_Policy.csv")
raw_vehicle_data <- read_csv("Claim_Fraud_data_Vehicle.csv")

## Creating a function to make the columns easier to work with
pretty_cols <- function(x) {
  x %>% 
    str_replace_all(pattern = " " ,replacement = "_") %>% 
    tolower()
}

## Changing the column names to make them easier to work with
colnames(raw_claims_data) <- colnames(raw_claims_data) %>% pretty_cols()
colnames(raw_assessment_data) <- colnames(raw_assessment_data) %>% pretty_cols()
colnames(raw_driver_data) <- colnames(raw_driver_data) %>% pretty_cols()
colnames(raw_policy_data) <- colnames(raw_policy_data) %>% pretty_cols()
colnames(raw_vehicle_data) <- colnames(raw_vehicle_data) %>% pretty_cols()

## Remove some extra columns at the end which should not be getting read in from the csv's
raw_assessment_data <- raw_assessment_data %>% 
  select(-contains("..."))

raw_driver_data <- raw_driver_data %>% 
  select(-contains("..."))


#########################################################
## Data Checking & Joining
#########################################################
#### Policy Data Pre-Join Check
## Checking the uniqueness of the variables that we are going to be joining on
raw_policy_data %>%
  count(policy_id) %>% 
  filter(n > 1) %>% 
  arrange(desc(n))
  
## Policy Id is not unique ... 142 duplicates. Need to understand it better
raw_policy_data %>% 
  filter(policy_id == "P101444063304") %>% View()

## Seems like it is a pure duplicate in this example
raw_policy_data %>% 
  filter(policy_id == "P101444063304") %>% 
  unique()

## Using unique fixes the issue
raw_policy_data %>%
  unique() %>% 
  count(policy_id) %>% 
  filter(n > 1) %>% 
  arrange(desc(n))

## Removing the duplicate records in the policy dataset
unique_policy_data <- raw_policy_data %>%
  unique()


#### Claims Data Pre-Join Check
## Checking the uniqueness of Claim Id
raw_claims_data %>% 
  count(claim_id) %>% 
  filter(n > 1) %>% 
  arrange(desc(n))

## Seeing if unique fixes the issue
raw_claims_data %>%
  unique() %>% 
  count(claim_id) %>% 
  arrange(desc(n))

## It does. Going to apply it to the raw_claims_data
unique_claims_data <- raw_claims_data %>%
  unique()


#### Driver Data Pre-Join Check
## Checking Driver Id
raw_driver_data %>% 
  count(driver_id) %>% 
  filter(n > 1) %>% 
  arrange(desc(n))

## Seeing if unique fixes the issue
raw_driver_data %>%
  unique() %>% 
  count(driver_id) %>% 
  filter(n > 1) %>% 
  arrange(desc(n))

## It does. Going to apply it to the raw_driver_data
unique_driver_data <- raw_driver_data %>%
  unique() 


#### Vehicle Data Pre-Join Check
raw_vehicle_data %>% 
  count(vehicle_id) %>% 
  filter(n > 1) %>% 
  arrange(desc(n))

## Seeing if unique fixes the issue
raw_vehicle_data %>%
  unique() %>% 
  count(vehicle_id) %>% 
  filter(n > 1) %>% 
  arrange(desc(n))

## It does. Going to apply it to the raw_vehicle_data
unique_vehicle_data <- raw_vehicle_data %>% 
  unique()

### Would normally remove most the code above as it's no longer necessary
### Could just keep the unique statements ... going to keep as-is 
### just to show the work behind finding the need for "unique"

##













