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

## Combining all the datasets together
combined_data <- unique_claims_data %>% 
  left_join(unique_policy_data ,by = "policy_id") %>% 
  left_join(unique_driver_data ,by = "driver_id") %>% 
  left_join(unique_vehicle_data ,by = "vehicle_id") %>% 
  select(all_of(colnames(raw_assessment_data)) ,fraud_ind)


#########################################################
## Data Checking & Joining
#########################################################
## Taking a look at all the fields
glimpse(combined_data)

## Going to look at some of the character fields as some appear inconsistent
combined_data %>% 
  count(gender)
  
combined_data %>% 
  count(education)

combined_data %>% 
  count(limits)

combined_data %>% 
  count(make)

combined_data %>% 
  count(model)

combined_data %>% 
  count(marital_status)

combined_data %>% 
  count(seat_belt)

combined_data %>% 
  count(income)

## Fixing the inconsistent character fields
combined_data <- combined_data %>% 
  mutate(gender_new          = case_when(gender %in% c('Boy' ,'M' ,'Male') ~ 'Male'
                                         ,gender %in% c('F' ,'Female' ,'Girl') ~ 'Female') %>% 
                                  factor(levels = c('Male' ,'Female'))
         ,education_new      = factor(education ,levels = c('Some High School' ,'High School or GED'
                                                            ,'Bachelors' ,'Masters' ,'Doctorate'))
         ,limits_new         = str_replace_all(limits ,pattern = "k" ,replacement = "000") %>% 
                                  str_replace_all(pattern = "000$" ,replacement = "k") %>% 
                                  factor(levels = c("15k" ,"20k" ,"25k" ,"50k" ,"100k" ,"250k" ,"500k"))
         ,marital_status_new = case_when(marital_status %in% c('M' ,'Marr' ,'Married') ~ 'Married'
                                         ,marital_status %in% c('S' ,'Single') ~ 'Single') %>% 
                                  as.factor()
         ,seat_belt_new      = seat_belt %>%
                                  factor(levels = c('Never' ,'Rarely' ,'Occasionally' ,'Usually' ,'Always' 
                                                    ,'Unknown'))
         ,income_new         = case_when(income %in% c('Mid' ,'Middle') ~ 'Middle'
                                         ,income %in% c('Working' ,'Wrk') ~ 'Working'
                                         ,TRUE ~ income) %>% 
                                  factor(levels = c('Poverty' ,'Working' ,'Middle' ,'Upper'))
                                  
         ) 

## At some point can come back to the "make" field and classify the brands as Luxury, etc...

## Want to understand what percent of claims are actually fraudulent
combined_data %>% 
  summarise(n          = n()
            ,fraud_ind = sum(fraud_ind)
            ,fraud_pct = fraud_ind / n
            ) 

## ... honestly a higher amount than I was expecting: ~16%

## Digging into the claim notes to try to text mine/find predictive phrases
## Going to separate the notes by sentence
claim_notes_testing <- combined_data %>% 
  select(claim_id ,claim_notes ,fraud_ind) %>% 
  separate_wider_delim(claim_notes 
                       ,delim = "." 
                       ,names_sep = "_"
                       ,too_few = "align_start"
                       ) %>% 
  mutate_at(vars(contains("claim_notes")) ,str_trim)

## A lot of the phrases/formats used actually do seem pretty consistent
claim_notes_testing %>% 
  group_by(claim_notes_1) %>% 
  summarise(n          = n()
            ,fraud_ind = sum(fraud_ind)
            ,fraud_pct = fraud_ind / n) %>% 
  arrange(desc(n)) %>% 
  View()

## The notes are also fairly consistent
## For claim_notes_1 can probably extract the intro phrase, vehicle (and make sure it matches)
## vehicle/object that was collided with, and then where it occurred
claim_notes_testing %>% 
  group_by(claim_notes_2) %>% 
  summarise(n          = n()
            ,fraud_ind = sum(fraud_ind)
            ,fraud_pct = fraud_ind / n) %>% 
  arrange(desc(n)) %>% 
  View()

## For claim notes 2, this is useful to tell the severity of the injury
## Would likely be good to cross this with the incurred amount to see if they are inconsistent
## i.e. a small injury with a large loss amt
claim_notes_testing %>% 
  group_by(claim_notes_3) %>% 
  summarise(n          = n()
            ,fraud_ind = sum(fraud_ind)
            ,fraud_pct = fraud_ind / n) %>% 
  arrange(desc(n)) %>% 
  View()

## For claim notes 3, this is useful as it has some thoughts around fraud
## Also has more info on injury severity and police report status
claim_notes_testing %>% 
  group_by(claim_notes_4) %>% 
  summarise(n          = n()
            ,fraud_ind = sum(fraud_ind)
            ,fraud_pct = fraud_ind / n) %>% 
  arrange(desc(n)) %>% 
  View()


## Claim notes 4 has more information on the fraud, lots of blanks
claim_notes_testing %>% 
  group_by(claim_notes_5) %>% 
  summarise(n          = n()
            ,fraud_ind = sum(fraud_ind)
            ,fraud_pct = fraud_ind / n) %>% 
  arrange(desc(n)) %>% 
  View()

## More fraud info in claim_notes_5
claim_notes_testing %>% 
  group_by(claim_notes_6) %>% 
  summarise(n          = n()
            ,fraud_ind = sum(fraud_ind)
            ,fraud_pct = fraud_ind / n) %>% 
  arrange(desc(n)) %>% 
  View()

## All blanks for claim_notes_6 - can probably just be discarded



