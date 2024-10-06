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
library(naniar)

## Turn off scientific notation
options(scipen = 999)


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
  mutate(gender_new                = case_when(gender %in% c('Boy' ,'M' ,'Male') ~ 'Male'
                                               ,gender %in% c('F' ,'Female' ,'Girl') ~ 'Female') %>% 
                                        factor(levels = c('Male' ,'Female'))
         ,education_new            = factor(education ,levels = c('Some High School' ,'High School or GED'
                                                                  ,'Bachelors' ,'Masters' ,'Doctorate'))
         ,limits_new               = str_replace_all(limits ,pattern = "k" ,replacement = "000") %>% 
                                        str_replace_all(pattern = "000$" ,replacement = "k") %>% 
                                        factor(levels = c("15k" ,"20k" ,"25k" ,"50k" ,"100k" ,"200k" ,"250k" ,"300k" ,"500k"))
         ,limits_numeric           = limits_new %>% 
                                        str_replace(pattern = "k" ,replacement = "000") %>% 
                                        as.numeric()
         ,marital_status_new       = case_when(marital_status %in% c('M' ,'Marr' ,'Married') ~ 'Married'
                                               ,marital_status %in% c('S' ,'Single') ~ 'Single') %>% 
                                        as.factor()
         ,seat_belt_new            = seat_belt %>%
                                        factor(levels = c('Never' ,'Rarely' ,'Occasionally' ,'Usually' ,'Always' 
                                                          ,'Unknown'))
         ,income_new               = case_when(income %in% c('Mid' ,'Middle') ~ 'Middle'
                                               ,income %in% c('Working' ,'Wrk') ~ 'Working'
                                               ,TRUE ~ income) %>% 
                                        factor(levels = c('Poverty' ,'Working' ,'Middle' ,'Upper'))
         ,claim_greater_than_limit = as.numeric(claimamount > limits_numeric)
         ) 

## At some point may want to come back to the "make" field and classify the brands as Luxury, etc...

## Want to understand what percent of claims are actually fraudulent
combined_data %>% 
  summarise(n          = n()
            ,fraud_ind = sum(fraud_ind)
            ,fraud_pct = fraud_ind / n
            ) 

## ... honestly a higher amount than I was expecting: ~16%

#########################################################
## Claim Notes
#########################################################
## Digging into the claim notes to try to text mine/find predictive phrases
## Going to separate the notes by sentence
separated_claim_data <- combined_data %>% 
  select(claim_id ,claim_notes) %>% 
  separate_wider_delim(claim_notes 
                       ,delim = "." 
                       ,names_sep = "_"
                       ,too_few = "align_start"
                       ) %>% 
  mutate_at(vars(contains("claim_notes")) ,str_trim) %>% 
  mutate_at(vars(contains("claim_notes")) ,function(x) {if_else(x == "" ,NA ,x)})
  
## Joining the notes back onto the original dataset
combined_data <- combined_data %>% 
  left_join(separated_claim_data ,by = "claim_id")


## Going to see how variables are correlated with fraud
check_fraud_pct <- function(x) {
  combined_data %>% 
    group_by(!!sym(x)) %>% 
    summarise(n          = n()
              ,fraud_ind = sum(fraud_ind)
              ,fraud_pct = fraud_ind / n) %>% 
    arrange(desc(n))
}

## A lot of the phrases/formats used actually do seem pretty consistent
check_fraud_pct("claim_notes_1") %>% View()

## For claim_notes_1 can probably extract the intro phrase, vehicle (and make sure it matches)
## vehicle/object that was collided with, and then where it occurred

check_fraud_pct("claim_notes_2") %>% View()

## For claim notes 2, this is useful to tell the severity of the injury
## Would likely be good to cross this with the incurred amount to see if they are inconsistent
## i.e. a small injury with a large loss amt

check_fraud_pct("claim_notes_3") %>% View()

## For claim notes 3, this is useful as it has some thoughts around fraud
## Also has more info on injury severity and police report status

check_fraud_pct("claim_notes_4") %>% View()


## Claim notes 4 has more information on the fraud, lots of blanks

check_fraud_pct("claim_notes_5") %>% View()

## More fraud info in claim_notes_5

check_fraud_pct("claim_notes_6") %>% View()

## All blanks for claim_notes_6 - can probably just be discarded

## Going to start with breaking down each of the claim notes
combined_data <- combined_data %>% 
  # select(claim_notes_1) %>% 
  mutate(note_type_1            = str_sub(claim_notes_1
                                          ,end = str_locate(claim_notes_1 ,pattern = ":")[,1])
         ,note_vehicle_1        = str_sub(claim_notes_1
                                          ,start = str_locate(claim_notes_1 ,pattern = "\\(")[,"start"]
                                          ,end = str_locate(claim_notes_1 ,pattern = "\\)")[,"start"]
                                          ) %>% 
                                     str_remove_all(pattern = "\\(|\\)") %>% 
                                     str_trim()
         ,fraud_ind_1           = str_extract(note_type_1 %>% 
                                                str_replace(pattern = "Frauuuuud" ,replacement = "Fraud") %>% 
                                                tolower()
                                              ,pattern = "fraud\\s*(\\S+)")
         ,pedestrian_2_flag     = str_extract(claim_notes_2 %>% tolower()
                                              ,pattern = "cyclist|pedestrian") %>% 
                                     coalesce("Other")
         ,injury_type_2         = str_extract(claim_notes_2 %>% tolower() 
                                              ,pattern = "(\\S+)\\s*damage|(\\S+)\\s*injuries")
         ,injury_level_2        = str_extract(claim_notes_2 %>% tolower() 
                                              ,pattern = "(\\S+)\\s*(?=damage)|(\\S+)\\s*(?=injuries)")
         ,injury_vs_damage_2    = str_extract(claim_notes_2 %>% tolower() 
                                              ,pattern = "damage|injuries")
         ,fraud_ind_2           = str_extract(claim_notes_2 %>% 
                                                tolower() %>% 
                                                str_replace(pattern = "frauuuuud" ,replacement = "fraud")
                                              ,pattern = "(\\S+)\\s*(\\S+)\\s*fraud")
         ,injury_type_3         = str_extract(claim_notes_3 %>% tolower() 
                                              ,pattern = "(\\S+)\\s*damage|(\\S+)\\s*injuries|injuries reported")
         ,injury_level_3        = str_extract(claim_notes_3 %>% tolower() 
                                              ,pattern = "(\\S+)\\s*(?=damage)|(\\S+)\\s*(?=injuries)")
         ,injury_vs_damage_3    = str_extract(claim_notes_3 %>% tolower() 
                                              ,pattern = "damage|injuries")         
         ,police_type_3         = str_extract(claim_notes_3 %>% tolower() 
                                              ,pattern = "police and medical assistance|police\\s*(\\S+)")
         ,triple_exclamation_3  = as.numeric(str_detect(claim_notes_3 ,pattern = "^!!!$"))
         ,fraud_ind_3           = str_extract(claim_notes_3 %>% 
                                                tolower() %>% 
                                                str_replace(pattern = "frauuuuud" ,replacement = "fraud")
                                              ,pattern = "(\\S+)\\s*(\\S+)\\s*fraud|fraud")
         ,police_type_4         = str_extract(claim_notes_4 %>% tolower() 
                                              ,pattern = "police and medical assistance|police\\s*(\\S+)|officer on site")
         ,alcohol_or_drugs_4    = str_extract(claim_notes_4 %>% tolower()
                                              ,pattern = "alcohol or drugs suspected")
         ,triple_exclamation_4  = as.numeric(str_detect(claim_notes_4 ,pattern = "^!!!$"))
         ,fraud_ind_4           = str_extract(claim_notes_4 %>% 
                                                tolower() %>% 
                                                str_replace(pattern = "frauuuuud" ,replacement = "fraud")
                                              ,pattern = "(\\S+)\\s*(\\S+)\\s*fraud|fraud|no signs of fraud")
         ,alcohol_or_drugs_5    = str_extract(claim_notes_5 %>% tolower()
                                              ,pattern = "alcohol or drugs suspected")
         ,triple_exclamation_5  = as.numeric(str_detect(claim_notes_5 ,pattern = "^!!!$"))
         ,fraud_ind_5           = str_extract(claim_notes_5 %>% 
                                                tolower() %>% 
                                                str_replace(pattern = "frauuuuud" ,replacement = "fraud")
                                              ,pattern = "(\\S+)\\s*(\\S+)\\s*fraud|fraud|no signs of fraud")
         ,note_type             = note_type_1 %>% 
                                     str_replace(pattern = "Frauuuuud" ,replacement = "Fraud")
         ,vehicle_mismatch_flag = as.numeric(str_c(make ," " ,model) != note_vehicle_1)
         ,pedestrian_type       = pedestrian_2_flag
         ,injury_type           = if_else(!is.na(injury_type_2) & !is.na(injury_type_3)
                                          ,str_c(injury_type_2 ," and " ,injury_type_3)
                                          ,coalesce(injury_type_2 ,injury_type_3))
         ,police_type           = coalesce(police_type_3 ,police_type_4)
         ,alcohol_or_drugs      = coalesce(alcohol_or_drugs_4 ,alcohol_or_drugs_5)
         ,triple_exclamation    = coalesce(triple_exclamation_3 ,triple_exclamation_4 ,triple_exclamation_5)
         ,fraud_note_ind        = if_else(!is.na(fraud_ind_1) & !is.na(fraud_ind_2) 
                                          ,str_c(fraud_ind_1 ," " ,fraud_ind_2)
                                          ,coalesce(fraud_ind_1 ,fraud_ind_2 ,fraud_ind_3 ,fraud_ind_4 ,fraud_ind_5)
                                          )
         ,fraud_note_ind        = if_else(str_detect(fraud_note_ind ,pattern = "susp")
                                          ,"suspected fraud"
                                          ,fraud_note_ind
                                          ) %>% 
                                     factor(levels = c('no signs of fraud' ,'probably not fraud' ,'suspected fraud' ,'fraud'))
         ) %>% 
  select(-matches("_[0-9]"))


check_fraud_pct("note_type") ## Potentially useful ... although may be duplicative with fraud_note_ind
check_fraud_pct("vehicle_mismatch_flag") ## Not useful, remove
check_fraud_pct("pedestrian_type") ## Maybe helpful? Can see if interaction term would help
check_fraud_pct("injury_type") ## Maybe helpful? Can see if interaction term would help
check_fraud_pct("police_type") ## Maybe helpful? Can see if interaction term would help
check_fraud_pct("alcohol_or_drugs") ## Maybe helpful? Can see if interaction term would help
check_fraud_pct("triple_exclamation") ## Extremely predictive ... although few data points
check_fraud_pct("fraud_note_ind") ## Extremely predictive ... although few data points
check_fraud_pct("claim_greater_than_limit") ## Seems potentially helpful

## Thought there would be instances where the claim notes vehicle doesn't match
## Turns out there aren't and it's not predictive --> remove column
combined_data <- combined_data %>% 
  select(-vehicle_mismatch_flag)

#########################################################
## Exploratory Data Analysis
#########################################################
## Looking for missing values in the fields
combined_data %>% 
  select_if(is.numeric) %>% 
  summary()

## Roughly 23 - 38 NA's for most numeric fields
## No NA's for the Fraud Indicator

## Creating a function to look at the numeric variable bands and their fraud rates
check_fraud_pct_num <- function(x ,y = 5) {
  combined_data %>% 
    mutate(band = cut_number(!!sym(x) ,n = y ,dig.lab=10)) %>% 
    filter(!is.na(band)) %>% 
    group_by(band) %>% 
    summarise(n          = n()
              ,fraud_ind = sum(fraud_ind)
              ,fraud_pct = fraud_ind / n)
}  

## Creating a function to look at the numeric variables directly and their fraud rates
check_fraud_pct_num2 <- function(x) {
  combined_data %>% 
    filter(!is.na(!!sym(x))) %>% 
    group_by(!!sym(x)) %>% 
    summarise(n          = n()
              ,fraud_ind = sum(fraud_ind)
              ,fraud_pct = fraud_ind / n)
} 

combined_data %>% 
  mutate(band = as.numeric(credit_score > 1)) %>% 
  filter(!is.na(band)) %>% 
  group_by(band) %>% 
  summarise(n          = n()
            ,fraud_ind = sum(fraud_ind)
            ,fraud_pct = fraud_ind / n)

## Checking the numeric cols
## Ones where I'm using function 2 should be considered for becoming factors
check_fraud_pct_num("policy_orig_eff_date") ## pretty stable over time
check_fraud_pct_num("accident_date") ## very small claims do not seem to be fraudulent usually
check_fraud_pct_num("claimamount") ## come back to this one ... not working well
check_fraud_pct_num("age") ## Younger bands seem to more fraudulent
check_fraud_pct_num("yrs_licensed") ## Earlier bands more fraudulent ... likely correlated with age
check_fraud_pct_num("hp_vehicle") ## pretty stable
check_fraud_pct_num("model_year") ## pretty stable
check_fraud_pct_num2("policy_orig_eff_date") %>% View() ## checking for outliers
check_fraud_pct_num2("accident_date") %>% View() ## checking for outliers ... lot of claims on 30th of months
check_fraud_pct_num2("age") %>% View() ## checking for outliers
check_fraud_pct_num2("yrs_licensed") %>% View() ## checking for outliers
check_fraud_pct_num2("hp_vehicle") %>% View() ## checking for outliers
check_fraud_pct_num2("model_year") %>% View()  ## checking for outliers
check_fraud_pct_num2("num_drivers") ## less drivers seems more fraudulent ... fix those with 17
check_fraud_pct_num2("clms_flt1") ## Seems very predictive ... 1+ is very fraudulent ... fix those with 17
check_fraud_pct_num2("clms_flt2") ## Seems very predictive ... 1+ is very fraudulent
check_fraud_pct_num2("clms_flt3") ## Seems very predictive ... 1+ is very fraudulent
check_fraud_pct_num2("clms_flt4") ## Seems very predictive ... 1+ is very fraudulent
check_fraud_pct_num2("clms_flt5") ## Seems very predictive ... 1+ is very fraudulent
check_fraud_pct_num2("clms_naf1") ## Seems very predictive ... 1+ is very fraudulent
check_fraud_pct_num2("clms_naf2") ## Seems very predictive ... 1+ is very fraudulent
check_fraud_pct_num2("clms_naf3") ## Seems very predictive ... 1+ is very fraudulent
check_fraud_pct_num2("clms_naf4") ## Seems very predictive ... 1+ is very fraudulent
check_fraud_pct_num2("clms_naf5") ## Seems very predictive ... 1+ is very fraudulent
check_fraud_pct_num("late_90d") ## More late --> more fraudulent
check_fraud_pct_num2("late_90d") %>% View() ## Large number of them at 500 --> need to fix
check_fraud_pct_num2("num_accts") ## Pretty stable
check_fraud_pct_num2("outs_bal") ## larger balance --> more fraudulent ... need to fix the 99 value
check_fraud_pct_num2("viol_mjr1") ## Seems very predictive ... 1+ is very fraudulent
check_fraud_pct_num2("viol_mjr2") ## Seems very predictive ... 1+ is very fraudulent ... fix those with 17
check_fraud_pct_num2("viol_mjr3") ## Seems very predictive ... 1+ is very fraudulent
check_fraud_pct_num2("viol_mjr4") ## Seems very predictive ... 1+ is very fraudulent
check_fraud_pct_num2("viol_mjr5") ## Seems very predictive ... 1+ is very fraudulent
check_fraud_pct_num2("pop_density") ## Seems pretty stable
check_fraud_pct_num("time_bet10pm2am") ## Later seems slightly more fraudulent
check_fraud_pct_num2("time_bet10pm2am") %>% View() ## Checking for outliers ... fix those with 0.75
check_fraud_pct_num("time_highway") ## Seems pretty stable
check_fraud_pct_num2("time_highway") %>% View() ## Checking for outliers
check_fraud_pct_num("exposure")  ## Seems pretty stable .. unclear why 0.5 or 1 isn't more common
check_fraud_pct_num2("exposure") %>% View() ## Checking for outliers
check_fraud_pct_num2("policy_tenure") %>% View() ## Seems pretty stable
check_fraud_pct_num("credit_score") ## low credit scores tend to be better?? ... shouldn't be values above 1
check_fraud_pct_num2("credit_score") %>% View() ## lots of values at 1.5 - will need to be fixed
check_fraud_pct_num("report_lag" ,2) ## less fraud for lower reports
check_fraud_pct_num2("report_lag") %>% View() ## Fix the 99 issue
check_fraud_pct_num2("limits_numeric") ## Higher limits tend to have much less fruad
check_fraud_pct_num2("claim_greater_than_limit") ## Predictive of fraud if claim > limit



## Numeric variables that are missing:
## 1) Cell_Usage
## 2) Altitude
## 3) Foggy_Days
## 4) Braking_Mile
## 5) Left_Mile


  

#########################################################
## Model Building
#########################################################
## Adding some additional variables before the modeling
combined_data <- combined_data %>% 
  mutate(acc_minus_eff_date = accident_date - policy_orig_eff_date)

## Get the variables I'm interested in using for modeling purposes
model_vars <- colnames(combined_data) %>% 
  enframe(name = NULL) %>% 
  filter(!str_detect(value ,pattern = "_id$"))


combined_data %>% 
  
  







