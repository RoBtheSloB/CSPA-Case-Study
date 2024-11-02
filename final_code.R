#########################################################
## Libraries
#########################################################
library(ggthemes)
library(scales)
library(knitr)
library(kableExtra)
library(RColorBrewer)
library(rstudioapi)
library(naniar)
library(lubridate)
library(ggcorrplot)
library(leaps)
library(glmnet)
library(caret)
library(magrittr)
library(MASS)
library(randomForest)
library(gbm)
library(tidyverse)

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
## Going to comment out sections that were previously used for checking
## Can uncomment those sections to see the checks

#### Policy Data Pre-Join Check
## Checking the uniqueness of the variables that we are going to be joining on
# raw_policy_data %>%
#   count(policy_id) %>% 
#   filter(n > 1) %>% 
#   arrange(desc(n))

## Policy Id is not unique ... 142 duplicates. Need to understand it better
# raw_policy_data %>% 
#   filter(policy_id == "P101444063304") %>% View()

## Seems like it is a pure duplicate in this example
# raw_policy_data %>% 
#   filter(policy_id == "P101444063304") %>% 
#   unique()

## Using unique fixes the issue
# raw_policy_data %>%
#   unique() %>% 
#   count(policy_id) %>% 
#   filter(n > 1) %>% 
#   arrange(desc(n))

## Removing the duplicate records in the policy dataset
unique_policy_data <- raw_policy_data %>%
  unique()


#### Claims Data Pre-Join Check
## Checking the uniqueness of Claim Id
# raw_claims_data %>% 
#   count(claim_id) %>% 
#   filter(n > 1) %>% 
#   arrange(desc(n))

## Seeing if unique fixes the issue
# raw_claims_data %>%
#   unique() %>% 
#   count(claim_id) %>% 
#   arrange(desc(n))

## It does. Going to apply it to the raw_claims_data
unique_claims_data <- raw_claims_data %>%
  unique()


#### Driver Data Pre-Join Check
## Checking Driver Id
# raw_driver_data %>% 
#   count(driver_id) %>% 
#   filter(n > 1) %>% 
#   arrange(desc(n))

## Seeing if unique fixes the issue
# raw_driver_data %>%
#   unique() %>% 
#   count(driver_id) %>% 
#   filter(n > 1) %>% 
#   arrange(desc(n))

## It does. Going to apply it to the raw_driver_data
unique_driver_data <- raw_driver_data %>%
  unique() 


#### Vehicle Data Pre-Join Check
# raw_vehicle_data %>% 
#   count(vehicle_id) %>% 
#   filter(n > 1) %>% 
#   arrange(desc(n))

## Seeing if unique fixes the issue
# raw_vehicle_data %>%
#   unique() %>% 
#   count(vehicle_id) %>% 
#   filter(n > 1) %>% 
#   arrange(desc(n))

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
# check_fraud_pct("claim_notes_1") %>% View()

## For claim_notes_1 can probably extract the intro phrase, vehicle (and make sure it matches)
## vehicle/object that was collided with, and then where it occurred

# check_fraud_pct("claim_notes_2") %>% View()

## For claim notes 2, this is useful to tell the severity of the injury
## Would likely be good to cross this with the incurred amount to see if they are inconsistent
## i.e. a small injury with a large loss amt

# check_fraud_pct("claim_notes_3") %>% View()

## For claim notes 3, this is useful as it has some thoughts around fraud
## Also has more info on injury severity and police report status

# check_fraud_pct("claim_notes_4") %>% View()


## Claim notes 4 has more information on the fraud, lots of blanks

# check_fraud_pct("claim_notes_5") %>% View()

## More fraud info in claim_notes_5

# check_fraud_pct("claim_notes_6") %>% View()

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
         ,pedestrian_flag_2     = str_extract(claim_notes_2 %>% tolower()
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
           str_replace(pattern = "Frauuuuud|frauuuuud" ,replacement = "Fraud")
         ,vehicle_mismatch_flag = as.numeric(str_c(make ," " ,model) != note_vehicle_1)
         ,pedestrian_type       = pedestrian_flag_2
         ,injury_type           = if_else(!is.na(injury_type_2) & !is.na(injury_type_3)
                                          ,str_c(injury_type_2 ," and " ,injury_type_3)
                                          ,coalesce(injury_type_2 ,injury_type_3))
         ,police_type           = coalesce(police_type_3 ,police_type_4)
         ,alcohol_or_drugs      = coalesce(alcohol_or_drugs_4 ,alcohol_or_drugs_5)
         ,triple_exclamation    = as.numeric(coalesce(triple_exclamation_3 ,0) + coalesce(triple_exclamation_4 ,0) 
                                             + coalesce(triple_exclamation_5 ,0))
         ,fraud_note_ind        = if_else(!is.na(fraud_ind_1) & !is.na(fraud_ind_2) 
                                          ,str_c(fraud_ind_1 ," " ,fraud_ind_2)
                                          ,coalesce(fraud_ind_1 ,fraud_ind_2 ,fraud_ind_3 ,fraud_ind_4 ,fraud_ind_5
                                                    ,"no mention of fraud")
         )
         ,fraud_note_ind        = if_else(str_detect(fraud_note_ind ,pattern = "susp")
                                          ,"suspected fraud"
                                          ,fraud_note_ind
         ) %>% 
           factor(levels = c('no mention of fraud' ,'no signs of fraud' 
                             ,'probably not fraud' ,'suspected fraud' ,'fraud'))
  ) %>% 
  select(-matches("_[0-9]$"))


# check_fraud_pct("note_type") ## Potentially useful ... although may be duplicative with fraud_note_ind
# check_fraud_pct("vehicle_mismatch_flag") ## Not useful, remove
# check_fraud_pct("pedestrian_type") ## Maybe helpful? Can see if interaction term would help
# check_fraud_pct("injury_type") ## Maybe helpful? Can see if interaction term would help
# check_fraud_pct("police_type") ## Maybe helpful? Can see if interaction term would help
# check_fraud_pct("alcohol_or_drugs") ## Maybe helpful? Can see if interaction term would help
# check_fraud_pct("triple_exclamation") ## Extremely predictive ... although few data points
# check_fraud_pct("fraud_note_ind") ## Extremely predictive ... although few data points
# check_fraud_pct("claim_greater_than_limit") ## Seems potentially helpful

## Thought there would be instances where the claim notes vehicle doesn't match (which could indicate fraud)
## Turns out there aren't  --> remove column
combined_data <- combined_data %>% 
  select(-vehicle_mismatch_flag)

#########################################################
## Exploratory Data Analysis - Numeric Columns
#########################################################
## Looking for missing values in the numeric fields
# combined_data %>% 
#   select_if(is.numeric) %>% 
#   summary()

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

## Checking the numeric cols
## Ones where I'm using function 2 should be considered for becoming factors
# check_fraud_pct_num("policy_orig_eff_date") ## pretty stable over time
# check_fraud_pct_num("accident_date") ## very small claims do not seem to be fraudulent usually
# check_fraud_pct_num("claimamount") ## come back to this one ... not working well
# check_fraud_pct_num("age") ## Younger bands seem to more fraudulent
# check_fraud_pct_num("yrs_licensed") ## Earlier bands more fraudulent ... likely correlated with age
# check_fraud_pct_num("hp_vehicle") ## pretty stable
# check_fraud_pct_num("model_year") ## pretty stable
# check_fraud_pct_num2("policy_orig_eff_date") %>% View() ## checking for outliers
# check_fraud_pct_num2("accident_date") %>% View() ## checking for outliers ... lot of claims on 30th of months
# check_fraud_pct_num2("age") %>% View() ## checking for outliers
# check_fraud_pct_num2("yrs_licensed") %>% View() ## checking for outliers
# check_fraud_pct_num2("hp_vehicle") %>% View() ## checking for outliers
# check_fraud_pct_num2("model_year") %>% View()  ## checking for outliers
# check_fraud_pct_num2("num_drivers") ## less drivers seems more fraudulent ... fix those with 17
# check_fraud_pct_num2("clms_flt1") ## Seems very predictive ... 1+ is very fraudulent ... fix those with 17
# check_fraud_pct_num2("clms_flt2") ## Seems very predictive ... 1+ is very fraudulent
# check_fraud_pct_num2("clms_flt3") ## Seems very predictive ... 1+ is very fraudulent
# check_fraud_pct_num2("clms_flt4") ## Seems very predictive ... 1+ is very fraudulent
# check_fraud_pct_num2("clms_flt5") ## Seems very predictive ... 1+ is very fraudulent
# check_fraud_pct_num2("clms_naf1") ## Seems very predictive ... 1+ is very fraudulent
# check_fraud_pct_num2("clms_naf2") ## Seems very predictive ... 1+ is very fraudulent
# check_fraud_pct_num2("clms_naf3") ## Seems very predictive ... 1+ is very fraudulent
# check_fraud_pct_num2("clms_naf4") ## Seems very predictive ... 1+ is very fraudulent
# check_fraud_pct_num2("clms_naf5") ## Seems very predictive ... 1+ is very fraudulent
# check_fraud_pct_num("late_90d") ## More late --> more fraudulent
# check_fraud_pct_num2("late_90d") %>% View() ## Large number of them at 500 --> need to fix
# check_fraud_pct_num2("num_accts") ## Pretty stable
# check_fraud_pct_num2("outs_bal") ## larger balance --> more fraudulent ... need to fix the 99 value
# check_fraud_pct_num2("viol_mjr1") ## Seems very predictive ... 1+ is very fraudulent
# check_fraud_pct_num2("viol_mjr2") ## Seems very predictive ... 1+ is very fraudulent ... fix those with 17
# check_fraud_pct_num2("viol_mjr3") ## Seems very predictive ... 1+ is very fraudulent
# check_fraud_pct_num2("viol_mjr4") ## Seems very predictive ... 1+ is very fraudulent
# check_fraud_pct_num2("viol_mjr5") ## Seems very predictive ... 1+ is very fraudulent
# check_fraud_pct_num2("pop_density") ## Seems pretty stable
# check_fraud_pct_num("time_bet10pm2am") ## Later seems slightly more fraudulent
# check_fraud_pct_num2("time_bet10pm2am") %>% View() ## Checking for outliers ... fix those with 0.75
# check_fraud_pct_num("time_highway") ## Seems pretty stable
# check_fraud_pct_num2("time_highway") %>% View() ## Checking for outliers
# check_fraud_pct_num("exposure")  ## Seems pretty stable .. unclear why 0.5 or 1 isn't more common
# check_fraud_pct_num2("exposure") %>% View() ## Checking for outliers
# check_fraud_pct_num2("policy_tenure") %>% View() ## Seems pretty stable
# check_fraud_pct_num("credit_score") ## low credit scores tend to be better?? ... shouldn't be values above 1
# check_fraud_pct_num2("credit_score") %>% View() ## lots of values at 1.5 - will need to be fixed
# check_fraud_pct_num("report_lag" ,2) ## less fraud for lower reports
# check_fraud_pct_num2("report_lag") %>% View() ## Fix the 99 issue
# check_fraud_pct_num2("limits_numeric") ## Higher limits tend to have much less fruad
# check_fraud_pct_num2("claim_greater_than_limit") ## Predictive of fraud if claim > limit

## Numeric variables that are missing:
## 1) Cell_Usage
## 2) Altitude
## 3) Foggy_Days
## 4) Braking_Mile
## 5) Left_Mile


## Fixing the columns that were identified as having issues above
combined_data <- combined_data %>% 
  mutate(num_drivers_new      = na_if(num_drivers ,17)
         ,clms_flt1_new       = na_if(clms_flt1 ,17)
         ,late_90d_new        = na_if(late_90d ,500)
         ,outs_bal_new        = na_if(outs_bal ,99)
         ,viol_mjr2_new       = na_if(viol_mjr2 ,17)
         ,time_bet10pm2am_new = na_if(time_bet10pm2am ,0.75)
         ,credit_score_new    = na_if(credit_score ,1.5)
         ,report_lag_new      = na_if(report_lag ,99)
         ,pop_density         = na_if(pop_density ,5) %>% as.factor()
  )


#########################################################
## Exploratory Data Analysis - Character Columns
#########################################################
## Looking at the character fields
# combined_data %>% 
#   select_if(is.character) %>% 
#   summarise(across(everything(), ~ sum(is.na(.x)))) %>% 
#   pivot_longer(policy_id:alcohol_or_drugs) %>% 
#   rename(n = value)

## Checking their relationships to fraud
# check_fraud_pct_num2("gender") ## Need to fix these fields
# check_fraud_pct_num2("education") ## Seems predictive --> should convert to factor
# check_fraud_pct_num2("limits") ## Need to fix the inconsistencies
# check_fraud_pct_num2("make") %>% arrange(desc(n)) %>% View() ## Doesn't feel too predictive
# check_fraud_pct_num2("model") %>% arrange(desc(n)) %>% View() ## Doesn't feel too predictive ... so many lvls
# check_fraud_pct_num2("marital_status") ## Need to fix these fields
# check_fraud_pct_num2("num_cars") ## Need to fix these fields
# check_fraud_pct_num2("seat_belt") ## Doesn't seem too predictive --> should convert to factor anyways
# check_fraud_pct_num2("income") ## Need to fix these fields
# check_fraud_pct_num2("note_type") ## Should condense this field as many aren't useful
# check_fraud_pct_num2("pedestrian_type") ## Doesn't seem too useful
# check_fraud_pct_num2("injury_type") %>% arrange(desc(n)) ## Could be somewhat useful?
# check_fraud_pct_num2("police_type") ## Could be somewhat useful?
# check_fraud_pct_num2("alcohol_or_drugs") ## Could be somewhat useful?

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
         ,num_cars_new             = case_when(num_cars == "Four" ~ "4"
                                               ,num_cars == "Three" ~ "3"
                                               ,num_cars == "Two" ~ "2"
                                               ,TRUE ~ num_cars
         ) %>% 
           as.factor()
         ,num_cars_band_new        = case_when(num_cars  == "1" ~ "1"
                                               ,num_cars == "2" ~ "2"
                                               ,num_cars == "3" ~ "3"
                                               ,num_cars == "4" ~ "4"
                                               ,TRUE ~ "5+") %>% 
           as.factor()         
         ,seat_belt_new            = seat_belt %>%
           factor(levels = c('Never' ,'Rarely' ,'Occasionally' ,'Usually' ,'Always' 
                             ,'Unknown'))
         ,income_new               = case_when(income %in% c('Mid' ,'Middle') ~ 'Middle'
                                               ,income %in% c('Working' ,'Wrk') ~ 'Working'
                                               ,TRUE ~ income) %>% 
           factor(levels = c('Poverty' ,'Working' ,'Middle' ,'Upper'))
         ,note_type_new            = case_when(!(note_type %in% c(":" ,"Fraud Suspected:" ,"Hit-and-run incident:")) ~ "All Other"
                                               ,TRUE ~ note_type
         ) %>% 
           factor(levels = c(":" ,"All Other" ,"Hit-and-run incident:" ,"Fraud Suspected:"))
         ,pedestrian_type_new      = pedestrian_type %>% tolower() %>% factor(levels = c("other" ,"cyclist" ,"pedestrian"))
         ,injury_type_new          = injury_type %>% as.factor()
         ,police_type_new          = police_type %>% 
           coalesce("no mention of police") %>% 
           factor(levels = c("officer on site" ,"police report" 
                             ,"police and medical assistance"
                             ,"police notified" ,"no mention of police"))
         ,alcohol_or_drugs_new     = alcohol_or_drugs %>% coalesce("All Other") %>% as.factor()
         ,claim_greater_than_limit = as.numeric(claimamount > limits_numeric)
  ) 

## Confirming the new fields are looking better
# check_fraud_pct_num2("gender_new")
# check_fraud_pct_num2("education_new")
# check_fraud_pct_num2("limits_new")
# check_fraud_pct_num2("marital_status_new")
# check_fraud_pct_num2("num_cars_new")
# check_fraud_pct_num2("num_cars_band_new")
# check_fraud_pct_num2("seat_belt_new")
# check_fraud_pct_num2("income_new")
# check_fraud_pct_num2("pedestrian_type_new")
# check_fraud_pct_num2("injury_type_new")
# check_fraud_pct_num2("police_type_new")
# check_fraud_pct_num2("alcohol_or_drugs_new")
# check_fraud_pct_num2("claim_greater_than_limit")

## Fixing the columns that were identified as having issues above
combined_data <- combined_data %>% 
  mutate(num_cars_new = na_if(num_cars_new ,"17"))


#########################################################
## Exploratory Data Analysis - Date Columns
#########################################################
## Checking the conversion of the date columns from numeric
# combined_data %>% 
#   mutate(policy_orig_eff_date_new = ymd(policy_orig_eff_date)
#          ,accident_date_new       = ymd(accident_date)
#          ,pol_date_issue          = as.numeric(is.na(policy_orig_eff_date_new) & !is.na(policy_orig_eff_date))
#          ,acc_date_issue          = as.numeric(is.na(accident_date_new) & !is.na(accident_date))
#          ) %>% 
#   group_by(pol_date_issue ,acc_date_issue) %>% 
#   summarise(n          = n()
#             ,fraud_ind = sum(fraud_ind)
#             ,fraud_pct = fraud_ind / n) %>% 
#   arrange(desc(n))

## Given that the date issue (February 29th & 30th) doesn't seem useful/predictive
## just going to convert everything and have those be NA

## Also going to confirm that there aren't any instances where claims are being paid
## when the policy is not inforce
# combined_data %>% 
#   mutate(policy_orig_eff_date_new = ymd(policy_orig_eff_date)
#          ,accident_date_new       = ymd(accident_date)
#          ,final_eff_date          = policy_orig_eff_date + years(policy_tenure)
#          ,acc_greater_eff         = as.numeric(accident_date_new > final_eff_date)
#          ) %>% 
#   group_by(acc_greater_eff) %>% 
#   summarise(n          = n()
#             ,fraud_ind = sum(fraud_ind)
#             ,fraud_pct = fraud_ind / n) %>% 
#   arrange(desc(n))

## Confirmed

## Converting to dates
combined_data <- combined_data %>% 
  mutate(policy_orig_eff_date_new = ymd(policy_orig_eff_date)
         ,accident_date_new       = ymd(accident_date)
         ,policy_month            = month(policy_orig_eff_date_new)
         ,policy_day              = day(policy_orig_eff_date_new)
         ,policy_day_of_week      = wday(policy_orig_eff_date_new ,label = TRUE)
         ,acc_month               = month(accident_date_new)
         ,acc_day                 = day(accident_date_new)
         ,acc_day_of_week         = wday(accident_date_new ,label = TRUE) 
         ,report_date             = accident_date_new + days(report_lag))


## Date variables that are missing:
## 1) Report_date - re-created it with the report lag


#########################################################
## Variable Relationships/Bucketing
#########################################################
## Creating a function to see if there are any polynomial relationships
plot_num_bucket <- function(x ,y = 10) {
  combined_data %>% 
    mutate(band = cut_number(!!sym(x) ,n = y ,dig.lab = 10)) %>% 
    group_by(band) %>% 
    summarise(fraud_pct = sum(fraud_ind) / n()) %>% 
    ungroup() %>% 
    ggplot(aes(x = band , y = fraud_pct ,group = 1)) +
    geom_line() +
    geom_smooth(se = FALSE) +
    labs(title = x) +
    theme_fivethirtyeight()
}


plot_all_levels <- function(x) {
  combined_data %>% 
    group_by(!!sym(x)) %>% 
    summarise(fraud_pct = sum(fraud_ind) / n()) %>% 
    ungroup() %>% 
    ggplot(aes(x = !!sym(x) , y = fraud_pct ,group = 1)) +
    geom_line() +
    geom_smooth(se = FALSE) +
    labs(title = x) +
    theme_fivethirtyeight()  
}

## Creating a function to see the relationship b/w different variables
plot_x_y_vars <- function(x , y) {
  combined_data %>%
    filter(!!sym(x) != 0 & !!sym(y) != 0) %>% 
    mutate(indicator = if_else(!!sym(x) > !!sym(y)
                               ,"Weird"
                               ,"Normal") %>% 
             factor(levels = c("Weird" ,"Normal"))
    ) %>% 
    ggplot(aes(x = !!sym(x) ,fill = indicator)) +
    geom_bar() +
    scale_fill_brewer(palette = "Set1") +
    facet_wrap(str_c("~" ,y ,sep = " ")) +
    scale_y_continuous(label = comma) +
    ylab(y) 
}

## Looking at some of the numeric variables
# plot_num_bucket("age") ## Uniformly decreasing
# plot_num_bucket("claimamount") ## Pretty uniformly increasing, slightly quadratic; NA matters
# plot_num_bucket("yrs_licensed") ## Uniformly decreasing
# plot_num_bucket("hp_vehicle") ## Pretty flat; NA matters
# plot_num_bucket("model_year") ## Pretty flat; NA matters
# plot_all_levels("num_drivers_new") ## Good candidate for spline
# plot_all_levels("clms_flt1_new") ## Good candidate for quadratic
# plot_all_levels("clms_flt2") ## Good candidate for quadratic
# plot_all_levels("clms_flt3") ## Pretty unform increasing
# plot_all_levels("clms_flt4") ## Somewhat non-uniform
# plot_all_levels("clms_flt5") ## Good candidate for quadratic
# plot_all_levels("clms_naf1") ## Non-uniform near after 4+
# plot_all_levels("clms_naf2") ## Good candidate for quadratic
# plot_all_levels("clms_naf3") ## Pretty uniformly increasing
# plot_all_levels("clms_naf4") ## Somewhat non-linear
# plot_all_levels("clms_naf5") ## Pretty uniformly increasing
# plot_all_levels("late_90d_new") ## Pretty uniformly increasing; somewhat sporadic at tail
# plot_all_levels("num_accts") ## Reasonably flat; somewhat jumpy
# plot_all_levels("outs_bal_new") ## Pretty uniformly increasing; somewhat sporadic at tail
# plot_all_levels("viol_mjr1") ## Increasing; quadratic
# plot_all_levels("viol_mjr2_new") ## Pretty uniformly increasing
# plot_all_levels("viol_mjr3") ## Pretty uniformly increasing; somewhat quadratic
# plot_all_levels("viol_mjr4") ## Pretty uniformly increasing
# plot_all_levels("viol_mjr5") ## Pretty uniformly increasing
# plot_all_levels("viol_mnr1") ## Pretty uniformly increasing; somewhat quadratic
# plot_all_levels("viol_mnr2") ## Pretty uniformly increasing; much higher in tail
# plot_all_levels("viol_mnr3") ## Quadratic
# plot_all_levels("viol_mnr4") ## Pretty uniformly increasing
# plot_all_levels("viol_mnr5") ## Quadratic
# plot_all_levels("pop_density") ## Doesn't seem predictive
# plot_all_levels("time_bet10pm2am_new") ## Very different pattern; spline could be good
# plot_all_levels("time_highway") ## Pretty flat
# plot_num_bucket("exposure") ## Doesn't seem too predictive
# plot_all_levels("policy_tenure") ## Doesn't seem too predictive
# plot_all_levels("credit_score_new") ## Uniformly increasing; somewhat quadratic
# plot_num_bucket("credit_score_new")
# plot_all_levels("report_lag") ## Somewhat increasing
# plot_all_levels("limits_numeric") ## Somewhat quadratic
# plot_all_levels("claim_greater_than_limit") ## Predictive; only two levels

## Looking at one of the variables that has x years
# plot_x_y_vars("clms_flt1_new" ,"clms_flt2")
# plot_x_y_vars("clms_flt1_new" ,"clms_flt3")
# plot_x_y_vars("clms_flt1_new" ,"clms_flt4")
# plot_x_y_vars("clms_flt1_new" ,"clms_flt5")
# plot_x_y_vars("clms_flt2" ,"clms_flt3")
# plot_x_y_vars("clms_flt2" ,"clms_flt4")
# plot_x_y_vars("clms_flt2" ,"clms_flt5")
# plot_x_y_vars("clms_flt3" ,"clms_flt4")
# plot_x_y_vars("clms_flt3" ,"clms_flt5")
# plot_x_y_vars("clms_flt4" ,"clms_flt5")

## It's really unclear to me why there would ever be a scenario 
## where the number of historical at-fault claims in the past # year(s) for the insured
## would be greater than the historical at-fault claims in the past (#-1) year(s)

## Going to create an indicator    
combined_data <- combined_data %>% 
  mutate(indicator   = as.numeric(viol_mjr1 > viol_mjr2_new | 
                                    viol_mjr1 > viol_mjr3 |
                                    viol_mjr1 > viol_mjr4 |
                                    viol_mjr1 > viol_mjr5 |
                                    viol_mjr2_new > viol_mjr3 |
                                    viol_mjr2_new > viol_mjr4 |
                                    viol_mjr2_new > viol_mjr5 |
                                    viol_mjr2_new > viol_mjr5 |
                                    viol_mjr3 > viol_mjr4 |
                                    viol_mjr3 > viol_mjr4 |
                                    viol_mjr4 > viol_mjr5
  )
  ,indicator2 = as.numeric(clms_flt1_new > clms_flt2 | 
                             clms_flt1_new > clms_flt3 |
                             clms_flt1_new > clms_flt4 |
                             clms_flt1_new > clms_flt5 |
                             clms_flt2 > clms_flt3 |
                             clms_flt2 > clms_flt4 |
                             clms_flt2 > clms_flt5 |
                             clms_flt2 > clms_flt5 |
                             clms_flt3 > clms_flt4 |
                             clms_flt3 > clms_flt4 |
                             clms_flt4 > clms_flt5
  )
  ,indicator3 = as.numeric(viol_mnr1 > viol_mnr2 | 
                             viol_mnr1 > viol_mnr3 |
                             viol_mnr1 > viol_mnr4 |
                             viol_mnr1 > viol_mnr5 |
                             viol_mnr2 > viol_mnr3 |
                             viol_mnr2 > viol_mnr4 |
                             viol_mnr2 > viol_mnr5 |
                             viol_mnr2 > viol_mnr5 |
                             viol_mnr3 > viol_mnr4 |
                             viol_mnr3 > viol_mnr4 |
                             viol_mnr4 > viol_mnr5)
  ,issues     = as.numeric(viol_mjr1 > viol_mjr2_new) + 
    as.numeric(viol_mjr1 > viol_mjr3) +
    as.numeric(viol_mjr1 > viol_mjr4) +
    as.numeric(viol_mjr1 > viol_mjr5) +
    as.numeric(viol_mjr2_new > viol_mjr3) +
    as.numeric(viol_mjr2_new > viol_mjr4) +
    as.numeric(viol_mjr2_new > viol_mjr5) +
    as.numeric(viol_mjr2_new > viol_mjr5) +
    as.numeric(viol_mjr3 > viol_mjr4) +
    as.numeric(viol_mjr3 > viol_mjr4) +
    as.numeric(viol_mjr4 > viol_mjr5)
  ,issues2   = as.numeric(clms_flt1_new > clms_flt2) + 
    as.numeric(clms_flt1_new > clms_flt3) +
    as.numeric(clms_flt1_new > clms_flt4) +
    as.numeric(clms_flt1_new > clms_flt5) +
    as.numeric(clms_flt2 > clms_flt3) +
    as.numeric(clms_flt2 > clms_flt4) +
    as.numeric(clms_flt2 > clms_flt5) +
    as.numeric(clms_flt2 > clms_flt5) +
    as.numeric(clms_flt3 > clms_flt4) +
    as.numeric(clms_flt3 > clms_flt4) +
    as.numeric(clms_flt4 > clms_flt5)
  ,issues3   = as.numeric(viol_mnr1 > viol_mnr2) + 
    as.numeric(viol_mnr1 > viol_mnr3) +
    as.numeric(viol_mnr1 > viol_mnr4) +
    as.numeric(viol_mnr1 > viol_mnr5) +
    as.numeric(viol_mnr2 > viol_mnr3) +
    as.numeric(viol_mnr2 > viol_mnr4) +
    as.numeric(viol_mnr2 > viol_mnr5) +
    as.numeric(viol_mnr2 > viol_mnr5) +
    as.numeric(viol_mnr3 > viol_mnr4) +
    as.numeric(viol_mnr3 > viol_mnr4) +
    as.numeric(viol_mnr4 > viol_mnr5)
  ,issues4   = as.numeric(clms_naf1 > clms_naf2) + 
    as.numeric(clms_naf1 > clms_naf3) +
    as.numeric(clms_naf1 > clms_naf4) +
    as.numeric(clms_naf1 > clms_naf5) +
    as.numeric(clms_naf2 > clms_naf3) +
    as.numeric(clms_naf2 > clms_naf4) +
    as.numeric(clms_naf2 > clms_naf5) +
    as.numeric(clms_naf2 > clms_naf5) +
    as.numeric(clms_naf3 > clms_naf4) +
    as.numeric(clms_naf3 > clms_naf4) +
    as.numeric(clms_naf4 > clms_naf5)         
  ,total_issues = issues + issues2 + issues3 + issues4
  ,avg_maj_viol = case_when(yrs_licensed == 1 ~ viol_mjr1
                            ,yrs_licensed == 2 ~ viol_mjr2_new / 2
                            ,yrs_licensed == 3 ~ viol_mjr3 / 3
                            ,yrs_licensed == 4 ~ viol_mjr4 / 4
                            ,yrs_licensed >= 5 ~ viol_mjr5 / 5
                            ,TRUE ~ 0)
  ,avg_clms_flt = case_when(yrs_licensed == 1 ~ clms_flt1_new
                            ,yrs_licensed == 2 ~ clms_flt2 / 2
                            ,yrs_licensed == 3 ~ clms_flt3 / 3
                            ,yrs_licensed == 4 ~ clms_flt4 / 4
                            ,yrs_licensed >= 5 ~ clms_flt5 / 5
                            ,TRUE ~ 0)
  ,avg_viol_mnr = case_when(yrs_licensed == 1 ~ viol_mnr1
                            ,yrs_licensed == 2 ~ viol_mnr2 / 2
                            ,yrs_licensed == 3 ~ viol_mnr3 / 3
                            ,yrs_licensed == 4 ~ viol_mnr4 / 4
                            ,yrs_licensed >= 5 ~ viol_mnr5 / 5
                            ,TRUE ~ 0)
  ,avg_clms_naf = case_when(yrs_licensed == 1 ~ clms_naf1
                            ,yrs_licensed == 2 ~ clms_naf2 / 2
                            ,yrs_licensed == 3 ~ clms_naf3 / 3
                            ,yrs_licensed == 4 ~ clms_naf4 / 4
                            ,yrs_licensed >= 5 ~ clms_naf5 / 5
                            ,TRUE ~ 0)    
  ,avg_total    = avg_maj_viol + avg_clms_flt + avg_viol_mnr + avg_clms_naf
  ,avg_total    = coalesce(avg_total ,0)
  )
# group_by(total_issues) %>% 
# summarise(n          = n()
#           ,fraud_ind = sum(fraud_ind)
#           ,fraud_pct = fraud_ind / n) %>%
# ggplot(aes(x = total_issues ,y = fraud_pct)) +
#   geom_point() +
#   geom_line() +
#   geom_smooth(se = FALSE) +
#   theme_fivethirtyeight()
# ungroup() %>% 
# na.exclude() %>% 
# View()


#########################################################
##  Model Building
#########################################################
## Removing the unnecessary columns
remove_cols <- combined_data %>% 
  colnames() %>% 
  enframe() %>% 
  filter(str_detect(value ,pattern = "_new")) %>% 
  filter(!str_detect(value ,pattern = "num_cars_band")) %>% 
  mutate(remove_columns = str_remove_all(value ,pattern = "_new")) %>% 
  select(remove_columns) %>% 
  pull()

## Fixing all the NA's as well
## It would be much better to have the shadows ... but unfortunately
## that makes the matrix to big below (i.e. cannot allocate vector of x.x Gb error)
## Just going to move forward without them
model_data <- combined_data %>% 
  select(-contains("id") ,-all_of(remove_cols) ,-contains("clms")
         ,-matches("^clms") ,-matches("^viol") ,-matches("^indicator")
         ,-matches("^issues") ,-num_cars_new ,-limits_numeric ,-claim_notes ,-make ,-model) %>% 
  select(fraud_ind ,everything()) %>% 
  # bind_shadow(only_miss = TRUE) %>% ## Taking this out since many of the variables don't have any NA's at this point
  # add_label_shadow() %>% 
  impute_median_if(is.numeric) %>%
  impute_median_if(is.factor) %>%
  impute_median_if(is.Date)

## Going to create a scaled version of the data as this will impact the lasso regression penalty term
numeric_cols <- model_data %>% 
  select_if(is.numeric) %>% 
  select(-fraud_ind) %>% 
  colnames()

scaled_model_data <- model_data %>% 
  mutate_at(all_of(numeric_cols) ,function(x) {scale(x)[,1]})

## Setting up a training vs test dataset
set.seed(741995)

train <- sample(nrow(model_data) ,round(nrow(model_data) * 0.65 ,0))

## Transforming into a matrix
x <- model.matrix(fraud_ind ~ . ,scaled_model_data)[ ,-1]
y <- scaled_model_data$fraud_ind

## Going to try a lasso regression
cv_lasso <- cv.glmnet(x[train ,]
                      ,y[train]
                      ,alpha = 1 
                      ,family = binomial
)

## Outputting the model so can just read it in from here
saveRDS(cv_lasso ,file = "cv_lasso.RDS")
# cv_lasso <- readRDS("cv_lasso.RDS")

## Getting the best lambda value
best_lasso_lamba <- cv_lasso$lambda.min

## Getting predictions on the testing data
test_lasso_pred <- predict(cv_lasso ,s = best_lasso_lamba ,newx = x[-train ,] ,type = "response")

## Going to create a confusion matrix
model_data %>% 
  slice(-train) %>% 
  mutate(lasso_preds = as.numeric(test_lasso_pred[ ,"s1"] > 0.25) %>% factor(levels = c(1 ,0))
         ,fraud_ind  = fraud_ind %>% factor(levels = c(1 ,0))) %$% 
  table(fraud_ind ,lasso_preds) %>% 
  confusionMatrix()

## Sensitivity = 36% which is greater than the 25% requirement

## Want to see the coefficients for the lasso model
# lasso_model <- glmnet(x[train ,]
#                       ,y[train]
#                       ,alpha = 1 
#                       ,family = binomial
#                       ,lambda = best_lasso_lamba
#                       )
# 
# coef(lasso_model)[ ,"s0"] %>% 
#   enframe() %>% 
#   mutate(abs_value = abs(value)) %>% 
#   arrange(desc(abs_value)) %>% 
#   View()

## Converting the fraud_ind to a factor so that the randomForest runs a classification and not a regression
rf_model_data <- model_data %>% 
  mutate(fraud_ind = as.factor(fraud_ind)) 

## Setting seed for reproducibility
set.seed(741995)

## Random forest
## Still ned to work on the xtest & ytest section here
bag_rf <- randomForest(fraud_ind ~ .
                       ,data = rf_model_data[train ,]
                       # ,xtest = rf_model_data[-train ,-1]
                       # ,ytest = rf_model_data[-train ,]$fraud_ind
                       ,importance = TRUE
)

## Random forest predictions
test_rf_pred <- predict(bag_rf 
                        ,rf_model_data[-train ,]
)

length(test_rf_pred)
summary(bag_rf)

varImp(bag_rf)
varImpPlot(bag_rf)

rf_model_data %>% 
  slice(-train) %>% 
  mutate(rf_preds = as.numeric(test_rf_pred[ ,1] > 0.25) %>% factor(levels = c(1 ,0))) %$% 
  table(fraud_ind ,lasso_preds) %>% 
  confusionMatrix()






