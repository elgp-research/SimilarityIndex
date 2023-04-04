## ----libraries, include=FALSE--------------------------------------------------------------------------------------
library(tidyverse)
library(tidycensus)
library(readxl)
options(scipen = 999)

## ----acs_lookup----------------------------------------------------------------------------------------------------
# varlist <- load_variables(year = 2019, dataset = "acs1")


## ----acs_variables-------------------------------------------------------------------------------------------------
# creating variable list for acs
my_vars <- c(
  total_population = "B01001_001",
  male_population = "B01001_002",
  female_population = "B01001_026",
  
  white_population = "B01001A_001",
  black_population = "B01001B_001",
  native_population = "B01001C_001",
  asian_population = "B01001D_001",
  hawaiin_population = "B01001E_001",
  other_race_population = "B01001F_001",
  two_race_population = "B01001G_001",
  hispanic_population = "B01001I_001",
  
  median_household_income = "B19013_001",
  median_family_income = "B19113_001",
  
  total_laborforce = "B23025_001",
  total_in_laborforce = "B23025_002",
  civilian_laborforce = "B23025_003",
  employed_civilian_labor = "B23025_004",
  unemployed_civilian_labor = "B23025_005",
  
  enrolled_school = "B14001_002",
  unenrolled_school = "B14001_010",
  enrolled_bachelor = "B14001_008",
  enrolled_graduate = "B14001_009",
  
  poverty_pop = "B17001_002",
  
  gini_coeff = "B19083_001"
)


## ----acs_clean-----------------------------------------------------------------------------------------------------
# getting data from ACS 5-YEAR survey
acs_dta <- get_acs(
  geography = "county",
  variables = my_vars,
  year = 2019,
  survey = "acs5"
) %>% 
  select(-moe)
# reshaping data for easier merging later on
acs_dta <- acs_dta %>% 
  spread(variable, estimate) %>% 
  separate(NAME, c("County", "State"), ",") # string cleaning for merging 


## ----cbp_import, include=FALSE-------------------------------------------------------------------------------------
cbp_dta <- read_csv(gzfile("Data/CBP2019.CB1900CBP-Data.csv.gz"))


## ----cbp_clean-----------------------------------------------------------------------------------------------------
# removing first row from dataset
cbp_dta <- cbp_dta[-1,]

# calculating sector size 
cbp_dta <- cbp_dta %>% 
  filter(EMPSZES_LABEL == "All establishments") %>%
  group_by(NAME) %>% 
  mutate(#estab_size = as.numeric(ESTAB)/as.numeric(ESTAB[1]), # sector size by number of establishments
         employee_size = as.numeric(EMP)/as.numeric(EMP[1])) %>% # sector size by number of employees
  select(GEO_ID, NAME, NAICS2017_LABEL, employee_size)

# cleaning GEOID for merging with ACS dataset
cbp_dta <- cbp_dta %>% 
  mutate(GEO_ID = str_replace_all(GEO_ID, "0500000US", "")) %>% 
  rename(GEOID= GEO_ID) %>% 
  separate(NAME, c("County", "State"), ",") # string cleaning for merging 

# reshaping data to wide for easier merging later on 
cbp_dta <- cbp_dta %>% 
  spread(NAICS2017_LABEL, employee_size)

# replacing NAs with zeros
cbp_dta[is.na(cbp_dta)] <- 0


## ----crime_clean---------------------------------------------------------------------------------------------------
crime_dta <- read.csv("Data/crime_data_w_population_and_crime_rate.csv")
# string cleaning county name variable
crime_dta <- crime_dta %>% 
  separate(county_name, c("County", "State"), ",") 
# only keeping crime rate statistics
crime_dta <- crime_dta %>% 
  select(County, State, crime_rate_per_100000) 
# cleaning state names for merging with master dataset
crime_dta <- crime_dta %>% 
  mutate(State = str_replace_all(State, " ", ""),
         State = toupper(State)) 
#::::::::::::::::::::::::::::::::::::
crime_dta <- crime_dta %>% 
  mutate(State_new = ifelse(State == "AL", "Alabama", 
                            ifelse(State == "AK", "Alaska",
                               ifelse(State == "AR", "Arkansas",
                                      ifelse(State == "CA", "California",
                                             ifelse(State == "CO", "Colorado",
                                                    ifelse(State == "CT", "Connecticut",
                                                           ifelse(State == "DE", "Delaware",
                                                                  ifelse(State == "FL", "Florida",
                                                                         ifelse(State == "GA", "Georgia",
                                                                                ifelse(State == "HI", "Hawaii",
                                                                                       ifelse(State == "ID", "Idaho",
                                                                                              ifelse(State == "IL", "Illinois",
                                                                                                     ifelse(State == "IN", "Indiana",
                                                                                                            ifelse(State == "IA", "Iowa",
                                 ifelse(State == "KS", "Kansas",
                                        ifelse(State == "KY", "Kentucky",
                                               ifelse(State == "LA", "Louisiana",
                                                      ifelse(State == "ME", "Maine",
                                                             ifelse(State == "MD", "Maryland",
                                                                    ifelse(State == "MA", "Massachusetts",
                                                                           ifelse(State == "MI", "Michigan",
                                                                                  ifelse(State == "MN", "Minnesota",
                                                                                         ifelse(State == "MS", "Mississippi",
                           ifelse(State == "MO", "Missouri",
                                  ifelse(State == "MT", "Montana",
                                         ifelse(State == "NE", "Nebraska",
                                                ifelse(State == "NV", "Nevada",
                                                       ifelse(State == "NH", "New Hampshire",
                                                              ifelse(State == "NJ", "New Jersey",
                                                                     ifelse(State == "NM", "New Mexico",
                                                                            ifelse(State == "NY", "New York",
                                                                                   ifelse(State == "NC", "North Carolina",
                           ifelse(State == "ND", "North Dakota",
                                  ifelse(State == "OH", "Ohio",
                                         ifelse(State == "OK", "Oklahoma",
                                                ifelse(State == "OR", "Oregon",
                                                       ifelse(State == "PA", "Pennsylvania",
                                                              ifelse(State == "RI", "Rhode Island",
                                                                     ifelse(State == "SC", "South Carolina",
                                                                            if_else(State == "SD", "South Dakota",
                                                                                    ifelse(State == "TN", "Tennessee",
                                                                                           ifelse(State == "TX", "Texas",
                            ifelse(State == "UT", "Utah",
                                   ifelse(State == "VT", "Vermont",
                                          ifelse(State  == "VA", "Virginia",
                                                 ifelse(State == "WA", "Washington",
                                                        ifelse(State == "WV", "West Virginia",
                                                               ifelse(State == "WI", "Wisconsin",
                                                                      ifelse(State == "WY", "Wyoming", State ))))))))))))))))))))))))))))))))))))))))))))))))))    
crime_dta <- crime_dta %>% 
  mutate(State_new = ifelse(State == "DC", " District of Columbia", State_new))
# cleaning State variable 
crime_dta <- crime_dta %>% 
  select(County, State_new, crime_rate_per_100000) %>% 
  rename(State = State_new) %>% 
  mutate(State = str_replace_all(State, " ", ""),
         State = toupper(State))


## ----life_exp_clean------------------------------------------------------------------------------------------------
life_dta <- read.csv(gzfile("Data/Life_Expectancy_CensusTracts_2015_2019.csv.gz"))

# calculating life expectancy by county => data is available at census tracts
life_dta <- life_dta %>% 
  group_by(State, County) %>% 
  mutate(life_expectancy = mean(Life.Expectancy, na.rm = TRUE)) %>% 
  distinct(life_expectancy)

# cleaning state and county variables for merging
life_dta <- life_dta %>% 
  separate(County, c("County_Name", "State_Abr"), ",") %>% # string cleaning for merging
  mutate(State = str_replace_all(State, " ", ""),
         State = toupper(State)) %>%
  rename(County = County_Name) %>% 
  select(County, State, life_expectancy) %>% 
  filter(!is.na(life_expectancy))


## ----gdp_clean-----------------------------------------------------------------------------------------------------
gdp_dta <- read_excel("Data/lagdp1221.xlsx", sheet = "Sheet1")
gdp_dta <- gdp_dta %>% 
  filter(!is.na(County)) %>%
  filter(!is.na(`2017`)) %>% 
  filter(`2017` != "(NA)") %>% 
  filter(State!=County) %>% 
  filter(`2017` != "253726") # REMOVING DUPLICATE VALUE FOR RICHMOND, VA
# storing 2019 gdp variable  
gdp_dta <- gdp_dta %>% 
  mutate(gdp_2019 = as.numeric(`2019`)) 
# cleaning string variables of county and state
gdp_dta <- gdp_dta %>% 
  mutate(label = "County",
         County = paste(County, label, sep = " "),
         State = str_replace_all(State, " ", ""),
         State = toupper(State)) %>% 
  select(County, State, gdp_2019)
# changing label of New Orleans Parish 
gdp_dta <- gdp_dta %>% 
  mutate(County = ifelse(County == "Orleans County", "Orleans Parish", County))

## ----master_merge--------------------------------------------------------------------------------------------------
# merging ACS and CBP datasets
master_dta <- acs_dta %>% 
  inner_join(cbp_dta, by = c("GEOID", "County", "State"))

# cleaning string variables for state and county 
master_dta <- master_dta %>% 
  mutate(State = str_replace_all(State, " ", ""),
         State = toupper(State))

# merging updated master data with crime data 
master_dta <- master_dta %>% 
  inner_join(crime_dta, by = c("County", "State"))

# merging updated master data with life expectancy data
master_dta <- master_dta %>% 
  inner_join(life_dta, by = c("County", "State"))

# merging updated master data with county gdp data 
master_dta <- master_dta %>% 
  inner_join(gdp_dta, by = c("County", "State"))


## ----master_clean--------------------------------------------------------------------------------------------------
# importing state abbreviations data
states_dta <- read.csv("Data/50+States.csv")
states_dta <- states_dta %>% 
  mutate(State = str_replace_all(State, " ", ""),
         State = toupper(State))

# merging datasets together 
master_dta <- master_dta %>% 
  inner_join(states_dta, by = c("State"))

# cleaning county variable string
master_dta <- master_dta %>% 
  mutate(County = str_replace_all(County, "County", ""),
         County = str_replace_all(County, " ", ""),
         County = paste(County, Abbr, sep = ", "))


# calculating additional economic variables
master_dta <- master_dta %>% 
  mutate(poverty_rate = poverty_pop/total_population,
         enroll_rate = enrolled_school/(enrolled_school+unenrolled_school),
         male_prop = male_population/total_population,
         female_prop = female_population/total_population, 
         laborforce_participation = civilian_laborforce/total_laborforce,
         highered_enroll = enrolled_bachelor+enrolled_graduate,
         gdp_per_capita = gdp_2019/total_population
  )

# creating population quartiles
master_dta <- master_dta %>% 
  mutate(population_quartile = ntile(total_population, 4))
