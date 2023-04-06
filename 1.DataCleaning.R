###############################################################
#   Month: December                                           #
#   Year: 2022                                                #
#   Leading Indicator: Similarity Index                       #
#   R-File Type: ACS Data Extraction                          #
#   Author: Haseeb Bajwa                                      #
###############################################################
# Libraries
library(tidyverse)
library(tidycensus)
rm(list = ls())
options(scipen = 999)
# Setting working directory
setwd("~/Library/CloudStorage/OneDrive-EconomyLeagueofGreaterPhiladelphia/Documents/Leading Indicators")
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# getting tidy census data
#varlist <- load_variables(year = 2019, dataset = "acs1")
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
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
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ______________ IMPORTING COUNTY BUSINESS PATTERN DATA ____________________
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
cbp_dta <- read.csv("2022_Dec_1/Data/RawData/CBP2019.CB1900CBP-Data.csv")
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
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# merging ACS and CBP datasets
master_dta <- acs_dta %>% 
  inner_join(cbp_dta, by = c("GEOID", "County", "State"))
# cleaning string variables for state and county 
master_dta <- master_dta %>% 
  mutate(State = str_replace_all(State, " ", ""),
         State = toupper(State))
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# _________________________ IMPORTING CRIME DATA  __________________________
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Source: Mike Johnson Jr. (https://www.kaggle.com/datasets/mikejohnsonjr/united-states-crime-rates-by-county?resource=download)
crime_dta <- read.csv("2022_Dec_1/Data/RawData/crime_data_w_population_and_crime_rate.csv")
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
                                                                      ifelse(State == "WY", "Wyoming", State))))))))))))))))))))))))))))))))))))))))))))))))))    
crime_dta <- crime_dta %>% 
  mutate(State_new = ifelse(State == "DC", " District of Columbia", State_new))
# cleaning State variable 
crime_dta <- crime_dta %>% 
  select(County, State_new, crime_rate_per_100000) %>% 
  rename(State = State_new) %>% 
  mutate(State = str_replace_all(State, " ", ""),
         State = toupper(State))
# :::::::::::::::::::::::::::::::::::::::::::::
# merging with master dataset
master_dta <- master_dta %>% 
  inner_join(crime_dta, by = c("County", "State"))
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# _____________________ IMPORTING LIFE EXPECTANCY DATA  ____________________
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
life_dta <- read.csv("2022_Dec_1/Data/RawData/U.S._Life_Expectancy_at_Birth_by_State_and_Census_Tract_-_2010-2015.csv")
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
# merging with master dataset
master_dta <- master_dta %>% 
  inner_join(life_dta, by = c("County", "State"))
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# _____________________ IMPORTING COUNTY GDP DATA __________________________
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
library(readxl)
gdp_dta <- read_excel("2022_Dec_1/Data/RawData/lagdp1221.xlsx", sheet = "Sheet1")
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
# merging with master dataset
master_dta <- master_dta %>% 
  inner_join(gdp_dta, by = c("County", "State"))
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# __________________ CREATING SOCIOECONOMIC INDICATORS _____________________
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# creating separate dataset to calculate diversity index 
diversity_dta <- master_dta %>% 
  select(GEOID, County, State, white_population, black_population, asian_population, hispanic_population, 
         native_population, hawaiin_population, other_race_population) %>% 
  gather(ethnicity, population, white_population:other_race_population) %>% 
  mutate(population = ifelse(population %in% NA, 0, population))
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# calculating Simpsons's diversity index
# Step 1: calculate total population in each region
diversity_dta <- diversity_dta %>% 
  group_by(GEOID) %>% 
  mutate(total_population = sum(population))
# Step 2a: calculate n(n-1), where n is the number of people of each ethnicity 
diversity_dta <- diversity_dta %>% 
  group_by(GEOID, ethnicity) %>% 
  mutate(ethnic_richness = (population*(population-1)))
# Step 2b: calculating sum of n(n-1)
diversity_dta <- diversity_dta %>% 
  group_by(GEOID) %>% 
  mutate(sum_ethnic_richness = sum(ethnic_richness))
# Step 3: calculating sum of N(N-1), where N is the sum of all ethnicities calculated in Step 1 
diversity_dta <- diversity_dta %>% 
  group_by(GEOID) %>% 
  mutate(sqr_total_population = (total_population*(total_population-1)))
# Step 4: calculating Simpson's index => sum(n(n-1))/N(N-1)
diversity_dta <- diversity_dta %>% 
  group_by(GEOID) %>% 
  mutate(simpson_index = sum_ethnic_richness/sqr_total_population)
# Step 5: calculate diversity index => 1 - Simpson's index
diversity_dta <- diversity_dta %>% 
  group_by(GEOID) %>% 
  mutate(diversity_index = 1 - simpson_index)
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# keeping relevant variables only
diversity_dta <- diversity_dta %>% 
  distinct(diversity_index, .keep_all = TRUE) %>% 
  select(GEOID, County, State, simpson_index, diversity_index)
# merging diversity data with master dataset
master_dta <- master_dta %>% 
  left_join(diversity_dta, by = c("GEOID", "County", "State"))
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
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
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# importing state abbreviations data
states_dta <- read.csv("2022_Dec_1/Data/RawData/50+States.csv")
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
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# creating quartiles of population 
master_dta <- master_dta %>% 
  mutate(population_quartile = ntile(total_population, 4))
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ______________________ CREATING SIMILARITY INDEX  ________________________
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Method 1. Using Euclidean Distance along N = 29 dimensions 
# Set of variables used to create Similarity Index 
# total_population, diversity_index, male_prop, median_household_income, laborforce_participation, 
# poverty_rate, industry_makeup, crime_rate_1000, life_expectancy, gdp_per_capita, enroll_rate, gini_coeff  
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# scaling variables 
similarity_dta <- master_dta %>% 
  mutate(total_population = total_population/total_population[which.max(total_population)],
         median_household_income = median_household_income/median_household_income[which.max(median_household_income)],
         crime_rate_per_100000 = crime_rate_per_100000/crime_rate_per_100000[which.max(crime_rate_per_100000)],
         life_expectancy = life_expectancy/life_expectancy[which.max(life_expectancy)],
         gdp_per_capita = gdp_per_capita/gdp_per_capita[which.max(gdp_per_capita)]
         )
# subtracting values for Philadelphia from the other counties
similarity_dta <- similarity_dta %>% 
  mutate(total_population = total_population-total_population[which(County == "Philadelphia, PA")],
         diversity_index = diversity_index-diversity_index[which(County == "Philadelphia, PA")],
         male_prop = male_prop-male_prop[which(County == "Philadelphia, PA")],
         median_household_income = median_household_income-median_household_income[which(County == "Philadelphia, PA")],
         laborforce_participation = laborforce_participation-laborforce_participation[which(County == "Philadelphia, PA")],
         poverty_rate = poverty_rate-poverty_rate[which(County == "Philadelphia, PA")],
         crime_rate_per_100000 = crime_rate_per_100000-crime_rate_per_100000[which(County == "Philadelphia, PA")],
         life_expectancy = life_expectancy-life_expectancy[which(County == "Philadelphia, PA")],
         gdp_per_capita = gdp_per_capita-gdp_per_capita[which(County == "Philadelphia, PA")],
         enroll_rate = enroll_rate-enroll_rate[which(County == "Philadelphia, PA")],
         gini_coeff = gini_coeff-gini_coeff[which(County == "Philadelphia, PA")],
         
         `Accommodation and food services` = `Accommodation and food services`-`Accommodation and food services`[which(County == "Philadelphia, PA")],
         `Administrative and support and waste management and remediation services` = `Administrative and support and waste management and remediation services`-`Administrative and support and waste management and remediation services`[which(County == "Philadelphia, PA")],
         `Arts, entertainment, and recreation` = `Arts, entertainment, and recreation`-`Arts, entertainment, and recreation`[which(County == "Philadelphia, PA")],
          Construction = Construction-Construction[which(County == "Philadelphia, PA")],
         `Educational services` = `Educational services`-`Educational services`[which(County == "Philadelphia, PA")],
         `Finance and insurance` = `Finance and insurance`-`Finance and insurance`[which(County == "Philadelphia, PA")],
         `Health care and social assistance` = `Health care and social assistance`-`Health care and social assistance`[which(County == "Philadelphia, PA")],
          Information = Information-Information[which(County == "Philadelphia, PA")],
         `Management of companies and enterprises` = `Management of companies and enterprises`-`Management of companies and enterprises`[which(County == "Philadelphia, PA")],
          Manufacturing = Manufacturing-Manufacturing[which(County == "Philadelphia, PA")],
         `Mining, quarrying, and oil and gas extraction` = `Mining, quarrying, and oil and gas extraction`-`Mining, quarrying, and oil and gas extraction`[which(County == "Philadelphia, PA")],
         `Other services (except public administration)` = `Other services (except public administration)`-`Other services (except public administration)`[which(County == "Philadelphia, PA")],
         `Professional, scientific, and technical services` = `Professional, scientific, and technical services`-`Professional, scientific, and technical services`[which(County == "Philadelphia, PA")],
         `Real estate and rental and leasing` = `Real estate and rental and leasing`-`Real estate and rental and leasing`[which(County == "Philadelphia, PA")],
         `Retail trade` = `Retail trade`-`Retail trade`[which(County == "Philadelphia, PA")],
         `Transportation and warehousing` = `Transportation and warehousing`-`Transportation and warehousing`[which(County == "Philadelphia, PA")],
          Utilities = Utilities-Utilities[which(County == "Philadelphia, PA")],
         `Wholesale trade` = `Wholesale trade`-`Wholesale trade`[which(County == "Philadelphia, PA")]
         )
# creating similarity index
# Step 1. Calculating Squared Sum
similarity_dta <- similarity_dta %>% 
  mutate(sqrd_sum = ( ((total_population)^2) + ((diversity_index)^2) + ((male_prop)^2) + ((median_household_income)^2) +
  ((laborforce_participation)^2) + ((poverty_rate)^2)  + ((crime_rate_per_100000)^2) + ((life_expectancy)^2) +
  ((gdp_per_capita)^2) + ((enroll_rate)^2) + ((gini_coeff)^2) + ((`Accommodation and food services`)^2) + 
  ((`Administrative and support and waste management and remediation services`)^2) +
  ((`Arts, entertainment, and recreation`)^2) + ((Construction)^2) + ((`Educational services`)^2) +
  ((`Finance and insurance`)^2) + ((`Health care and social assistance`)^2) + ((Information)^2) +
  ((`Management of companies and enterprises`)^2) + ((Manufacturing)^2) + ((`Mining, quarrying, and oil and gas extraction`)^2) +
  ((`Other services (except public administration)`)^2) + ((`Professional, scientific, and technical services`)^2) +
  ((`Real estate and rental and leasing`)^2) + ((`Retail trade`)^2) + ((`Transportation and warehousing`)^2) +
  ((Utilities)^2) + ((`Wholesale trade`)^2)) )
# Step 2. Calculating Root Mean Square Estimate
similarity_dta <- similarity_dta %>% 
  mutate( RMSE = sqrt((sqrd_sum/29)) ) # where N=29 is the number of dimensions
# Step 3. Calculating Similarity Index 
similarity_dta <- similarity_dta %>% 
  mutate( similarity_index = 1 - RMSE )
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#________________ DESCRIPTIVE FIGURES _______________________#
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
dev.off()
# Layout to split the screen
layout(mat = matrix(c(1,2),2,1, byrow=TRUE),  height = c(1,8))
# Draw the boxplot  
par(bg="#1b1464", family = "Georgia", mar=c(0, 1.1, 1.1, 2.1))
boxplot(similarity_dta$similarity_index, horizontal=TRUE , ylim=c(0.7,1), xaxt="n", frame=F,
        color = "#F79B2E", fill = "#F79B2E", outcol = "#F79B2E", border = "#F79B2E", col.main = "#ffffff", main = "")
# Draw the histogram 
par(bg="#1b1464", family = "Georgia", mar=c(4, 4.5, 3.1, 3.1))
hist(similarity_dta$similarity_index , border=F, xlab="Distribution of Similarity Index",
     col = "#ec008c", 
     col.lab = "#ffffff",
     col.axis = "#ffffff",
     col.main = "#ffffff",
     main = "")
axis(1,col="#ffffff")
axis(1,col.axis="#ffffff")
axis(2,col="#ffffff")
axis(2,col.axis="#ffffff")
# MAKING AN ORDERED BARPLOT OF CLOSEST COUNTIES TO PHILLY
similarity_top20 <- similarity_dta %>% 
  select(GEOID, County, State, Region, RMSE, similarity_index) %>% 
  arrange(RMSE) %>% 
  mutate(rank = row_number()) %>% 
  filter(rank <= 20)
# making bar plot of top 20 closest counties
dev.off()
ggplot(similarity_top20, aes(x = RMSE, y=reorder(County, -RMSE), fill = Region)) + 
  geom_col(width = 0.6) + 
  coord_cartesian(xlim = c(0.04, 0.07)) +
  labs(x = "Distance from Philadelphia", y = "") +
  theme(
    axis.text = element_text(color = "#ffffff", family = "Georgia"),
    axis.title.x = element_text(colour = "#ffffff"),
    legend.background = element_rect(fill = "#1b1464"),
    legend.text = element_text(color = "#ffffff"),
    legend.title = element_text(colour = "#ffffff"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#1b1464",
                                  colour = "#1b1464"),
    plot.background = element_rect(fill = "#1b1464", colour = "#1b1464")
  )
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
dev.off()
# boxplot + beeswarm of similarity index by Region
par(bg="#1b1464", family = "Georgia")
boxplot(similarity_index ~ Region, data = similarity_dta,
        border="#ffffff",
        col.lab = "#ffffff",
        col.axis = "#ffffff",
        col.main = "#ffffff",
        axes = FALSE, 
        main = "Distribution of Similarity Index by Region",
        xlab = "Region",
        ylab = "Similarity Index")
beeswarm(similarity_index ~ Region, data = similarity_dta,
         col = c("#3FA0FF", "#F79B2E", "#00ffc5", "#ec008c"),
         corral = "omit",
         add = TRUE)
axis(1,col="#ffffff")
axis(1, at=1:4, labels=c("Midwest", "Northeast", "South", "West"), col.axis="#ffffff")
axis(2,col="#ffffff")
axis(2,col.axis="#ffffff")
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# boxplot + beeswarm of similarity index by population quartile
boxplot(similarity_index ~ population_quartile, data = similarity_dta,
        border="#ffffff",
        col.lab = "#ffffff",
        col.axis = "#ffffff",
        col.main = "#ffffff",
        axes = FALSE, 
        main = "Distribution of Similarity Index by Population",
        xlab = "Population Quartile",
        ylab = "Similarity Index")
beeswarm(similarity_index ~ population_quartile, data = similarity_dta,
         col = c("#3FA0FF", "#F79B2E", "#00ffc5", "#ec008c"),
         corral = "omit",
         add = TRUE)
axis(1,col="#ffffff")
axis(1,col.axis="#ffffff")
axis(2,col="#ffffff")
axis(2,col.axis="#ffffff")
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# saving master dataset 
write.csv(master_dta, "2022_Dec_1/Data/CleanData/master_data.csv")
# saving similarity index dataset 
write.csv(similarity_dta, "2022_Dec_1/Data/CleanData/similarity_data.csv")
temp_dta <- similarity_dta %>% 
  select(GEOID, County, State, similarity_index)

#_____________________END OF SCRIPT_____________________________#




