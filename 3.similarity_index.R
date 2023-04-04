## ----sourcing R file ----------------------------------------------------------------------------------------------
source("2.diversity_index.R")

## ----similarity_index_description ---------------------------------------------------------------------------------

  # Method 1. Using Euclidean Distance along N = 29 dimensions 
  # Set of variables used to create Similarity Index 
  # total_population, diversity_index, male_prop, median_household_income, laborforce_participation, 
  # poverty_rate, industry_makeup, crime_rate_1000, life_expectancy, gdp_per_capita, enroll_rate, gini_coeff 

## ----scaling variables --------------------------------------------------------------------------------------------
similarity_dta <- master_dta %>% 
  mutate(total_population = total_population/total_population[which.max(total_population)],
         median_household_income = median_household_income/median_household_income[which.max(median_household_income)],
         crime_rate_per_100000 = crime_rate_per_100000/crime_rate_per_100000[which.max(crime_rate_per_100000)],
         life_expectancy = life_expectancy/life_expectancy[which.max(life_expectancy)],
         gdp_per_capita = gdp_per_capita/gdp_per_capita[which.max(gdp_per_capita)]
  )
## ----subtracting philly scores -------------------------------------------------------------------------------------
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

## ----calculating similarity index using Euclidean distance -------------------------------------------------------------

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
