## ----sourcing R file for clean data -------------------------------------------------------------------------------
source("3.similarity_index.R")

## ----dashboard indicators--------------------------------------------------------------------------------------------------

dashboard_indicators <- master_dta %>% 
  select(GEOID, County, State, total_population, diversity_index, male_prop,
         median_household_income, laborforce_participation, poverty_rate,
         crime_rate_per_100000, life_expectancy, gdp_per_capita, 
         enroll_rate, gini_coeff)

# changing to long format for dashboard indicator selection
dashboard_indicators <- dashboard_indicators %>% 
  gather(indicator, value, total_population:gini_coeff)

# changing labels of economic indicators 
dashboard_indicators <- dashboard_indicators %>% 
  mutate(indicator = recode(indicator, 
                             total_population = "Total Population",
                             diversity_index = "Diversity Index",
                             male_prop = "Male Proportion",
                             median_household_income = "Median Household Income",
                             laborforce_participation = "Laborforce Participation",
                             poverty_rate = "Poverty Rate",
                             crime_rate_per_100000 = "Crimes per 100,000 residents",
                             life_expectancy = "Life Expectancy",
                             gdp_per_capita = "GDP per capita",
                             enroll_rate = "Enrollment Rate",
                             gini_coeff = "Gini Coefficient"))

# saving indicators dataset
write.csv(dashboard_indicators, "db_indicators.csv")

## ----dashboard sectors ----------------------------------------------------------------------------------------------------

dashboard_sectors <- master_dta %>% 
  select(GEOID, County, State, `Accommodation and food services`,
         `Administrative and support and waste management and remediation services`,
         `Arts, entertainment, and recreation`, Construction, `Educational services`,
         `Finance and insurance`, `Health care and social assistance`, 
         Information, `Management of companies and enterprises`,
         Manufacturing, `Mining, quarrying, and oil and gas extraction`,
         `Other services (except public administration)`, 
         `Professional, scientific, and technical services`,
         `Real estate and rental and leasing`, `Retail trade`,
         `Transportation and warehousing`, Utilities, `Wholesale trade`
  )

# changing to long format for dashboard sector map
dashboard_sectors <- dashboard_sectors %>% 
  gather(sector, size, `Accommodation and food services`:`Wholesale trade`)

# changing labels of sectors 
dashboard_sectors <- dashboard_sectors %>% 
  mutate(sector = recode(sector, "Administrative and support and waste management and remediation services" = "Administrative and Waste Management"))

# saving sector dataset
write.csv(dashboard_sectors, "db_sectors.csv")
