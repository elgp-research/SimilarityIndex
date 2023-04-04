## ----sourcing R file for clean data -------------------------------------------------------------------------------
source("1.data_clean.R")

## ----Simpson's diversity index-------------------------------------------------------------------------------------
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
