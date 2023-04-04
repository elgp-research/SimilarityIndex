Similarity Index
================

## 1. Overview

The Similarity Index is a multi-dimensional index used to benchmark and
compare U.S. cities across various metrics that can allow for a deeper
and more robust comparison. Comparing cities and municipalities across
the U.S. is quite complex, considering historic contexts, socioeconomic
issues, and vastly different population sizes. A multidimensional
benchmarking index allows for a better comparison since it smooths over
these differences and produces a proportional score for different areas
\[1\]. For example, while the crime rate is an important indicator of
social well-being in a region, it is challenging to compare crime rates
across different municipalities because of the variety of crimes and
areas of higher population reporting statistically higher types of
crimes committed. A multidimensional index can account for these
differences by both normalizing and equalizing socioeconomic indicators
leading to fairer regional comparisons. This can potentially aid
policymakers and practitioners to identify new solutions implemented in
geographic areas that resemble Philadelphia across a comprehensive list
of socioeconomic indicators and are facing similar policy issues.

## 2. Data Sources

We used the most recent data available from the following data sources
to construct the similarity index at the county level:

- American Community Survey (ACS) used to extract demographics
  information
- County Business Patterns (CBP) used to collect information on economic
  sectors
- Kaggle (US Crime Data by County)
- Life Expectancy Data
- Bureau of Labor Statistics (BLS) used to collect GDP data

## 3. Data Analysis

### 3.1 American Community Survey

We used the following code to look up relevant variables from the ACS
using the tidycensus package

``` r
# varlist <- load_variables(year = 2019, dataset = "acs1")
```

``` r
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
```

``` r
# getting data from ACS 5-YEAR survey
acs_dta <- get_acs(
  geography = "county",
  variables = my_vars,
  year = 2019,
  survey = "acs5"
) %>% 
  select(-moe)
```

    ## Getting data from the 2015-2019 5-year ACS

``` r
# reshaping data for easier merging later on
acs_dta <- acs_dta %>% 
  spread(variable, estimate) %>% 
  separate(NAME, c("County", "State"), ",") # string cleaning for merging 
```

### 3.2 County Business Patterns

``` r
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
```
