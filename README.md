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

## 3. Future Ideas

- Want to contrsuct a GIS map of how counties that look similar lie across the country
- Make the Similarity Index more robust by incorporating regression models with fixed levels 
- I could model how different counties that look similar have increased their gdp, for example, 
over time after controlling for fixed effects. 

## 4. Data Analysis

The following files clean and analyze the raw datasets extracted from
the data sources to create the **Similarity Index**.

- `1.data_clean.R` cleans the raw datasets from the various datasets and
  merges them together in a `master dataset`.
- `2.diversity_index.R` creates a [Simpson’s Diversity
  Index](https://www.statology.org/simpsons-diversity-index/) at the
  county level to create an ethnic heterogeneity map.
- `3.similarity_index.R` assimilates the master dataset and the
  diversity index to construct the Similarity Index using the Euclidean
  distance where the reference point for similarity is the county of
  Philadelphia, PA.
  
