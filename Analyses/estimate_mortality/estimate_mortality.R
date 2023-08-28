#-------------------------------------------------------------------------------
# INFO FOR CODE IN THIS FILE
#-------------------------------------------------------------------------------

# Author: Trey Mace (marvin.mace@maryland.gov)
# Date Created: 03 November 2022

# This code is for estimating mortality from age (and in some cases, number of
# spawning) data for the 2023 benchmark river herring stock assessment done
# through the ASMFC. Specifically, this code is used to estimate mortality
# using a few different methods.

# The input data file clean_data.csv is created in the file
# Analyses/clean_data/clean_data.R

# The code is in two sections:
#
# A) Code used to estimate mortality based on the decisions of the SAS
#
# B) Extra code for estimating mortality using other methods considered by
#    the SAS but not ultimately chosen.



# Comments:

# 14 July 2023
#   a) Modified code from using loops to using dplyr style coding

#   b) Estimates from this date will be first since I corrected the errors I found
#      in the clean_data.R file to make sure my data lined up with data that Katie
#      Drew created. See check_data.R for comparisons between data sets.

#   c) The SAS decided to use the age of full selection as the age of full maturity,
#      or very close to full maturity (like 99% ish). Almost (All?) regions have an
#      age of full maturity of 5 years. Need to confirm with SAS to use this age

#   d) The SAS decided to go with the Poisson GLM method to estimate Z so I have
#      code that produces just those results. However, I also wanted to have
#      estimates from some of the other methods still available if questions come
#      up later about estimates from those methods.

#-------------------------------------------------------------------------------
# Load Packages, set options
#-------------------------------------------------------------------------------

library(tidyverse)
library(glmmTMB) # Poisson GLMM

#-------------------------------------------------------------------------------
# Load data
#-------------------------------------------------------------------------------

data <- read.csv("clean_data.csv",
                 header = TRUE) %>%
        mutate(Date = as.POSIXct(Date, format="%Y-%m-%d")) %>%
        mutate(Year = as.integer(format(Date, format= "%Y")))

# number of records with an age estimate (should be 243041)
data %>%
  filter(!is.na(AgeScale) | !is.na(AgeOtolith)) %>%
  summarize(n = n())

# Some criteria for decisions that have to be made for some of the mortality
# estimates. These were decided on by the life history work group and came
# from the shad assessment.

# 1.	A minimum of three age classes greater than or equal to age of full
#     recruitment (see #2 below) and at least 30 individuals across these age
#     classes had to be observed to in a data set for it to be included in the analysis.
#
# 2.	14 July 2023 - age of full recruitment now based on age of maturity (see above comments)

#     Age of full recruitment was set based on the recommendations of simulation work
#     for each method separately based on the peak age observed in the data when
#     pooled across years
#             a.	Chapman-Robson: Peak age + 1
#             b.	Linear regression: Peak age
#             c.	GLMM: Peak age + 1
#
#-------------------------------------------------------------------------------


# flag to separate data (estimates) by sex or not - 1 (yes) or not 1 (no)

sep_by_sex <- 0

if(sep_by_sex == 1)
{
  grouping_variables <- c("Region", "State", "Year", "Location", "Species", "Sex", "AgeMethod", "Age")
} else
{
  grouping_variables <- c("Region", "State", "Year", "Location", "Species", "AgeMethod", "Age")
}

# Total number of age records in below data will be a little bigger (802) b/c
# some fish get counted twice if they have both age and otolith estimate
age_grouped <-
  data %>%
  pivot_longer(cols = c(AgeScale, AgeOtolith),
               names_to = "AgeMethod",
               values_to = "Age",
               values_drop_na = TRUE) %>%
  filter(Age < 20) %>% # get rid of age 99
  group_by(across(all_of(grouping_variables))) %>%
  summarize(n_individuals = n()) %>%
  mutate(ID = cur_group_id()) %>%
  ungroup()

#-------------------------------------------------------------------------------
# Estimate mortality using the poisson glm method and with criteria given above
#
# Out of potential methods, this is the method chosen by the SAS for getting
# mortality estimates.
#
# First get mortality estimates by river and then by region
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# SECTION A




# subset data based on criteria of at least 3 ages present and 30 individuals
# across those ages. Also we are now using age of full recruitment of 5 years.
# Get mortality estimates

if(sep_by_sex == 1)
{
  river_grouping_variables_a <- c("Region", "State", "Year", "Location", "Species", "Sex", "AgeMethod")
} else
  {
    river_grouping_variables_a <- c("Region", "State", "Year", "Location", "Species", "AgeMethod")
  }

age_grouped_river_data <-
  age_grouped %>%
  filter(Age > 4) %>%
  group_by(across(all_of(river_grouping_variables_a))) %>%
  mutate(n_ages = n()) %>%
  mutate(total_fish = sum(n_individuals)) %>%
  ungroup() %>%
  filter(n_ages > 2) %>%
  filter(total_fish > 29)

# Clean up format of estimates from above to format for output
estimates_by_river <-
  age_grouped_river_data %>%
  group_by(across(all_of(river_grouping_variables_a))) %>%
  group_modify(~ broom::tidy(glm(n_individuals ~ Age, family = "poisson", data = .x))) %>%
  mutate(Zmethod = "z_glm") %>%
  filter(term == "Age") %>%
  rename(Z = "estimate",
         Zse = "std.error") %>%
  mutate(Z = if_else(Z < 0, Z*(-1), -9999)) %>%
  select(c(all_of(river_grouping_variables_a), Zmethod, Z, Zse))

# write out estimates to .csv file

if(sep_by_sex == 1)
{
  file_name_river <- "Zestimates_by_River_by_sex.csv"
} else
{
  file_name_river <- "Zestimates_by_River.csv"
}

write.csv(estimates_by_river,
          file = file_name_river,
          row.names = FALSE)




# Get mortality estimates by region instead of by river

if(sep_by_sex == 1)
{
  region_grouping_variables_a <- c("Region", "Year", "Species", "Sex", "AgeMethod", "Age")
} else
{
  region_grouping_variables_a <- c("Region", "Year", "Species", "AgeMethod", "Age")
}


age_grouped_region <-
  data %>%
  pivot_longer(cols = c(AgeScale, AgeOtolith),
               names_to = "AgeMethod",
               values_to = "Age",
               values_drop_na = TRUE) %>%
  group_by(across(all_of(region_grouping_variables_a))) %>%
  summarize(n_individuals = n()) %>%
  mutate(ID = cur_group_id()) %>%
  ungroup()


if(sep_by_sex == 1)
{
  region_grouping_variables_b <- c("Region", "Year", "Species", "Sex", "AgeMethod")
} else
{
  region_grouping_variables_b <- c("Region", "Year", "Species", "AgeMethod")
}


age_grouped_region_data <-
  age_grouped_region %>%
  filter(Age > 4) %>%
  group_by(across(all_of(region_grouping_variables_b))) %>%
  mutate(n_ages = n()) %>%
  mutate(total_fish = sum(n_individuals)) %>%
  ungroup() %>%
  filter(n_ages > 2) %>%
  filter(total_fish > 29)

estimates_by_region <-
  age_grouped_region_data %>%
  group_by(across(all_of(region_grouping_variables_b))) %>%
  group_modify(~ broom::tidy(glm(n_individuals ~ Age, family = "poisson", data = .x))) %>%
  mutate(Zmethod = "z_glm") %>%
  filter(term == "Age") %>%
  rename(Z = "estimate",
         Zse = "std.error") %>%
  mutate(Z = if_else(Z < 0, Z*(-1), -9999)) %>%
  #select(Region, Year, Species, Sex, AgeMethod, Zmethod, Z, Zse)
  select(c(all_of(region_grouping_variables_b), Zmethod, Z, Zse))

if(sep_by_sex == 1)
{
  file_name_region <- "Zestimates_by_Region_by_sex.csv"
} else
{
  file_name_region <- "Zestimates_by_Region.csv"
}

write.csv(estimates_by_region,
          file = file_name_region,
          row.names = FALSE)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# SECTION B

# Look at data for region estimates

region_data_grouped <-
  age_grouped_region %>%
  filter(Age > 4) %>%
  group_by(across(all_of(region_grouping_variables_b))) %>%
  mutate(n_ages = n()) %>%
  mutate(total_fish = sum(n_individuals)) %>%
  ungroup() %>%
  filter(n_ages > 2) %>%
  filter(total_fish > 29) %>%
  arrange(Region, Species, Sex, AgeMethod, Year)


#-------------------------------------------------------------------------------
# Code for estimating mortality using Chapman Robson method with age of full
# recruitment based on maturity, as done above, and using same criteria as above.
#-------------------------------------------------------------------------------

estimate_z_cr <- function(data)
{
  ages <- data$Age
  n_at_age <- data$n_individuals
  new_ages <- ages - min(ages)
  age_mean <- weighted.mean(new_ages, n_at_age)
  n <- sum(n_at_age)
  s_cr <- (age_mean - 0)/(age_mean - 0 + ((n-1)/n))
  z_cr <- -1*log(s_cr)
  result <- data.frame(estimate = z_cr, estimate_se = NA)
  return(result)
}


z_cr <-
  age_grouped %>%
  filter(Age > 4) %>%
  group_by(across(all_of(grouping_variables_a))) %>%
  mutate(n_ages = n()) %>%
  mutate(total_fish = sum(n_individuals)) %>%
  ungroup() %>%
  filter(n_ages > 2) %>%
  filter(total_fish > 29) %>%
  group_by(across(all_of(grouping_variables_a))) %>%
  group_modify(~ estimate_z_cr(data = .x))


#-------------------------------------------------------------------------------
# Code for getting age of full recruitment based on the number of individuals
# in each age group. This method is probably the one used most often for deciding
# the age of full recruitment to use, but is not the method chosen by the SAS

# subset data based on criteria of at least 3 ages present and 30 individuals
# across those ages. Age of recruitment varies depending on number of individuals
# in each age class.

# peak = age of recruitment is age with most individuals
# peak plus = age of recruitment peak age plus one

# If there is a tie for peak age then the oldest age is chosen

peak <-
  age_grouped %>%
  group_by(across(all_of(grouping_variables_a))) %>%
  mutate(ranks = rank(n_individuals, ties.method = "first")) %>%
  mutate(PeakAge = Age[which(ranks == max(ranks))]) %>%
  filter(Age >= PeakAge)

peak_plus <-
  age_grouped %>%
  group_by(across(all_of(grouping_variables_a))) %>%
  mutate(ranks = rank(n_individuals, ties.method = "first")) %>%
  mutate(PeakAge = Age[which(ranks == max(ranks))]) %>%
  filter(Age > PeakAge)

