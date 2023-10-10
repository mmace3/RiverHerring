#-------------------------------------------------------------------------------
# INFO FOR CODE IN THIS FILE
#-------------------------------------------------------------------------------

# Author: Trey Mace (marvin.mace@maryland.gov)
# Date Created: 24 August 2023

# This code is for estimating mortality from age (and in some cases, number of
# spawning) data for the 2023 benchmark river herring stock assessment done
# through the ASMFC. Specifically, this code is used to estimate mortality
# using a few different methods.

# For this file I am trying to follow cohorts over time to estimate mortality

# The input data file clean_data.csv is created in the file
# Analyses/clean_data/clean_data.R

#-------------------------------------------------------------------------------
# Load Packages, set options
#-------------------------------------------------------------------------------

library(tidyverse)
# library(glmmTMB) # Poisson GLMM

#-------------------------------------------------------------------------------
# Load data
#-------------------------------------------------------------------------------

dat <- read.csv("clean_data.csv",
                 header = TRUE) %>%
  mutate(Date = as.POSIXct(Date, format="%Y-%m-%d")) %>%
  mutate(Year = as.integer(format(Date, format= "%Y")))

# number of records with an age estimate (should be 243041) (06 Oct 2023 - probably
# not anymore)
dat %>%
  filter(!is.na(AgeScale) | !is.na(AgeOtolith)) %>%
  summarize(n = n())



#-------------------------------------------------------------------------------
# SECTION A - Get data grouped by cohorts for mortality estimates
#-------------------------------------------------------------------------------


# flag to separate data (estimates) by sex or not - 1 (yes) or not 1 (no)

# 29 Aug 2023 - keep at 0 for now, no code for sex specific below yet
sep_by_sex <- 0

if(sep_by_sex == 1)
{
  grouping_variables <- c("Region", "State", "Year", "Location", "Species", "Sex", "AgeMethod", "Age")
} else
{
  grouping_variables <- c("Region", "State", "Year", "Location", "Species", "AgeMethod", "Age")
}


age_grouped_a <-
  dat %>%
  pivot_longer(cols = c(AgeScale, AgeOtolith),
               names_to = "AgeMethod",
               values_to = "Age",
               values_drop_na = TRUE) %>%
  filter(Age < 20) %>% # get rid of age 99
  group_by(across(all_of(grouping_variables))) %>%
  summarize(n_individuals = n()) %>%
  mutate(ID = cur_group_id()) %>%
  ungroup()


age_grouped_blank <-
  age_grouped_a %>%
  mutate(Yearf = as.factor(Year)) %>%
  mutate(Agef = as.factor(Age)) %>%
  group_by(Region, State, Location, Species, AgeMethod) %>%
  expand(Yearf, Agef) %>%
  ungroup() %>%
  mutate(Year = as.integer(as.character(Yearf)),
         Age = as.integer(as.character(Agef))) %>%
  select(-Yearf, -Agef)

age_grouped <-
  age_grouped_a %>%
  full_join(age_grouped_blank, by = c("Region", "State", "Location",
                                      "Species", "AgeMethod", "Year",
                                      "Age")) %>%
  mutate(n_individuals = if_else(is.na(n_individuals), 0L, n_individuals)) %>%
  select(-ID) %>%
  pivot_wider(names_from = Age, values_from = n_individuals) %>%
  pivot_longer(-c(Region, State, Year, Location, Species, AgeMethod),
               names_to = "Age", values_to = "n_individuals") %>%
  mutate(Age = as.integer(Age)) %>%
  arrange(Region, State, Location, Species, AgeMethod, Year, Age) %>%
  pivot_wider(names_from = Age, values_from = n_individuals) %>%
  group_by(Region, State, Location, Species, AgeMethod) %>%
  mutate(ID = cur_group_id()) %>%
  ungroup()


aa <- function(dat, ...){
  # https://stackoverflow.com/questions/27849171/in-r-convert-data-frame-diagonals-to-rows?rq=3
  n <- ncol(dat) - 7 # the number of ages - 1

  temp <- mapply(function(x, y) lead(x, n = y), dat[, -seq_len(7)], seq_len(n))
  return(cbind(dat[seq_len(7)], temp))

}

age_grouped_cohorts_a <- data.frame()
grouper <- unique(age_grouped$ID)

for(i in grouper){
  data_temp <- subset(age_grouped, ID == i)
  age_grouped_cohorts_a <- rbind(aa(data_temp), age_grouped_cohorts_a)

}

# bb <- filter(age_grouped, Location == "Cocheco River")
# bbb <- filter(age_grouped_cohorts, Location == "Cocheco River")


# Final data set for age data by cohorts
age_grouped_cohorts <-
  age_grouped_cohorts_a %>%
  select(-ID) %>%
  pivot_longer(-c(Region, State, Year, Location, Species, AgeMethod),
               names_to = "Age", values_to = "n_individuals") %>%
  filter(!is.na(n_individuals) & n_individuals != 0) %>%
  mutate(Age = as.integer(Age))

#-------------------------------------------------------------------------------
# SECTION B - Use data set from Section A to estimate mortality
#-------------------------------------------------------------------------------

#-------------------------
# ESTIMATES BY REGION
#-------------------------

if(sep_by_sex == 1)
{
  region_grouping_variables_a <- c("Region", "Year", "Species", "Sex", "AgeMethod", "Age")
} else
{
  region_grouping_variables_a <- c("Region", "Year", "Species", "AgeMethod", "Age")
}


age_grouped_cohorts_region <-
  age_grouped_cohorts %>%
  group_by(across(all_of(region_grouping_variables_a))) %>%
  summarize(n_individuals = sum(n_individuals)) %>%
  mutate(ID = cur_group_id()) %>%
  ungroup()


if(sep_by_sex == 1)
{
  region_grouping_variables_b <- c("Region", "Year", "Species", "Sex", "AgeMethod")
} else
{
  region_grouping_variables_b <- c("Region", "Year", "Species", "AgeMethod")
}


age_grouped_cohorts_region_data <-
  age_grouped_cohorts_region %>%
  filter(Age > 4) %>%
  group_by(across(all_of(region_grouping_variables_b))) %>%
  mutate(n_ages = n()) %>%
  mutate(total_fish = sum(n_individuals)) %>%
  ungroup() %>%
  filter(n_ages > 2) %>%
  filter(total_fish > 29)

estimates_cohorts_by_region <-
  age_grouped_cohorts_region_data %>%
  group_by(across(all_of(region_grouping_variables_b))) %>%
  group_modify(~ broom::tidy(glm(n_individuals ~ Age, family = "poisson", data = .x))) %>%
  mutate(Zmethod = "z_glm") %>%
  filter(term == "Age") %>%
  rename(Z = "estimate",
         Zse = "std.error") %>%
  mutate(Z = if_else(Z < 0, Z*(-1), -9999)) %>%
  #select(Region, Year, Species, Sex, AgeMethod, Zmethod, Z, Zse)
  select(c(all_of(region_grouping_variables_b), Zmethod, Z, Zse))


# estimates_cohorts_by_region_out <-
#   estimates_cohorts_by_region %>%
#   mutate(Yearf = as.factor(Year)) %>%
#   group_by(Region, Species, AgeMethod) %>%
#   expand(Yearf) %>%
#   ungroup() %>%
#   mutate(Year = as.integer(as.character(Yearf))) %>%
#   select(-Yearf) %>%
#   #arrange(Region, Year, Species, AgeMethod)
#   full_join(estimates_cohorts_by_region, by = c("Region", "Species", "AgeMethod",
#                                                       "Year")) %>%
#   arrange(Region, Species, AgeMethod, Year)

write.csv(estimates_cohorts_by_region,
          "Zestimates_cohort_by_region.csv",
          row.names = FALSE)


#-------------------------
# ESTIMATES BY RIVER
#-------------------------



if(sep_by_sex == 1)
{
  river_grouping_variables_a <- c("Region", "Year", "Location", "Species", "Sex", "AgeMethod", "Age")
} else
{
  river_grouping_variables_a <- c("Region", "Year", "Location", "Species", "AgeMethod", "Age")
}


age_grouped_cohorts_river <-
  age_grouped_cohorts %>%
  group_by(across(all_of(river_grouping_variables_a))) %>%
  summarize(n_individuals = sum(n_individuals)) %>%
  mutate(ID = cur_group_id()) %>%
  ungroup()


if(sep_by_sex == 1)
{
  river_grouping_variables_b <- c("Region", "Year", "Location", "Species", "Sex", "AgeMethod")
} else
{
  river_grouping_variables_b <- c("Region", "Year", "Location", "Species", "AgeMethod")
}


age_grouped_cohorts_river_data <-
  age_grouped_cohorts_river %>%
  filter(Age > 4) %>%
  group_by(across(all_of(river_grouping_variables_b))) %>%
  mutate(n_ages = n()) %>%
  mutate(total_fish = sum(n_individuals)) %>%
  ungroup() %>%
  filter(n_ages > 2) %>%
  filter(total_fish > 29)


estimates_cohorts_by_river <-
  age_grouped_cohorts_river_data %>%
  group_by(across(all_of(river_grouping_variables_b))) %>%
  group_modify(~ broom::tidy(glm(n_individuals ~ Age, family = "poisson", data = .x))) %>%
  mutate(Zmethod = "z_glm") %>%
  filter(term == "Age") %>%
  rename(Z = "estimate",
         Zse = "std.error") %>%
  mutate(Z = if_else(Z < 0, Z*(-1), -9999)) %>%
  select(c(all_of(river_grouping_variables_b), Zmethod, Z, Zse))


write.csv(estimates_cohorts_by_river,
          "Zestimates_cohort_by_river.csv",
          row.names = FALSE)
