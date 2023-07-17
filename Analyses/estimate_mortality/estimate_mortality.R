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

# number of records with an age estimate (should be 243141)
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


# flag to separate data by sex or not - 1 (yes) or not 1 (no)

sep_by_sex <- 1

if(sep_by_sex == 1)
{
  grouping_variables <- c("State", "Year", "Location", "Species", "Sex", "AgeMethod", "Age")
} else
{
  grouping_variables <- c("State", "Year", "Location", "Species", "AgeMethod", "Age")
}

#-------------------------------------------------------------------------------
# Estimate mortality using the poisson glm method and with criteria given above
#
# Out of potential methods, this is the method chosen by the SAS for getting
# mortality estimates.
#-------------------------------------------------------------------------------


# Total number of age records in below data will be a little bigger (802) b/c
# some fish get counted twice if they have both age and otolith estimate
age_grouped <-
  data %>%
  pivot_longer(cols = c(AgeScale, AgeOtolith),
               names_to = "AgeMethod",
               values_to = "Age",
               values_drop_na = TRUE) %>%
  group_by(across(all_of(grouping_variables))) %>%
  summarize(n_individuals = n()) %>%
  mutate(ID = cur_group_id()) %>%
  ungroup()


# subset data based on criteria of at least 3 ages present and 30 individuals
# across those ages. Also we are now using age of full recruitment of 5 years

grouping_variables_a <- c("State", "Year", "Location", "Species", "Sex", "AgeMethod")

z_pois_glm <-
  age_grouped %>%
  filter(Age > 4) %>%
  group_by(across(all_of(grouping_variables_a))) %>%
  mutate(n_ages = n()) %>%
  mutate(total_fish = sum(n_individuals)) %>%
  ungroup() %>%
  filter(n_ages > 2) %>%
  filter(total_fish > 29) %>%
  group_by(across(all_of(grouping_variables_a))) %>%
  group_modify(~ broom::tidy(glm(n_individuals ~ Age, family = "poisson", data = .x)))

estimates_by_river <-
  z_pois_glm %>%
  mutate(Zmethod = "z_glm") %>%
  filter(term == "Age") %>%
  rename(Z = "estimate",
         Zse = "std.error") %>%
  mutate(Z = if_else(Z < 0, Z*(-1), -9999)) %>%
  select(State, Year, Location, Species, Sex, AgeMethod, Zmethod, Z, Zse)

# write out estimates to .csv file

write.csv(estimates_by_river,
          file = "Zestimates_by_River.csv",
          row.names = FALSE)


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
# the age of full recruitment to use.

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





#-------------------------------------------------------------------------------
# Analyses using number of previous spawns as independent variable instead
# of age
#-------------------------------------------------------------------------------

# !!!! UNDER CONSTRUCTION - WILL CHANGE IN FUTURE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!

if(sep_by_sex == 1)
{
  grouping_variables <- c("State", "Year", "Location", "Species", "Sex", "AgeMethod", "AgeMaturity", "RepeatSpawn")
} else
{
  grouping_variables <- c("State", "Year", "Location", "Species", "AgeMethod", "AgeMaturity", "RepeatSpawn")
}


aa <-
data %>%
  mutate(BothAge = if_else((!is.na(AgeScale) & !is.na(AgeOtolith)), "Both",
                   if_else((!is.na(AgeScale) & is.na(AgeOtolith)), "Scale",
                   if_else((is.na(AgeScale) & !is.na(AgeOtolith)), "Otolith",
                   if_else((is.na(AgeScale) & is.na(AgeOtolith)), "Neither",
         "something wrong"))))) %>%
  filter(BothAge != "Neither") %>%
  mutate(AgeMaturity = if_else(BothAge != "Otolith", AgeScale - RepeatSpawn,
                               AgeOtolith - RepeatSpawn)) %>%
  filter(AgeMaturity > 0) %>%
  pivot_longer(
      cols = c(AgeScale, AgeOtolith),
      names_to = "AgeMethod",
      values_to = "Age",
      values_drop_na = TRUE
    ) %>%
  # group_by(State, Year, Location, Species, AgeMethod, AgeMaturity, RepeatSpawn) %>%
  group_by(across(all_of(grouping_variables))) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  group_by(State, Year, Location, Species, Sex, AgeMethod) %>%
  mutate(ID = cur_group_id()) %>%
  ungroup()

estimates_b <- data.frame()

index <- unique(aa$ID)

for(i in index)
# for(i in unique(c(1:100)))
{

  data_temp <- subset(aa, ID == i)
  #data_temp$"FullRecruitment" <- "PeakPlus"

  #peak_ages <- data_temp[which(data_temp$n == max(data_temp$n)),]$Age

  # Check if there is a tie among ages that have the same number of fish
  # if there is a tie make peak_age = max(ages that are tied)

  # if(length(peak_ages) == 1)
  # {
  #   peak_age <- peak_ages
  #
  # } else
  # {
  #   peak_age <- max(peak_ages)
  #
  # }


  # peak_age_plus <- peak_age + 1
  #
  # data_temp_sub <- subset(data_temp, Age >= peak_age_plus)

  rows <- unique(subset(data_temp,
                        select = c("State", "Year", "Location", "Species", "Sex", "AgeMethod")))

  #estimates_temp <- rbind(rows, rows, rows, rows)
  estimates_temp <- rows
  # estimates_temp$"Zmethod" <- c("z_glmm", "z_glm", "z_cr", "z_lm")
  estimates_temp$"Zmethod" <- c("z_glmm")
  estimates_temp$"Z" <- NA
  estimates_temp$"Zse" <- NA

  # if(nrow(data_temp_sub) < 3)
  # {
  #   data_temp$"ConditionsMet" <- 0
  #   data_temp$"Reason" <- "Nages"
  #
  # } else
  #   if(sum(data_temp_sub$n) < 30)
  #   {
  #     data_temp$"ConditionsMet" <- 0
  #     data_temp$"Reason" <- "Nfish"
  #
  #   } else
  #   {
  #     data_temp$"ConditionsMet" <- 1
  #     data_temp$"Reason" <- NA


      #data_temp <- subset(aa, ID == i)

       z_rsp_model <- glmmTMB(n~RepeatSpawn + (1|AgeMaturity:RepeatSpawn),
                              family = poisson(link = "log"),
                              data = data_temp)

       sum_z_rsp_model <- summary(z_rsp_model)

       # Get estimates from model
       Zest_var <- z_rsp_model[["sdr"]][["cov.fixed"]][2,2]
       estimates_temp[which(estimates_temp$Zmethod == "z_glmm"),"Z"] <- -1*z_rsp_model$fit$par[[2]]
       estimates_temp[which(estimates_temp$Zmethod == "z_glmm"),"Zse"] <- sqrt(Zest_var)
    #}


       estimates_b <- rbind(estimates_b, estimates_temp)
}

estimates_B <-
  estimates_b %>%
  mutate(RandomEffect = 1)



write.csv(estimates_B,
          file = "Zestimates_b2.csv",
          row.names = FALSE)


#-------------------------------------------------------------------------------






