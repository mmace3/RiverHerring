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


# Some criteria for decisions that have to be made for some of the mortality
# estimates. These were decided on by the life history work group and came
# from the shad assessment.

# 1.	A minimum of three age classes greater than or equal to age of full
#     recruitment (see #2 below) and at least 30 individuals across these age
#     classes had to be observed to in a data set for it to be included in the analysis.
#
# 2.	Age of full recruitment was set based on the recommendations of simulation work
#     for each method separately based on the peak age observed in the data when
#     pooled across years
#             a.	Chapman-Robson: Peak age + 1
#             b.	Linear regression: Peak age
#             c.	GLMM: Peak age + 1
#


#-------------------------------------------------------------------------------
# First estimate mortality using Chapman Robson, Poisson GLM, Poisson GLMM,
# and linear regression. Get data in proper format, create objects to hold
# new data and estimates, and then loop through available to do analyses for
# everything all at once. The output should be two new objects:

# new_data - data used in estimating mortality and a column for whether data met
#            the above criteria or not.

# estimates - mortality estimates for each state, year, location, age method,
#             species combination for data that met criteria above


yy <-
data %>%
  pivot_longer(cols = c(AgeScale, AgeOtolith),
               names_to = "AgeMethod",
               values_to = "Age",
               values_drop_na = TRUE) %>%
  group_by(State, Year, Location, Species, AgeMethod, Age) %>%
  summarize(n = n()) %>%
  mutate(ID = cur_group_id()) %>%
  ungroup()

new_data <- NULL

estimates <- data.frame()

index <- unique(yy$ID)

for(i in index)
{
  data_temp <- subset(yy, ID == i)
  data_temp$"FullRecruitment" <- "PeakPlus"

  peak_ages <- data_temp[which(data_temp$n == max(data_temp$n)),]$Age

  # Check if there is a tie among ages that have the same number of fish
  # if there is a tie make peak_age = max(ages that are tied)

  if(length(peak_ages) == 1)
  {
    peak_age <- peak_ages

  } else
    {
      peak_age <- max(peak_ages)

    }


  peak_age_plus <- peak_age + 1

  data_temp_sub <- subset(data_temp, Age >= peak_age_plus)

  rows <- unique(subset(data_temp,
                             select = c("State", "Year", "Location", "Species", "AgeMethod")))

  estimates_temp <- rbind(rows, rows, rows, rows)

  estimates_temp$"Zmethod" <- c("z_glmm", "z_glm", "z_cr", "z_lm")
  estimates_temp$"Z" <- NA
  estimates_temp$"Zse" <- NA

  # Check number of ages requirement

  if(nrow(data_temp_sub) < 3)
  {
    data_temp$"ConditionsMet" <- 0
    data_temp$"Reason" <- "Nages"

    # Check sample size requirements

  } else
    if(sum(data_temp_sub$n) < 30)
    {
      data_temp$"ConditionsMet" <- 0
      data_temp$"Reason" <- "Nfish"

    } else
      {
        # run analyses for data that passes requirements

        data_temp$"ConditionsMet" <- 1
        data_temp$"Reason" <- NA

        #-----------------------------------------------------------------------
        # GLMM Poisson model
        #-----------------------------------------------------------------------

        # Get data ready
        model_data <- subset(data_temp_sub, select = c("Age", "n"))
        model_data$"NewAge" <- model_data$Age - min(model_data$Age)
        max_age <- max(model_data$NewAge)

        # extend data for Poisson GLMM model (see Millar 2015)
        # using this extenstion should match estimates from fishmethods package
        model_data_ext <- rbind(subset(model_data, select = -Age),
                                cbind(NewAge=(max_age+1):(3*max_age),
                                n=rep(0,max_age)))
        # run model
        z_mm_model <- glmmTMB(n~NewAge + (1|NewAge),
                         family = poisson(link = "log"),
                         data = model_data_ext)

        # Get estimates from model
        Zest_var <- z_mm_model[["sdr"]][["cov.fixed"]][2,2]
        estimates_temp[which(estimates_temp$Zmethod == "z_glmm"),"Z"] <- -1*z_mm_model$fit$par[[2]]
        estimates_temp[which(estimates_temp$Zmethod == "z_glmm"),"Zse"] <- sqrt(Zest_var)



        #-----------------------------------------------------------------------
        # GLM Poisson model
        #-----------------------------------------------------------------------

        # run model
        z_glm_model <- glm(n~NewAge,
                           family = poisson(link = "log"),
                           data = model_data_ext)

        sum_z_glm_model <- summary(z_glm_model)

        # Get estimates from model

        estimates_temp[which(estimates_temp$Zmethod == "z_glm"),"Z"] <- -1*sum_z_glm_model$coefficients[2,1]
        estimates_temp[which(estimates_temp$Zmethod == "z_glm"),"Zse"] <- sum_z_glm_model$coefficients[2,2]

        #-----------------------------------------------------------------------
        # Chapman Robson model - need to add in variance for this model
        #-----------------------------------------------------------------------

        age_mean <- weighted.mean(model_data$NewAge, model_data$n)
        n <- sum(model_data$n)
        s_cr <- (age_mean - 0)/(age_mean - 0 + ((n-1)/n))
        z_cr <- -1*log(s_cr)
        # zse_cr

        estimates_temp[which(estimates_temp$Zmethod == "z_cr"),"Z"] <- z_cr
        estimates_temp[which(estimates_temp$Zmethod == "z_cr"),"Zse"] <- NA

      }


  # Now do linear regression analysis
  # Need a different peak age for this one based on our rules for these analyses

  data_temp_b <- data_temp
  data_temp_b$"FullRecruitment" <- "Peak"
  data_temp_subB <- subset(data_temp, Age >= peak_age)

  # Check number of ages requirement

  if(nrow(data_temp_subB) < 3)
  {
    data_temp_b$"ConditionsMet" <- 0
    data_temp_b$"Reason" <- "Nages"

    # Check sample size requirement
  } else
    if(sum(data_temp_subB$n) < 30)
    {
      data_temp_b$"ConditionsMet" <- 0
      data_temp_b$"Reason" <- "Nfish"

    } else
    {

      # run analyses for data that passes requirements

      data_temp_b$"ConditionsMet" <- 1
      data_temp_b$"Reason" <- NA

      model_data_b <- subset(data_temp_subB, select = c("Age", "n"))

      #-------------------------------------------------------------------------
      # linear regression model
      #-------------------------------------------------------------------------

      z_lm_model <- lm(log(n)~Age,
                       data = model_data_b)
      sum_z_lm_model <- summary(z_lm_model)

      estimates_temp[which(estimates_temp$Zmethod == "z_lm"),"Z"] <- -1*sum_z_lm_model$coefficients[2,1]
      estimates_temp[which(estimates_temp$Zmethod == "z_lm"),"Zse"] <- sum_z_lm_model$coefficients[2,2]

    }

  new_data <- rbind(new_data, data_temp, data_temp_b)

  estimates <- rbind(estimates, estimates_temp)

}


# write out estimates to .csv file

write.csv(estimates,
          file = "Zestimates.csv",
          row.names = FALSE)



#-------------------------------------------------------------------------------
# Analyses using number of previous spawns as independent variable instead
# of age
#-------------------------------------------------------------------------------

#!!!! UNDER CONSTRUCTION - WILL CHANGE IN FUTURE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!

aa <-
data %>%
  #filter(
  pivot_longer(cols = c(AgeScale, AgeOtolith),
               names_to = "AgeMethod",
               values_to = "Age",
               values_drop_na = TRUE) %>%
  group_by(State, Year, Location, Species, AgeMethod, Age, RepeatSpawn) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  group_by(State, Year, Location, Species, AgeMethod) %>%
  mutate(ID = cur_group_id()) %>%
  ungroup()


for(i in unique(c(1:10)))
{

  data_temp <- subset(aa, ID == i)
  data_temp$"FullRecruitment" <- "PeakPlus"

  peak_ages <- data_temp[which(data_temp$n == max(data_temp$n)),]$Age

  # Check if there is a tie among ages that have the same number of fish
  # if there is a tie make peak_age = max(ages that are tied)

  if(length(peak_ages) == 1)
  {
    peak_age <- peak_ages

  } else
  {
    peak_age <- max(peak_ages)

  }


  peak_age_plus <- peak_age + 1

  data_temp_sub <- subset(data_temp, Age >= peak_age_plus)

  rows <- unique(subset(data_temp,
                        select = c("State", "Year", "Location", "Species", "AgeMethod")))

  estimates_temp <- rbind(rows, rows, rows, rows)

  estimates_temp$"Zmethod" <- c("z_glmm", "z_glm", "z_cr", "z_lm")
  estimates_temp$"Z" <- NA
  estimates_temp$"Zse" <- NA

  if(nrow(data_temp_sub) < 3)
  {
    data_temp$"ConditionsMet" <- 0
    data_temp$"Reason" <- "Nages"

  } else
    if(sum(data_temp_sub$n) < 30)
    {
      data_temp$"ConditionsMet" <- 0
      data_temp$"Reason" <- "Nfish"

    } else
    {
      data_temp$"ConditionsMet" <- 1
      data_temp$"Reason" <- NA




       z_rsp_model <- glmmTMB(n~RepeatSpawn + (1|Age) +(1|RepeatSpawn),
                              family = poisson(link = "log"),
                              data = data_temp_sub)

       sum_z_rsp_model <- summary(z_rsp_model)
    }

}

#-------------------------------------------------------------------------------






