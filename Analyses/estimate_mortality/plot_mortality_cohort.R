#-------------------------------------------------------------------------------
# INFO FOR CODE IN THIS FILE
#-------------------------------------------------------------------------------

# Author: Trey Mace (marvin.mace@maryland.gov)
# Date Created: 29 August 2023

# This code is for organizing and producing plots, tables, etc., for
# mortality estimates from age (and in some cases, number of
# spawning) data for the 2023 benchmark river herring stock assessment done
# through the ASMFC.

# The code for estimating these mortality rates is in estimate_mortality_cohort.R

# These estimates are done by following specific cohorts over time.

#-------------------------------------------------------------------------------
# Load Packages, set options
#-------------------------------------------------------------------------------

library(tidyverse)
# library(glmmTMB)
library(readxl)

#-------------------------------------------------------------------------------
# Load data
#-------------------------------------------------------------------------------

region_cohort_data_raw <- read.csv("Zestimates_cohort_by_region.csv",
                        header = TRUE)

region_cohort_data <-
  region_cohort_data_raw %>%
  mutate(Yearf = as.factor(Year)) %>%
  group_by(Region, Species, AgeMethod) %>%
  expand(Yearf) %>%
  ungroup() %>%
  mutate(Year = as.integer(as.character(Yearf))) %>%
  select(-Yearf) %>%
  #arrange(Region, Year, Species, AgeMethod)
  full_join(region_cohort_data_raw, by = c("Region", "Species", "AgeMethod",
                                                "Year")) %>%
  arrange(Region, Species, AgeMethod, Year) %>%
  group_by(Region, Species, AgeMethod) %>%
  mutate(ID = cur_group_id()) %>%
  ungroup()


river_cohort_data_raw <- read.csv("Zestimates_cohort_by_river.csv",
                                  header = TRUE)

river_cohort_data <-
  river_cohort_data_raw %>%
  mutate(Yearf = as.factor(Year)) %>%
  group_by(Region, Location, Species, AgeMethod) %>%
  expand(Yearf) %>%
  ungroup() %>%
  mutate(Year = as.integer(as.character(Yearf))) %>%
  select(-Yearf) %>%
  #arrange(Region, Year, Species, AgeMethod)
  full_join(river_cohort_data_raw, by = c("Region", "Location", "Species", "AgeMethod",
                                           "Year")) %>%
  arrange(Region, Location, Species, AgeMethod, Year) %>%
  group_by(Region, Location, Species, AgeMethod) %>%
  mutate(ID = cur_group_id()) %>%
  ungroup()


Z_spr_a <- read_excel("Preliminary_Z40_ref_pts_RH_SA_2023.xlsx",
                      sheet = "Sheet1",
                      skip = 1,
                      range = cell_limits(c(2, 1), c(5, 5))) %>%
  mutate(Species = "Alewife")

Z_spr_b <- read_excel("Preliminary_Z40_ref_pts_RH_SA_2023.xlsx",
                      sheet = "Sheet1",
                      skip = 1,
                      range = cell_limits(c(8, 1), c(13, 5))) %>%
  mutate(Species = "Blueback")

Z_spr <- bind_rows(Z_spr_a, Z_spr_b) %>%
  rename(Z40 = `N-Weighted Z40`) %>%
  mutate(Region = if_else(Region == "CAN_NNE", "CAN-NNE", Region))




#-------------------------------------------------------------------------------


# Plot region combined sexes data

# Region_Species_AgeMethod <- unique(region_cohort_data$Region_Species_AgeMethod)

Max_total_years <-
  region_cohort_data %>%
  group_by(ID) %>%
  mutate(TotalYears = max(Year) - min(Year)) %>%
  ungroup() %>%
  filter(TotalYears == max(TotalYears))

my_breaks <- c(min(Max_total_years$Year, na.rm = TRUE):max(Max_total_years$Year, na.rm = TRUE))

pdf("region_mortality_cohort_plots.pdf", width = 7, height = 6)

for(i in unique(region_cohort_data$ID))
{

  sub_data <- subset(region_cohort_data, ID == i)

  sub_species <- unique(sub_data$Species)
  sub_region <- unique(sub_data$Region)
  sub_agemethod <- unique(sub_data$AgeMethod)

  title_temp <- paste(sub_region, sub_species, sub_agemethod, sep = " ")

  my_labels <- ifelse(my_breaks %% 5 == 0, my_breaks, "")

  Z_spr_temp <- subset(Z_spr, Species == sub_species & Region == sub_region)$Z40

  zplot <-
    ggplot(sub_data, aes(x = Year, y = Z)) +
    geom_errorbar(aes(ymin=Z-2*Zse, ymax=Z+2*Zse), width=1) +
    geom_point(size = 3) +
    coord_cartesian(ylim = c(0, 3)) +
    scale_x_continuous(breaks = my_breaks,
                       labels = my_labels) +
    labs(y = "Z (Instantaneous mortality rate)",
         x = "Cohort",
         title = title_temp) +
    geom_hline(yintercept = Z_spr_temp, linetype = "dashed", color = "black") +
    #geom_smooth() +
    theme_bw()

  print(zplot)


  zplot_smoother <- zplot + geom_smooth()

  print(zplot_smoother)

}

dev.off()


# Plot river combined sexes data

Max_total_years <-
  river_cohort_data %>%
  group_by(ID) %>%
  mutate(TotalYears = max(Year) - min(Year)) %>%
  ungroup() %>%
  filter(TotalYears == max(TotalYears))

my_breaks <- c(min(Max_total_years$Year, na.rm = TRUE):max(Max_total_years$Year, na.rm = TRUE))


pdf("river_mortality_cohort_plots.pdf", width = 7, height = 6)

for(i in unique(river_cohort_data$ID))
{

  sub_data <- subset(river_cohort_data, ID == i)

  sub_species <- unique(sub_data$Species)
  sub_region <- unique(sub_data$Region)
  sub_location <- unique(sub_data$Location)
  sub_agemethod <- unique(sub_data$AgeMethod)

  title_temp <- paste(sub_region, sub_location, sub_species, sub_agemethod, sep = " ")

  my_labels <- ifelse(my_breaks %% 5 == 0, my_breaks, "")

  Z_spr_temp <- subset(Z_spr, Species == sub_species & Region == sub_region)$Z40

  zplot <-
    ggplot(sub_data, aes(x = Year, y = Z)) +
    geom_errorbar(aes(ymin=Z-2*Zse, ymax=Z+2*Zse), width=1) +
    geom_point(size = 3) +
    coord_cartesian(ylim = c(0, 3)) +
    scale_x_continuous(breaks = my_breaks,
                       labels = my_labels) +
    labs(y = "Z (Instantaneous mortality rate)",
         x = "Cohort",
         title = title_temp) +
    geom_hline(yintercept = Z_spr_temp, linetype = "dashed", color = "black") +
    #geom_smooth() +
    theme_bw()

  print(zplot)


  zplot_smoother <- zplot + geom_smooth()

  print(zplot_smoother)

}

dev.off()



