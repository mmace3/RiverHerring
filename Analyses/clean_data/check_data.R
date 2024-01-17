#-------------------------------------------------------------------------------
# INFO FOR CODE IN THIS FILE
#-------------------------------------------------------------------------------

# Author: Trey Mace (marvin.mace@maryland.gov)
# Date Created: 17 March 2023

# This code is for comparing my cleaned river herring data to the data sets
# created by Katie Drew. When she sent me her data sets I found quite a few
# mistakes in my data and a few mistakes in her data set. So now I wanted to
# write some code to do comparisons after I went through and modified my data
# set.

#-------------------------------------------------------------------------------
# Load Packages, set options
#-------------------------------------------------------------------------------

library(tidyverse)
library(readxl)

#-------------------------------------------------------------------------------
# Read in data
#-------------------------------------------------------------------------------

# Data sets from Katie Drew

# New data sets from Katie Drew

# Alewife data
ALE_new <- read.csv("data/ALE_biodata_for_Z_01-12-24.csv",
                    header = TRUE) %>%
  mutate(Species = "Alewife")


# Blue back herring data
BBH_new <- read.csv("data/BBH_biodata_for_Z_01-12-24.csv",
                header = TRUE) %>%
  mutate(Species = "Blueback")


k_clean_data_new <-
  ALE_new %>%
  bind_rows(BBH_new) %>%
  mutate(AgeScale = Scale.Age) %>%
  mutate(AgeOtolith = Oto.Age) %>%
  mutate(Region = if_else(Region == "Mixed stock", "Mixed Stock", Region))




# my cleaned data set
my_clean_data <- read.csv("clean_data.csv",
                          header = TRUE) %>%
                 mutate(New_State = case_when(State %in% c("NC_DMF_fd", "NC_DMF_fi") ~ "NC DMF",
                                  State %in% c("NC_WRC") ~ "NC WRC",
                                  State %in% c("SC_fd", "SC_fi", "SC_rec") ~ "SC",
                                  State %in% c("NY_fd", "NY_fi") ~ "NY",
                                  TRUE ~ State)) %>%
                 select(-State) %>%
                 rename(State = New_State) %>%
                 filter(Species != "unknown")

# old cleaned data set
my_clean_data_old <- read.csv("clean_data_old_b.csv",
                              header = TRUE)



#-------------------------------------------------------------------------------
# Comparison with new data from Katie Drew


# First compare numbers by age among species and states

# Otolith ages

my_StSpAge_Otolith <-
  my_clean_data %>%
  group_by(State, Species, AgeOtolith) %>%
  summarize(n = n()) %>%
  ungroup()

StateSpeciesAge_Otolith <-
  k_clean_data_new %>%
  group_by(State, Species, AgeOtolith) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  full_join(my_StSpAge_Otolith, by = c("State", "Species", "AgeOtolith")) %>%
  rename(Mydata = n.y) %>%
  rename(Katiedata = n.x) %>%
  mutate(diff = Mydata - Katiedata) %>%
  filter(!is.na(AgeOtolith))


# Scale ages

my_StSpAge_Scale_Scale <-
  my_clean_data %>%
  group_by(State, Species, AgeScale) %>%
  summarize(n = n()) %>%
  ungroup()

StateSpeciesAge_Scale <-
  k_clean_data_new %>%
  group_by(State, Species, AgeScale) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  full_join(my_StSpAge_Scale_Scale, by = c("State", "Species", "AgeScale")) %>%
  rename(Mydata = n.y) %>%
  rename(Katiedata = n.x) %>%
  mutate(diff = Mydata - Katiedata)%>%
  filter(!is.na(AgeScale))





# Now compare numbers at age by species, state, and region

# Otolith ages
my_RStSpAge_Otolith <-
  my_clean_data %>%
  group_by(Region, State, Species, AgeOtolith) %>%
  summarize(n = n()) %>%
  ungroup()

RegionStateSpeciesAge_Otolith <-
  k_clean_data_new %>%
  group_by(Region, State, Species, AgeOtolith) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  full_join(my_RStSpAge_Otolith, by = c("Region", "State", "Species", "AgeOtolith")) %>%
  # mutate(diff = n.y - n.x) %>%
  rename(Mydata = n.y) %>%
  rename(Katiedata = n.x) %>%
  mutate(diff = Mydata - Katiedata)%>%
  filter(!is.na(AgeOtolith))

# Scale Ages
my_RStSpAge_Scale <-
  my_clean_data %>%
  group_by(Region, State, Species, AgeScale) %>%
  summarize(n = n()) %>%
  ungroup()

RegionStateSpeciesAge_Scale <-
  k_clean_data_new %>%
  group_by(Region, State, Species, AgeScale) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  full_join(my_RStSpAge_Scale, by = c("Region", "State", "Species", "AgeScale")) %>%
  #mutate(diff = n.y - n.x)
  rename(Mydata = n.y) %>%
  rename(Katiedata = n.x) %>%
  mutate(diff = Mydata - Katiedata)%>%
  filter(!is.na(AgeScale)) %>%
  filter(AgeScale != 99)



# Include check on difference columns in above data sets to see if any differences.
# Should also manually inspect above data sets just to make sure no weird things
# going on.
StateSpeciesAge_Otolith_check <- sum(StateSpeciesAge_Otolith$diff, na.rm = TRUE) == 0
StateSpeciesAge_Scale_check <- sum(StateSpeciesAge_Scale$diff, na.rm = TRUE) == 0

RegionStateSpeciesAge_Otolith_check <- sum(RegionStateSpeciesAge_Otolith$diff, na.rm = TRUE) == 0
RegionStateSpeciesAge_Scale_check <- sum(RegionStateSpeciesAge_Scale$diff, na.rm = TRUE) == 0

if(any(c(StateSpeciesAge_Otolith_check,
         StateSpeciesAge_Scale_check,
         RegionStateSpeciesAge_Otolith_check,
         RegionStateSpeciesAge_Scale_check) != TRUE))
  {
    print("Check totals! There is a difference between the data sets.")
  } else
    {
      print("No differences between data sets.")
    }

#-------------------------------------------------------------------------------

# Age 0, alewife from Maine that I don't have in my data set.

k_clean_data_new %>%
  filter(State == "MA" & Oto.Age == 0 & Species == "Alewife")

#-------------------------------------------------------------------------------












#-------------------------------------------------------------------------------
# Look at sample sizes for various combinations of data

yy <-
  my_clean_data %>%
  filter(Region == "CAN-NNE") %>%
  filter(AgeScale > 4 | AgeOtolith > 4) %>%
  group_by(State, Year, Sex) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  filter(n > 29)


my_clean_data %>%
  filter(State == "MA")


