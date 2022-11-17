#-------------------------------------------------------------------------------
# INFO FOR CODE IN THIS FILE
#-------------------------------------------------------------------------------

# Author: Trey Mace (marvin.mace@maryland.gov)
# Date Created: 24 October 2022

# This code is for cleaning data for use in analyses related to the 2023
# benchmark river herring stock assessment done through the ASMFC. Specifically,
# this code is for compiling age data to use in analyses to estimate mortality
# from age composition data.

# Code in this file first reads in each state data set separately and formats
# all them in a common format to then be combined at the end of the file and
# written out to .csv. Also will copy this .csv file to another folder named
# estimate_mortality

#-------------------------------------------------------------------------------
# Load Packages, set options
#-------------------------------------------------------------------------------

library(tidyverse)
library(readxl)

#-------------------------------------------------------------------------------
# Want to create data set that combines data from all states into one file.

# Columns for combined data set:

# State
# Date
# River/Location
# Species
# AgeScale
# AgeOtolith
# ForkLength
# TotalLength
# Weight
# Sex
# RepeatSpawn



#-------------------------------------------------------------------------------
# CT
#   file - data/CT/UConn/2023 River Herring Assessment FI Data Template
#   sheet - FI BioSamples bride 03-06
#           FI BioSamples CT River 05-07
#           FI BioSamples bride 09-20
#-------------------------------------------------------------------------------

CT_A <- read_excel("data/CT/UConn/2023 River Herring Assessment FI Data Template.xlsx",
                   sheet = "FI BioSamples bride 03-06",
                   skip = 6) %>%
        mutate(State = "CT") %>%
        mutate(Species = "Alewife") %>%
        select(State,
               Date,
               Location,
               Species,
               AgeScale = 'Final Scale Age',
               AgeOtolith = 'Final Otolith Age',
               ForkLength = 'Fork length (mm)',
               TotalLength = 'Total length (mm)',
               Weight = 'Weight (g)',
               Sex,
               RepeatSpawn = 'Final Repeat Spawner Mark Count')

CT_B <- read_excel("data/CT/UConn/2023 River Herring Assessment FI Data Template.xlsx",
                   sheet = "FI BioSamples CT River 05-07",
                   skip = 6) %>%
        mutate(State = "CT") %>%
        mutate(Species = "Blueback") %>%
        select(State,
               Date,
               Location,
               Species,
               AgeScale = 'Final Scale Age',
               AgeOtolith = 'Final Otolith Age',
               ForkLength = 'Fork length (mm)',
               TotalLength = 'Total length (mm)',
               Weight = 'Weight (g)',
               Sex,
               RepeatSpawn = 'Final Repeat Spawner Mark Count')


CT_C <- read_excel("data/CT/UConn/2023 River Herring Assessment FI Data Template.xlsx",
                   sheet = "FI BioSamples bride 09-20",
                   skip = 6) %>%
        mutate(State = "CT") %>%
        mutate(Species = "Alewife") %>%
        select(State,
               Date,
               Location,
               Species,
               AgeScale = 'Final Scale Age',
               AgeOtolith = 'Final Otolith Age',
               ForkLength = 'Fork length (mm)',
               TotalLength = 'Total length (mm)',
               Weight = 'Weight (g)',
               Sex,
               RepeatSpawn = 'Final Repeat Spawner Mark Count')

CT <-
  CT_A %>%
  bind_rows(CT_B) %>%
  bind_rows(CT_C) %>%
  mutate(Sex = case_when((Sex == "f" | Sex == "F") ~ "female",
                         (Sex == "M" | Sex == "m") ~ "male",
                         is.na(Sex) ~ "unknown"))

#-------------------------------------------------------------------------------
# DE - none
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# FL - none
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# MA -
#   file - data/MA/MA RH Data -ASMFC SA Benchmark 2022
#   sheet - RH Bio Data
# NOTE: Had to change column N to numeric variable in excel b/c some values stored
#       as text
#-------------------------------------------------------------------------------

MA <- read_excel("data/MA/MA RH Data -ASMFC SA Benchmark 2022.xlsx",
           sheet = "RH Bio Data",
           skip = 0,
           na = c("", "NA")) %>%
        mutate(State = "MA") %>%
        mutate(AgeOtolith = NA) %>%
        # mutate(Sex2 = ifelse(Sex == "F", "female",
        #              ifelse(Sex == "M", "male",
        #              ifelse(is.na(Sex) == TRUE, "unknown",
        #                      "unknown")))) %>%
        mutate(Sex = case_when(Sex == "F" ~ "female",
                                Sex == "M" ~ "male",
                                is.na(Sex) ~ "unknown")) %>%
        mutate(Species = case_when(Species == "ALEWIFE" ~ "Alewife",
                                   Species == "BLUEBACK" ~ "Blueback")) %>%
        select(State,
               Date = Obsdate,
               Location = River,
               Species = Species,
               AgeScale = Age,
               AgeOtolith,
               ForkLength = Fork.length,
               TotalLength = Total.length,
               Weight,
               Sex,
               RepeatSpawn = Repeat.Spawning)


#-------------------------------------------------------------------------------
# MD -
#   file - data/MD/MD_2023 River Herring Assessment Raw Data
#   sheet - FI BioSamples NAN BH
#           FI BioSamples NAN ALE
#           FI BioSamples NE BH
#           FI BioSamples NE ALE

# NOTE: get warning about some values in column n begin converted from text
#       to numeric. They are ages stored as text in the excel file
#-------------------------------------------------------------------------------
MD_A <- read_excel("data/MD/MD_2023 River Herring Assessment Raw Data.xlsx",
                   sheet = "FI BioSamples NAN BH",
                   skip = 5) %>%
        mutate(Species = "Blueblack")

MD_B <- read_excel("data/MD/MD_2023 River Herring Assessment Raw Data.xlsx",
                   sheet = "FI BioSamples NAN ALE",
                   skip = 5) %>%
        mutate(Species = "Alewife")

MD_C <- read_excel("data/MD/MD_2023 River Herring Assessment Raw Data.xlsx",
                   sheet = "FI BioSamples NE BH",
                   skip = 5) %>%
        mutate(Species = "Blueback")

MD_D <- read_excel("data/MD/MD_2023 River Herring Assessment Raw Data.xlsx",
                   sheet = "FI BioSamples NE ALE",
                   skip = 5) %>%
        mutate(Species = "Alewife")

MD <-
  MD_A %>%
  bind_rows(MD_B) %>%
  bind_rows(MD_C) %>%
  bind_rows(MD_D) %>%
  mutate(State = "MD") %>%
  mutate(Sex = case_when(Sex == "F" ~ "female",
                         Sex == "M" ~ "male",
                         (Sex == "U" | is.na(Sex)) ~ "unknown")) %>%
  mutate(Species = if_else(Species == "Blueblack", "Blueback",
                           Species)) %>%
  select(State,
         Date,
         Location,
         Species,
         AgeScale = 'Final Scale Age',
         AgeOtolith = 'Final Otolith Age',
         ForkLength = 'Fork length (mm)',
         TotalLength = 'Total length (mm)',
         Weight = 'Weight (g)',
         Sex,
         RepeatSpawn = 'Final Repeat Spawner Mark Count')

#-------------------------------------------------------------------------------
# ME -
#   file - Maine #### SWNC Combined (where #### is years 2008-2021)
#   sheet - Maine #### SWNC Combined (where #### is years 2008-2021)
#-------------------------------------------------------------------------------

years <- c(2008:2018, 2020:2021)

ME_file_names <- paste0("data/ME/Maine ", years, " SWNC Combined.csv")

# read in all .csv files at once and put in list object. Have to select
# columns 1:9 (i.e., map(select, c(1:9))) below because some .csv files
# have extra columns with no values that shouldn't be there.

ME <-
  ME_file_names %>%
  map(function(x) read_csv(x,
                           skip = 1,
                           col_types = list(col_character(),
                                            col_number(),
                                            col_number(),
                                            col_number(),
                                            col_number(),
                                            col_number(),
                                            col_number(),
                                            col_number(),
                                            col_number()),
                           col_names = c("river",
                                         "year",
                                         "species",
                                         "sex",
                                         "age",
                                         "rps",
                                         "tl",
                                         "fl",
                                         "wt"))) %>%
  map(select, c(1:9)) %>%
  reduce(bind_rows) %>%
  mutate(State = "ME") %>%
  mutate(Species = if_else(species == 1, "Alewife",
                   if_else(species == 2, "Blueback",
                           "Something is wrong with species column"))) %>%
  mutate(Date = as.POSIXct(paste0(year, "-01-31", format="%Y-%m-%d"))) %>%
  mutate(AgeOtolith = NA) %>%
  mutate(Sex = if_else(sex == 1, "male",
               if_else(sex == 2, "female",
                       "unknown"))) %>%
  select(State,
         Date,
         Location = river,
         Species,
         AgeScale = age,
         AgeOtolith,
         ForkLength = fl,
         TotalLength = tl,
         Weight = wt,
         Sex,
         RepeatSpawn = rps)

#-------------------------------------------------------------------------------
# NC -
#   file - data/NC/NCDMF 2023 River Herring Assessment Raw Data_Final_20220701
#   sheet - CommBioSamples_BB
#           CommBioSamples_ALE
#           FI BioSamples_135_BB (no age data until 1999)
#           FI Biosamples_135_ALE (no age data until 1999)
#           FI BioSamples_150_BB (no age data from 2011-2021)
#           FI BioSamples_150_ALE (no age data from 2011-2021)
#-------------------------------------------------------------------------------

NC_sheets <- c("CommBioSamples_BB",
               "CommBioSamples_ALE",
               "FI BioSamples_135_BB",
               "FI Biosamples_135_ALE",
               "FI BioSamples_150_BB",
               "FI BioSamples_150_ALE")

NC_all <-
  NC_sheets %>%
  set_names() %>%
  map2(.y = c(5, 5, 8, 7, 8, 8),
       .f = function(.x, .y) read_excel(
       path = "data/NC/NCDMF 2023 River Herring Assessment Raw Data_Final_20220701.xlsx",
       sheet = .x,
       skip = .y,
       na = c("", ".")))

# First deal with fishery dependent data (fd)

# Note: will get warning about expecting a logical variable but getting 'FEMALE' or
# 'MALE' instead. I think this is okay b/c those are legite entries for that column (sex)

NC_fd <-
  NC_all[c(1,2)] %>%
  reduce(bind_rows) %>%
  mutate(Date = as.POSIXct(paste0(Year, "-", Month, "-", Day), format="%Y-%m-%d")) %>%
  mutate(Location = paste(UNIT, RIVER, sep = "_")) %>%
  mutate(AgeOtolith = NA) %>%
  mutate(State = "NC_fd") %>%
  select(State,
         Date,
         Location,
         Species = SPECIES_NAME,
         AgeScale = 'Final Scale Age',
         AgeOtolith,
         ForkLength = 'Fork length (mm)',
         TotalLength = 'Total length (mm)',
         Weight = 'Weight (g)',
         Sex,
         RepeatSpawn = 'Final Repeat Spawner Mark Count')

# Now deal with fishery independent data

# First get column names that NC_all[1], NC_all[2] have in common with
# NC_all[3], NC_all[4]. Use this in select() function below

NC_columns <- intersect(names(NC_all[[3]]), names(NC_all[[5]]))

NC_fi <-
  NC_all[c(3:6)] %>%
  map(select, all_of(NC_columns)) %>%
  map(mutate, YEAR = as.character(YEAR)) %>%
  map(mutate, MONTH = as.character(MONTH)) %>%
  map(mutate, DAY = as.character(DAY)) %>%
  map(mutate, GEAR = as.character(GEAR)) %>%
  map(mutate, PROGRAM = as.character(PROGRAM)) %>%
  reduce(bind_rows) %>%
  mutate(Date = as.POSIXct(paste0(YEAR, "-", MONTH, "-", DAY), format="%Y-%m-%d")) %>%
  mutate(Location = paste(UNIT, RIVER, sep = "_")) %>%
  mutate(AgeOtolith = NA) %>%
  mutate(State = "NC_fi") %>%
  select(State,
         Date,
         Location,
         Species = SPECIES_NAME,
         AgeScale = 'FINAL SCALE AGE',
         AgeOtolith,
         ForkLength = 'FORK LENGTH (mm)',
         TotalLength = 'TOTAL LENGTH (mm)',
         Weight = 'WEIGHT (g)',
         Sex = SEX,
         RepeatSpawn = 'FINAL REPEAT SPAWNER MARK COUNT')

NC <-
  NC_fd %>%
  bind_rows(NC_fi) %>%
  mutate(Sex = case_when(Sex == "MALE" ~ "male",
                         Sex == "FEMALE" ~ "female",
                         is.na(Sex) ~ "unknown")) %>%
  mutate(Species = if_else(Species %in% c("ALEWIFE", "ALE"), "Alewife",
                   if_else(Species == "BLUEBACK", "Blueback",
                   "Something is wrong with species column")))

#-------------------------------------------------------------------------------
# NEFSC - no files in folder
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# NH -
#   file - data/NH/2023 River Herring Assessment Raw Data Template_NH-byspecies
#   sheet - FI BioSamples
#-------------------------------------------------------------------------------

NH <- read_excel("data/NH/2023 River Herring Assessment Raw Data Template_NH-byspecies.xlsx",
                 sheet = "FI BioSamples",
                 skip = 5,
                 col_types = c("date", # Date
                               "text", # Location
                               "text", # Gear
                               "numeric", # Fork length (mm)
                               "numeric", # Total length (mm)
                               "numeric", # Weight (g)
                               "numeric", # Final Ototlith Age
                               "numeric", # Final Scale Age
                               "numeric", # Reader 1 ototlith age
                               "numeric", # Reader 1 scale age
                               "numeric", # Reader 2 otolith age
                               "numeric", # Reader 2 scale age
                               "numeric", # Known age
                               "numeric", # Final repear spawner mark
                               "numeric", # Reader 1 repeat spawner mark
                               "numeric", # Reader 2 repeat spanwer mark
                               "numeric", # Known repeat spawner mark
                               "text", # disposition
                               "text", # sex
                               "text" # Species
                 )) %>%
      mutate(AgeOtolith = NA) %>%
      mutate(Sex = case_when(Sex == "Male"~ "male",
                             Sex == "Female" ~ "female",
                             (is.na(Sex) | Sex == "Unknown") ~ "unknown")) %>%
      mutate(Species = if_else(is.na(Species) | Species == "Unknown", "unknown",
                               Species)) %>%
      mutate(State = "NH") %>%
      select(State,
             Date,
             Location,
             Species,
             AgeScale = 'Final Scale Age',
             AgeOtolith = 'Final Otolith Age',
             ForkLength = 'Fork length (mm)',
             TotalLength = 'Total length (mm)',
             Weight = 'Weight (g)',
             Sex,
             RepeatSpawn = 'Final Repeat Spawner Mark Count'
             )

#-------------------------------------------------------------------------------
# NJ - no age data
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# NY -
#   file - data/NY/2023 River Herring Assessment Raw Data Template_NY
#   sheet - CommBioSamples
#           FI BioSamples
#-------------------------------------------------------------------------------

NY_fd <- read_excel("data/NY/2023 River Herring Assessment Raw Data Template_NY.xlsx",
                 sheet = "CommBioSamples",
                 skip = 5) %>%
         mutate(State = "NY_fd") %>%
         select(State,
                Date,
                Location,
                Species = 'Common Name',
                AgeScale = 'Final Scale Age',
                AgeOtolith = 'Final Otolith Age',
                ForkLength = 'Fork length (mm)',
                TotalLength = 'Total length (mm)',
                Weight = 'Weight (g)',
                Sex,
                RepeatSpawn = 'Final Repeat Spawner Mark Count') %>%
        mutate(Sex = if_else(Sex == "Male", "male",
                     if_else(Sex == "Female", "female",
                             "unknown")))

NY_fi <- read_excel("data/NY/2023 River Herring Assessment Raw Data Template_NY.xlsx",
                    sheet = "FI BioSamples",
                    skip = 6) %>%
         mutate(State = "NY_fi") %>%
         select(State,
               Date,
               Location,
               Species,
               AgeScale = 'Final Scale Age',
               AgeOtolith = 'Final Otolith Age',
               ForkLength = 'Fork length (mm)',
               TotalLength = 'Total length (mm)',
               Weight = 'Weight (g)',
               Sex,
               RepeatSpawn = 'Final Repeat Spawner Mark Count')

NY <-
  NY_fd %>%
  bind_rows(NY_fi) %>%
  mutate(Species = if_else(Species == "Unk River Herring", "unknown", Species)) %>%
  mutate(Sex = case_when(Sex %in% c("female", "Female") ~ "female",
                         Sex %in% c("male", "Male") ~ "male",
                         Sex %in% c("unknown", "Unknown") ~ "unknown"))

#-------------------------------------------------------------------------------
# PA -
#   file - data/PA/2023 River Herring Assessment Raw Data_PA
#   sheet - FI BioSamples-SusqFishLift
#-------------------------------------------------------------------------------

PA <- read_excel("data/PA/2023 River Herring Assessment Raw Data_PA.xlsx",
                    sheet = "FI BioSamples-SusqFishLift",
                    skip = 5,
                    na = c("", "no age", "not read", "5+")) %>%
      mutate(State = "PA") %>%
      mutate(Species = if_else(Species == "Blueback Herring", "Blueback",
                               Species)) %>%
      mutate(Sex = if_else(Sex == "F", "female",
                   if_else(Sex == "M", "male",
                   "unknown"))) %>%
      select(State,
             Date,
             Location,
             Species,
             AgeScale = 'Final Scale Age',
             AgeOtolith = 'Final Otolith Age',
             ForkLength = 'Fork length (mm)',
             TotalLength = 'Total length (mm)',
             Weight = 'Weight (g)',
             Sex,
             RepeatSpawn = 'Final Repeat Spawner Mark Count')

#-------------------------------------------------------------------------------
# RI - no age data
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# SC -
#   file - data/SC/2023 River Herring Assessment Raw Data Template_SC
#   sheet - CommBioSamples
#           RecBioSamples
#           FI BioSamples
#-------------------------------------------------------------------------------

SC_sheets <- c("CommBioSamples",
               "RecBioSamples",
               "FI BioSamples")

SC_all <-
  SC_sheets %>%
  set_names() %>%
  map(read_excel,
      path = "data/SC/2023 River Herring Assessment Raw Data Template_SC.xlsx",
      skip = 5,
      na = c("Non-Consensus", "BAD", "bad"))

SC_columns <- intersect(intersect(names(SC_all[[1]]), names(SC_all[[2]])), names(SC_all[[3]]))


SC <-
  SC_all %>%
  map(select, all_of(SC_columns)) %>%
  map2(.y = c("SC_fd", "SC_rec", "SC_fi"),
       .f = function(.x, .y)
         mutate(.x, State = .y)) %>%
  map(mutate, 'Reader 1 Scale Age' = as.character('Reader 1 Scale Age')) %>%
  map(mutate, 'Reader 2 Scale Age' = as.character('Reader 2 Scale Age')) %>%
  reduce(bind_rows) %>%
  mutate(Species = "Blueback") %>%
  mutate(Sex = if_else(Sex %in% c("M", "Male"), "male",
               if_else(Sex %in% c("F", "Female"), "female",
                       "unknown"))) %>%
  select(State,
         Date,
         Location,
         Species,
         AgeScale = 'Final Scale Age',
         AgeOtolith = 'Final Otolith Age',
         ForkLength = 'Fork length (mm)',
         TotalLength = 'Total length (mm)',
         Weight = 'Weight (g)',
         Sex,
         RepeatSpawn = 'Final Repeat Spawner Mark Count')


#-------------------------------------------------------------------------------
# USFWS (CT/MA) -
#  file - data/USFWS/2023 River Herring Assessment Raw Data Template FWS CTR
#  sheet - FI BioSamples
#-------------------------------------------------------------------------------

USFWS <- read_excel("data/USFWS/2023 River Herring Assessment Raw Data Template FWS CTR.xlsx",
                 sheet = "FI BioSamples",
                 skip = 5,
                 na = c("", "na", "x")) %>%
  mutate(State = "USFWS") %>%
  mutate(Species = if_else(Species == "BBH", "Blueback",
                   if_else(Species == "ALW", "Alewife",
                           "Check USFWS species column"))) %>%
  mutate(Sex = if_else(Sex == "f", "female",
                       if_else(Sex == "m", "male",
                               "unknown"))) %>%
  select(State,
         Date,
         Location,
         Species,
         AgeScale = 'Final Scale Age',
         AgeOtolith = 'Final Otolith Age',
         ForkLength = 'Fork length (mm)',
         TotalLength = 'Total length (mm)',
         Weight = 'Weight (g)',
         Sex,
         RepeatSpawn = 'Final Repeat Spawner Mark Count')

#-------------------------------------------------------------------------------
# SERC -
#  file - data/VA/Smithsonian/SERC_Chesapeake Bay Rivers Raw Data
#  sheet - BioSamples
#-------------------------------------------------------------------------------

SERC <- read_excel("data/VA/Smithsonian/SERC_Chesapeake Bay Rivers Raw Data.xlsx",
                    sheet = "BioSamples",
                    skip = 2,
                    #na = c("", "na")
                   ) %>%
  mutate(State = "SERC_MD") %>%
  mutate(Species = if_else(Species == "Blueback Herring", "Blueback",
                           Species)) %>%
  mutate(Sex = case_when(Sex %in% c("F", "F (spent)") ~ "female",
                         Sex %in% c("M", "M (spent)") ~ "male",
                         Sex %in% c("F?", "F? no milt maybe eggs", "immature", "U") ~ "unknown",
                         is.na(Sex) ~ "unknown")) %>%
  select(State,
         Date,
         Location = Tributary,
         Species,
         AgeScale = 'Final Scale Age',
         AgeOtolith = 'Final Otolith Age',
         ForkLength = 'Fork length (mm)',
         TotalLength = 'Total length (mm)',
         Weight = 'Weight (g)',
         Sex,
         RepeatSpawn = 'Final Repeat Spawner Mark Count')

#-------------------------------------------------------------------------------
# VA -
#  file - data/VA/ASMFC_VA_RiverHerringAssessment_VIMS_2023_FINAL
#  sheet - FI BioSamples Alewife
#  sheet - FI BioSamples Blueback
#-------------------------------------------------------------------------------


VA_sheets <- c("FI BioSamples Alewife",
               "FI BioSamples Blueback")

VA_all <-
  VA_sheets %>%
  set_names() %>%
  map(read_excel,
      path = "data/VA/ASMFC_VA_RiverHerringAssessment_VIMS_2023_FINAL.xlsx",
      skip = 7)

VA_columns <- intersect(names(VA_all[[1]]), names(VA_all[[2]]))



VA <-
  VA_all %>%
  map(select, all_of(VA_columns)) %>%
  map2(.y = c("Alewife", "Blueback"),
       .f = function(.x, .y)
         mutate(.x, Species = .y)) %>%
  # map(mutate, 'Reader 1 Scale Age' = as.character('Reader 1 Scale Age')) %>%
  # map(mutate, 'Reader 2 Scale Age' = as.character('Reader 2 Scale Age')) %>%
  reduce(bind_rows) %>%
  rename(sex = 'Sex (1=male, 2=female)') %>%
  # mutate(Species = "Blueback") %>%
  mutate(Sex = if_else(sex == 1, "male",
                       if_else(sex== 2, "female",
                               "unknown"))) %>%
  mutate(State = "VA") %>%
  select(State,
         Date,
         Location,
         Species,
         AgeScale = 'Final Scale Age',
         AgeOtolith = 'Final Otolith Age',
         ForkLength = 'Fork length (mm)',
         TotalLength = 'Total length (mm)',
         Weight = 'Weight (g)',
         Sex,
         RepeatSpawn = 'Final Repeat Spawner Mark Count')



#-------------------------------------------------------------------------------
# Combine all state specific data sets above into one data set and write out
# to .csv
#-------------------------------------------------------------------------------

AllStates <-
  CT %>%
  bind_rows(MD) %>%
  bind_rows(MA) %>%
  bind_rows(ME) %>%
  bind_rows(NC) %>%
  bind_rows(NH) %>%
  bind_rows(NY) %>%
  bind_rows(PA) %>%
  bind_rows(SC) %>%
  bind_rows(USFWS) %>%
  bind_rows(SERC) %>%
  bind_rows(VA)


write.csv(AllStates,
          file = "clean_data.csv",
          row.names = FALSE)

# now copy clean data file to estimate_mortality directory

# first get first part of path to estimate_mortality directory
dir_a <- dirname(getwd())

dir_b <- "/estimate_mortality"

dir <- paste0(dir_a, dir_b)

file.copy(from = "clean_data.csv", to = dir, overwrite = TRUE)

