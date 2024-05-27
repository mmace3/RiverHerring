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

# Usually all data is read in at a section at the top of the file, but there was
# so many data sets I decided to read in data for each state in a separate section
# below. Each section deals only with one state and reads in the data and formats
# it. Then all data is put together at the end in one data set.


# 9 March 2023
# read in data from Katie Drew that has regions for each record. Use this to
# assign regions for my data set.

# 16 Jan 2024
# read in New Hampshire data from Katie Drew and use her NH data instead of
# my NH data (see below)

#-------------------------------------------------------------------------------
# Load Packages, set options
#-------------------------------------------------------------------------------

library(tidyverse)
library(readxl)

#-------------------------------------------------------------------------------
# Read in data
#-------------------------------------------------------------------------------

# Data sets from Katie Drew

# Alewife data
ALE <- read.csv("data/ALE_biodata_09-11-23.csv",
                header = TRUE)

# Blue back herring data
BBH <- read.csv("data/BBh_biodata_09-11-23.csv",
                header = TRUE)

# Updated NY from Wes (in same format as files above from Katie Drew)

updated_NY_BBH <- read_excel("data/Updated_NY_BBH_biodata_09_07_23.xlsx",
                             sheet = "Sheet1",
                             skip = 0,
                             na = c("", "NA"),
                             col_types = c("numeric", # Year
                                           "text", # Region
                                           "text", # State
                                           "text", # River
                                           "text", # Sex
                                           "numeric", # Fork.Length
                                           "numeric", # Total.Length
                                           "numeric", # weight
                                           "numeric", # Scale.Age
                                           "numeric", # Oto.Age
                                           "numeric", # RPS
                                           "text", # maturity
                                           "text" # data source
                                           )
                            ) %>%
                  mutate(Species = "Blueback") %>%
                  mutate(Date = as.POSIXct(paste(Year, "01", "01", sep = "-"),
                                           format="%Y-%m-%d")) %>%
                  mutate(Sex = if_else(Sex == "Male", "male",
                       if_else(Sex == "Female", "female",
                               "unknown"))) %>%
                  select(State,
                         Date,
                         Location = River,
                         Species,
                         AgeScale = Scale.Age,
                         AgeOtolith = Oto.Age,
                         ForkLength = Fork.Length,
                         TotalLength = Total.Length,
                         Weight,
                         Sex,
                         RepeatSpawn = RPS)


updated_NY_ALE <- read_excel("data/Updated_NY_ALE_biodata_09_07_23.xlsx",
                             sheet = "Sheet1",
                             skip = 0,
                             na = c("", "NA"),
                             col_types = c("numeric", # Year
                                           "text", # Region
                                           "text", # State
                                           "text", # River
                                           "text", # Sex
                                           "numeric", # Fork.Length
                                           "numeric", # Total.Length
                                           "numeric", # weight
                                           "numeric", # Scale.Age
                                           "numeric", # Oto.Age
                                           "numeric", # RPS
                                           "text", # maturity
                                           "text" # data source
                                          )
                            ) %>%
                  mutate(Species = "Alewife") %>%
                  mutate(Date = as.POSIXct(paste(Year, "01", "01", sep = "-"),
                                           format="%Y-%m-%d")) %>%
                  mutate(Sex = if_else(Sex == "Male", "male",
                       if_else(Sex == "Female", "female",
                               "unknown"))) %>%
                  select(State,
                         Date,
                         Location = River,
                         Species,
                         AgeScale = Scale.Age,
                         AgeOtolith = Oto.Age,
                         ForkLength = Fork.Length,
                         TotalLength = Total.Length,
                         Weight,
                         Sex,
                         RepeatSpawn = RPS) # %>%
                  # filter(!is.na(AgeScale) | !is.na(AgeOtolith))

NY_new <-
  updated_NY_BBH %>%
  bind_rows(updated_NY_ALE)

# 16 Jan 2024
# Update data sets from Katie Drew to include new New Hampshire age data. She
# used age length keys to determine the age of NH samples that only had length
# data originally. I will replace my NH data with NH data from her updated
# data set.

# updated alewife data for New Hampshire

ALE_NH <- read.csv("data/ALE_biodata_for_Z_01-12-24.csv",
                header = TRUE,
                colClasses = c("integer", # Year
                               "character", # Region
                               "character", # State
                               "character", # River
                               "character", # Sex
                               "numeric", # Fork.Length
                               "numeric", # Total.Length
                               "numeric", # weight
                               "numeric", # Scale.Age
                               "numeric", # Oto.Age
                               "numeric", # RPS
                               "character", # Maturity
                               "character", # Data.Source
                               "character" # Age.Type
                )) %>%
          filter(State == "NH") %>%
          mutate(Species = "Alewife") %>%
          select(State, Location = River, Species, AgeScale = Scale.Age,
                 AgeOtolith = Oto.Age, ForkLength = Fork.Length,
                 TotalLength = Total.Length, Weight, Sex, RepeatSpawn = RPS,
                 Region, Year) %>%
          mutate(Sex = if_else(Sex %in% c("Male"), "male",
                       if_else(Sex %in% c("Female"), "female",
                               "unknown"))) %>%
          mutate(Location = if_else(Location == "Cocheco River", "Cocheco",
                            if_else(Location == "Exeter River", "Exeter",
                            if_else(Location == "Lamprey River", "Lamprey",
                            if_else(Location == "Oyster River", "Oyster",
                            if_else(Location == "Winnicut River", "Winnicut",
                                    Location)
                            )))))

# updated blueback data for New Hamphshire

BBH_NH <- read.csv("data/BBH_biodata_for_Z_01-12-24.csv",
                header = TRUE,
                colClasses = c("integer", # Year
                               "character", # Region
                               "character", # State
                               "character", # River
                               "character", # Sex
                               "numeric", # Fork.Length
                               "numeric", # Total.Length
                               "numeric", # weight
                               "numeric", # Scale.Age
                               "numeric", # Oto.Age
                               "numeric", # RPS
                               "character", # Maturity
                               "character", # Data.Source
                               "character" # Age.Type
                )) %>%
          filter(State == "NH") %>%
          mutate(Species = "Blueback") %>%
          select(State, Location = River, Species, AgeScale = Scale.Age,
                 AgeOtolith = Oto.Age, ForkLength = Fork.Length,
                 TotalLength = Total.Length, Weight, Sex, RepeatSpawn = RPS,
                 Region, Year) %>%
          mutate(Sex = if_else(Sex %in% c("Male"), "male",
                       if_else(Sex %in% c("Female"), "female",
                               "unknown"))) %>%
          mutate(Location = if_else(Location == "Cocheco River", "Cocheco",
                            if_else(Location == "Exeter River", "Exeter",
                            if_else(Location == "Lamprey River", "Lamprey",
                            if_else(Location == "Oyster River", "Oyster",
                            if_else(Location == "Winnicut River", "Winnicut",
                                    Location)
                            )))))


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
                   skip = 6,
                   col_types = c("date", # Date
                                    "numeric", # individual identifier
                                    "text", # Location
                                    "text", # Gear
                                    "numeric", # Fork length (mm)
                                    "numeric", # Total length (mm)
                                    "numeric", # Weight (g)
                                    "numeric", # Final Otolith Age
                                    "numeric", # Final Scale Age
                                    "numeric", # Reader 1 Otolith Age
                                    "numeric", # Reader 1 Scale Age
                                    "numeric", # Reader 2 Otolith Age
                                    "numeric", # Reader 2 Scale Age
                                    "numeric", # Known Age
                                    "numeric", # Final Repeat Spawner Mark Count
                                    "numeric", # Reader 1 Repeat Spawner Mark Count
                                    "numeric", # Reader 2 Repeat Spawner Mark Count
                                    "numeric", # Known Repeat Spawner Mark Count
                                    "text", # Disposition
                                    "text" # Sex
                                    )
                   ) %>%
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
                   skip = 6,
                   col_types = c("date",
                                 "text", # Location
                                 "text", # Gear
                                 "numeric", # Fork length (mm)
                                 "numeric", # Total length (mm)
                                 "numeric", # Weight (g)
                                 "numeric", # Final Otolith Age
                                 "numeric", # Final Scale Age
                                 "numeric", # Reader 1 Otolith Age
                                 "numeric", # Reader 1 Scale Age
                                 "numeric", # Reader 2 Otolith Age
                                 "numeric", # Reader 2 Scale Age
                                 "numeric", # Reader 3 Scale Age
                                 "numeric", # Known Age
                                 "numeric", # Final Repeat Spawner Mark Count
                                 "numeric", # Reader 1 Repeat Spawner Mark Count
                                 "numeric", # Reader 2 Repeat Spawner Mark Count
                                 "numeric", # Known Repeat Spawner Mark Count
                                 "text", # Disposition
                                 "text" # Sex
                                 )) %>%
        mutate(State = "CT") %>%
        mutate(Species = "Blueback") %>%
        mutate(Otolith_dif = `Reader 2 Otolith Age` - `Reader 1 Otolith Age`) %>%
        mutate(Otolith_dif_a = case_when(Otolith_dif == 0 ~ `Reader 1 Otolith Age`,
                                           Otolith_dif != 0 | is.na(Otolith_dif) ~ NA_real_)) %>%
        mutate(Otolith_dif_b = case_when(is.na(`Reader 2 Otolith Age`) & !is.na(`Reader 1 Otolith Age`) ~ `Reader 1 Otolith Age`,
                                         !is.na(`Reader 2 Otolith Age`) & is.na(`Reader 1 Otolith Age`) ~ `Reader 2 Otolith Age`)) %>%

#
#         mutate(New_AgeOtolithAge_a = case_when(is.na(`Reader 2 Otolith Age`) & !is.na(`Reader 1 Otolith Age`) ~ `Reader 1 Otolith Age`,
#                                              !is.na(`Reader 2 Otolith Age`) & is.na(`Reader 1 Otolith Age`) ~ `Reader 2 Otolith Age`,
#                                              Otolith_dif == 0 ~ `Reader 2 Otolith Age`,
#                                              Otolith_dif != 0 ~ NA_real_,
#                                              TRUE ~ Otolith_dif)) %>%
        mutate(New_AgeOtolithAge_b = case_when(!is.na(Otolith_dif_a) & !is.na(Otolith_dif_b) ~ "Check")) %>%
        mutate(New_AgeOtolithAge_a = case_when(is.na(Otolith_dif_a) & !is.na(Otolith_dif_b) ~ Otolith_dif_b,
                                               !is.na(Otolith_dif_a) & is.na(Otolith_dif_b) ~ Otolith_dif_a)) %>%
        select(State,
               Date,
               Location,
               Species,
               AgeScale = 'Reader 1 Scale Age',
               AgeOtolith = New_AgeOtolithAge_a,
               ForkLength = 'Fork length (mm)',
               TotalLength = 'Total length (mm)',
               Weight = 'Weight (g)',
               Sex,
               RepeatSpawn = 'Final Repeat Spawner Mark Count')


CT_C <- read_excel("data/CT/UConn/2023 River Herring Assessment FI Data Template.xlsx",
                   sheet = "FI BioSamples bride 09-20",
                   skip = 6,
                   col_types = c("date",
                                 "text", # Location
                                 "text", # Gear
                                 "numeric", # Fork length (mm)
                                 "numeric", # Total length (mm)
                                 "numeric", # Weight (g)
                                 "numeric", # Final Otolith Age
                                 "numeric", # Final Scale Age
                                 "numeric", # Reader 1 Otolith Age
                                 "numeric", # Reader 1 Scale Age
                                 "numeric", # Reader 2 Otolith Age
                                 "numeric", # Reader 2 Scale Age
                                 "numeric", # Reader 3 Scale Age
                                 "numeric", # Known Age
                                 "numeric", # Final Repeat Spawner Mark Count
                                 "numeric", # Reader 1 Repeat Spawner Mark Count
                                 "numeric", # Reader 2 Repeat Spawner Mark Count
                                 "numeric", # Known Repeat Spawner Mark Count
                                 "text", # Disposition
                                 "text" # Sex
                   )) %>%
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

# MA <- read_excel("data/MA/MA RH Data -ASMFC SA Benchmark 2022.xlsx",
#            sheet = "RH Bio Data",
#            skip = 0,
#            na = c("", "NA"),
#            col_types = c("text", # ID
#                          "text", # River
#                          "date", # Obsdate
#                          "text", # Species
#                          "numeric", # Age
#                          "numeric", # Fork.length
#                          "numeric", # Total.length
#                          "numeric", # Weight
#                          "text", # Sex
#                          "numeric" # Repeat.Spawning
#                          )) %>%
#         mutate(State = "MA") %>%
#         mutate(AgeOtolith = NA_real_) %>%
#         # mutate(Sex2 = ifelse(Sex == "F", "female",
#         #              ifelse(Sex == "M", "male",
#         #              ifelse(is.na(Sex) == TRUE, "unknown",
#         #                      "unknown")))) %>%
#         mutate(Sex = case_when(Sex == "F" ~ "female",
#                                 Sex == "M" ~ "male",
#                                 is.na(Sex) ~ "unknown")) %>%
#         mutate(Species = case_when(Species == "ALEWIFE" ~ "Alewife",
#                                    Species == "BLUEBACK" ~ "Blueback")) %>%
#         select(State,
#                Date = Obsdate,
#                Location = River,
#                Species = Species,
#                AgeScale = Age,
#                AgeOtolith,
#                ForkLength = Fork.length,
#                TotalLength = Total.length,
#                Weight,
#                Sex,
#                RepeatSpawn = Repeat.Spawning)


MA_new <-
  read.csv("data/MA/2022 QAQC Bio Data_v2.csv",
           header = TRUE,
           na.strings = c("", "NA"),
           colClasses = c("numeric", # Row numbers
                          "character", # ID
                          "character", # River
                          "character", # Obsdate
                          "character", # Species
                          "numeric", # Age
                          "numeric", # Fork.length
                          "numeric", # Total.length
                          "numeric", # Weight
                          "character", # Sex
                          "numeric", # Repeat.Spawning
                          "character" # Age.Structure
           )) %>%
  mutate(State = "MA") %>%
  mutate(Date = as.POSIXct(Obsdate, format="%m/%d/%Y")) %>%
  mutate(Age_Structure = case_when(is.na(Age.Structure) | Age.Structure == "SCALE" ~ "SCALE",
                                   Age.Structure %in% c("OTOLITH", "Otolith") ~ "OTOLITH",
                                   TRUE ~ Age.Structure)) %>%
  # pivot_wider(names_from = Age.Structure, values_from = Age)
  pivot_wider(names_from = Age_Structure, values_from = Age) %>%
  mutate(Sex = case_when(Sex == "F" | Sex == "f" ~ "female",
                         Sex == "M" | Sex == "m" ~ "male",
                         is.na(Sex) ~ "unknown")) %>%
  select(State,
         Date,
         Location = River,
         Species = Species,
         AgeScale = SCALE,
         AgeOtolith = OTOLITH,
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

MD_data_file <- "data/MD/MD_2023 River Herring Assessment Raw Data_10-17-22.xlsx"

MD_A <- read_excel(MD_data_file,
                   sheet = "FI BioSamples NAN BH",
                   skip = 5,
                   col_types = c("date", # Date
                                 "text", # Location
                                 "text", # Gear
                                 "numeric", # Fork length (mm)
                                 "numeric", # Total length (mm)
                                 "numeric", # Weight
                                 "numeric", # Final Otolith Age
                                 "numeric", # Final Scale Age
                                 "numeric", # Reader 1 Otolith Age
                                 "numeric", # Reader 1 Scale Age
                                 "numeric", # Reader 2 Otolith Age
                                 "numeric", # Reader 2 Scale Age
                                 "numeric", # Known Age
                                 "text", # Final Repeat Spawner Mark Count
                                 "numeric", # Reader 1 Repeat Spawner Mark Count
                                 "numeric", # Reader 2 Repeat Spawner Mark Count
                                 "numeric", # Known Repeat Spawner Mark Count
                                 "text", # Disposition
                                 "text", # Sex
                                 "text" # Comments
                     )) %>%
        mutate(Species = "Blueblack") %>%
        mutate(`Final Repeat Spawner Mark Count` = as.numeric(`Final Repeat Spawner Mark Count`))

MD_B <- read_excel(MD_data_file,
                   sheet = "FI BioSamples NAN ALE",
                   skip = 5,
                   col_types = c("date", # Date
                                 "text", # Location
                                 "text", # Gear
                                 "numeric", # Fork length (mm)
                                 "numeric", # Total length (mm)
                                 "numeric", # Weight
                                 "numeric", # Final Otolith Age
                                 "numeric", # Final Scale Age
                                 "numeric", # Reader 1 Otolith Age
                                 "numeric", # Reader 1 Scale Age
                                 "numeric", # Reader 2 Otolith Age
                                 "numeric", # Reader 2 Scale Age
                                 "numeric", # Known Age
                                 "numeric", # Final Repeat Spawner Mark Count
                                 "numeric", # Reader 1 Repeat Spawner Mark Count
                                 "numeric", # Reader 2 Repeat Spawner Mark Count
                                 "numeric", # Known Repeat Spawner Mark Count
                                 "text", # Disposition
                                 "text", # Sex
                                 "text" # Comments
                   )) %>%
        mutate(Species = "Alewife")

MD_C <- read_excel(MD_data_file,
                   sheet = "FI BioSamples NE BH",
                   skip = 5,
                   col_types = c("date", # Date
                                 "text", # Location
                                 "text", # Gear
                                 "numeric", # Fork length (mm)
                                 "numeric", # Total length (mm)
                                 "numeric", # Weight
                                 "numeric", # Final Otolith Age
                                 "numeric", # Final Scale Age
                                 "numeric", # Reader 1 Otolith Age
                                 "numeric", # Reader 1 Scale Age
                                 "numeric", # Reader 2 Otolith Age
                                 "numeric", # Reader 2 Scale Age
                                 "numeric", # Known Age
                                 "numeric", # Final Repeat Spawner Mark Count
                                 "numeric", # Reader 1 Repeat Spawner Mark Count
                                 "numeric", # Reader 2 Repeat Spawner Mark Count
                                 "numeric", # Known Repeat Spawner Mark Count
                                 "text", # Disposition
                                 "text", # Sex
                                 "text" # Comments
                   )) %>%
        mutate(Species = "Blueback")

MD_D <- read_excel(MD_data_file,
                   sheet = "FI BioSamples NE ALE",
                   skip = 5,
                   col_types = c("date", # Date
                                 "text", # Location
                                 "text", # Gear
                                 "numeric", # Fork length (mm)
                                 "numeric", # Total length (mm)
                                 "numeric", # Weight
                                 "numeric", # Final Otolith Age
                                 "numeric", # Final Scale Age
                                 "numeric", # Reader 1 Otolith Age
                                 "numeric", # Reader 1 Scale Age
                                 "numeric", # Reader 2 Otolith Age
                                 "numeric", # Reader 2 Scale Age
                                 "numeric", # Known Age
                                 "numeric", # Final Repeat Spawner Mark Count
                                 "numeric", # Reader 1 Repeat Spawner Mark Count
                                 "numeric", # Reader 2 Repeat Spawner Mark Count
                                 "numeric", # Known Repeat Spawner Mark Count
                                 "text", # Disposition
                                 "text", # Sex
                                 "text" # Comments
                   )) %>%
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

# years <- c(2008:2018, 2020:2021)
#
# ME_file_names <- paste0("data/ME/Maine ", years, " SWNC Combined.csv")
#
# # read in all .csv files at once and put in list object. Have to select
# # columns 1:9 (i.e., map(select, c(1:9))) below because some .csv files
# # have extra columns with no values that shouldn't be there.
#
# ME <-
#   ME_file_names %>%
#   map(function(x) read_csv(x,
#                            skip = 1,
#                            col_types = list(col_character(),
#                                             col_number(),
#                                             col_number(),
#                                             col_number(),
#                                             col_number(),
#                                             col_number(),
#                                             col_number(),
#                                             col_number(),
#                                             col_number()),
#                            col_names = c("river",
#                                          "year",
#                                          "species",
#                                          "sex",
#                                          "age",
#                                          "rps",
#                                          "tl",
#                                          "fl",
#                                          "wt"))) %>%
#   map(select, c(1:9)) %>%
#   reduce(bind_rows) %>%
#   mutate(State = "ME") %>%
#   mutate(Species = if_else(species == 1, "Alewife",
#                    if_else(species == 2, "Blueback",
#                            "Something is wrong with species column"))) %>%
#   mutate(Date = as.POSIXct(paste0(year, "-01-31", format="%Y-%m-%d"))) %>%
#   mutate(AgeOtolith = NA) %>%
#   mutate(Sex = if_else(sex == 1, "male",
#                if_else(sex == 2, "female",
#                        "unknown"))) %>%
#   select(State,
#          Date,
#          Location = river,
#          Species,
#          AgeScale = age,
#          AgeOtolith,
#          ForkLength = fl,
#          TotalLength = tl,
#          Weight = wt,
#          Sex,
#          RepeatSpawn = rps)


# New Maine

# Had to insert an empty column in ME Non_Com Sep_by River 2008_2021.xlsx
# after column cj to have two empty columns between each data set in this file.
# It seems like someone left that out when creating the data set. Also had to add
# in column name for cj (= loc)

ME_file_names <- c("data/ME/New_files_from_Katie/ME Com Sep_by_River 2008_2021.xlsx",
                   "data/ME/New_files_from_Katie/ME Non_Com Sep_by River 2008_2021.xlsx")

ME_new <-
  ME_file_names %>%
  map(function(x) read_excel(x,
                           #skip = 1,
                           sheet = "Sheet1")
      )



years <- c(2008:2021)

# First read in ME Com Sep_by_River 2008_2021.xlsx to a list
new_data_a <- vector(mode = "list", length = length(years))
cols_to_select <- c(1:8)

for(i in c(1:length(years)))
{

  new_data_a[[i]] <- ME_new[1][[1]][,cols_to_select]
  new_data_a[[i]][,7][[1]] <- ifelse(class(new_data_a[[i]][,7][[1]]) != "numeric",
                              as.numeric(new_data_a[[i]][,7][[1]]), new_data_a[[i]][,7][[1]])
  cols_to_select <- cols_to_select + c(10, 10)

}


# Second read in ME Com Sep_by_River 2008_2021.xlsx to a list
new_data_b <- vector(mode = "list", length = length(years))
cols_to_select <- c(1:8)

for(i in c(1:length(years)))
{

  new_data_b[[i]] <- ME_new[2][[1]][,cols_to_select]
  new_data_b[[i]][,7][[1]] <- ifelse(class(new_data_b[[i]][,7][[1]]) != "numeric",
                            as.numeric(new_data_b[[i]][,7][[1]]), new_data_b[[i]][,7][[1]])
  cols_to_select <- cols_to_select + c(10, 10)

}



# Now combine both data sets above into one data frame
ME_temp <-
  new_data_a %>%
  reduce(bind_rows) %>%
  bind_rows(reduce(new_data_b, bind_rows))


# Now take ME_temp and get rid of extra rows created when reading in data and
# then other stuff to get it ready to combine with other states data
ME <-
  ME_temp %>%
  filter(!is.na(river) & !is.na(year) & !is.na(species)) %>%
  mutate(State = "ME") %>%
  mutate(Species = if_else(species == 1, "Alewife",
                           if_else(species == 2, "Blueback",
                                   "Something is wrong with species column"))) %>%
  mutate(Date = as.POSIXct(paste0(year, "-01-31", format="%Y-%m-%d"))) %>%
  mutate(AgeOtolith = NA) %>%
  mutate(Sex = if_else(sex == 1, "male",
                       if_else(sex == 2, "female",
                               "unknown"))) %>%
  mutate(AgeOtolith = NA_real_,
         ForkLength = NA_real_,
         Weight = NA_real_) %>%
  select(State,
         Date,
         Location = river,
         Species,
         AgeScale = age,
         AgeOtolith,
         ForkLength,
         TotalLength = tl,
         Weight,
         Sex,
         RepeatSpawn = rps)

#--------------------------------------------------------------------------
# 6 October 2023 - Will use Maine data from datasets provided by Katie Drew.
#                  They have the river locations in the data set instead of
#                  all combined together like the data I created above.
#--------------------------------------------------------------------------

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

new_ME_BBH <-
  BBH %>%
  filter(State == "ME") %>%
  mutate(Species = "Blueback") %>%
  mutate(Date = as.POSIXct(paste(Year, "01", "01", sep = "-"),
                           format="%Y-%m-%d")) %>%
  mutate(Sex = if_else(Sex == "Male", "male",
                       if_else(Sex == "Female", "female",
                               "unknown"))) %>%
  select(State,
         Date,
         Location = River,
         Species,
         AgeScale = Scale.Age,
         AgeOtolith = Oto.Age,
         ForkLength = Fork.Length,
         TotalLength = Total.Length,
         Weight,
         Sex,
         RepeatSpawn = RPS)


new_ME_ALE <-
  ALE %>%
  filter(State == "ME") %>%
  mutate(Species = "Alewife") %>%
  mutate(Date = as.POSIXct(paste(Year, "01", "01", sep = "-"),
                           format="%Y-%m-%d")) %>%
  mutate(Sex = if_else(Sex == "Male", "male",
                       if_else(Sex == "Female", "female",
                               "unknown"))) %>%
  select(State,
         Date,
         Location = River,
         Species,
         AgeScale = Scale.Age,
         AgeOtolith = Oto.Age,
         ForkLength = Fork.Length,
         TotalLength = Total.Length,
         Weight,
         Sex,
         RepeatSpawn = RPS)

ME_new <-
  new_ME_BBH %>%
  bind_rows(new_ME_ALE)

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

# NC_sheets <- c("CommBioSamples_BB",
#                "CommBioSamples_ALE",
#                "FI BioSamples_135_BB",
#                "FI Biosamples_135_ALE",
#                "FI BioSamples_150_BB",
#                "FI BioSamples_150_ALE")
#
#
# NC_all <-
#   NC_sheets %>%
#   set_names() %>%
#   map2(.y = c(5, 5, 8, 7, 8, 8),
#        .f = function(.x, .y) read_excel(
#        path = "data/NC/NCDMF 2023 River Herring Assessment Raw Data_Final_20220701.xlsx",
#        sheet = .x,
#        skip = .y,
#        na = c("", ".")))



NC_coltypes_a <- c("text", # DATE_YYYMMDD
                   "text", # UNIT
                   "text", # RIVER
                   "text", # Gear
                   "numeric", # Fork length (mm)
                   "numeric", # Total length (mm)
                   "numeric", # Weight (g)
                   "numeric", # Final Scale Age
                   "numeric", # Final Repeat Spawner Mark Count
                   "text", # Disposition
                   "text", # Sex
                   "numeric", # NCDMF Program
                   "text", # SPECIES_NAME
                   "numeric", # NCDMF Species Code
                   "text", # NCDMF Location Code
                   "text", # Year
                   "text", # Month
                   "text", # Day
                   "text", # Control1 (unique to collection)
                   "text" # Control4 (unique to individual)
                   )

NC_coltypes_b <- c("text", # Date
                   "text", # UNIT
                   "text", # RIVER
                   "text", # SPECIES_NAME
                   "numeric", # FORK LENGTH (mm)
                   "numeric", # TOTAL LENGTH
                   "numeric", # WEIGHT (g)
                   "text", # SEX
                   "numeric", # FINAL SCALE AGE
                   "numeric", # FINAL REPEAT SPAWNER MARK COUNT
                   "text", # DISPOSITION
                   "text", # CONTROL4
                   "text", # GEAR
                   "numeric", # MESH
                   "numeric", # NET LENGTH (FT)
                   "numeric", # NET DEPTH (FT)
                   "numeric", # LONGITUDE
                   "numeric", # LATTITUDE
                   "numeric", # GRID
                   "numeric", # QUAD
                   "numeric", # Depth_M
                   "numeric", # SURTEMP_c
                   "numeric", # BOTTEMP_c
                   "numeric", # SURSAL_ppt
                   "numeric", # BOTSAL_ppt
                   "numeric", # SURDO_mfL
                   "numeric", # BOTDO_mgL
                   "text", # TOW ID (CONTROL1)
                   "text", # YEAR
                   "text", # MONTH
                   "text", # DAY
                   "text", # PROGRAM
                   "text", # NCDMF WATERBODY
                   "text" # NCDMF WATERBODY_DESCRIPTION
                   )

NC_coltypes_c <- c("text", # Date
                   "text", # UNIT
                   "text", # RIVER
                   "text", # SPECIES_NAME
                   "text", # STATION
                   "numeric", # FORK LENGTH (mm)
                   "numeric", # TOTAL LENGTH
                   "numeric", # WEIGHT (g)
                   "text", # SEX
                   "numeric", # FINAL SCALE AGE
                   "numeric", # FINAL REPEAT SPAWNER MARK COUNT
                   "text", # DISPOSITION
                   "text", # CONTROL4
                   "text", # GEAR
                   "numeric", # NET LENGTH (FT)
                   "numeric", # MESH_BAR_IN
                   "numeric", # NET DEPTH (FT)
                   "numeric", # WEEK
                   "numeric", # SOAK TIME (MINUTES)
                   "numeric", # Depth_M
                   "text", # WEATHER_ELM
                   "text", # WIND_DIRECTION
                   "text", # WATER LEVEL
                   "numeric", # AIRTEMP_c
                   "numeric", # SURTEMP_c
                   "numeric", # BOTTEMP_c
                   "numeric", # SURSAL_ppt
                   "numeric", # BOTSAL_ppt
                   "numeric", # SURDO_mfL
                   "numeric", # BOTDO_mgL
                   "numeric", # PH_
                   "text", # SURCOND
                   "text", # BOTCOND
                   "text", # TOW ID (CONTROL1)
                   "text", # YEAR
                   "text", # MONTH
                   "text", # DAY
                   "text", # PROGRAM
                   "text", # NCDMF WATERBODY
                   "text" # NCDMF WATERBODY_DESCRIPTION
                  )



NC_sheets_a <- c("CommBioSamples_BB",
                 "CommBioSamples_ALE")

NC_A <-
map(NC_sheets_a,
    .f = function(.x) read_excel(
      path = "data/NC/NCDMF 2023 River Herring Assessment Raw Data_Final_20220701.xlsx",
      sheet = .x,
      skip = 5,
      na = c("", "."),
      col_types = NC_coltypes_a))

NC_sheets_b <- c("FI BioSamples_135_BB",
                 "FI Biosamples_135_ALE")

NC_B <-
map2(.x = NC_sheets_b,
     .y = c(8, 7),
     .f = function(.x, .y) read_excel(
       path = "data/NC/NCDMF 2023 River Herring Assessment Raw Data_Final_20220701.xlsx",
       sheet = .x,
       skip = .y,
       col_types = NC_coltypes_b,
       na = c("", ".")))

NC_sheets_c <- c("FI BioSamples_150_BB",
                 "FI BioSamples_150_ALE")

NC_C <-
map(NC_sheets_c,
    .f = function(.x) read_excel(
      path = "data/NC/NCDMF 2023 River Herring Assessment Raw Data_Final_20220701.xlsx",
      sheet = .x,
      skip = 8,
      col_types = NC_coltypes_c,
      na = c("", ".")))


# First deal with fishery dependent data (fd)


# NC_fd <-
#   NC_all[c(1,2)] %>%
#   reduce(bind_rows) %>%
#   mutate(Date = as.POSIXct(paste0(Year, "-", Month, "-", Day), format="%Y-%m-%d")) %>%
#   mutate(Location = paste(UNIT, RIVER, sep = "_")) %>%
#   mutate(AgeOtolith = NA) %>%
#   mutate(State = "NC_DMF_fd") %>%
#   select(State,
#          Date,
#          Location,
#          Species = SPECIES_NAME,
#          AgeScale = 'Final Scale Age',
#          AgeOtolith,
#          ForkLength = 'Fork length (mm)',
#          TotalLength = 'Total length (mm)',
#          Weight = 'Weight (g)',
#          Sex,
#          RepeatSpawn = 'Final Repeat Spawner Mark Count')


NC_fd_2 <-
  NC_A %>%
  reduce(bind_rows) %>%
  mutate(Date = as.POSIXct(paste0(Year, "-", Month, "-", Day), format="%Y-%m-%d")) %>%
  mutate(Location = paste(UNIT, RIVER, sep = "_")) %>%
  mutate(AgeOtolith = NA) %>%
  mutate(State = "NC_DMF_fd") %>%
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

# NC_columns <- intersect(names(NC_all[[3]]), names(NC_all[[5]]))

NC_columns <- intersect(names(NC_B[[1]]), names(NC_C[[1]]))

NC_C_temp <-
  NC_C %>%
  map(select, all_of(NC_columns)) %>%
  bind_rows()



NC_fi_b <-
  NC_B %>%
  reduce(bind_rows) %>%
  bind_rows(NC_C_temp) %>%
  mutate(Date = as.POSIXct(paste0(YEAR, "-", MONTH, "-", DAY), format="%Y-%m-%d")) %>%
  mutate(Location = paste(UNIT, RIVER, sep = "_")) %>%
  mutate(AgeOtolith = NA) %>%
  mutate(State = "NC_DMF_fi") %>%
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


# NC_fi <-
#   NC_all[c(3:6)] %>%
#   map(select, all_of(NC_columns)) %>%
#   map(mutate, YEAR = as.character(YEAR)) %>%
#   map(mutate, MONTH = as.character(MONTH)) %>%
#   map(mutate, DAY = as.character(DAY)) %>%
#   map(mutate, GEAR = as.character(GEAR)) %>%
#   map(mutate, PROGRAM = as.character(PROGRAM)) %>%
#   reduce(bind_rows) %>%
#   mutate(Date = as.POSIXct(paste0(YEAR, "-", MONTH, "-", DAY), format="%Y-%m-%d")) %>%
#   mutate(Location = paste(UNIT, RIVER, sep = "_")) %>%
#   mutate(AgeOtolith = NA) %>%
#   mutate(State = "NC_DMF_fi") %>%
#   select(State,
#          Date,
#          Location,
#          Species = SPECIES_NAME,
#          AgeScale = 'FINAL SCALE AGE',
#          AgeOtolith,
#          ForkLength = 'FORK LENGTH (mm)',
#          TotalLength = 'TOTAL LENGTH (mm)',
#          Weight = 'WEIGHT (g)',
#          Sex = SEX,
#          RepeatSpawn = 'FINAL REPEAT SPAWNER MARK COUNT')

NC_A <-
  NC_fd_2 %>%
  bind_rows(NC_fi_b) %>%
  mutate(Sex = case_when(Sex == "MALE" ~ "male",
                         Sex == "FEMALE" ~ "female",
                         is.na(Sex) ~ "unknown")) %>%
  mutate(Species = if_else(Species %in% c("ALEWIFE", "ALE"), "Alewife",
                   if_else(Species == "BLUEBACK", "Blueback",
                   "Something is wrong with species column")))


NC_WRC <-
  read_excel("data/NC/2023 River Herring Assessment Raw Data_NCWRC.xlsx",
           sheet = "FI NCWRC BioSamples",
           skip = 5,
           na = c("", "NA"),
           col_types = c("text", # Sample source: Project
                          "text", # Gear
                          "text", # watershed
                          "text", # waterbody
                          "numeric", # year
                          "date", # Date
                          "text", # site
                          "text", # sitID
                          "text", # Species
                          "numeric", # Fork length (mm)
                          "numeric", # Total length (mm)
                          "numeric", # Weight (g)
                          "numeric", # Final Otolith Age
                          "numeric", # Final Scale Age
                          "text", # Sex
                          "text", # Reader 1 Otolith Age
                          "text", # Reader 1 Scale Age
                          "text", # Reader 2 Otolith Age
                          "text", # Reader 2 Scale Age
                          "text", # Known Age
                          "numeric", # Final Repeat Spawner Mark Count
                          "numeric", # Reader 1 Repeat Spawner Mark Count
                          "numeric", # Reader 2 Repeat Spawner Mark Count
                          "numeric", # Known Repeat Spawner Mark Count
                          "text", # Disposition
                          "text" # Survey_Type
           )) %>%
  mutate(State = "NC_WRC") %>%
  mutate(Sex = case_when(Sex == "Male"~ "male",
                         Sex == "Female" ~ "female",
                         # Sex == "Juvenile" ~ "juvenile",
                         (is.na(Sex) | Sex == "Unknown" | Sex == "Juvenile") ~ "unknown")) %>%
  mutate(Species = case_when(Species == "BBH" ~ "Blueback",
                         Species == "ALE" ~ "Alewife")) %>%
  select(State,
         Date,
         Location = "watershed",
         Species,
         AgeScale = 'Final Scale Age',
         AgeOtolith = 'Final Otolith Age',
         ForkLength = 'Fork length (mm)',
         TotalLength = 'Total length (mm)',
         Weight = 'Weight (g)',
         Sex,
         RepeatSpawn = 'Final Repeat Spawner Mark Count'
  )


NC <-
  NC_A %>%
  filter(!str_detect(Location, "Other")) %>% # To match how Katie Drew did NC DMF data
  bind_rows(NC_WRC)


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
                               "numeric", # Final repeat spawner mark
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
                 skip = 5,
                 col_types = c("date", # Date
                               "text", # Location
                               "numeric", # Species Code
                               "text", # Common Name
                               "numeric", # Gear Code
                               "text", # Gear Type
                               "numeric", # Mesh Code
                               "numeric", # Stretch Mesh Size (inches)
                               "numeric", # Sex Code
                               "text", # Sex
                               "numeric", # Maturity Code
                               "text", # Maturity Description
                               "numeric", # Fork length (mm)
                               "numeric", # Total length (mm)
                               "numeric", # Weight (g)
                               "numeric", # Final Otolith Age
                               "numeric", # Final Scale Age
                               "numeric", # Reader 1 Otolith Age
                               "numeric", # Reader 1 Scale age
                               "numeric", # Reader 2 Otolith Age
                               "numeric", # Reader 2 Scale Age
                               "numeric", # Known Age
                               "numeric", # Final Repeat Spawner Mark Count
                               "numeric", # Reader 1 Repeat Spawner Mark Count
                               "numeric", # Reader 2 Repeat Spawner Mark Count
                               "numeric", # Known Repeat Spawner Mark Count
                               "text", # Disposition
                               "numeric" # Year
                               )) %>%
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
                    skip = 6,
                    col_types = c("date", # Date
                                  "text", # Location
                                  "numeric", # Species Code
                                  "text", # Species
                                  "numeric", # Gear Code
                                  "text", # Gear
                                  "numeric", # Stretch Mesh (in.)
                                  "numeric", # Sex Code
                                  "text", # Sex
                                  "numeric", # Maturity Code
                                  "text", # Maturity
                                  "numeric", # Fork length (mm)
                                  "numeric", # Total length (mm)
                                  "numeric", # Weight (g)
                                  "numeric", # Final Otolith Age
                                  "numeric", # Final Scale Age
                                  "numeric", # Reader 1 Otolith Age
                                  "numeric", # Reader 1 Scale Age
                                  "numeric", # Reader 2 Otolith Age
                                  "numeric", # Reader 2 Scale Age
                                  "numeric", # Known Age
                                  "numeric", # Final Repeat Spawner Mark Count
                                  "numeric", # Reader 1 Repeat Spawner Mark Count
                                  "numeric", # Reader 2 Repeat Spawner Mark Count
                                  "numeric", # Known Repeat Spawner Mark Count
                                  "text", # Disposition
                                  "numeric" # Year
                                  )) %>%
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
# 06 October 2023 - Use new data from files that Wes sent to Katie Drew
#                   instead of data created above.
#-------------------------------------------------------------------------------






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
# RI - data/RI/RH 2010 2015 Age Data.xlsx
#      sheet - see below (RI_sheets_a)

#      data/RI/RIDEM 2017 scales.xlsx

#      data/RI/RIDEM 2018 Scales.xlsx

#-------------------------------------------------------------------------------

RI_coltypes_a <- c("date", # Date
                   "text", # Location
                   "numeric", # Ref #
                   "numeric", # TL
                   "numeric", # FL
                   "numeric", # W
                   "text", # Sex
                   "numeric", # Age
                   "numeric" # RS
)



RI_sheets_a <- c("GS 2011",
                 "GS 2012",
                 "GS 2013",
                 "GS 2014",
                 "Non 2011",
                 "Non 2012",
                 "Non 2013",
                 "Non 2014",
                 "Non 2015")

RI_A <-
  map(RI_sheets_a,
      .f = function(.x) read_excel(
        path = "data/RI/RH 2010 2015 Age Data.xlsx",
        sheet = .x,
        skip = 0,
        na = c("", "R"),
        col_types = RI_coltypes_a)) %>%
  reduce(bind_rows) %>%
  mutate(State = "RI") %>%
  mutate(Species = "Alewife") %>%
  mutate(AgeOtolith = NA_real_) %>%
  mutate(Sex = case_when(Sex == "M" ~ "male",
                         Sex == "F" ~ "female",
                         is.na(Sex) ~ NA_character_,
                         TRUE ~ Sex)) %>%
  select(State,
         Date,
         Location,
         Species,
         AgeScale = Age,
         AgeOtolith,
         ForkLength = FL,
         TotalLength = TL,
         Weight = W,
         Sex,
         RepeatSpawn = RS)


RI_coltypes_b <- c("date", # Date
                   "text", # Site
                   "numeric", # ID#
                   "text", # Sex
                   "numeric", # TL
                   "numeric", # FL
                   "numeric", # W
                   "numeric", # Age
                   "numeric", # Spawn Mark
                   "text" # Spawn Age
)



RI_sheets_b <- c("Gilbert Stuart",
                 "Nonquit")

RI_B <-
  map(RI_sheets_b,
      .f = function(.x) read_excel(
        path = "data/RI/RIDEM 2017 scales.xlsx",
        sheet = .x,
        skip = 0,
        na = c("", "R"),
        col_types = RI_coltypes_b)) %>%
  reduce(bind_rows) %>%
  mutate(State = "RI") %>%
  mutate(Species = "Alewife") %>%
  mutate(AgeOtolith = NA_real_) %>%
  mutate(Sex = case_when(Sex == "M" ~ "male",
                         Sex == "F" ~ "female",
                         is.na(Sex) ~ NA_character_,
                         TRUE ~ Sex)) %>%
  select(State,
         Date,
         Location = Site,
         Species,
         AgeScale = Age,
         AgeOtolith,
         ForkLength = FL,
         TotalLength = TL,
         Weight = W,
         Sex,
         RepeatSpawn = 'Spawn Mark')




RI_coltypes_c <- c("text", # Site
                   "date", # Date
                   "text", # Species
                   "numeric", # RIDEM ID
                   "numeric", # TL
                   "numeric", # W
                   "numeric", # Age
                   "numeric", # FWZ
                   "text", # Mark
                   "text" # Sex
)


RI_sheets_c <- c("GS 24-APR-18",
                 "NON 24-APR-18",
                 "NON 1-MAY-18")

RI_C <-
  map(RI_sheets_c,
      .f = function(.x) read_excel(
        path = "data/RI/RIDEM 2018 Scales.xlsx",
        sheet = .x,
        skip = 0,
        na = c("", "–"),
        col_types = RI_coltypes_c)) %>%
  reduce(bind_rows) %>%
  mutate(State = "RI") %>%
  mutate(AgeOtolith = NA_real_) %>%
  mutate(Sex = case_when(Sex == "M" ~ "male",
                         Sex == "F" ~ "female",
                         is.na(Sex) ~ "unknown",
                         TRUE ~ Sex)) %>%
  mutate(RepeatSpawn = Age-FWZ) %>%
  mutate(FL = NA_real_) %>%
  select(State,
         Date,
         Location = Site,
         Species,
         AgeScale = Age,
         AgeOtolith,
         ForkLength = FL,
         TotalLength = TL,
         Weight = W,
         Sex,
         RepeatSpawn)


RI <-
  RI_A %>%
  bind_rows(RI_B) %>%
  bind_rows(RI_C)


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

SC_coltypes <-c("date", # Date
                  "text", # Location
                  "text", # Gear
                  "numeric", # Fork length (mm)
                  "numeric", # Total length (mm)
                  "numeric", # Weight (g)
                  "numeric", # Final Otolith Age
                  "numeric", # Final Scale Age
                  "numeric", # Reader 1 Otolith Age
                  "numeric", # Reader 1 Scale Age
                  "numeric", # Reader 2 Otolith Age
                  "numeric", # Reader 2 Scale Age
                  "numeric", # Known Age
                  "numeric", # Final Repeat Spawner Mark Count
                  "numeric", # Reader 1 Repeat Spawner Mark Count
                  "numeric", # Reader 2 Repeat Spanwer Mark Count
                  "numeric", # Known Repeat Spawner Mark Count
                  "text", # Disposition
                  "text" # Sex
)

SC_all <-
  SC_sheets %>%
  set_names() %>%
  map(read_excel,
      path = "data/SC/2023 River Herring Assessment Raw Data Template_SC.xlsx",
      skip = 5,
      na = c("Non-Consensus", "BAD", "bad", "toss", "bad scales", ""),
      col_types = SC_coltypes)

SC <-
  SC_all %>%
  # map(select, all_of(SC_columns)) %>%
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
  mutate(State = "SERC") %>%
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
      skip = 7,
      col_types = c("date", # Date
                    "text", # Location
                    "text", # Gear
                    "numeric", # Fork length (mm)
                    "numeric", # Total length (mm)
                    "numeric", # Weight (g)
                    "numeric", # Final Otolith Age
                    "numeric", # Final Scale Age
                    "numeric", # Reader 1 Otolith Age
                    "numeric", # Reader 1 Scale Age
                    "numeric", # Reader 2 Otolith Age
                    "numeric", # Reader 2 Scale Age
                    "numeric", # Known Age
                    "numeric", # Final Repeat Spawner Mark Count
                    "numeric", # Reader 1 Repeat Spawner Mark Count
                    "numeric", # Reader 2 Repeat Spawner Mark Count
                    "numeric", # Known Repeat Spawner Mark Count
                    "text", # Disposition
                    "numeric" # Sex (1=male, 2=female)
                    ))

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
# Get regions from data Katie Drew sent.
#-------------------------------------------------------------------------------

Alewife_regions <-
  ALE %>%
  group_by(Region, State) %>%
  filter(!(Region %in% c("Mixed stock", "Mixed Stock"))) %>%
  summarize(n = n())


ALE %>%
  filter(State == "MA") %>%
  group_by(River, Region) %>%
  summarize(n = n())



Blueback_regions <-
  BBH %>%
  group_by(Region, State) %>%
  filter(!(Region %in% c("Mixed stock", "Mixed Stock"))) %>%
  summarize(n = n())


BBH %>%
  filter(State == "MA") %>%
  group_by(River, Region) %>%
  summarize(n = n())





#-------------------------------------------------------------------------------
# Combine all state specific data sets above into one data set and write out
# to .csv
#-------------------------------------------------------------------------------


NC_WRC_SAT <- c("Cape Fear River", "Northeast Cape Fear River")
NC_WRC_MAT <- c("Chowan River", "Albermarle Sound", "Neuse River", "Tar River",
                "Roanoke River")

AllStates <-
  CT %>%
  bind_rows(MD) %>%
  bind_rows(MA_new) %>%
  bind_rows(ME_new) %>%
  # bind_rows(ME) %>%
  bind_rows(NC) %>%
  # bind_rows(NH) %>%
  # bind_rows(NY) %>%
  bind_rows(NY_new) %>%
  bind_rows(PA) %>%
  bind_rows(RI) %>%
  bind_rows(SC) %>%
  bind_rows(USFWS) %>%
  bind_rows(SERC) %>%
  bind_rows(VA) %>%
  mutate(Region = case_when(Species %in% c("Blueback", "Alewife") & State %in% c("NC_DMF_fi", "NC_DMF_fd") &
                              Location == "ALBEMARLESOUND_NOT IN RIVER" ~ "Mixed Stock",
                           # (Species == "Alewife" | Species == "unknown")
                           #   & (State %in% c("MD", "NC_DMF_fi", "NC_DMF_fd", "NJ",
                           #                   "NY_fd", "NY_fi", "PA", "SERC", "VA")) |
                           #    (State == "NC_WRC" & Location %in% NC_WRC_MAT) ~ "MAT",
                           (Species == "Alewife" | Species == "unknown")
                           & (State %in% c("MD", "NC_DMF_fi", "NC_DMF_fd", "NJ",
                                           "NY", "PA", "SERC", "VA")) |
                             (State == "NC_WRC" & Location %in% NC_WRC_MAT) ~ "MAT",
                            Species == "Alewife" & State %in% c("CT", "MA", "USFWS", "RI") ~ "SNE",
                            (Species == "Alewife" | Species == "unknown") & State %in% c("ME", "NH") ~ "NNE",
                            Species == "Blueback" & State %in% c("ME") ~ "CAN-NNE",
                            # Species == "Blueback"
                            #   & (State %in% c("CT", "MD", "NC_DMF_fi", "NC_DMF_fd", "NJ","NY_fd", "NY_fi", "PA",
                            #                "SERC", "USFWS", "VA") | (State == "NC_WRC" &
                            #                                            Location %in% NC_WRC_MAT)) ~ "MAT",
                           Species == "Blueback"
                           & (State %in% c("CT", "MD", "NC_DMF_fi", "NC_DMF_fd", "NJ", "NY", "PA",
                                           "SERC", "USFWS", "VA") | (State == "NC_WRC" &
                                                                       Location %in% NC_WRC_MAT)) ~ "MAT",
                            Species == "Blueback" & (State == "NH" | (State == "MA" & Location == "Parker")) ~ "MNE",
                            Species == "Blueback" & ((State %in% c("SC_fd", "SC_rec", "SC_fi")) |
                                                       (State %in% c("NC_WRC") &
                                                        Location %in% NC_WRC_SAT))~ "SAT",
                            Species == "Blueback" & State %in% c("MA") & Location != "Parker" ~ "SNE",
                            Species %in% c("Blueback", "Alewife") & State == "NC_WRC" &
                              !(Location %in% c(NC_WRC_MAT, NC_WRC_SAT)) ~ NA_character_,
                            TRUE ~ State)) %>%
  mutate(Year = as.integer(format(Date, format= "%Y"))) %>%
  mutate(Location = if_else(State == "NC_WRC" & is.na(Location) & is.na(Region), "Neuse River", Location)) %>%
  mutate(Region = if_else(State == "NC_WRC" & is.na(Region), "MAT", Region)) %>%
  select(-Date) %>%
  bind_rows(ALE_NH) %>%
  bind_rows(BBH_NH) %>%
  filter(AgeScale < 20 | is.na(AgeScale)) # get rid of 100 records with age of 99


write.csv(AllStates,
          file = "clean_data.csv",
          row.names = FALSE)

# now copy clean data file to estimate_mortality directory

# first get first part of path to estimate_mortality directory
dir_a <- dirname(getwd())

dir_b <- "/estimate_mortality"

dir <- paste0(dir_a, dir_b)

file.copy(from = "clean_data.csv", to = dir, overwrite = TRUE)


#-------------------------------------------------------------------------------
