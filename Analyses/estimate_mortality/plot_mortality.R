#-------------------------------------------------------------------------------
# INFO FOR CODE IN THIS FILE
#-------------------------------------------------------------------------------

# Author: Trey Mace (marvin.mace@maryland.gov)
# Date Created: 03 November 2022

# This code is for organizing and producing plots, tables, etc., for
# mortality estimates from age (and in some cases, number of
# spawning) data for the 2023 benchmark river herring stock assessment done
# through the ASMFC.

# The code for estimating these mortality rates is in estimate_mortality.R

#-------------------------------------------------------------------------------
# Load Packages, set options
#-------------------------------------------------------------------------------

library(tidyverse)
# library(glmmTMB)
library(readxl)

#-------------------------------------------------------------------------------
# Load data
#-------------------------------------------------------------------------------



region_data_a <- read.csv("Zestimates_by_Region_by_sex.csv",
                 header = TRUE) %>%
        filter(Zmethod %in% c("z_glm")) %>%
        #mutate(State_Species = paste(State, Species, sep = "_")) %>%
        mutate(Region_Species_AgeMethod_Sex = paste(Region, Species, AgeMethod, Sex, sep = "_")) %>%
        mutate(Region_Species_AgeMethod = paste(Region, Species, AgeMethod, sep = "_")) #%>%
        #filter(Z >= 0) %>%
        #filter(Sex != "unknown")

region_data_year_blank <-
  region_data_a %>%
  expand(Region_Species_AgeMethod_Sex, Year) %>%
  mutate(Region_Species_AgeMethod = paste(str_split_fixed(Region_Species_AgeMethod_Sex, pattern = "_", n = 4)[,1],
                                          str_split_fixed(Region_Species_AgeMethod_Sex, pattern = "_", n = 4)[,2],
                                          str_split_fixed(Region_Species_AgeMethod_Sex, pattern = "_", n = 4)[,3],
                                          sep ="_")) %>%
  mutate(Sex = str_split_fixed(Region_Species_AgeMethod_Sex, pattern = "_", n = 4)[,4]) %>%
  mutate(Species = str_split_fixed(Region_Species_AgeMethod_Sex, pattern = "_", n = 4)[,2]) %>%
  mutate(AgeMethod = str_split_fixed(Region_Species_AgeMethod_Sex, pattern = "_", n = 4)[,3]) %>%
  mutate(Region = str_split_fixed(Region_Species_AgeMethod_Sex, pattern = "_", n = 4)[,1])

region_data <-
  region_data_a %>%
  full_join(region_data_year_blank, by = c("Year", "Region_Species_AgeMethod_Sex",
                                           "Sex", "Species", "AgeMethod",
                                           "Region_Species_AgeMethod", "Region")) %>%
  filter(Sex != "unknown")


region_data_a <- read.csv("Zestimates_by_Region.csv",
                          header = TRUE) %>%
  filter(Zmethod %in% c("z_glm")) %>%
  #mutate(State_Species = paste(State, Species, sep = "_")) %>%
  #mutate(Region_Species_AgeMethod_Sex = paste(Region, Species, AgeMethod, Sex, sep = "_")) %>%
  mutate(Region_Species_AgeMethod = paste(Region, Species, AgeMethod, sep = "_"))


region_data_year_blank <-
  region_data_a %>%
  expand(Region_Species_AgeMethod, Year) %>%
  mutate(Region_Species_AgeMethod = paste(str_split_fixed(Region_Species_AgeMethod, pattern = "_", n = 3)[,1],
                                          str_split_fixed(Region_Species_AgeMethod, pattern = "_", n = 3)[,2],
                                          str_split_fixed(Region_Species_AgeMethod, pattern = "_", n = 3)[,3],
                                          sep ="_")) %>%
  # mutate(Sex = str_split_fixed(Region_Species_AgeMethod_Sex, pattern = "_", n = 4)[,4]) %>%
  mutate(Species = str_split_fixed(Region_Species_AgeMethod, pattern = "_", n = 4)[,2]) %>%
  mutate(AgeMethod = str_split_fixed(Region_Species_AgeMethod, pattern = "_", n = 4)[,3]) %>%
  mutate(Region = str_split_fixed(Region_Species_AgeMethod, pattern = "_", n = 4)[,1])

region_data <-
  region_data_a %>%
  full_join(region_data_year_blank, by = c("Year", "Species", "AgeMethod",
                                           "Region_Species_AgeMethod", "Region")) #%>%
  #filter(Sex != "unknown")



# river level combined all sexes

river_data_a <-
  read.csv("Zestimates_by_River.csv",
                          header = TRUE,
           stringsAsFactors = FALSE) %>%
  filter(Zmethod %in% c("z_glm")) %>%
  filter(!is.na(Location)) %>%
  mutate(Region_State_Species = paste(Region, State, Species, sep = "_")) %>%
  mutate(Region_State_Species_AgeMethod = paste(Region, State, Species, AgeMethod, sep = "__")) %>%
  mutate(Region_State_Location_Species_AgeMethod = paste(Region, State, Location, Species, AgeMethod, sep = "__"))


river_data_year_blank <-
  river_data_a %>%
  expand(Region_State_Location_Species_AgeMethod, Year) %>%
  mutate(Region_State_Location_Species_AgeMethod = paste(str_split_fixed(Region_State_Location_Species_AgeMethod, pattern = "__", n = 5)[,1],
                                          str_split_fixed(Region_State_Location_Species_AgeMethod, pattern = "__", n = 5)[,2],
                                          str_split_fixed(Region_State_Location_Species_AgeMethod, pattern = "__", n = 5)[,3],
                                          str_split_fixed(Region_State_Location_Species_AgeMethod, pattern = "__", n = 5)[,4],
                                          str_split_fixed(Region_State_Location_Species_AgeMethod, pattern = "__", n = 5)[,5],
                                          sep ="__")) %>%
  mutate(Region_State_Species_AgeMethod = paste(str_split_fixed(Region_State_Location_Species_AgeMethod, pattern = "__", n = 5)[,1],
                                                str_split_fixed(Region_State_Location_Species_AgeMethod, pattern = "__", n = 5)[,2],
                                                str_split_fixed(Region_State_Location_Species_AgeMethod, pattern = "__", n = 5)[,4],
                                                str_split_fixed(Region_State_Location_Species_AgeMethod, pattern = "__", n = 5)[,5],
                                                      sep = "__")) %>%
  # mutate(Sex = str_split_fixed(Region_Species_AgeMethod_Sex, pattern = "_", n = 4)[,4]) %>%
  mutate(Species = str_split_fixed(Region_State_Location_Species_AgeMethod, pattern = "__", n = 5)[,4]) %>%
  mutate(AgeMethod = str_split_fixed(Region_State_Location_Species_AgeMethod, pattern = "__", n = 5)[,5]) %>%
  mutate(Region = str_split_fixed(Region_State_Location_Species_AgeMethod, pattern = "__", n = 5)[,1]) %>%
  mutate(State = str_split_fixed(Region_State_Location_Species_AgeMethod, pattern = "__", n = 5)[,2]) %>%
  mutate(Location = str_split_fixed(Region_State_Location_Species_AgeMethod, pattern = "__", n = 5)[,3])

river_data <-
  river_data_a %>%
  full_join(river_data_year_blank, by = c("Year", "Species", "AgeMethod",
                                           "Region_State_Location_Species_AgeMethod",
                                           "Region", "State", "Location",
                                          "Region_State_Species_AgeMethod"))


# river level separate sexes

river_data_a <-
  read.csv("Zestimates_by_River_by_sex.csv",
           header = TRUE,
           stringsAsFactors = FALSE) %>%
  filter(Zmethod %in% c("z_glm")) %>%
  filter(!is.na(Location)) %>%
  mutate(Region_State_Species = paste(Region, State, Species, sep = "_")) %>%
  mutate(Region_State_Species_AgeMethod = paste(Region, State, Species, AgeMethod, sep = "__")) %>%
  mutate(Region_State_Location_Species_Sex_AgeMethod = paste(Region, State, Location, Species, Sex, AgeMethod, sep = "__"))


river_data_year_blank <-
  river_data_a %>%
  expand(Region_State_Location_Species_Sex_AgeMethod, Year) %>%
  mutate(Region_State_Location_Species_Sex_AgeMethod = paste(str_split_fixed(Region_State_Location_Species_Sex_AgeMethod, pattern = "__", n = 6)[,1],
                                                         str_split_fixed(Region_State_Location_Species_Sex_AgeMethod, pattern = "__", n = 6)[,2],
                                                         str_split_fixed(Region_State_Location_Species_Sex_AgeMethod, pattern = "__", n = 6)[,3],
                                                         str_split_fixed(Region_State_Location_Species_Sex_AgeMethod, pattern = "__", n = 6)[,4],
                                                         str_split_fixed(Region_State_Location_Species_Sex_AgeMethod, pattern = "__", n = 6)[,5],
                                                         str_split_fixed(Region_State_Location_Species_Sex_AgeMethod, pattern = "__", n = 6)[,6],
                                                         sep ="__")) %>%
  mutate(Region_State_Species_AgeMethod = paste(str_split_fixed(Region_State_Location_Species_Sex_AgeMethod, pattern = "__", n = 6)[,1],
                                                str_split_fixed(Region_State_Location_Species_Sex_AgeMethod, pattern = "__", n = 6)[,2],
                                                str_split_fixed(Region_State_Location_Species_Sex_AgeMethod, pattern = "__", n = 6)[,4],
                                                str_split_fixed(Region_State_Location_Species_Sex_AgeMethod, pattern = "__", n = 6)[,6],
                                                sep = "__")) %>%
  # mutate(Region_State_Species_AgeMethod = paste(str_split_fixed(Region_State_Location_Species_Sex_AgeMethod, pattern = "__", n = 6)[,1],
  #                                                   str_split_fixed(Region_State_Location_Species_Sex_AgeMethod, pattern = "__", n = 6)[,2],
  #                                                   str_split_fixed(Region_State_Location_Species_Sex_AgeMethod, pattern = "__", n = 6)[,4],
  #                                                   str_split_fixed(Region_State_Location_Species_Sex_AgeMethod, pattern = "__", n = 6)[,6],
  #                                                   sep = "__")) %>%
  # mutate(Sex = str_split_fixed(Region_Species_AgeMethod_Sex, pattern = "_", n = 4)[,4]) %>%
  mutate(Species = str_split_fixed(Region_State_Location_Species_Sex_AgeMethod, pattern = "__", n = 6)[,4]) %>%
  mutate(AgeMethod = str_split_fixed(Region_State_Location_Species_Sex_AgeMethod, pattern = "__", n = 6)[,6]) %>%
  mutate(Region = str_split_fixed(Region_State_Location_Species_Sex_AgeMethod, pattern = "__", n = 6)[,1]) %>%
  mutate(State = str_split_fixed(Region_State_Location_Species_Sex_AgeMethod, pattern = "__", n = 6)[,2]) %>%
  mutate(Location = str_split_fixed(Region_State_Location_Species_Sex_AgeMethod, pattern = "__", n = 6)[,3]) %>%
  mutate(Sex = str_split_fixed(Region_State_Location_Species_Sex_AgeMethod, pattern = "__", n = 6)[,5])


river_data <-
  river_data_a %>%
  full_join(river_data_year_blank, by = c("Year", "Species", "AgeMethod", "Sex",
                                          "Region_State_Location_Species_Sex_AgeMethod",
                                          "Region", "State", "Location",
                                          "Region_State_Species_AgeMethod")) %>%
  filter(Sex != "unknown")











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

# Region data separate sexes

Region_Species_AgeMethod <- unique(region_data$Region_Species_AgeMethod)

Max_total_years <-
  region_data %>%
  group_by(Region_Species_AgeMethod) %>%
  mutate(TotalYears = max(Year) - min(Year)) %>%
  ungroup() %>%
  filter(TotalYears == max(TotalYears))

my_breaks <- c(min(Max_total_years$Year, na.rm = TRUE):max(Max_total_years$Year, na.rm = TRUE))

pdf("region_mortality_plots_by_sex.pdf", width = 7, height = 6)

for(i in Region_Species_AgeMethod)
{

  sub_data <- subset(region_data, Region_Species_AgeMethod == i)

  sub_species <- unique(sub_data$Species)
  sub_region <- unique(sub_data$Region)
  sub_agemethod <- unique(sub_data$AgeMethod)

  title_temp <- paste(sub_region, sub_species, sub_agemethod, sep = " ")

  my_labels <- ifelse(my_breaks %% 5 == 0, my_breaks, "")

  Z_spr_temp <- subset(Z_spr, Species == sub_species & Region == sub_region)$Z40

zplot <-
ggplot(sub_data, aes(x = Year, y = Z, fill = Sex)) +
  geom_errorbar(aes(ymin=Z-2*Zse, ymax=Z+2*Zse), width=1,
                position=position_dodge(0.3)) +
  geom_point(aes(shape = Sex, fill = Sex), position=position_dodge(0.3), size = 3) +
  coord_cartesian(ylim = c(0, 3)) +
  scale_shape_manual(values=c(21, 22)) +
  scale_fill_manual(values = c("grey", "black")) +
    scale_x_continuous(breaks = my_breaks,
                     labels = my_labels) +
    labs(y = "Z (Instantaneous mortality rate)",
         x = "",
         title = title_temp) +
  geom_hline(yintercept = Z_spr_temp, linetype = "dashed", color = "black") +
  theme_bw() #+

print(zplot)

}
dev.off()







# Region data sexes combined

Region_Species_AgeMethod <- unique(region_data$Region_Species_AgeMethod)

Max_total_years <-
  region_data %>%
  group_by(Region_Species_AgeMethod) %>%
  mutate(TotalYears = max(Year) - min(Year)) %>%
  ungroup() %>%
  filter(TotalYears == max(TotalYears))

my_breaks <- c(min(Max_total_years$Year, na.rm = TRUE):max(Max_total_years$Year, na.rm = TRUE))

pdf("region_mortality_plots.pdf", width = 7, height = 6)

for(i in Region_Species_AgeMethod)
{

  sub_data <- subset(region_data, Region_Species_AgeMethod == i)

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
         x = "",
         title = title_temp) +
    geom_hline(yintercept = Z_spr_temp, linetype = "dashed", color = "black") +
    theme_bw()

  print(zplot)

}
dev.off()




# River data sexes combined

Region_State_Species_AgeMethod <- unique(river_data$Region_State_Species_AgeMethod)

Max_total_years <-
  river_data %>%
  group_by(Region_State_Location_Species_AgeMethod) %>%
  mutate(TotalYears = max(Year) - min(Year)) %>%
  ungroup() %>%
  filter(TotalYears == max(TotalYears))

my_breaks <- c(min(Max_total_years$Year, na.rm = TRUE):max(Max_total_years$Year, na.rm = TRUE))

pdf("river_mortality_plots.pdf", width = 7, height = 6)

for(i in Region_State_Species_AgeMethod)
{

  sub_data <- subset(river_data, Region_State_Species_AgeMethod == i)

  sub_species <- unique(sub_data$Species)
  sub_region <- unique(sub_data$Region)
  sub_state <- unique(sub_data$State)
  sub_agemethod <- unique(sub_data$AgeMethod)

  title_temp <- paste(sub_region, sub_state, sub_species, sub_agemethod, sep = " ")

  my_labels <- ifelse(my_breaks %% 10 == 0, my_breaks, "")

  Z_spr_temp <- subset(Z_spr, Species == sub_species & Region == sub_region)$Z40

  zplot <-
    ggplot(sub_data, aes(x = Year, y = Z)) +
    geom_errorbar(aes(ymin=Z-2*Zse, ymax=Z+2*Zse), width=1) +
    geom_point(size = 1) +
    coord_cartesian(ylim = c(0, 3)) +
    scale_x_continuous(breaks = my_breaks,
                       labels = my_labels) +
    labs(y = "Z (Instantaneous mortality rate)",
         x = "",
         title = title_temp) +
    geom_hline(yintercept = Z_spr_temp, linetype = "dashed", color = "black") +
    facet_wrap(~Location
    ) +
    theme_bw()

  print(zplot)

}
dev.off()




# River data sexes separate

Region_State_Species_AgeMethod <- unique(river_data$Region_State_Species_AgeMethod)

Max_total_years <-
  river_data %>%
  group_by(Region_State_Location_Species_Sex_AgeMethod) %>%
  mutate(TotalYears = max(Year) - min(Year)) %>%
  ungroup() %>%
  filter(TotalYears == max(TotalYears))

my_breaks <- c(min(Max_total_years$Year, na.rm = TRUE):max(Max_total_years$Year, na.rm = TRUE))

pdf("river_mortality_plots_by_sex.pdf", width = 7, height = 6)

for(i in Region_State_Species_AgeMethod)
{

  sub_data <- subset(river_data, Region_State_Species_AgeMethod == i)

  sub_species <- unique(sub_data$Species)
  sub_region <- unique(sub_data$Region)
  sub_state <- unique(sub_data$State)
  sub_agemethod <- unique(sub_data$AgeMethod)

  title_temp <- paste(sub_region, sub_state, sub_species, sub_agemethod, sep = " ")

  my_labels <- ifelse(my_breaks %% 10 == 0, my_breaks, "")

  Z_spr_temp <- subset(Z_spr, Species == sub_species & Region == sub_region)$Z40

  zplot <-
    ggplot(sub_data, aes(x = Year, y = Z, fill = Sex, group = Sex)) +
    geom_errorbar(aes(ymin=Z-2*Zse, ymax=Z+2*Zse), width=1,
                  position=position_dodge(0.3)) +
    geom_point(aes(shape = Sex, fill = Sex, color = Sex), position=position_dodge(0.3), size = 3) +
    coord_cartesian(ylim = c(0, 3)) +
    scale_x_continuous(breaks = my_breaks,
                       labels = my_labels) +
    labs(y = "Z (Instantaneous mortality rate)",
         x = "",
         title = title_temp) +
    geom_hline(yintercept = Z_spr_temp, linetype = "dashed", color = "black") +
    facet_wrap(~Location
    ) +
    theme_bw()


  print(zplot)

}
dev.off()












































Zdiffs <-
data %>%
  group_by(Species, Sex, AgeMethod, Zmethod) %>%
  summarize(Mean = mean(Z, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = Sex,
              values_from = Mean) %>%
  mutate(diff = male - female)









sub_data <- subset(data, State_Species_AgeMethod == "VA_Blueback_AgeOtolith")


title_temp <- paste(sub_data$State, sub_data$Species, sub_data$AgeMethod, sep = " ")


my_breaks <- c(min(sub_data$Year, na.rm = TRUE):max(sub_data$Year, na.rm = TRUE))

my_labels <- ifelse(my_breaks %% 5 == 0, my_breaks, "")


ggplot(sub_data, aes(x = Year, y = Z, group = Zmethod, fill = Zmethod)) +
  geom_errorbar(aes(ymin=Z-Zse, ymax=Z+Zse), width=1,
                position=position_dodge(0.2)) +
  geom_point(aes(shape = Zmethod, fill = Zmethod), size = 3, position=position_dodge(0.2)) +
  scale_shape_manual(values=c(21, 22, 23, 24)) +
  #scale_fill_manual(values = rep("grey", times = 4)) +
  coord_cartesian(ylim = c(0, 3)) +
  # scale_x_discrete(breaks = as.factor(my_breaks),
  #                  labels = my_labels) +
  scale_x_continuous(breaks = my_breaks,
                     labels = my_labels) +
  labs(y = "Z (Instantaneous mortality rate)",
       x = "",
       title = title_temp) +
  scale_color_manual(name = "Z method",
                     labels = c("Chapman Robson", "Poisson GLM",
                                "Poisson GLMM", "Linear \n Regression")) +
  facet_wrap(~Location) +
  theme_bw()


#-------------------------------------------------------------------------------



data_spawn %>%

  group_by(Species, Sex, AgeMethod, Zmethod, RandomEffect) %>%
  summarize(Mean = mean(Z, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = c(RandomEffect),
              values_from = Mean) %>%
  mutate(diff = `1` - `2`)





State_Species_AgeMethod_Sex <- unique(data_spawn$State_Species_AgeMethod_Sex)

pdf("mortality_plots_b.pdf", width = 7, height = 7)

# for(i in States)
# {
#   for(j in Species)
#   {
for(i in State_Species_AgeMethod_Sex)
{


  # sub_data <- subset(data, State == i & Species == j)

  sub_data <- subset(data_spawn, State_Species_AgeMethod_Sex == i)

  title_temp <- paste(sub_data$State, sub_data$Species, sub_data$AgeMethod, sub_data$Sex, sep = " ")

  my_breaks <- c(min(sub_data$Year, na.rm = TRUE):max(sub_data$Year, na.rm = TRUE))

  my_labels <- ifelse(my_breaks %% 5 == 0, my_breaks, "")


  zplot <-
    #ggplot(sub_data, aes(x = Year, y = Z, group = RandomEffect, fill = Zmethod)) +
    ggplot(sub_data, aes(x = Year, y = Z, group = as.factor(RandomEffect))) +
    geom_errorbar(aes(ymin=Z-Zse, ymax=Z+Zse), width=1,
                  position=position_dodge(0.2)) +
    geom_point(aes(shape = as.factor(RandomEffect), fill = as.factor(RandomEffect)), size = 3, position=position_dodge(0.2)) +
    scale_shape_manual(values=c(21, 22)) +
    #scale_fill_manual(values = rep("grey", times = 4)) +
    coord_cartesian(ylim = c(0, 3)) +
    # scale_x_discrete(breaks = as.factor(my_breaks),
    #                  labels = my_labels) +
    scale_x_continuous(breaks = my_breaks,
                       labels = my_labels) +
    labs(y = "Z (Instantaneous mortality rate)",
         x = "",
         title = title_temp) +
    facet_wrap(~Location) +
    theme_bw()

  print(zplot)

  #   }
  # }
}
dev.off()


#-------------------------------------------------------------------------------


# compare estimates from age versus previous spawning

hh <-
data %>%
  right_join(data_a, by = c("State", "Year", "Location", "Species", "Sex", "Zmethod")) %>%
  filter(Z.y > 0.01) %>%
  filter(!is.na(Z.x)) %>%
  mutate(Zdiff = Z.y - Z.x) %>%
  group_by(State, Location, Species, Sex) %>%
  mutate(ID = cur_group_id()) %>%
  ungroup()

























mean(hh$Zdiff)

  group_by(State, Year, Location, Species, Sex) %>%
  summarize(Z.y.Mean = mean(Z.y),
            Z.x.Mean = mean(Z.x)) %>%
  ungroup()





