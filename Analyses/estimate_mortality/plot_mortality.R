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


# region estimates sexes combined

region_data_raw <- read.csv("Zestimates_by_Region.csv",
                           header = TRUE) %>%
                   filter(Region != "Mixed Stock")

region_data <-
  region_data_raw %>%
  mutate(Yearf = as.factor(Year)) %>%
  group_by(Region, Species, AgeMethod) %>%
  expand(Yearf) %>%
  ungroup() %>%
  mutate(Year = as.integer(as.character(Yearf))) %>%
  select(-Yearf) %>%
  #arrange(Region, Year, Species, AgeMethod)
  full_join(region_data_raw, by = c("Region", "Species", "AgeMethod",
                                           "Year")) %>%
  arrange(Region, Species, AgeMethod, Year) %>%
  group_by(Region, Species, AgeMethod) %>%
  mutate(ID = cur_group_id()) %>%
  ungroup()

# region estimates separate sexes

region_data_by_sex_raw <-
  read.csv("Zestimates_by_Region_by_sex.csv",
           header = TRUE) %>%
  filter(Region != "Mixed Stock")

region_data_by_sex <-
  region_data_by_sex_raw %>%
  mutate(Yearf = as.factor(Year)) %>%
  group_by(Region, Species, Sex, AgeMethod) %>%
  expand(Yearf) %>%
  ungroup() %>%
  mutate(Year = as.integer(as.character(Yearf))) %>%
  select(-Yearf) %>%
  #arrange(Region, Year, Species, AgeMethod)
  full_join(region_data_by_sex_raw, by = c("Region", "Species", "Sex", "AgeMethod",
                                    "Year")) %>%
  # arrange(Region, Species, AgeMethod, Year) %>%
  group_by(Region, Species, AgeMethod) %>%
  mutate(ID = cur_group_id()) %>%
  ungroup()


# river level sexes combined

river_data_raw <-
  read.csv("Zestimates_by_River.csv",
           header = TRUE,
           stringsAsFactors = FALSE) %>%
  filter(Region != "Mixed Stock")

river_data <-
  river_data_raw %>%
  mutate(Yearf = as.factor(Year)) %>%
  group_by(Region, State, Location, Species, AgeMethod) %>%
  expand(Yearf) %>%
  ungroup() %>%
  mutate(Year = as.integer(as.character(Yearf))) %>%
  select(-Yearf) %>%
  full_join(river_data_raw, by = c("Region", "State", "Location", "Species", "AgeMethod",
                                  "Year")) %>%
  group_by(Region, State, Species, AgeMethod) %>%
  mutate(ID = cur_group_id()) %>%
  ungroup()


# river level separate sexes

river_data_by_sex_raw <-
  read.csv("Zestimates_by_River_by_sex.csv",
           header = TRUE,
           stringsAsFactors = FALSE)  %>%
  filter(Region != "Mixed Stock")

river_data_by_sex <-
  river_data_by_sex_raw %>%
  mutate(Yearf = as.factor(Year)) %>%
  group_by(Region, State, Location, Species, Sex, AgeMethod) %>%
  expand(Yearf) %>%
  ungroup() %>%
  mutate(Year = as.integer(as.character(Yearf))) %>%
  select(-Yearf) %>%
  full_join(river_data_by_sex_raw, by = c("Region", "State", "Location",
                                          "Species", "Sex", "AgeMethod", "Year")) %>%
  group_by(Region, State, Species, AgeMethod) %>%
  mutate(ID = cur_group_id()) %>%
  ungroup()

# Z reference points from SPR analysis

# Z_spr_a <- read_excel("Preliminary_Z40_ref_pts_RH_SA_2023.xlsx",
#                     sheet = "Sheet1",
#                     skip = 1,
#                     range = cell_limits(c(2, 1), c(5, 5))) %>%
#            mutate(Species = "Alewife")
#
# Z_spr_b <- read_excel("Preliminary_Z40_ref_pts_RH_SA_2023.xlsx",
#                       sheet = "Sheet1",
#                       skip = 1,
#                       range = cell_limits(c(8, 1), c(13, 5))) %>%
#            mutate(Species = "Blueback")
#
# Z_spr <- bind_rows(Z_spr_a, Z_spr_b) %>%
#          rename(Z40 = `N-Weighted Z40`) %>%
#          mutate(Region = if_else(Region == "CAN_NNE", "CAN-NNE", Region))

# 6 Feb 2024 - New reference points from Wes
Z_spr <- read.csv("Regional_Z40_ALE_BBH_V2.csv",
                  header = TRUE) %>%
         mutate(Species = if_else(Species == "ALE", "Alewife", "Blueback")) %>%
         rename(Z40 = Z.med,
                Z40L = Z_LCI,
                Z40U = Z_UCI)

#-------------------------------------------------------------------------------

# Region data separate sexes

my_breaks <- c(min(region_data_by_sex$Year, na.rm = TRUE):max(region_data_by_sex$Year, na.rm = TRUE))

pdf("region_mortality_plots_by_sex.pdf", width = 7, height = 6)

for(i in unique(region_data_by_sex$ID))
{

  sub_data <- subset(region_data_by_sex, ID == i & Sex != "unknown")

  sub_species <- unique(sub_data$Species)
  sub_region <- unique(sub_data$Region)
  sub_agemethod <- unique(sub_data$AgeMethod)


  title_temp <- paste(sub_region, sub_species, sub_agemethod, sep = " ")
  print(title_temp)
  my_labels <- ifelse(my_breaks %% 5 == 0, my_breaks, "")

  Z_spr_temp <- subset(Z_spr, Species == sub_species & Region == sub_region)$Z40
  Z40L_spr_temp <- subset(Z_spr, Species == sub_species & Region == sub_region)$Z40L
  Z40U_spr_temp <- subset(Z_spr, Species == sub_species & Region == sub_region)$Z40U

  x_min <- min(sub_data$Year)
  x_max <- max(sub_data$Year)

zplot <-
ggplot(sub_data, aes(x = Year, y = Z, group = Sex)) +
  geom_ribbon(aes(ymin = Z40L_spr_temp, ymax = Z40U_spr_temp), fill = "grey", alpha = 0.15) +
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
  # geom_hline(yintercept = Z_spr_temp, linetype = "dashed", color = "black") +
  geom_segment(aes(x = x_min, y = Z_spr_temp, xend = x_max, yend = Z_spr_temp), linetype = "dashed") +

  theme_bw()

print(zplot)

zplot_smoother <-
  zplot + geom_smooth(aes(linetype = Sex), se = FALSE)

print(zplot_smoother)

}
dev.off()


# Region data sexes combined

my_breaks <- c(min(region_data$Year, na.rm = TRUE):max(region_data$Year, na.rm = TRUE))

pdf("region_mortality_plots.pdf", width = 7, height = 6)

for(i in unique(region_data$ID))
{

  sub_data <- subset(region_data, ID == i)

  sub_species <- unique(sub_data$Species)
  sub_region <- unique(sub_data$Region)
  sub_agemethod <- unique(sub_data$AgeMethod)

  title_temp <- paste(sub_region, sub_species, sub_agemethod, sep = " ")

  my_labels <- ifelse(my_breaks %% 5 == 0, my_breaks, "")

  Z_spr_temp <- subset(Z_spr, Species == sub_species & Region == sub_region)$Z40
  Z40L_spr_temp <- subset(Z_spr, Species == sub_species & Region == sub_region)$Z40L
  Z40U_spr_temp <- subset(Z_spr, Species == sub_species & Region == sub_region)$Z40U

  x_min <- min(sub_data$Year)
  x_max <- max(sub_data$Year)

  zplot <-
    ggplot(sub_data, aes(x = Year, y = Z)) +
    geom_ribbon(aes(ymin = Z40L_spr_temp, ymax = Z40U_spr_temp), fill = "grey", alpha = 0.15) +
    geom_errorbar(aes(ymin=Z-2*Zse, ymax=Z+2*Zse), width=1) +
    geom_point(size = 3) +
    coord_cartesian(ylim = c(0, 3)) +
    scale_x_continuous(breaks = my_breaks,
                       labels = my_labels) +
    labs(y = "Z (Instantaneous mortality rate)",
         x = "",
         title = title_temp) +
    # geom_hline(yintercept = Z_spr_temp, linetype = "dashed", color = "black") +
    # geom_hline(yintercept = Z_spr_temp, linetype = "dashed", color = "black") +
    geom_segment(aes(x = x_min, y = Z_spr_temp, xend = x_max, yend = Z_spr_temp), linetype = "dashed") +
    theme_bw()

  print(zplot)

  zplot_smoother <- zplot + geom_smooth()

  plot(zplot_smoother)


}
dev.off()




# River data sexes combined

my_breaks <- c(min(river_data$Year, na.rm = TRUE):max(river_data$Year, na.rm = TRUE))

pdf("river_mortality_plots.pdf", width = 7, height = 6)

for(i in unique(river_data$ID))
{

  sub_data <- subset(river_data, ID == i)

  sub_species <- unique(sub_data$Species)
  sub_region <- unique(sub_data$Region)
  sub_state <- unique(sub_data$State)
  sub_agemethod <- unique(sub_data$AgeMethod)

  title_temp <- paste(sub_region, sub_state, sub_species, sub_agemethod, sep = " ")

  my_labels <- ifelse(my_breaks %% 10 == 0, my_breaks, "")

  Z_spr_temp <- subset(Z_spr, Species == sub_species & Region == sub_region)$Z40
  Z40L_spr_temp <- subset(Z_spr, Species == sub_species & Region == sub_region)$Z40L
  Z40U_spr_temp <- subset(Z_spr, Species == sub_species & Region == sub_region)$Z40U

  x_min <- min(sub_data$Year)
  x_max <- max(sub_data$Year)


  zplot <-
    ggplot(sub_data, aes(x = Year, y = Z)) +
    geom_ribbon(aes(ymin = Z40L_spr_temp, ymax = Z40U_spr_temp), fill = "grey", alpha = 0.15) +
    geom_errorbar(aes(ymin=Z-2*Zse, ymax=Z+2*Zse), width=1) +
    geom_point(size = 1) +
    coord_cartesian(ylim = c(0, 3)) +
    scale_x_continuous(breaks = my_breaks,
                       labels = my_labels) +
    labs(y = "Z (Instantaneous mortality rate)",
         x = "",
         title = title_temp) +
    # geom_hline(yintercept = Z_spr_temp, linetype = "dashed", color = "black") +
    geom_segment(aes(x = x_min, y = Z_spr_temp, xend = x_max, yend = Z_spr_temp), linetype = "dashed") +
    facet_wrap(~Location
    ) +
    theme_bw()

  print(zplot)

}
dev.off()




# River data sexes separate

my_breaks <- c(min(river_data_by_sex$Year, na.rm = TRUE):max(river_data_by_sex$Year, na.rm = TRUE))

pdf("river_mortality_plots_by_sex.pdf", width = 7, height = 6)

for(i in unique(river_data_by_sex$ID))
{

  sub_data <- subset(river_data_by_sex, ID == i & Sex != "unknown")

  sub_species <- unique(sub_data$Species)
  sub_region <- unique(sub_data$Region)
  sub_state <- unique(sub_data$State)
  sub_agemethod <- unique(sub_data$AgeMethod)

  title_temp <- paste(sub_region, sub_state, sub_species, sub_agemethod, sep = " ")

  my_labels <- ifelse(my_breaks %% 10 == 0, my_breaks, "")

  Z_spr_temp <- subset(Z_spr, Species == sub_species & Region == sub_region)$Z40
  Z40L_spr_temp <- subset(Z_spr, Species == sub_species & Region == sub_region)$Z40L
  Z40U_spr_temp <- subset(Z_spr, Species == sub_species & Region == sub_region)$Z40U

  x_min <- min(sub_data$Year)
  x_max <- max(sub_data$Year)


  zplot <-
    ggplot(sub_data, aes(x = Year, y = Z, fill = Sex, group = Sex)) +
    geom_ribbon(aes(ymin = Z40L_spr_temp, ymax = Z40U_spr_temp), fill = "grey", alpha = 0.15) +
    geom_errorbar(aes(ymin=Z-2*Zse, ymax=Z+2*Zse), width=1,
                  position=position_dodge(0.3)) +
    geom_point(aes(shape = Sex, fill = Sex, color = Sex), position=position_dodge(0.3), size = 3) +
    coord_cartesian(ylim = c(0, 3)) +
    scale_shape_manual(values=c(21, 22)) +
    scale_fill_manual(values = c("grey", "black")) +
    scale_x_continuous(breaks = my_breaks,
                       labels = my_labels) +
    labs(y = "Z (Instantaneous mortality rate)",
         x = "",
         title = title_temp) +
    #geom_hline(yintercept = Z_spr_temp, linetype = "dashed", color = "black") +
    geom_segment(aes(x = x_min, y = Z_spr_temp, xend = x_max, yend = Z_spr_temp), linetype = "dashed") +
    facet_wrap(~Location
    ) +
    theme_bw()


  print(zplot)

  zplot_smoother <-
    zplot + geom_smooth(aes(linetype = Sex), se = FALSE) #+

  print(zplot_smoother)


}
dev.off()


#-------------------------------------------------------------------------------
# Plots with age and scale estimates on the same plot
#-------------------------------------------------------------------------------



# Region data sexes combined

region_data_b <-
  region_data_raw %>%
  mutate(Yearf = as.factor(Year)) %>%
  group_by(Region, Species, AgeMethod) %>%
  expand(Yearf) %>%
  ungroup() %>%
  mutate(Year = as.integer(as.character(Yearf))) %>%
  select(-Yearf) %>%
  #arrange(Region, Year, Species, AgeMethod)
  full_join(region_data_raw, by = c("Region", "Species", "AgeMethod",
                                    "Year")) %>%
  arrange(Region, Species, AgeMethod, Year) %>%
  group_by(Region, Species, AgeMethod) %>%
  mutate(ID = cur_group_id()) %>%
  ungroup() %>%
  group_by(Region, Species) %>%
  mutate(ID2 = cur_group_id()) %>%
  ungroup()

my_breaks <- c(min(region_data_b$Year, na.rm = TRUE):max(region_data_b$Year, na.rm = TRUE))

pdf("region_mortality_plots_b.pdf", width = 7, height = 6)

for(i in unique(region_data_b$ID2))
{

  sub_data <- subset(region_data_b, ID2 == i)

  sub_species <- unique(sub_data$Species)
  sub_region <- unique(sub_data$Region)
  sub_agemethod <- unique(sub_data$AgeMethod)

  title_temp <- paste(sub_region, sub_species, sub_agemethod, sep = " ")

  my_labels <- ifelse(my_breaks %% 5 == 0, my_breaks, "")

  Z_spr_temp <- subset(Z_spr, Species == sub_species & Region == sub_region)$Z40

  zplot <-
    ggplot(sub_data, aes(x = Year, y = Z, group = AgeMethod)) +
    geom_errorbar(aes(ymin=Z-2*Zse, ymax=Z+2*Zse), width=1) +
    geom_point(aes(shape = AgeMethod, fill = AgeMethod), size = 3) +
    coord_cartesian(ylim = c(0, 3)) +
    scale_shape_manual(values=c(21, 22)) +
    scale_fill_manual(values = c("grey", "black")) +
    scale_x_continuous(breaks = my_breaks,
                       labels = my_labels) +
    labs(y = "Z (Instantaneous mortality rate)",
         x = "",
         title = title_temp) +
    geom_hline(yintercept = Z_spr_temp, linetype = "dashed", color = "black") +
    theme_bw()

  print(zplot)

  zplot_smoother <- zplot + geom_smooth()

  plot(zplot_smoother)


}
dev.off()


# Region data separate sexes

region_data_by_sex_b <-
  region_data_by_sex_raw %>%
  mutate(Yearf = as.factor(Year)) %>%
  group_by(Region, Species, Sex, AgeMethod) %>%
  expand(Yearf) %>%
  ungroup() %>%
  mutate(Year = as.integer(as.character(Yearf))) %>%
  select(-Yearf) %>%
  #arrange(Region, Year, Species, AgeMethod)
  full_join(region_data_by_sex_raw, by = c("Region", "Species", "Sex", "AgeMethod",
                                           "Year")) %>%
  # arrange(Region, Species, AgeMethod, Year) %>%
  group_by(Region, Species, AgeMethod) %>%
  mutate(ID = cur_group_id()) %>%
  ungroup() %>%
  group_by(Region, Species) %>%
  mutate(ID2 = cur_group_id()) %>%
  ungroup()



pdf("region_mortality_plots_by_sex_b.pdf", width = 7, height = 6)

for(i in unique(region_data_by_sex_b$ID2))
{

  sub_data <- subset(region_data_by_sex_b, ID2 == i & Sex != "unknown")

  sub_species <- unique(sub_data$Species)
  sub_region <- unique(sub_data$Region)
  sub_agemethod <- unique(sub_data$AgeMethod)

  title_temp <- paste(sub_region, sub_species, sep = " ")

  my_labels <- ifelse(my_breaks %% 5 == 0, my_breaks, "")

  Z_spr_temp <- subset(Z_spr, Species == sub_species & Region == sub_region)$Z40

  zplot <-
    ggplot(sub_data, aes(x = Year, y = Z, shape = interaction(Sex, AgeMethod),
                         fill = interaction(Sex, AgeMethod))
    ) +
    geom_errorbar(aes(ymin=Z-2*Zse, ymax=Z+2*Zse), width=1) +
    geom_point(size = 3) +
    scale_shape_manual(name= "",
                       values=c(21, 22, 23, 24),
                       labels = c("Female - Otolith", "Male - Otolith",
                                  "Female - Scale", "Male - Scale"),
                       guide = guide_legend(color = "black",
                                            shape = c(21, 22, 23, 24),
                                            fill = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF"),
                                            title = "",
                                            labels = c("d", "d", "d", "d"))
    ) +

    scale_fill_manual(values = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF"),
                      labels = c("Female - Otolith", "Male - Otolith",
                                 "Female - Scale", "Male - Scale"),
                      guide = guide_legend(color = "black",
                                           shape = c(21, 22, 23, 24),
                                           fill = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF"),
                                           title = "",
                                           labels = c("d", "d", "d", "d"))) +
    coord_cartesian(ylim = c(0, 3.5)) +
    scale_x_continuous(breaks = my_breaks,
                       labels = my_labels) +
    labs(y = "Z (Instantaneous mortality rate)",
         x = "",
         title = title_temp) +
    geom_hline(yintercept = Z_spr_temp, linetype = "dashed", color = "black") +
    # geom_smooth() +
    theme_bw()


  print(zplot)

  zplot_smoother <-
    zplot + geom_smooth(aes(linetype = Sex), se = FALSE)

  print(zplot_smoother)

}
dev.off()











  ggplot(sub_data, aes(x = Year, y = Z, shape = interaction(Sex, AgeMethod),
                       fill = interaction(Sex, AgeMethod))
  ) +
    geom_errorbar(aes(ymin=Z-2*Zse, ymax=Z+2*Zse), width=1) +
    geom_point(size = 3) +
    scale_shape_manual(name= "",
    values=c(21, 22, 23, 24),
                       labels = c("Female - Otolith", "Male - Otolith",
                                  "Female - Scale", "Male - Scale"),
                       guide = guide_legend(color = "black",
                                            shape = c(21, 22, 23, 24),
                                            fill = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF"),
                                            title = "",
                                            labels = c("d", "d", "d", "d"))
                       ) +

    scale_fill_manual(values = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF"),
                      labels = c("Female - Otolith", "Male - Otolith",
                                 "Female - Scale", "Male - Scale"),
                      guide = guide_legend(color = "black",
                                           shape = c(21, 22, 23, 24),
                                           fill = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF"),
                                           title = "",
                                           labels = c("d", "d", "d", "d"))) +
  coord_cartesian(ylim = c(0, 3.5)) +
    scale_x_continuous(breaks = my_breaks,
                       labels = my_labels) +
    labs(y = "Z (Instantaneous mortality rate)",
         x = "",
         title = title_temp) +
    geom_hline(yintercept = Z_spr_temp, linetype = "dashed", color = "black") +
    # geom_smooth() +
    theme_bw()

