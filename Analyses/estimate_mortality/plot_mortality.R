#-------------------------------------------------------------------------------
# INFO FOR CODE IN THIS FILE
#-------------------------------------------------------------------------------

# Author: Trey Mace (marvin.mace@maryland.gov)
# Date Created: 03 November 2022

# This code is for orgnazing and producing plots, tables, etc., for
# mortality estimates from age (and in some cases, number of
# spawning) data for the 2023 benchmark river herring stock assessment done
# through the ASMFC.

# The code for estimating these mortality rates is in estimate_mortality.R

#-------------------------------------------------------------------------------
# Load Packages, set options
#-------------------------------------------------------------------------------

library(tidyverse)
# library(glmmTMB)

#-------------------------------------------------------------------------------
# Load data
#-------------------------------------------------------------------------------

data <- read.csv("Zestimates.csv",
                 header = TRUE) %>%
        mutate(State_Species = paste(State, Species, sep = "_")) %>%
        mutate(State_Species_AgeMethod_Sex = paste(State, Species, AgeMethod, Sex, sep = "_"))

#-------------------------------------------------------------------------------

# States <- unique(data$State)
#
# Species <- unique(data$Species)
#
# State_Species <- unique(data$State_Species)
#
# State_Species_AgeMethod <- unique(data$State_Species_AgeMethod)

State_Species_AgeMethod_Sex <- unique(data$State_Species_AgeMethod_Sex)

pdf("mortality_plots.pdf", width = 7, height = 7)

# for(i in States)
# {
#   for(j in Species)
#   {
for(i in State_Species_AgeMethod_Sex)
{


# sub_data <- subset(data, State == i & Species == j)

  sub_data <- subset(data, State_Species_AgeMethod_Sex == i)

  title_temp <- paste(sub_data$State, sub_data$Species, sub_data$AgeMethod, sub_data$Sex, sep = " ")

my_breaks <- c(min(sub_data$Year, na.rm = TRUE):max(sub_data$Year, na.rm = TRUE))

my_labels <- ifelse(my_breaks %% 5 == 0, my_breaks, "")


zplot <-
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
  facet_wrap(~Location) +
  theme_bw()

print(zplot)

#   }
# }
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
