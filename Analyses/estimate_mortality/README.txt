The files in this directory contain total mortality estimates for river herring
that were estimated as part of the 2023 Benchmark Stock Assessment through the
ASFMC. (These files with the estimates will eventually be put up on github)

This directory also contains files that have code that was used to estimate
total mortality for the 2023 Benchmark Stock Assessment through the
ASFMC.

Code used to calculate estimates is available at:
https://github.com/mmace3/RiverHerring/tree/main/Analyses/estimate_mortality

There are 6 data files in this directory (not added to github yet).

Each file has up to 10 columns:

Region:    (character) Region location for where mortality estimate is from
State:     (character) State where data for mortality estimate is from
Year:      (integer)   Year for mortality estimate
Location:  (character) Specific geographic location in state where data for mortality
                       estimate if from
Species:   (character) Blueback or Alewife
Sex:       (character) female, male, or unknown
AgeMethod: (character) structure used to age fish. AgeScale = Scale, AgeOtolith = otolith
Zmethod:   (character) method used to calculate mortality. z_glm = poisson regression
Z:         (real number) total instantaneous mortality estimate
Zse:       (real number) standard error of Z. From poisson regression.

The six files are:

Zestimates_by_Region.csv
    total mortality estimates by region, year, and species

Zestimates_by_Region_by_sex.csv
    total mortality estimates by region, year, species, and sex

Zestimates_by_River.csv
    total mortality esimates by river, year, and species

Zestimates_by_River_by_sex.csv
    total mortality estimates by river, year, species, and sex

Zestimates_by_cohort_by_region.csv
    total mortality estimates calculated by following cohorts over time.
    Estimates are by region, year, and species

Zestimates_by_cohort_by_river.csv
    total mortality estimates calculated by following cohorts over time.
    Estimates are by river, year, and species

The files that contain code to estimate mortality are:

estimate_mortality.R
estimate_mortality_cohort.R
plot_mortality.R
plot_mortality_cohort.R

NOTES:

06 October 2023 - Uploaded new versions of all six files after 
                  including modified data from NY and data from Maine
                  that has rivers separated instead of all grouped
                  together.

30 Jan 2024 - Uploaded new versions of all six files after
              exluding all records where sex was "unknown" and
              excluding data from SERC. Also, new records were added
              in from New Hampshire where quite a few records with
              only length data were converted to age.