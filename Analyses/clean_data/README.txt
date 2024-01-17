The files in this directory contain code for cleaning data that is 
subsequently used for estimating total mortality for river herring 
as part of the 2023 Benchmark Stock Assessment through the ASFMC.

There are 2 files in this directory:

check_data.R
  This file checks the data file created in clean_data.R against
  the data files created by Katie Drew to make sure the numbers
  at age are the same between the files. We both wrote code to
  create a clean data set from the raw data sets.
clean_data.R
  This file contains code to take the raw data and return a cleaned
  data file that can be used in subesequent analyses.



Notes:
clean_data_old_a is a file with the clean data I was using for the Z estimates before I got
updated from Wes for NY and I switched to using the Maine data that Katie compiled. The
Maine data Katie compiled has the specific rivers listed and not grouped together. The
updated NY data from Wes has some additional data from recent years.


17 Jan 2024
clean_data_old_b is a file with clean data I was using after clean_data_old_a (see above).
The newest update for clean_data now has modified New Hampshire data. I took the NH
data compiled by Katie and added it to my data and removed my NH data. She added in
more ages by using an age-length key to convert records with only lengths to ages. This
change in NH data should be the only difference between clean_data_old_b and clean_data.