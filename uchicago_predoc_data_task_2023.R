# [2023] Pre-doc Data Task
# Source: https://uchicago.app.box.com/s/0lrni05mpgsvtgnzafp81kmnghoff8ya
# Author: Bryan Zin
# Date: Nov 2026

library(readr)
library(tidyverse)

df_types <- read_csv("Data/ercot_resource_types.csv")
df_outputs <- read_csv("Data/ercot_resource_output.csv")

df_outputs %>% summarize(unique_pairs = n_distinct(resource_name, qse))
# Q1: How many unique values does the variable Resource Name take in the data? the variable QSE? How many unique pairs of these variables are there?
# 1121 unique values for Resource Name, 194 unique values for QSE.
# 1127 unique pairs of Resource Name and QSE.

# Q2: What is a QSE? Do a quick online search for this ERCOT acronym. Provide a brief (1-3 sentences) definition for QSE as used in ERCOT’s market for electricity.
# QSE(s) are Qualifying Scheduling Entities in ERCOT (Electric Reliability Council of Texas). They are the required market middlemen between energy providers and the energy users.

is_unique <- df_outputs %>% 
  group_by(qse) %>% # assign rows by QSE
  summarize(n_vals = n_distinct(resource_name)) %>% # create a new column n_vals that returns the number of distinct resources for each QSE
  summarize(all(n_vals == 1)) %>% # boolean verification that n_vals == 1 for all rows
  pull() # takes the single column out of the 1x1 boolean dataframe and returns it as a vector 
df_outputs %>%
  group_by(qse) %>%
  summarize(n_resources = n_distinct(resource_name)) %>%
  arrange(desc(n_resources)) # sorts QSE by descending order of resources 
  
# Q3a:  Is it ever the case that a single QSE is paired to multiple resource names? 
#       What might this indicate about the relationship between QSEs and Resource Names? 
#       What are the 10 largest QSEs in terms of the number of unique Resource Names they are paired to in the data? 
# No, a single QSE can be paired to multiple resource names, such as QGOLDN being paired to AEEC_ANTLP_1 54 times. 
# This might indicate that QSEs provide the same resource to multiple different users.
# The 10 largest QSEs are:
#  1 QTENSK         173
#  2 QLUMN           75
#  3 QNRGTX          50
#  4 QCALP           44
#  5 QECNR           40
#  6 QAEN            34
#  7 QLCRA           32
#  8 QCPSE           25
#  9 QTEN23          24
# 10 QSHEL2          22

resource_is_unique <- df_outputs %>%
  group_by(resource_name) %>%
  summarize(n_vals = n_distinct(qse)) %>%
  summarize(all(n_vals == 1)) %>%
  pull()
df_outputs %>%
  group_by(resource_name) %>%
  summarize(n_qses = n_distinct(qse)) %>%
  arrange(desc(n_qses))

# Q3b:  Is it ever the case that a single Resource Name is paired to more than one QSE in the data? 
#       For how many Resource Names is this true for? 
#       Why might a single Resource Name pair with multiple QSEs in the data?
# There are 6 resource names that are paired to two QSEs each. 
# A single resource name might pair with multiple QSEs when it provides such a large output that it needs multiple QSEs to distribute its resources efficiently. 

df_types %>% summarize(unique = n_distinct(resource_type, na.rm = TRUE)) # summarize unique resource types, remove any NA values

# Q4a: How many unique, non-missing values does Resource Type take? Can you find definitions for them?
# 15 unique values for Resource Types. 
# WIND = wind turbines 
# HYDRO = hydroelectric
# NUC = nuclear

df_types %>% count(is_na = resource_type == "NA") # count the number of rows that have a resource_type called "NA"

df_types %>%
  filter(is.na(resource_type)) %>% # filter for rows with resource_type == "NA"
  select(resource_name, resource_type) # select and show the resource_name and resource_types for those filtered results

df_types <- df_types %>%
  mutate(
    resource_type = case_when( # for each resource type that is NA and has "WIND", "WND", or "SOLAR" in their resource_name, replace NA with the appropiate resource type.
      is.na(resource_type) & str_detect(resource_name, "WIND") ~ "WIND",
      is.na(resource_type) & str_detect(resource_name, "WND") ~ "WIND",
      is.na(resource_type) & str_detect(resource_name, "SOLAR") ~ "PVGR",
      .default = resource_type
    )
  )

# Q4b:  Are there any empty strings in the resource type column? 
#       Which resource names are missing their type? 
#       Can you guess what the missing values should be? Fill in the missing values with your guesses.
# Yes, there are 4 empty strings in the resource type columns.
# Their corresponding resource names are GALLOWAY_SOLAR1, ROSELAND_SOLAR3, SSPURTWO_WIND_1, and SWEETWN2_WND24.
# I would predict their missing values are PVGR, PVGR, WIND, and WIND, respectively.