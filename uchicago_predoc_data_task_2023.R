# [2023 version] Pre-doc Data Task
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

df_types <- df_types %>%
  mutate(
    fuel_type = case_when( # add a category called fuel types with the following rules below
      str_detect(resource_type, "DSL") ~ "Other",
      str_detect(resource_type, "WIND") ~ "Wind",
      str_detect(resource_type, "SCGT90") ~ "Natural Gas",
      str_detect(resource_type, "PWRSTR") ~ "Other",
      str_detect(resource_type, "HYDRO") ~ "Other",
      str_detect(resource_type, "CCGT90") ~ "Natural Gas",
      str_detect(resource_type, "PVGR") ~ "Solar",
      str_detect(resource_type, "SCLE90") ~ "Natural Gas",
      str_detect(resource_type, "GSREH") ~ "Natural Gas",
      str_detect(resource_type, "CCLE90") ~ "Natural Gas",
      str_detect(resource_type, "CLLIG") ~ "Coal",
      str_detect(resource_type, "GSSUP") ~ "Natural Gas",
      str_detect(resource_type, "NUC") ~ "Nuclear",
      str_detect(resource_type, "RENEW") ~ "Other",
      str_detect(resource_type, "GSNONR") ~ "Natural Gas",
      .default = resource_type
    )
  )

df_types %>% summarize(unique_fuels = n_distinct(fuel_type)) # check that there are 6 distinct fuel types in df_types after the mutation

df_merged <- df_outputs %>%
  left_join(
    df_types %>%
      select(resource_name, resource_type, fuel_type), by = "resource_name"
  )

df_merged %>% summarize(unique_fuels = n_distinct(fuel_type)) # check that there are 6 distinct fuel types in df_merged now

# Q5: Based on the following definitions, use the resource type column to make a “Fuel Type” column. 
#     After doing so, merge Fuel Type and Resource Type onto ercot_resource_output.csv using Resource Name (you should end up with 6 unique values of Fuel Type).
    # • DSL - Other
    # • SCGT90 - Natural Gas
    # • WIND - Wind
    # • PWRSTR - Other
    # • HYDRO - Other
    # • CCGT90 - Natural Gas
    # • PVGR - Solar
    # • SCLE90 - Natural Gas
    # • GSREH - Natural Gas
    # • CCLE90 - Natural Gas
    # • CLLIG - Coal
    # • GSSUP - Natural Gas
    # • NUC - Nuclear
    # • GSNONR - Natural Gas
    # • RENEW - Other

output_by_day <- df_merged %>%
  mutate(day = as_date(mdy_hm(sced_time_stamp))) %>% # turn string sced_time_stamp to month, day, year, hour, minute
  group_by(day) %>% # tag each row with a day label
  summarize(total_output = sum(telemetered_net_output, na.rm = TRUE)) # sum each day's output into a total sum

ggplot(output_by_day, aes(x = day, y = total_output)) + 
         geom_line() + 
         labs(
           title = "Output summed by day",
           x = "Day",
           y = "Output per day"
         ) + 
         theme_linedraw() +
         theme(plot.title = element_text(hjust = 0.5)) + # center the title
         scale_x_date(date_breaks = "1 day", date_labels = "%b %d") + # add breaks by day
         theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) # rotate x labels by 90 deg

output_by_hour <- df_merged %>%
  mutate(hr = hour(mdy_hm(sced_time_stamp))) %>%
  group_by(hr) %>%
  summarize(total_output = sum(telemetered_net_output, na.rm = TRUE))

ggplot(output_by_hour, aes(x = hr, y = total_output)) + 
        geom_line() + 
        labs(
          title = "Output summed by hour",
          x = "Hour",
          y = "Output per hour"
        ) + 
        theme_linedraw() +
        theme(plot.title = element_text(hjust = 0.5)) + # center the title
        scale_x_continuous(breaks = 0:23) # add breaks by hour

output_by_hour_fuel <- df_merged %>%
  mutate(hr = hour(mdy_hm(sced_time_stamp))) %>%
  group_by(hr, fuel_type) %>%
  summarize(total_output = sum(telemetered_net_output, na.rm = TRUE), .groups = "drop")

ggplot(output_by_hour_fuel, aes(x = hr, y = total_output, color = fuel_type)) + 
  geom_line() + 
  labs(
    title = "Output summed by hour per fuel type",
    x = "Hour",
    y = "Output per hour per fuel type"
  ) + 
  theme_linedraw() +
  theme(plot.title = element_text(hjust = 0.5)) + # center the title
  scale_x_continuous(breaks = 0:23) # add breaks by hour

# Q6: Plot the following:
#     (a) output summed by day
#     (b) output summed by hour-of-day (hours 0-23)
#     (c) output summed by hour-of-day and by Fuel Type (the variable you defined in 5.)
#     Discuss the patterns you see in each plot.
# In the first plot, the energy output has local peaks on January 25th, February 11th, and February 17th. There is a global peak on January 31st. 
# The global minima is on February 6th, and there seems to be a sharp dropoff in early February (around the 3rd). 
# This could be caused by an ice storm hitting Texas around January 30th, which likely increased demand for energy (to heat people's homes). 
# It also seems like the local minimas are mostly happening on weekends (Jan 22, 28, 29 and Feb 4, 5, 18, 19), when people are likely out of their homes during the day. 

# In the second plot, the energy output has a global maxima at 8am, and a local maxima at 7pm. 
# The lowest energy output seems to be at around 2am, and a local minima can be observed around 3pm. 
# This seems to be consistent with the demand for energy during the winter months, as people heat their homes to start the day and as they come home in the evenings, hence the two maxima. 
# During the afternoon, the sun comes out so using less energy then is consistent with the data. 

# In the third plot, the lowest fuel output per hour is in the other category. Solar energy rises around 7am and diminishes around 6pm.
# Nuclear energy is held constant because they run at full power all the time due to high costs of power output fluctuations. 
# Energy from coal rises and peaks around around 7am and 6pm. 
# Afterwards, the energy output diminishes to a local minima around 4pm before quickly rising to another peak at 6pm.
# Wind energy starts high at midnight and gradually reduces down as the day progresses, down to its global minima at 10am. 
# From then on, it continues to rise back up before leveling off starting around 6pm until the end of the day. 
# Natural gas energy rises steadily from the start of the day to 8 am and drops to its global minima by 4pm. 
# Then it rises asymptotically again to a local maxima at 6pm before dropping back down to a local minima by midnight.  
# It seems like gas and coal rise when demand peaks and when sunlight is scarce (early mornings and dusk). It seems like in texas, the wind is stronger at night than in the day. 
