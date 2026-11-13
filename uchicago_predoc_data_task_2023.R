# [2023] Pre-doc Data Task
# Source: https://uchicago.app.box.com/s/0lrni05mpgsvtgnzafp81kmnghoff8ya
# Author: Bryan Zin
# Date: Nov 2026

library(readr)
library(dplyr)
df_types <- read_csv("Data/ercot_resource_types.csv")
df_outputs <- read_csv("Data/ercot_resource_output.csv")
# Q1: How many unique values does the variable Resource Name take in the data? the variable QSE?
# 15 unique values for Resource Name, 194 unique values for QSE

# Q2: What is a QSE? Do a quick online search for this ERCOT acronym. Provide a brief (1-3 sentences) definition for QSE as used in ERCOT’s market for electricity.
# QSE(s) are Qualifying Scheduling Entities in ERCOT (Electric Reliability Council of Texas). They are the required market middlemen between energy providers and the energy users.

is_unique <- df_outputs %>%
  group_by(qse) %>%
  summarize(n_vals = n_distinct(resource_name)) %>%
  summarize(all(n_vals == 1)) %>%
  pull()
df_outputs %>%
  group_by(qse) %>%
  summarize(n_resources = n_distinct(resource_name)) %>%
  arrange(desc(n_resources))
  
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

# Q3b:  Is it ever the case that a single Resource Name is paired to more than one QSE in the data? 
#       For how many Resource Names is this true for? 
#       Why might a single Resource Name pair with multiple QSEs in the data?

