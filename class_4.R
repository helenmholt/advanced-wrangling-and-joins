#---
#title: Advanced Wrangling and Joins
#author: Helen Holt
#date: 09/11/2025
#output: html_document
#---

#This week we will continue wrangling data in the Tidyverse.  The following are some very common functions you will use in your everyday data world.

#- merge()
#- mutate()
#- case_when()
#- group_by()
#- summarize()

#load libraries
library(tidyverse)

#All the exercises for this week use the "PSU_Fish_Density.csv" and the "taxonomic_data.csv" data sets.
PSU_Fish_Density <- read.csv("data/PSU_Fish_Density.csv")

taxonomic_data <- read.csv("data/taxonomic_data.csv")

#1. Add the common name (only common name) of each species in the density data set as a new field

PSU_Fish_Density <- left_join(PSU_Fish_Density, taxonomic_data, by = "SPECIES_CD")

#- Reorder the columns to be more readable.

#use arrange()


#2. Add a new field to the density data set named "fishery_target" at set all to TRUE
PSU_Fish_Density <- mutate(PSU_Fish_Density, fishery_target = TRUE)

#3. Add a new field named "group" at set the value based on fish species where...
#- grouper = Coney, Red hind, 
#- snapper = Mutton, Gray, Yellowtail
#- parrotfish = Stoplight
#- other = Triggerfish, Hogfish

#find the common names used
unique (PSU_Fish_Density$COMNAME)

PSU_FD_group <- PSU_Fish_Density %>%
  mutate(group = case_when(
    COMNAME %in% c("coney","red hind") ~ "grouper",
    COMNAME %in% c("mutton snapper","gray snapper","yellowtail snapper") ~ "snapper",
    COMNAME %in% c("stoplight parrotfish") ~ "parrotfish",
    COMNAME %in% c("queen triggerfish","hogfish") ~ "other"))

#4. Using the group_by function, how many unique PRIMARY_SAMPLE_UNIT were sampled in each YEAR?
unique_sample <- PSU_FD_group %>%
  group_by(YEAR) %>% 
  summarize( n = n_distinct(PRIMARY_SAMPLE_UNIT))
  
#5. How many unique PRIMARY_SAMPLE_UNITS were sampled in each YEAR and PROT combination?
unique_sam_yearPROT <- PSU_FD_group %>%
  group_by(YEAR,PROT) %>% 
  summarize( n = n_distinct(PRIMARY_SAMPLE_UNIT))
  
#6. How many unique PRIMARY_SAMPLE_UNITS were sampled in each YEAR, PROT and STRAT combination?
unique_sam_yearPROTSTRAT <- PSU_FD_group %>%
  group_by(YEAR,PROT,STRAT) %>% 
  summarize( n = n_distinct(PRIMARY_SAMPLE_UNIT))
  
#7. What is the difference between the following two expressions?  The summarize and mutate calls after the group_by do very different things.  In what situations would you use them?
  
#- data %>% # data here is the "PSU_Fish_Density" data set
# <br></br>
#  group_by(YEAR) %>% 
#  <br></br>
#  summarize( n = n_distinct(PRIMARY_SAMPLE_UNIT))
#use when you want a separate data frame with less columns and rows

#- data %>%
#  <br></br>
#  group_by(YEAR) %>% 
#  <br></br>
#  mutate( n = n_distinct(PRIMARY_SAMPLE_UNIT))
#use when you want the info added to the original data frame

#8. Create a new dataframe that shows mean density of each species per year...hint
#- YEAR  | SPECIES_CD  | meanDensity
#----- | ----------  | -----------
#  2017  | BAL VETU    |    0.224   
#  2019  | CEP FULV    |    0.685
Fish_Mean_Density <- PSU_FD_group %>% 
  group_by(YEAR, SPECIES_CD) %>% 
  summarize(meanDensity = mean(density))

#9. Create a new dataframe that shows mean density of each species in each PROT per year...hint
#- YEAR  | SPECIES_CD  | PROT | meanDensity
#----- | ----------  | -----|-----------
#  2017  | BAL VETU    | 0    |0.229  
#  2019  | CEP FULV    | 1    |0.308
Fish_Mean_Density_PROT <- PSU_FD_group %>% 
  group_by(YEAR, PROT, SPECIES_CD) %>% 
  summarize(meanDensity = mean(density))

#10. In the "PSU_Fish_Density.csv" data set, the PROT field refers to sites that are inside the VI National Park (PROT = 1) and sites that are outside the Park (PROT = 0).  How many of the 4 groups had higher densities inside the National Park?
Fish_Mean_Density_Park <- PSU_FD_group %>% 
  group_by(PROT, group) %>% 
  summarize(meanDensity = mean(density)) %>%
  mutate(highDensity = case_when(
    summarize (meanDensity >

                 