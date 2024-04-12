################################################################################
# TITLE: AB_Historic_Plots
# DESCRIPTION:  Plots Canada GHG emissions.
#
#
# AUTHOR: Jessica Van Os
# CONTACT: jvanos@ualberta.ca
# CREATED: January 2024; 
################################################################################

################################################################################
## LOAD REQUIRED PACKAGES AND FUNCTIONS
################################################################################
library(here)
source(here('Functions','Other_Functions.R'))       # Other functions used in plotting functions
source(here('Functions','Map_Functions.R')) 
source(here('Functions','Historical_Functions.R')) 


# Packages required
packs_to_load = c("tidyverse","ggplot2","scales","grid","gtable","gridExtra","odbc","ggpubr",
                  "DBI","lubridate","cowplot","scales","dplyr","reshape2","zoo",
                  "ggpattern","here","beepr","showtext","DescTools","pivottabler",
                  "openxlsx","sqldf","timeDate","writexl","viridis","ggnewscale","sf","broom","readxl")

# Function to check for packages, install if not present, and load
packs_check(packs_to_load)

################################################################################
## READ DATA FROM EXCEL
################################################################################
GHG_2021 <- read_excel("Data Files/GHG_data.xlsx", 
                       sheet = "Sheet1")

GHG_total_5year <- read_excel("Data Files/GHG_data.xlsx", 
                       sheet = "Sheet2")

GHG_AB_CAN_Elec<- read_excel("Data Files/GHG_data.xlsx", 
                       sheet = "Sheet3")

GHG_allprovs<- read_excel("Data Files/GHG_data.xlsx", 
                             sheet = "Sheet4")
################################################################################
## Plot Canada & AB
################################################################################

# Emissions for AB and Canada (split electricity and total)
emissions_zone(GHG_AB_CAN_Elec)

# Total and electricity emissions split by AB and Canada only
emissions_type_2(GHG_AB_CAN_Elec)

emissions_type_prov(GHG_allprovs)

# Total and electricity emissions splot all provs
GGSave_Simple('GHG_elec_total_scaled',emissions_type_prov(GHG_allprovs),12,8)
GGSave_Simple('GHG_elec_Can',elec_emissions_prov(GHG_allprovs),5,8)

