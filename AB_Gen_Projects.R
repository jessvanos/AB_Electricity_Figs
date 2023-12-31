################################################################################
# TITLE: AB_Gen_Projects
#
# DESCRIPTION:  Plots not directly related to simulation and modeling work. 
# Includes AESO planning area plots data and more.
#
#
# AUTHOR: Jessica Van Os
# CONTACT: jvanos@ualberta.ca
# CREATED: May 2023; 
#
# NOTES: Make sure the project file is open first or "here" commands wont work right.
#        Before running, create folder called "Data Files/AESO Planning Locations" inside project directory 
#        and populate it geographical data and project information. 
################################################################################

################################################################################
## LOAD REQUIRED PACKAGES AND FUNCTIONS
################################################################################
library(here)
source(here('Functions','Other_Functions.R'))       # Other functions used in plotting functions
source(here('Functions','Map_Functions.R')) 

# Packages required
packs_to_load = c("tidyverse","ggplot2","scales","grid","gtable","gridExtra","odbc","ggpubr",
                  "DBI","lubridate","cowplot","scales","dplyr","reshape2","zoo",
                  "ggpattern","here","beepr","showtext","DescTools","pivottabler",
                  "openxlsx","sqldf","timeDate","writexl","viridis","ggnewscale","sf","broom")

# Function to check for packages, install if not present, and load
packs_check(packs_to_load)

################################################################################
## BRING IN DATA
################################################################################
# Read Map Data from AESO 
# Data source:https://www.aeso.ca/market/market-and-system-reporting/data-requests/planning-area-boundary-data/
PlanningMapData<-st_read(dsn= here("Data Files","AESO Planning Locations","AESO_Planning_Areas.shp"))

# Data from excel to merge with rest
# Data Source: https://www.aeso.ca/market/market-and-system-reporting/long-term-adequacy-metrics/
    # Save new data
    #  saveRDS(August_2023_AESO_planning_analytics,file=here("Data Files","AESO Planning Locations","Aug_2023_AESO_Planning_Data.RData"))

# Read in the saved data
AllProjectsData<-readRDS(here("Data Files","AESO Planning Locations","Aug_2023_AESO_Planning_Data.RData"))%>%
  subset(., select = c("Area_ID","Solar_Capacity","Wind_Capacity","Storage_Capacity","Gas_Capacity"))

AllProjectsData_ExclAnnounced<-readRDS(here("Data Files","AESO Planning Locations","Aug_2023_AESO_Planning_Data.RData"))%>%
  subset(., select = c("Area_ID","Solar_Capacity_Const&ApprovedOnly","Wind_Capacity_Const&ApprovedOnly",
                       "Storage_Capacity_Const&ApprovedOnly","Gas_Capacity_Const&ApprovedOnly"))

# Merge together
PlanMapDataCombined<-merge(PlanningMapData,AllProjectsData,by="Area_ID")%>%
  rename("Solar"="Solar_Capacity",
         "Wind"="Wind_Capacity",
         "Storage"="Storage_Capacity",
         "Gas"="Gas_Capacity")

PlanMapDataCombined_ExclAnnounced<-merge(PlanningMapData,AllProjectsData_ExclAnnounced,by="Area_ID")%>%
  rename("Solar"="Solar_Capacity_Const&ApprovedOnly",
         "Wind"="Wind_Capacity_Const&ApprovedOnly",
         "Storage"="Storage_Capacity_Const&ApprovedOnly",
         "Gas"="Gas_Capacity_Const&ApprovedOnly")

################################################################################
## PLOT ON MAPS
################################################################################
# Create folder name to save as
  CaseName <- "August 2023 Generation Projects"
  
# Make window
# Save all full size images
  windows(10,12,buffered=FALSE)

# SOLAR  
  # Plot Solar
    ProjectMap_bytype(PlanMapDataCombined,"Solar","all")
    SaveIm(CaseName,"All August 2023 Solar Projects")
    
  # Plot Solar excluding announced 
    ProjectMap_bytype(PlanMapDataCombined_ExclAnnounced,"Solar","ExclAnnounced")
    SaveIm(CaseName,"August 2023 Solar Projects, under construction and recieved AUC approval only")

# WIND  
  # Plot Wind
    ProjectMap_bytype(PlanMapDataCombined,"Wind","all")
    SaveIm(CaseName,"August 2023 Wind Projects")
    
  # Plot Wind excluding announced 
    ProjectMap_bytype(PlanMapDataCombined_ExclAnnounced,"Wind","ExclAnnounced")
    SaveIm(CaseName,"August 2023 Wind Projects, under construction and recieved AUC approval only")
    
# STORAGE  
  # Plot Storage
    ProjectMap_bytype(PlanMapDataCombined,"Storage","all")
    SaveIm(CaseName,"August 2023 Storage Projects")
    
  # Plot Storage excluding announced 
    ProjectMap_bytype(PlanMapDataCombined_ExclAnnounced,"Storage","ExclAnnounced")
    SaveIm(CaseName,"August 2023 Strorage Projects, under construction and recieved AUC approval only")
  
# GAS    
  # Plot Gas
    ProjectMap_bytype(PlanMapDataCombined,"Gas","all")
    SaveIm(CaseName,"August 2023 Gas Projects")
    
  # Plot Gas excluding announced 
    ProjectMap_bytype(PlanMapDataCombined_ExclAnnounced,"Gas","ExclAnnounced")
    SaveIm(CaseName,"August 2023 Gas Projects, under construction and recieved AUC approval only")
  