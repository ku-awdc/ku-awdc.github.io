### A script file to read lameness data, 2023-01-23
library("tidyverse")
library("readxl")

# This line will show a different path for you, and won't be commented out!
# setwd("~/Documents/DataScience/ErasmusPlusCourse")


## Read the sheets available:

filename <- "solution_topic_1.xlsx"
sheets <- excel_sheets(filename)


## Read and check the farm data:

farm_data <- read_excel(filename, sheet="farm_data")
str(farm_data)

farm_data$HerdType <- factor(farm_data$HerdType)
farm_data$MilkSystem <- factor(farm_data$MilkSystem)
farm_data$VisitDate <- as.Date(farm_data$VisitDate)

str(farm_data)
summary(farm_data)


## Read and check the visit data:

visit_data <- read_excel(filename, sheet="visit_data")
str(visit_data)

visit_data$GaitScore <- factor(visit_data$GaitScore, levels=c(0,1,2,3,4))
str(visit_data)
summary(visit_data)


## Read and check the registry_animal data:

registry_animal <- read_excel(filename, sheet="registry_animal")
str(registry_animal)

registry_animal$DOB <- as.Date(registry_animal$DOB)
registry_animal$Breed <- factor(registry_animal$Breed)

str(registry_animal)
summary(registry_animal)


## Read and check the registry_calving data:

registry_calving <- read_excel(filename, sheet="registry_calving")
str(registry_calving)

registry_calving$CalvingDate <- as.Date(registry_calving$CalvingDate)
str(registry_calving)
summary(registry_calving)


## Read and check the registry_milk data:

registry_milk <- read_excel(filename, sheet="registry_milk")
str(registry_milk)

registry_milk$MilkDate <- as.Date(registry_milk$MilkDate)
str(registry_milk)
summary(registry_milk)


## Read and check the lookup tables:

breed_lookup <- read_excel(filename, sheet="breed_lookup")
str(breed_lookup)

breed_lookup$Breed <- factor(breed_lookup$Breed)
str(breed_lookup)
summary(breed_lookup)


gait_lookup <- read_excel(filename, sheet="gait_lookup")
str(gait_lookup)

gait_lookup$GaitScore <- factor(gait_lookup$GaitScore, levels=c(0,1,2,3,4))
str(gait_lookup)
summary(gait_lookup)


## Finished!
