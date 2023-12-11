
## Copyright © 2023 Lene J. Kjær, Michael P. Ward, Anette E. Boklund, Lars E. Larsen, Charlotte K. Hjulsager, and Carsten T. Kirkeby ”

##############################################################################################
# This file is part of the Shiny app for the ENIGMA HPAI model version 1.0.   

# The ENIGMA HPAI model is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.                                   

# The ENIGMA HPAI model is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.                                                

# You should have received a copy of the GNU General Public License along with the ENIGMA HPAI model. If not, see <https://www.gnu.org/licenses/>.
##############################################################################################


# Load a bunch of libraries for preformatting the code:
library(tidyverse)
library(sf)
library(readxl)
library(surveillance)
library(shiny)
library(shinydashboard)
library(countrycode)
library(tsibble)
library(ggplot2)
library(ggpubr)
library(spdep)
library(fanplot)
library(qs)

####### 1) Download seneste infur fil til denne folder: ############

#setwd("C:/ENIGMAdata/")
setwd("C:/Users/zxd598/Documents/GitHub/ku-awdc.github.io/ENIGMA2023/")
# Find the file:
ff <- list.files(pattern="infur")
# Define filename:
filename <- ff[which.max(file.info(ff)[,"mtime"])]
# Obs! filename cannot contain stuff like (1)!...
# Safety first:
stopifnot(length(filename)==1)
# Extract just the filename:
sheetname <-basename(filename) |>
  stringr::str_extract("(.*)\\.\\w+", group = 1) 
# Read in the data:
ai_data <- readxl::read_excel(filename, sheet=sheetname)

############################ From Lenes script #################################

### RUN THE CUSTOM FUNCTIONS SCRIPT ###
source('./src/ENIGMA_custom_functions.R')

### RUN THE DATA PREPARATION SCRIPT ###
source('./src/ENIGMA_DataPrep.R') 


### NOW RUN MODEL BASED ON THR LATEST DATA, BUT STARTING IN WEEK 39 IN  2021

# get week and year for the last date of data, to create a yearweek variable to be used later in the shiny app
endDataWeek <-as.numeric(strftime(endDate, format = '%V'))
endDataYear <-as.numeric(strftime(endDate, format = '%Y'))
europe_data_weekly$week <- paste0('W', europe_data_weekly$Week)
europe_data_weekly$yearweek <- yearweek(paste0(europe_data_weekly$Year, ' ', europe_data_weekly$week))

#subset data and covariates - to start in week 39 of 2021
subset_start <- 299
subset_end <- nrow(AI_weekly)
subset_trainTest<- 312 + (endYear-2021-1)*52+ endWeek
AI_weekly1 <-  AI_weekly[subset_start:subset_end,,drop=FALSE]
AI_wet1 <-  AI_wet[subset_start:subset_end,,drop=FALSE]
AI_coast1 <-  AI_coast[subset_start:subset_end,,drop=FALSE]
start_W <- 39
start_Y <- 2021

# set min and max date of data for shiny app
mindate <- '2021-09-27'
maxdate<- endDate

### CONSTRUCTION OF CLASS STS USED IN hhh4 MODELS ###
#Parameters in the sts object are created in the Data preparation script. Observed are the counts, start is start year and sample number, frequency is number of observations per year (here weekly), and neighborhood is based on adjacency calculations of spatial polygons (here europeanCountries.sub):
AI_sts <- sts(observed = AI_weekly1, start = c(as.numeric(start_Y), as.numeric(start_W)), 
              frequency = 52,neighbourhood = europe_adjmat, 
              map = europeanCountries.sub)

# here we set the training data set - which is basically the whole data starting at the second week, as we already validated the model earlier
TRAIN <- 2:(subset_trainTest-subset_start +1)

# these variables are created to keep track of country placement and country names in the AI_sts object. They are used for plotting predictions and simulations for individual countries
districts2plot<-  which(colSums(observed(AI_sts), na.rm=T) > 50) 
districts2plot1 <- countrycode(as.character(names(districts2plot)),"iso3c","country.name")
names(districts2plot) <- districts2plot1
districts2plot <-districts2plot[order((names(districts2plot)))]
districts2plot <-setNames(c(districts2plot, 0), c(names(districts2plot), "Summed all countries"))
districts2plot2 <-names(districts2plot)

#area fraction of summed countries
area_frac <- country_area[subset_start:subset_end,,drop=FALSE]/rowSums(country_area[subset_start:subset_end,,drop=FALSE])

### FINAL MODEL FROM KJÆR ET AL. 2023###
#model with long distance transmission, random effects and seasonality in the epidemic component
final_model_base <- list(end = list(f = ~1 + ri(type = "iid") - 1,offset=area_frac), ar = list(f=addSeason2formula( ~ 1 +t+ ri(type = "iid") - 1, S=2, period= AI_sts@freq)), 
                         ne = list(f = ~1 + ri(type = "iid") - 1, weights = W_powerlaw(maxlag = max(neighbourhood(AI_sts),log = TRUE,normalize = TRUE, from0 = TRUE))), 
                         family = "NegBin1",optimizer = list(stop = list(tol=1e-5, niter=500),
                                                             regression = list(method="nlminb"),
                                                             variance = list(method="Nelder-Mead")),subset = TRAIN, keep.terms = TRUE)
final_model <-hhh4(stsObj = AI_sts,control = final_model_base)

################################################################################


# Save the date:
save_date <- Sys.Date()
# Save the data in an (arbitrarily names file format) ".car"  format:
# We set the wd temporarily to tmp, to save a ".car" file:
setwd("C:/Users/zxd598/Documents/GitHub/ku-awdc.github.io/ENIGMA2023/tmp")
qs::qsave(list(ai_data=ai_data, 
               save_date=save_date, 
               filename=filename,
               endDate=endDate,
               updateDate=updateDate,
               europe_data_weekly=europe_data_weekly,
               AI_weekly=AI_weekly,
               AI_weekly1=AI_weekly1,
               AI_wet=AI_wet,
               AI_coast=AI_coast,                
               europe_adjmat=europe_adjmat,
               europeanCountries.sub=europeanCountries.sub,
               europeanCountries=europeanCountries,
               mindate=mindate,
               maxdate=maxdate,
               districts2plot=districts2plot,
               districts2plot2=districts2plot2,   
               AI_sts=AI_sts,
               final_model=final_model), 
          file="for_ai.car", preset="archive")
file.copy(file.path(getwd(), "for_ai.car"), "C:/Users/zxd598/Documents/GitHub/ku-awdc.github.io/ENIGMA2023/for_ai.car", overwrite=TRUE)
# Clean up:
rm(ai_data)

############# 2) Push to git ###############




#### Procedure: ####

# 1) Download latest infur file from the online WOAH repository to the ENGIMAdata 
#    folder
# 2) Run the above code that saves the file as a .car file (random file type)
# 3) Open Github Desktop and push the change made in the ENIGMA2023 repository 
#    within the ku-awdc.github.io folder (with the new .car file)
# 4) The app should automatically take the new file as input, and be updated.