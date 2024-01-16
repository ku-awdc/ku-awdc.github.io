
## Copyright © 2023 Lene J. Kjær, Michael P. Ward, Anette E. Boklund, Lars E. Larsen, Charlotte K. Hjulsager, and Carsten T. Kirkeby ”

##############################################################################################
# This file is part of the Shiny app for the ENIGMA HPAI model version 1.0.   

# The ENIGMA HPAI model is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.                                   

# The ENIGMA HPAI model is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.                                                

# You should have received a copy of the GNU General Public License along with the ENIGMA HPAI model. If not, see <https://www.gnu.org/licenses/>.
##############################################################################################


# READ IN SHAPEFILE OF THE STUDY REGION ###
europeanCountries = st_read('./data/GIS/Europe_shapefiles.shp')


#calculate area for each country
europeanCountries$area_sqkm <- as.numeric(st_area(europeanCountries)) / 1000000

### READ IN newest OIE DATA ###
##link to where files are - the below code will pick the newest file in the folder
# tt <- tempfile()
# download.file("http://www.costmodds.org/ENIGMA2023/for_ai.car", tt, mode="wb")
# qs::qload(tt)
 updateDate <-strftime(as.Date(substring(filename, 7,14), format='%Y%m%d'),format = '%d/%m/%Y')
# file.remove(tt)

# the code above downloaded the data set called ai_data, here we select only HPAI and only select some of the variables in the WOAH-WAHIS data, as we do not need them all
europe_data <-ai_data %>% 
  filter(disease_eng %in% c("High pathogenicity avian influenza viruses (poultry) (Inf. with)", "Influenza A viruses of high pathogenicity (Inf. with) (non-poultry including wild birds) (2017-)")) %>% 
  select(disease_eng,iso_code, sero_sub_genotype_eng,Outbreak_start_date,Species,cases )

# CLEAN AND PREPARE DATA---------------------------------

#make sure that outbreak start date is in date format
europe_data$Outbreak_start_date <- as.Date(europe_data$Outbreak_start_date, tryFormats = c("%Y-%m-%d", "%Y/%m/%d"))
names(europe_data)


#filter out other species than birds using the bird_names_20220517.csv file
birdFam <- as.list(read.csv("./data/bird_names_20220517.csv", sep=";", header=FALSE))
europe_data$Species <- gsub("\\s*\\([^\\)]+\\)","",as.character(europe_data$Species))
europe_data <- europe_data[europe_data$Species %in% birdFam$V1,]

#we only want data from Europe (so filter using the europe shapefile) and data from 2016 and onwards
europe_data <-europe_data %>%
  filter(iso_code %in% unique(europeanCountries$ADM0_A3),Outbreak_start_date >"2015-12-31")

#we only look at H5
europe_data <-europe_data %>%
  filter(grepl("H5",sero_sub_genotype_eng))

#we need the last date of data for different calculations later
endDate <- max(europe_data$Outbreak_start_date)

#set all cases to 1, so we are counting number of outbreaks instead of number of birds affected
europe_data$cases<- 1


## Dates and week numbers are tricky as they differ from year to year. Here we use iso 8601 to create the new variables - isoweek aned isoyear
europe_data <-europe_data %>%
  mutate(isoweek=lubridate::isoweek(Outbreak_start_date)) %>%
  mutate(isoyear=lubridate::isoyear(Outbreak_start_date))


# check if any observations with week 53 ?
europe_data[which(europe_data$isoweek == 53), ]

# In this data set, there are observations within week 53 from 2020/2021 and one from  January 2016. As the surveillance package needs a matrix of countries and week numbers, we cannot have week 53 in some years and not others. We also cannot create a week 53 for the remaining years and set it to zero observations as this week does not exist for those years. Instead, we force dates in week 53 to be either week 52 in 2020 (if in December 2020), week 1 in 2021 (if in January 2021) or week 1 in 2016.

for (i in 1:nrow(europe_data)){
  if(europe_data$isoweek[i]==53 & lubridate::month(europe_data$Outbreak_start_date[i]) == 12){
    europe_data$isoweek[i]<-52
    if(lubridate::year(europe_data$Outbreak_start_date[i]) != europe_data$isoyear[i]){
      europe_data$isoyear[i] <- lubridate::year(europe_data$Outbreak_start_date[i])
    }
  }
  else if(europe_data$isoweek[i]==53 & lubridate::month(europe_data$Outbreak_start_date[i]) == 1){
    europe_data$isoweek[i]<-1
    if(lubridate::year(europe_data$Outbreak_start_date[i]) != europe_data$isoyear[i]){
      europe_data$isoyear[i] <- lubridate::year(europe_data$Outbreak_start_date[i])
    }
  }
}

## now aggregate per week per year per country
europe_data_weekly <- europe_data  %>%
  group_by(iso_code, isoweek,isoyear) %>% summarise(no_outbreaks = sum(cases))

colnames(europe_data_weekly)<- c("ADM0_A3", "Week", "Year","no_outbreaks")


#this methods fill in missing years for each country, by adding week 1 of the missing year (zero number of detections)
europe_data_weekly <-europe_data_weekly %>%
  group_by(ADM0_A3) %>%
  complete(Year = 2016:2023, fill = list(Week=1, no_outbreaks = 0))

#this method then add missing weeks to the data set and set number of outbreaks for the missing weeks to zero
europe_data_weekly <-europe_data_weekly %>%
   group_by(ADM0_A3, Year) %>%
  complete(Week = 1:52, fill = list(no_outbreaks = 0))

#set rest of final year to NA after last week with detection reports
endWeek <-as.numeric(strftime(endDate, format = '%V'))
endYear <-as.numeric(strftime(endDate, format = '%Y'))

europe_data_weekly$no_outbreaks[europe_data_weekly$Year==endYear & europe_data_weekly$Week>endWeek] <- NA

europe_data_weekly$no_outbreaks[europe_data_weekly$Year>endYear ] <- NA

### READ IN COVARIATES ###
## read in file on coastal length and area of wetland
covar_eur <-read_excel("./data/covariates.xlsx", sheet=1)

#get iso_code from country name
covar_eur$iso_code  <- countrycode(covar_eur$SOVEREIGNT
                                   ,"country.name","iso3c")

#iso_code for Kosovo is not found in the countrycode function, so we set it manually
covar_eur$iso_code[is.na(covar_eur$iso_code)] <- "KOS"

#now that we have isocodes we merge with our data set
colnames(covar_eur)[4] <- "ADM0_A3"
europe_data_weekly <- merge(europe_data_weekly, covar_eur, by="ADM0_A3")

# get data on area of country
area_data <- as.data.frame(europeanCountries) %>%
  distinct(ADM0_A3,area_sqkm)

# now merge with our data set
europe_data_weekly <- merge(europe_data_weekly, area_data, by="ADM0_A3")

#select only needed columns
europe_data_weekly <-europe_data_weekly %>%
  select(ADM0_A3, Year, Week,no_outbreaks, Wet_km, Coast_km, area_sqkm )

# CREATE DATA, NEIGHBORHOOD AND COVARIATE MATRICES ###
#first read in shapefile where water is added as polygons to account for countries with water between them still being connected in a bird's perspective
europeanCountries_water = st_read('./data/GIS/Europe_shapefiles_water.shp')

#now calculate neighborhood (matrix) and set column and row names
europe_adjmat <-nbOrder(poly2adjmat(europeanCountries_water,zero.policy = TRUE), maxlag = Inf)
colnames(europe_adjmat) <- europeanCountries_water$ADM0_A3  # column names
rownames(europe_adjmat) <- europeanCountries_water$ADM0_A3 # row names

#Now only keep countries in the neighborhood matrix where we have outbreak data and remove the newly created water connections as well
europe_adjmat<-europe_adjmat[rownames(europe_adjmat)%in% unique(europe_data_weekly$ADM0_A3),colnames(europe_adjmat)%in% unique(europe_data_weekly$ADM0_A3)]

#create shape file with only countries where we have data - this is needed for the model
europeanCountries.sub<-europeanCountries_water[europeanCountries_water$ADM0_A3 %in% unique(europe_data_weekly$ADM0_A3),]
row.names(europeanCountries.sub)<- europeanCountries.sub$ADM0_A3

#convert sf object europeanCountries.sub to spatialpolygon dataframe as the sts class needs this format
europeanCountries.sub <- as(europeanCountries.sub, 'Spatial')

# Make sure that the country order in the data matrix we will create is the same order as in the neighborhood matrix
adjmat_order <- colnames(europe_adjmat)
keyDF <- data.frame(key=adjmat_order,weight=1:length(adjmat_order))
merged <- merge(europe_data_weekly,keyDF,by.x='ADM0_A3',by.y='key',all.x=T,all.y=F)
res <- merged[order(merged$weight, merged$Year,merged$Week),c('ADM0_A3','Year', "Week", "no_outbreaks")]

#create data matrix to be used in hhh4 model in HPAI_hhh4_master.R
AI_weekly <-res %>%  pivot_wider(
  names_from = ADM0_A3,
  values_from = no_outbreaks,
  id_cols = c(Year, Week)) %>%
  select(-Year,-Week) %>%
  as.matrix()

#Create separate covariate matrices to be used in hhh4 model
#Amount of coast
res1 <- merged[order(merged$weight, merged$Year,merged$Week),c('ADM0_A3','Year', "Week", "Coast_km")]
AI_coast <-res1 %>%  pivot_wider(
  names_from = ADM0_A3,
  values_from = Coast_km,
  id_cols = c(Year, Week)) %>%
  select(-Year,-Week) %>%
  as.matrix()

#amount of wetland
res2 <- merged[order(merged$weight, merged$Year,merged$Week),c('ADM0_A3','Year', "Week", "Wet_km")]
AI_wet <-res2 %>%  pivot_wider(
  names_from = ADM0_A3,
  values_from = Wet_km,
  id_cols = c(Year, Week)) %>%
  select(-Year,-Week) %>%
  as.matrix()

# country area
res3 <- merged[order(merged$weight, merged$Year,merged$Week),c('ADM0_A3','Year', "Week", "area_sqkm")]
country_area <-res3 %>%  pivot_wider(
  names_from = ADM0_A3,
  values_from = area_sqkm,
  id_cols = c(Year, Week)) %>%
  select(-Year,-Week) %>%
  as.matrix()

