#Perrin Shell
#Biometry FA '26
#Prof. Maitner
#                         Assignment 1
#------------------------------------------------------------------------------
#these only need to be installled once to the computer the assignment is run on

#Install the robis package from CRAN
install.packages("robis")

# latest development version (must have this installed)
remotes::install_github("https://iobis.github.io/robis/")

#work in the robis library to perform all of the specified functions
library(robis)

#------------------------------------------------------------------------------
#DATASET DESCRIPTION

#The Ocean Biodiversity Information System (OBIS) is a global,open-access dataset
#that contains occurrence records for marine species. Records come from many con-
#tributing datasets and can include information such as geographic locatio, ob-
#servation date, depth and various environmental variables.


#Source:
#Ocean Biodiversity Information System (OBIS)
#https://obis.org/data/access/

#------------------------------------------------------------------------------
#READ IN THE DATASET

#pull in all records for whale sharks (Rhincodon typus)
records <- occurrence(scientificname = "Rhincodon typus")

#------------------------------------------------------------------------------
#DATASET CLASS

class(records)

#the dataset class is a tibble, a modern data frame.
#------------------------------------------------------------------------------
#INTEREST VARIABLES AND CLASSES

#variables I am interested in for this analysis:
#decimalLatitude = latitude of whale shark observation
#decimalLongitude = longitude of whale shark observation
#depth = depth of observation (in meters (m))
#sst = sea surface temperature at time of observation
#date_year = year of observation in numeric values

#print class of interest variables
class(records$decimalLatitude)
class(records$decimalLongitude)
class(records$depth)
class(records$sst)
class(records$date_year)

#------------------------------------------------------------------------------
#DATASET STRUCTURE

str(records)

#------------------------------------------------------------------------------
#FORMAT CORRECTION

#make sure there are no impossible geographic coordinates
sum(records$decimalLatitude < -90 |
      records$decimalLatitude > 90,
    na.rm = TRUE)
#there are 0 impossible latitude coordinates

sum(records$decimalLongitude < -180 |
      records$decimalLongitude > 180,
    na.rm = TRUE)
#there are 0 impossible longitude coordinates

#check ranges of the numeric values, are there any peculiar numbers? signs?
range(records$depth, na.rm=TRUE)
range(records$sst, na.rm = TRUE)
range(records$date_year, na.rm = TRUE)

sort(records$sst) [1:20] #there are suspicious negative values for sst, why?

records[!is.na(records$sst) & records$sst < 0,
        c("sst", "decimalLatitude", "decimalLongitude",
          "depth", "year")] #show me the 16 strage recorded occurrences

#a small number of whale shark observations contained negative sst values and 
#occurred in the polar regions. This does not make biological sense, as whale
#sharks are tropical-subtropical, and do not tolerate cold temperatures nor 
#travel into the polar regions. 

#where did these 16 observtions come from?
bad_sst <- records[!is.na(records$sst) & records$sst < 0,]
bad_sst[, c("institutionCode")]

#all 16 came from the same institution code, so I am willing to believe that there
#is an issue with this dataset, maybe a taxonomic association error, an issue in 
#the way OBIS handles their data...either way I will remove these 16 observations
#for further analysis

clean_records <- records[
  is.na(records$sst) | records$sst >= 0,
] #remove rows where sst is negative but keep NAs

#in date_year, there is one observation for 1828...this is unlikely. Lets investigate
clean_records[
  clean_records$date_year < 1900,
  c("date_year",
    "year",
    "eventDate",
    "scientificName",
    "institutionCode")]

#the observations have different event dates and locations, and have different
#collection codes. Since the whale shark was described in 1828, and they are all
#unique it seems these may be old imported records from journals or something.
 
#------------------------------------------------------------------------------
#SUMMARY STATISTICS

summary(clean_records$decimalLatitude)
summary(clean_records$decimalLongitude)
summary(clean_records$depth)
summary(clean_records$date_year)
summary(clean_records$sst)

nrow(records)
nrow(clean_records)
#------------------------------------------------------------------------------
#FIGURE 1: GEOGRAPHIC DISTRIBUTION OF WHALE SHARKS

plot(clean_records$decimalLongitude,
     clean_records$decimalLatitude,
     xlab = "Longitude",
     ylab = "Latitude",
     main = "Geographic Distribution of Whale Sharks",
     pch = 16)

#This figure shows the geographic distrbution (with latitude and longitude coordinates)
#of whale shark observations in the OBIS dataset. Observations are primarily concentrated
#in the tropical and subtropical regions (which is whale sharks should be found). 

#------------------------------------------------------------------------------
#FIGURE 2: WHALE SHARK OBSERVATIONS BY YEAR

#count observations recorded in each year
year_counts <- table(clean_records$date_year)

barplot(year_counts,
        main = "Whale Shark Observations by Year",
        xlab = "Year Recorded",
        ylab = "Number of Observations",
        las = 2)

#The number of whale shark observations varies greatly among the recorded range of 
#years. the early portion of the dataset, dating all they way back to 1828 has 
#relatively few observtatons, while total obs seem to peak in the late 2000's 
#before following a cyclical decrease-increase pattern of observations after 2008. 
#This pattern represents the number of records in OBIS rather than changes in
#whale shark abundance.

#------------------------------------------------------------------------------
#FIGURE 3: SEA SURFACE TEMP

hist(clean_records$sst,
     main = "Sea Surface Temperature During Whale Shark Observations",
     xlab = "SST (°C)")

#Most whale shark observations occurred at relatively warm sea surface temperatures,
#with the highest concentration between 25-29°C. There are fewer observations at
#cooler temps below 23°C.

#------------------------------------------------------------------------------
#FIGURE 4: DEPTH VS SST

plot(clean_records$sst,
     clean_records$depth,
     xlab = "Sea Surface Temperature (°C)",
     ylab = "Depth (m)",
     main = "Depth vs SST(°C) During Whale Shark Observations",
     pch = 16)

#Most whale shark observations occurred at relatively shallow depths, and 
#sea surface temperatures between ~23-30°C. A small number of obsertaions were
#recorded at substantially greater depths.

#------------------------------------------------------------------------------
#FIGURE 5: DEPTH DISTRIBUTION

boxplot(clean_records$depth,
        main = "Distribution of Whale Shark Observation Depth",
        ylab = "Depth (m)")

#The distribution on whale shark observation depths is strongly skewed to the right.
#Most observations occured towards the surface or at relatively shallow depths,
#Potential outliers appear at greater depths, generally >100m. 
#------------------------------------------------------------------------------

