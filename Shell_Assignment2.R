#Perrin Shell
#Biometry
#Prof. Brian Maitner
#.                        Assignment #2: Distributions
#-------------------------------------------------------------------------------

#PART I: HYPOTHETICAL DISTRIBUTIONS

#For each scenario: a. identify discrete vs continuous b. choose the most appropriate
#distribution for visualization from Bolker Ch. 4.5 + explain reasoning


#////////////////////////////////////////////////////////////////////////////////////////
#Scenario 1: Number of Surviving Individuals within a forest plot (known starting #)

#a: Discrete
#b: Binomial Distribution: the starting number of individuals is fixed, and each
#individual can either survive or die. The outcomes are independent and have roughly
#the same probability of occurring. A survival is considered a success.




#Scenario 2: Species Abundance (counts of individuals in a plot)

#a: Discrete
#b: Negative Binomial Distribution: individuals are likely clustered rather than 
#spread evenly throughout the plot. A Negative Binomial is good for patchy or clumped
#data. Its variance is likely to be high, and can exceed the mean with this distribution. 




#Scenario 3: Pollinator Visitation Rates (visits per flower per unit time)

#a: Discrete
#b: Poisson Distribution: I am going to assume that the pollinators are visiting 
#at a relatively constant rate during the observation period. A Poisson dist. assumes
#that the events are independent (one pollinator visit does not impact another one later)
#and occur at relatively similar rates over time. If pollinators are arriving in
#clusters rather than at a fixed-ish rate I would choose a negative binomial but
#I don't have any way to know that. If it were me watching my bees I would say Poisson. 




#Scenario 4: Ages (years) of Surviving Indviduals within a Reserve

#a: Discrete (I presume we are saying whole years and not a kid like: "oh I'm 7 years, 5 months,
#22 days and 8 hours....)
#b: Geometric Distribution: see this is tricky because there are SO many factors that
#can influence this. Let's use my economics brain and say we are doing maximum simplification
#where we are ignoring stochastic variation like recruitment and only looking at a 
#constant probability of dying at every age (which I mean is kind of true if we ignore senescence
#which is lowkey rare in nature anyways).




#Scenario 5: Body Size (mass, length, etc.)

#a: Continuous
#b: Log Normal Distribution: 1. the book literally gives masses and sizes as an example for this
#2. we are suspecting that body size is postive (duh) and generally right-skewed, so
#a log-normal will help us visualize processes that invlive multiple growth factors over time.




#Scenario 6: Biomass per Unit Area

#a: Continuous
#b: Gamma Distribution: biomass is positive and often right-skewed with substantial variation
#(some areas may have very little and some may have a whole bunch), a Gamma distribution
#is best to visualize these sort of patterns




#Scenario 7: Time to Germination

#a: Continuous (lets say we meausred with a stopwatch that has like 1,000ths of a second)
#b: Exponential Distribution: lets say these are like radish seeds or something that are
#super easy to germinate and don't really need super complex conditions or dormancy periods.
#An exponential distribiution will model the time waiting for a single event to occur
#when probability of the event is relatively constant. I may add a time delay in the model
#where no events can occure before time t, because its super unlikely for any seed to 
#just drop to the ground and immediately germinate or that we detected it as soon as it occurred.




#Scenario 8: Proportion of Habitat Covered by Vegetation

#a: Continuous
#b: Beta Distribution: Vegetation cover is bounded between 0 and 1 (between 0% or 100%)
#The beta dist. is used for continuous variables that fall within this range and also
#the book literally says "cover proportion" as a use for Beta, so I guess we can't get 
#more explanatory than that. 




#Scenario 9: Larval Settlement Success (Number Settled out of Larvae Released)

#a: Discrete
#b: Binomial Distribution: each larvae released either settles or doesn't, so this models
#the number of successes out of a known total of larvae released. (Uh I released them
#and counted them all beforehand)




#Scenario 10: Proportion of Infected Fish in a Population (Parasite Prevalence)

#a: Continuous (if talking about the proportion value)
#b: Beta/ Beta-Binomial Distribution: Beta-Binomial dist. is great for modeling a proportion bounded 
#between 0 and N number of observations where we have a known N of fish sampled. If
#we are looking at prevalence proportions among many populations, we should use a Beta dist.
#that is bounded between 0 and 100% prevalence.




#Scenario 11: Daily Temperature at a Field Site

#a: Continuous (can take decimal values and SHOULD if its in C)
#b: Normal Distribution: temperature may be appromixately normally distributed,
#especially here in St. Pete where values will vary generally semetrically around a mean
#if we wanted to make a model for a location with actual seasons we may need a more
#complex distribution to actually make the data make more sense.




#Scenario 12: Population Growth Rate (Population (future)/Population Size (present))

#a: Continuous
#b: Log Normal Distribution: the book kind of also explains this one. In this case we are
#likely looking at multiplicative population growth factors, so a log normal can help
#visualize the result of changes that have accumulated over time and are positively-skewed.



#-------------------------------------------------------------------------------

#PART II: FOCAL DATASET DISTRIBUTIONS

#For at least 3 numeric variables: 1.provide a histogram, 2. discrete/continuous, 3.
#mean and median, 4. written argument for distribution and what it shows

#///////////////////////////////////////////////////////////////////////////////

#Get Da Packages and Libraries Ready

install.packages("robis")
library(robis)
library(dplyr)

#-------------------------------------------------------------------------------

#Download Whale Shark Occurrences 
records <- occurrence(scientificname = "Rhincodon typus")

#Clean Dataset (like in Assignment 1)
clean_records <- records[
  is.na(records$sst) | records$sst >= 0,
]

#Check Number of Observations to Make Sure it Worked
nrow(records)
nrow(clean_records)

#-------------------------------------------------------------------------------

#Variable 1: Depth

#Depth is Continuous

#remove any missing values
depth_values <- clean_records$depth[
  !is.na(clean_records$depth)
]

#Histogram
hist(depth_values,
     main = "Distribution of Whale Shark Observation Depths",
     xlab = "Depth (m)")

#Log Histogram
hist(log10(depth_values),
     main = "Log-Transformed Whale Shark Depth",
     xlab = "Log10(Depth in meters)")

#Calculate Mean and Median
mean(depth_values)
median(depth_values)

#For this variable I am reading the Logged Histogram.

#In this case, there is not a distribution included in the Bestiary that would be
#super useful to model this data. The mean is much greater than the median, indicating
#a strong right-skew as well as zero-inflated values (there are a lot
#of zeros as many whale shark observations occurred at the surface). There would likely need
#to be separate treatment of zeros and positive depth values (>0 m). 

#-------------------------------------------------------------------------------

#Variable 2: Sea Surface Temperature (SST)

#SST is Continuous

#remove any missing SST values
sst_values <- clean_records$sst[
  !is.na(clean_records$sst)
]

# Histogram
hist(sst_values,
     main = "Distribution of Sea Surface Temperature",
     xlab = "Sea Surface Temperature (°C)")

# Calculate Mean and Median
mean(sst_values)
median(sst_values)

#The mean and the median are relatively similar, which initially pointed me to a 
#normal distribution. However, the Histogram does not exhibit a traditional symmetirc
#bell-shaped distribution. The observations have several peaks and a long tail
#at cooler temps. After this inspection, it seems that a normal distribution is not
#a good fit for this data, and there doesn't seem to be any obvious choices to model
#this data given in the Bestiary. (Maybe we can talk after class about additional options?)

#-------------------------------------------------------------------------------

#Variable 3: Latitude

#Latitude is Continuous 

#remove any missing latutitude variables
latitude_values <- clean_records$decimalLatitude[
  !is.na(clean_records$decimalLatitude)
]


# Histogram 
hist(latitude_values,
     main = "Distribution of Whale Shark Observation Latitude",
     xlab = "Latitude (degrees)")


# Calculate Mean and Median
mean(latitude_values)
median(latitude_values)

#For latitude data, the histogram also has many defined peaks. These likely correspond
#to whale sharks being observed in similar geographic regions. The mean and median
#differ quite a bit, but multiple peaks is the strongest indiation that this variable
#also unfortunately cannot fit a single standard distribution model decribed in the Bestiary.

#-------------------------------------------------------------------------------

