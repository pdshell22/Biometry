#Perrin Shell
#Prof. Brian Maitner
#Fall 2026 
#.                          Evil Biometry Midterm 

#-------------------------------------------------------------------------------

#Part I

citation("robis")

#taken from asignment 1
# Perrin Shell
# Biometry Midterm - Part I
# Prof. Maitner
#-------------------------------------------------------

#Install robis if it is not already installed
if (!requireNamespace("robis", quietly = TRUE)) {
  install.packages("robis")
}

#Load package
library(robis)

#Download whale shark occurrence records
records <- occurrence(scientificname = "Rhincodon typus")

#Check number of original observations
nrow(records)

#data cleaning procedure
#Check for impossible geographic coordinates
sum(records$decimalLatitude < -90 |
      records$decimalLatitude > 90,
    na.rm = TRUE)

sum(records$decimalLongitude < -180 |
      records$decimalLongitude > 180,
    na.rm = TRUE)

#Identify records with the negative SST values that are fucking my shit
sum(records$sst < 0, na.rm = TRUE)

#Remove negative SST records (bc aint no whale shark in negative degree water bruh)
clean_records <- records[
  is.na(records$sst) | records$sst >= 0,
]

#Check number of observations remaining
nrow(clean_records)

#summary stats
summary(clean_records$decimalLatitude)
summary(clean_records$depth)
summary(clean_records$sst)

#Number of observations in each hemisphere
sum(clean_records$decimalLatitude < 0, na.rm = TRUE)
sum(clean_records$decimalLatitude > 0, na.rm = TRUE)

#Range of observed latitudes
range(clean_records$decimalLatitude, na.rm = TRUE)

#Number of observations recorded at the surface
sum(clean_records$depth == 0, na.rm = TRUE)

#Number of observations with recorded depths
sum(!is.na(clean_records$depth))
#Figure 1: Geographic distribution
plot(clean_records$decimalLongitude,
     clean_records$decimalLatitude,
     xlab = "Longitude (degrees)",
     ylab = "Latitude (degrees)",
     main = "Geographic Distribution of Whale Sharks",
     pch = 16)
sum(clean_records$decimalLatitude < 0, na.rm = TRUE)
sum(clean_records$decimalLatitude > 0, na.rm = TRUE)

#Figure 2: Observation depth
boxplot(clean_records$depth,
        main = "Distribution of Whale Shark Observation Depth",
        ylab = "Depth (m)")
#^^this figure sucks ass so lets fix it 

hist(clean_records$depth,
     main = "Distribution of Whale Shark Observation Depth",
     xlab = "Observation Depth (m)",
     ylab = "Number of Observations",
     breaks = 30)
#why are there so few records?
sum(!is.na(clean_records$depth))
sum(is.na(clean_records$depth))

#Figure 3: Sea surface temperature
hist(clean_records$sst,
     main = "Sea Surface Temperature During Whale Shark Observations",
     xlab = "Sea Surface Temperature (°C)")

#how many are there? 36? last year i had 37!!!! ITS OKAY PERRINKINS
#we will not get you two more figures bc we are tired but you can go to bed
#before 2... how does that sound?
sum(!is.na(clean_records$sst))
sum(is.na(clean_records$sst))

#-------------------------------------------------------------------------------

#Part II a

library(readr)

#read in and look at teh first few rows of data
twoa <- read_rds("https://github.com/bmaitner/biometry_course/raw/refs/heads/main/midterm/b_2a.RDS")
head(twoa)

#make a scatter plot, what is going on here?
plot(twoa$Adult.Body.Mass.g, twoa$Egg.Mass.g,
     xlab = "Adult Body Mass (g)",
     ylab = "Egg Mass (g)",
     main = "Adult Body Mass vs. Egg Mass",
     pch = 16)

#a couple of birdies are making this lot unreadable! let us fix that logicarithmically
plot(twoa$Adult.Body.Mass.g, twoa$Egg.Mass.g,
     xlab = "Log Adult Body Mass (g, Log Scale)",
     ylab = "Log Egg Mass (g, Log Scale)",
     main = "Adult Body Mass vs. Egg Mass",
     pch = 16,
     log = "xy")

#lets look at a histogram to tell us more
hist(twoa$Egg.Mass.g,
     main = "Distribution of Egg Mass",
     xlab = "Egg Mass (g)")

#lets log the histogram to tell us EVEN more bc i need more information to make a choice (i am a woman after all)
hist(log(twoa$Egg.Mass.g),
     main = "Distribution of log Egg Mass",
     xlab = "Log Egg Mass")

#lets look at a line to figure out some scatter nonsense
bird_model <- lm(log(Egg.Mass.g) ~ log(Adult.Body.Mass.g), data = twoa)
summary(bird_model)

#lets make a residual plot (i thought i would get away from these after i wrote 
#my economics thesis but here they are to haunt me again)
hist(residuals(bird_model),
     main = "Distribution of Model Residuals",
     xlab = "Residuals")

#Plot the birds with the fitted power function
plot(twoa$Adult.Body.Mass.g, twoa$Egg.Mass.g,
     xlab = "Adult Body Mass (g)",
     ylab = "Egg Mass (g)",
     main = "Adult Body Mass vs. Egg Mass with Fitted Model",
     pch = 16,
     log = "xy")

#Add the fitted power function so i can see it
curve(exp(coef(bird_model)[1]) * x^coef(bird_model)[2],
      add = TRUE,
      col = "pink",
      lwd = 2)

#Now: why did i do all of this?
#A(Function): I believe a power function is appropirate to describe the relationshio
#between adult body mass and egg mass for these birds. The log scatterplot shows 
#a strong, positive relationship between both variables. Upon close-ish inspection,
#the relationship also appears approximately linear, and the fitted model had an R^2 
#of 0.9443 which is very high and suggets a large proportion of the variation in the log-transformed
#data is explained by the model.

#B(Distribution): A normal distribution is a reasonable candidate for describing the
#variation around the fitted power function on the Log scale.

#C(Why?): The original egg mass values exhibited a strong right-skew, with most values
#clumped around the lower end along with a few very large values. After a log transformation
#of both variables, the relationship appears approximately linear. Analysis of the residual
#histogram also suggests that the residuals are roughly symmmetric around zero. All of
#these things support using a power function with normally distributed residuals on the log scale.

#-------------------------------------------------------------------------------

#Part II b

library(readr)

#Read in the data and look at the first few rows
twob <- read_rds("https://github.com/bmaitner/biometry_course/raw/refs/heads/main/midterm/b_2b.RDS")
head(twob) #this class makes me feel like a twoob (twit noob)

#Make a scatterplot of fish abundance and species richness
plot(twob$Total.Fish.Abundance, twob$Species.Richness,
     xlab = "Total Fish Abundance",
     ylab = "Species Richness",
     main = "Fish Abundance vs. Species Richness",
     pch = 16)
#what did I learn from this scatterplot? points are widely spread (similar abundance
#does not equal similar richness), shape is generally positive but not exceptionally clear,
#straight or line-like. Lets do some transformations to see if we can parse this out

#Try a logarithmic x-axis
plot(twob$Total.Fish.Abundance, twob$Species.Richness,
     xlab = "Total Fish Abundance (log scale)",
     ylab = "Species Richness",
     main = "Fish Abundance vs. Species Richness",
     pch = 16,
     log = "x")
#this is better, but lets do more analysis before i decide anything

#Look at the distribution of species richness
hist(twob$Species.Richness,
     main = "Distribution of Species Richness",
     xlab = "Number of Species",
     breaks = 10)

#Calculate the mean and variance of species richness
mean(twob$Species.Richness)
var(twob$Species.Richness)

#Fit a relationship between species richness and log fish abundance
fish_model <- lm(Species.Richness ~ log(Total.Fish.Abundance),
                 data = twob)

#Look at the model results
summary(fish_model)

#Look at the distribution of residuals
hist(residuals(fish_model),
     main = "Distribution of Model Residuals",
     xlab = "Residuals")

# Plot residuals against fitted values
plot(fitted(fish_model), residuals(fish_model),
     xlab = "Predicted Species Richness",
     ylab = "Residuals",
     main = "Residuals vs. Fitted Values",
     pch = 16)

abline(h = 0, col = "turquoise")

#Plot fish abundance and species richness with the fitted model
plot(twob$Total.Fish.Abundance, twob$Species.Richness,
     xlab = "Total Fish Abundance",
     ylab = "Species Richness",
     main = "Fish Abundance vs. Species Richness with Fitted Model",
     pch = 16)

#Add the fitted logarithmic curve
curve(coef(fish_model)[1] + coef(fish_model)[2] * log(x),
      add = TRUE,
      col = "turquoise",
      lwd = 2)

#WHAT DOES IT MEAN WHAT DOES IT MEANNN?? Let me tell you:
#A(Function): A Logarithmic function seems to be the best choice for the relationship
#betweem total fish abundance and species richness. The first scatterplot showed a 
#pseudo-positive relationship but observations were concentrated at lower fish abundance.
#When fish abundance was plotted on a log scale, the relationship seemed approximately
#linear, the fitted logaritmic relationship also had an R^2 of 0.3002, which means that
#~30% of the variation in species richness was explained by the log model.

#B(Distribution): A negative binomial distribution seems to be the best candidate I can
#think of to describe the variation in species richness.

#C(Why?): Species richness is count data. The distribution of species-richness was
#right skewed with a mean of 21.784 and a variance of 112.556. Because the Variance > Mean (by a lot),
#using a Poisson dist. is less convincing. Upon looking at the residual histogram, the residuals
#from the log model were approx. bell-shaped, but the residuals vs fitted model plot
#shows that spread increased as predicted species richness increased as well. This pattern
#suggests that a normal distribution may not accurately describe this scatter pattern.
#Thus, a negative binomial dist. is a reasonable candidate because it accommodates
#variance greater than the mean. 

#-------------------------------------------------------------------------------

#Part II c

library(readr)

#Read in da data
twoc <- read_rds("https://github.com/bmaitner/biometry_course/raw/refs/heads/main/midterm/b_2c.RDS")

# Look at the first few rows
head(twoc)
#hmm bottom cover is pretty consistent even though rugosity differs a lot, lets investigate

#Look at the relationship between rugosity and hard-bottom cover
plot(twoc$Rugosity, twoc$Hard.Bottom.Cover.Pct,
     xlab = "Rugosity",
     ylab = "Hard-Bottom Cover (%)",
     main = "Rugosity vs. Hard-Bottom Cover",
     pch = 16)

#woah that hoe look FUNKY
#natural upper boundary at 100% (bviously), no clear linear relationship, 
#spread changes with rugosity 

#Look at the distribution of hard-bottom cover
hist(twoc$Hard.Bottom.Cover.Pct,
     main = "Distribution of Hard-Bottom Cover",
     xlab = "Hard-Bottom Cover (%)")

#Calculate summary stats
mean(twoc$Hard.Bottom.Cover.Pct)
median(twoc$Hard.Bottom.Cover.Pct)

#Check how many values are exactly 0 or 100
sum(twoc$Hard.Bottom.Cover.Pct == 0)
sum(twoc$Hard.Bottom.Cover.Pct == 100)

#Fit a linear model
reef_model <- lm(Hard.Bottom.Cover.Pct ~ Rugosity, data = twoc)

#Look at the results
summary(reef_model)

#Check whether rugosity values are positive
min(twoc$Rugosity, na.rm = TRUE)

#Fit a logarithmic model
log_reef_model <- lm(Hard.Bottom.Cover.Pct ~ log(Rugosity),
                     data = twoc)

#Look at the logarithmic model results
summary(log_reef_model)

#Plot the original data
plot(twoc$Rugosity, twoc$Hard.Bottom.Cover.Pct,
     xlab = "Rugosity",
     ylab = "Hard-Bottom Cover (%)",
     main = "Linear vs. Logarithmic Model",
     pch = 16)

#Add the linear model
abline(reef_model, col = "pink", lwd = 2)

#Add the logarithmic model
curve(coef(log_reef_model)[1] +
        coef(log_reef_model)[2] * log(x),
      from = min(twoc$Rugosity),
      to = max(twoc$Rugosity),
      add = TRUE,
      col = "turquoise",
      lwd = 2)

#Add a legend
legend("bottomright",
       legend = c("Linear", "Logarithmic"),
       col = c("pink", "turquoise"),
       lty = 1,
       lwd = 2)

#ughhhhhhh im tired

#A(Function): A linear function seems to be a relatively reasonable choice for the relationship
#between rugosity and hard-bottom cover percentage. The scatterplot showed a weak positive
#relationship with no obvious nonlinear pattern. The fitted linear model supplied 
#a positive slope of 11.913 and an R^2 of 0.04076 (which is lowkey really bad), 
#and suggests that rugoisty is not a very good explanatory variable for the variation 
#in hard-bottom cover. (Linear vs log was almost zero difference lowkey)

#B(Distribution): A beta distribution seems to be the best starting option to describe
#the distribution of this dataset, though a zero/one inflated beta distribution would 
#likely better accommodate the observations at exactly 0% and 100% hard-bottom cover.

#C(Why?): Hard-bottom cover is a continuous percentage bounded between 0% and 100%.
#The histogram showed a strongly left-skewed distribution, with a mean of 81.71%
#and a median of 88.5%. A beta distribution can be used to describe skewed proportions
#between 0 and 1, making it a relatively reasonable choice for the variation in bottom cover.
#However, the dataset contains two observations at exactly 0% and 18 at exactly 100%
#which cannot be well-represented by a normal beta distribution. A zero/one inflated beta dist
#would better account for these vobservations and describe the continuous proportion 
#values between them. 

#-------------------------------------------------------------------------------

#Part III

#Make random results reproducible
set.seed(522)

#Choose three sample sizes
sample_sizes <- c(20, 50, 100)

#test standard deviations from 1 to 20
noise_levels <- 1:20

#Repeat each simulation 200 times
n_sims <- 200

#Make an empty table to store results
results <- data.frame()

#Go through each sample size
for (n in sample_sizes) {
  
  #Go through each noise level
  for (sd in noise_levels) {
    
    #Store whether each simulation is significant
    significant <- numeric(n_sims)
    
    #Repeat the simulation againnnn
    for (i in 1:n_sims) {
      
      #Create x values
      x <- runif(n, 0, 10)
      
      #Create y values with random noise
      y <- 2 + 1*x + rnorm(n, mean = 0, sd = sd)
      
      #Fit a linear regression
      model <- lm(y ~ x)
      
      #Get the p-value for the slope
      p_value <- summary(model)$coefficients[2, 4]
      
      #Record whether p < 0.05
      significant[i] <- p_value < 0.05
    }
    
    #Calculate statistical power
    power <- mean(significant)
    
    #Save the result
    results <- rbind(results,
                     data.frame(SampleSize = n,
                                SD = sd,
                                Power = power))
  }
}

#lets look at the first few results
head(results)
#does this make sense? yes. as we increase noise throighout the simulations the 
#relationship between x and y becomes harder to detect

#Make a graph showing how power changes with noise
#Start with sample size 20
plot(results$SD[results$SampleSize == 20],
     results$Power[results$SampleSize == 20],
     type = "l",
     col = "turquoise",
     ylim = c(0, 1),
     xlab = "Standard Deviation (Noise)",
     ylab = "Statistical Power",
     main = "Effect of Noise and Sample Size on Power")

#Add sample size 50 to the same graph
lines(results$SD[results$SampleSize == 50],
      results$Power[results$SampleSize == 50],
      col = "magenta")

#Add sample size 100 to the same graph
lines(results$SD[results$SampleSize == 100],
      results$Power[results$SampleSize == 100],
      col = "darkgreen")

#Add da legend so we know which line is which
legend("topright",
       legend = c("n = 20", "n = 50", "n = 100"),
       col = c("turquoise", "magenta", "darkgreen"),
       lty = 1)
#^premium figure

#power = chance of detecting a real effect

#Fig 1:Effect of standard deviation (noise) and sample size on statistical power
#in simulated models. Power was calculated as the proportion of 200 simulations in 
#which the slope was deemed statistically significant, where p < 0.05. Three sample sizes -
#20, 50 and 100 - were compared across standard deviations ranging from 1 to 20.
#Statistical power generally decreased as noise increased, while larger sample sizes 
#had higher power.

#Conclusion: These simulations show that statistical power decreased as standard dev.
#increased. Larger sample sizes generally tended to produce higher statistical power. 
#The sample size of 100 maintained the highest power across different simulated noise levels,
#while the smallest sample size (20) showed the most dramatic decline in statistical power.
#These results suggest that increasing total sample size makes it easier to detect statistically
#significant relationships, even if the data contain more random variation (noise). If I, as a
#researcher had to choose between trying to double my sample size of halve my measurement error,
#I would choose to halve measurement error, as it would provide a greater improvement in statistical
#power under these assumptions. Increasing observations would give more information,
#but halving std dev, makes the relationship clearer in every observation.
#(Maybe I actually teach the undergrads how to take samples correctly,
#or if I had the money get better equipment.)
