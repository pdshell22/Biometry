#Perrin Shell(oh yeah because of course why would my name be a word its "misspelled")
#Prof. Brian Maitner
#Fall 2026

#.                      Biometry Assignment Number 3

#-------------------------------------------------------------------------------

#Part 1

#its time to get funky 

#import the bits and work in da libraries

library(bbmle) #this hoe always takes a quadrillion years wth
library(biometrydata)

#time to copy da stuff

beetles <- beetle_competition_interspecific
# or: beetles <- read.csv("https://github.com/bmaitner/biometry_course/raw/refs/heads/main/data/Beetle_competition/competition_interspecific.csv")

beetles <- beetles[beetles$pair == "Cmac_Csub" & !is.na(beetles$progeny_1), ]

# plot if you like: offspring per female against the number of competitors
plot(beetles$start_females_1 + beetles$start_females_2,
     beetles$progeny_1 / beetles$start_females_1,
     xlab = "females in the dish, both species",
     ylab = "C. maculatus offspring per female")

# The starting model is a competition model in the Beverton-Holt family:

#   expected offspring = R * N1 / (1 + (a11 * N1 + a12 * N2)^d)

#   R     offspring per female with no competition at all
#   a11   how much each C. maculatus female reduces that (within species)
#   a12   how much each C. subinnotatus female reduces it (between species)
#   d     how sharply competition bites as the dish fills up
#   k     negative binomial size: how variable dishes are

# Bounds keep every parameter positive, so the optimizer cannot try
# impossible values. If you remove a parameter, remove it from start, lower
# and upper too.

beetle.fit <- mle2(progeny_1 ~ dnbinom(mu = R * start_females_1 /
                                         (1 + (a11 * start_females_1 +
                                                 a12 * start_females_2)^d),
                                       size = k),
                   start = list(R = 20, a11 = 0.1, a12 = 0.1, d = 1, k = 2),
                   lower = c(R = 0.01, a11 = 0, a12 = 0, d = 0.1, k = 0.01),
                   upper = c(R = 1000, a11 = 10, a12 = 10, d = 10, k = 100),
                   method = "L-BFGS-B",
                   data = beetles)

coef(beetle.fit)
AIC(beetle.fit)

# Questions to build models around:
#   - Is d needed? With d = 1 (leave it out of the formula) this is the
#     classic Beverton-Holt model.
#   - Does C. subinnotatus affect C. maculatus at all? (What value of a12
#     would mean "no"?)
#   - Is a C. subinnotatus female as harmful as a C. maculatus female?
#     (Hint: use one parameter for both.)
#   - A different shape: the Ricker model,
#     R * N1 * exp(-a11 * N1 - a12 * N2). Is it nested in the starting model?


#okie dokie now lets play around with this stuff

#Alternative Model I: Beverton-Holt (d = 1) (do we even need d? that sounds so bad jesus)

beetle.fit2 <- mle2(progeny_1 ~ dnbinom(
  mu = R * start_females_1 /
    (1 + a11 * start_females_1 +
       a12 * start_females_2),
  size = k),
  start = list(R = 20, a11 = 0.1, a12 = 0.1, k = 2),
  lower = c(R = 0.01, a11 = 0, a12 = 0, k = 0.01),
  upper = c(R = 1000, a11 = 10, a12 = 10, k = 100),
  method = "L-BFGS-B",
  data = beetles)

coef(beetle.fit2)
AIC(beetle.fit2)

#original vs simpler model
AICtab(beetle.fit, beetle.fit2, weights = TRUE)

#the B-H model i just made makes a TEENY improvement, lets contimue this investigation


#Alternative Model II: remove interspecific competition
#Remove a12 to test whether C. subinnotatus affects C. maculatus

beetle.fit3 <- mle2(progeny_1 ~ dnbinom(
  mu = R * start_females_1 /
    (1 + a11 * start_females_1),
  size = k),
  start = list(R = 20, a11 = 0.1, k = 2),
  lower = c(R = 0.01, a11 = 0, k = 0.01),
  upper = c(R = 1000, a11 = 10, k = 100),
  method = "L-BFGS-B",
  data = beetles)

coef(beetle.fit3)
AIC(beetle.fit3)

#Compare all three models
AICtab(beetle.fit, beetle.fit2, beetle.fit3, weights = TRUE)
#ur a beetle fit (gonna use this insult on my dad later)
#also what a marginal difference man

#Alternative Model III: Equal competition between the two beetle species
#make both species have the same competitive effect

beetle.fit4 <- mle2(progeny_1 ~ dnbinom(
  mu = R * start_females_1 /
    (1 + a * (start_females_1 + start_females_2)),
  size = k),
  start = list(R = 20, a = 0.1, k = 2),
  lower = c(R = 0.01, a = 0, k = 0.01),
  upper = c(R = 1000, a = 10, k = 100),
  method = "L-BFGS-B",
  data = beetles)

coef(beetle.fit4)
AIC(beetle.fit4)

#Compare all the nonsense
AICtab(beetle.fit, beetle.fit2, beetle.fit3, beetle.fit4,
       weights = TRUE)

#Four models were compared to determine whether the complexity of the initially 
#presented Beaverton-Holt (B-H) model was necessary to describe competition of Bean Beetle
#competition. The classic B-H model, with the exponent fixed at d=1, had the lowest AIC
#of all 4 analyzed models (654.86), and the greatest model weight at 0.516. The originally
#described model was competitive, with deltaAIC being 1.1. These findings suggest that 
#the additional exponent was not strongly supported by the data. Removing Interspecific
#competition between bean bean species increased AIC by 3.3, and assuming equal competition
#between species increased AIC by 3.7; these results suggest that both intra and interspecific
#competition influences offspring production in C. maculatus, but that competition
#within this species may also be stronger than competition with C. subinnotatus. 
#The second model, the "classic" B-H would be my personal choice, as it balances 
#model fit with simplicity, while simultaneously allowing the two beetle species to have competetive effects.
#Though the initial model remains a plausible explanation for observed competition.

#eugh okay enough real talk more foolishness Part 1b GO!


#step 1 copy the bullshit in
# 1b: Wing length in hawks and eagles -----------------------------------------

# Long-distance migrants should benefit from longer wings, which make
# sustained flight cheaper. Do migratory hawks and eagles (family
# Accipitridae) have longer wings than residents of the same body mass?

#   Wing.Length   mm
#   Mass          g
#   migration     resident, partial (some populations migrate), or migrant

# Each row is a species, so 249 species in all. One caution worth a
# sentence in your answer: related species share traits, so species are not
# fully independent data points. Accounting for that is beyond this course.

hawks <- avonet
# or: hawks <- readRDS(url("https://github.com/bmaitner/biometry_course/raw/refs/heads/main/data/Avonet/AVONET1_BirdLife.rds"))

hawks <- hawks[hawks$Family1 == "Accipitridae", c("Species1", "Wing.Length", "Mass", "Migration")]
hawks <- na.omit(hawks)
hawks$migration <- factor(hawks$Migration, levels = 1:3,
                          labels = c("resident", "partial", "migrant"))

# plot if you like

plot(log(hawks$Mass), log(hawks$Wing.Length), col = hawks$migration,
     xlab = "log body mass", ylab = "log wing length")
legend("topleft", levels(hawks$migration), col = 1:3, pch = 1)

# The starting model: wing length is lognormal, and its log rises linearly
# with log mass. That is a power law, wing = e^int * mass^b, the usual way
# one body measurement scales with another.

#   int     wing length at a given mass (on the log scale)
#   b       how fast wing length rises with mass
#   log_s   the log of the scatter around the line. Fitting the log keeps
#           the standard deviation positive, as in Lecture 16

# parameters = lets every one of the three differ among the migration
# classes, which is deliberately more than the data are likely to support.

hawk.fit <- mle2(Wing.Length ~ dlnorm(meanlog = int + b * log(Mass),
                                      sdlog = exp(log_s)),
                 start = list(int = 3, b = 0.3, log_s = -2),
                 parameters = list(int ~ migration,
                                   b ~ migration,
                                   log_s ~ migration),
                 data = hawks)

coef(hawk.fit)
AIC(hawk.fit)

# Questions to build models around:
#   - Do migrants have longer wings for their mass? (Which parameter is that?)
#   - Does wing length scale with mass differently in migrants?
#   - Are some migration classes more variable than others?
#   - Is the distinction between partial migrants and full migrants needed,
#     or would migrant versus resident do? (You would need a new column.)



#Alternative Model I: Common slope across migration groups
#does body mass scaling differ among migration classes?

hawk.fit2 <- mle2(Wing.Length ~ dlnorm(
  meanlog = int + b * log(Mass),
  sdlog = exp(log_s)),
  start = list(int = 3, b = 0.3, log_s = -2),
  parameters = list(int ~ migration,
                    log_s ~ migration),
  data = hawks)

coef(hawk.fit2)
AIC(hawk.fit2)

#compare models
AICtab(hawk.fit, hawk.fit2, weights = TRUE)
#the new model is a lillll bit better

#continuing on with Alternative Model II: Common slope and variation (only intercept
#differs among the migration groups)

hawk.fit3 <- mle2(Wing.Length ~ dlnorm(
  meanlog = int + b * log(Mass),
  sdlog = exp(log_s)),
  start = list(int = 3, b = 0.3, log_s = -2),
  parameters = list(int ~ migration),
  data = hawks)

coef(hawk.fit3)
AIC(hawk.fit3)

#Compare all three models
AICtab(hawk.fit, hawk.fit2, hawk.fit3, weights = TRUE)

#Hawk moth? How did you get here??? Tikki, Spots On! -> Lucky Charm! -> Time to de-evilize -> Bye bye little butterfly -> Miraculous Ladybug! -> Pound it!
#anyways...model 3 is pretty good lowkeyly


#Alternative Model III: No migration effect
#Wing length only depends on body mass n thatz it

hawk.fit4 <- mle2(Wing.Length ~ dlnorm(
  meanlog = int + b * log(Mass),
  sdlog = exp(log_s)),
  start = list(int = 3, b = 0.3, log_s = -2),
  data = hawks)

coef(hawk.fit4)
AIC(hawk.fit4)

#look at all models together???
AICtab(hawk.fit, hawk.fit2, hawk.fit3, hawk.fit4,
       weights = TRUE)


#Alternative Model IV: Combine partial vs full migrants
#are only two migration categories are sufficient?

hawks$migration2 <- factor(
  ifelse(hawks$migration == "resident", "resident", "migrant"),
  levels = c("resident", "migrant")
)

hawk.fit5 <- mle2(Wing.Length ~ dlnorm(
  meanlog = int + b * log(Mass),
  sdlog = exp(log_s)),
  start = list(int = 3, b = 0.3, log_s = -2),
  parameters = list(int ~ migration2),
  data = hawks)

coef(hawk.fit5)
AIC(hawk.fit5)

#Compare all five models
AICtab(hawk.fit, hawk.fit2, hawk.fit3, hawk.fit4, hawk.fit5,
       weights = TRUE)

#Part 1b Explanation:
#In this case I (had to :/ ) use 5 models to determine whether migration status 
#influences wing length in hawk and eagle species after accounting for body mass.
#The model combining partial and full migrants into one category (hawk.fit5) had the lowest
#AIC of all five models (2638.46) and the highest model weight (0.544). The model retaining
#all three migration categories also remained well supported, with a deltaAIC of 0.8.
#This suggests that distinguishing partial and full migrants may not be completely necessary.
#The model exluding migration status performed poorly, with a deltaAIC of 14.2; these findings
#suggest that migration status DOES help explain differences in wing length among hawks and eagles.
#The best model also shows that migratory species were predicted to have 8% longer wing length
#than non-migratory species. For me, i would choose the two-cateory model, as it strikes a good
#balance between model fit and overall complexity, though it does have limitations regarding
#observations, closely related species may share similar evolutionary traits, which means
#we cannot fully determine if these types of observations are completely independent.

#UGHH I WANT TO BE DONE WITH THIS 


# 1c: Reef fish species richness ----------------------------------------------

# How many fish species does a diver record on a survey of a Florida coral
# reef, and what explains the differences?

#   species_richness   number of species recorded on the survey
#   depth              tens of metres (so 1.5 is 15 m)
#   visibility         underwater visibility, tens of metres
#   year               years since 2014
#   region             Tortugas, Keys, or North (Miami to Martin County)
#   protected          "yes" inside a marine protected area, otherwise "no"

# Depth and visibility are in tens of metres because depth is squared in
# the model, and squaring 30 m gives numbers large enough to stall the
# optimizer. Rescaling a covariate changes nothing about the biology.

reef <- reef_fish_surveys
# or: reef <- read.csv("https://github.com/bmaitner/biometry_course/raw/refs/heads/main/data/Reef_fish/NCRMP_reef_fish_surveys.csv")

reef$depth <- reef$DEPTH / 10
reef$visibility <- reef$UNDERWATER_VISIBILITY / 10
reef$year <- reef$YEAR - 2014
reef$region <- factor(reef$REGION_DESCRIPTION,
                      levels = c("Dry Tortugas",
                                 "Florida Keys: from Key West to Miami",
                                 "Miami to North Martin County"),
                      labels = c("Tortugas", "Keys", "North"))
reef$protected <- ifelse(reef$MPA_NAME == "Unprotected", "no", "yes")

# plot if you like

plot(reef$DEPTH, reef$species_richness, col = rgb(0, 0, 0, 0.1),
     xlab = "depth (m)", ylab = "species recorded")

# The starting model: richness is Poisson, its log rises and then falls
# with depth (the d2 * depth^2 term makes a hump), and it also depends on
# visibility, year, region and protection.

reef.fit <- mle2(species_richness ~ dpois(lambda = exp(int +
                                                         d1 * depth +
                                                         d2 * depth^2 +
                                                         v * visibility +
                                                         t * year)),
                 start = list(int = 3, d1 = 0, d2 = 0, v = 0, t = 0),
                 parameters = list(int ~ region + protected),
                 data = reef)

coef(reef.fit)
AIC(reef.fit)

# Questions to build models around:
#   - Is the Poisson the right distribution? Check before anything else:
#     what did Lecture 15 use var() / mean() for?
#   - With 8,560 surveys, AIC may keep nearly every term. If so, which
#     effects are large enough to matter? Back-transform them: exp() of a
#     coefficient is the multiplier on expected richness.
#   - Visibility is a property of the water on the day, not of the reef.
#     What might a visibility effect be measuring?
#   - Does protection matter once region is accounted for?

#holy shit thats a massive AIC, lets check some things rq

#Mean fish species richness
mean(reef$species_richness, na.rm = TRUE)

#Variance in fish species richness
var(reef$species_richness, na.rm = TRUE)

#Variance-to-mean ratio
var(reef$species_richness, na.rm = TRUE) /
  mean(reef$species_richness, na.rm = TRUE)
#i see evidence of overdispersion so lets test a negative binom

#Alternative Model I: Negative binomial 
#does extra variation in fish richness improve the model fit?

reef.fit2 <- mle2(species_richness ~ dnbinom(
  mu = exp(int +
             d1 * depth +
             d2 * depth^2 +
             v * visibility +
             t * year),
  size = exp(log_k)),
  start = list(int = 3, d1 = 0, d2 = 0,
               v = 0, t = 0, log_k = 2),
  parameters = list(int ~ region + protected),
  data = reef)

coef(reef.fit2)
AIC(reef.fit2)

#Compare Poisson and negative binomial
AICtab(reef.fit, reef.fit2, weights = TRUE)


#Alternative Model II: Negative binomial without protection
#does MPA status helps explain species richness? (MY ECON THESIS AHHHH)

reef.fit3 <- mle2(species_richness ~ dnbinom(
  mu = exp(int +
             d1 * depth +
             d2 * depth^2 +
             v * visibility +
             t * year),
  size = exp(log_k)),
  start = list(int = 3, d1 = 0, d2 = 0,
               v = 0, t = 0, log_k = 2),
  parameters = list(int ~ region),
  data = reef)

coef(reef.fit3)
AIC(reef.fit3)

#Compare all three models
AICtab(reef.fit, reef.fit2, reef.fit3, weights = TRUE)


#Alternative Model III: Negative binomial with linear depth
#does quadratic depth term improve model fit

reef.fit4 <- mle2(species_richness ~ dnbinom(
  mu = exp(int +
             d1 * depth +
             v * visibility +
             t * year),
  size = exp(log_k)),
  start = list(int = 3, d1 = 0,
               v = 0, t = 0, log_k = 2),
  parameters = list(int ~ region + protected),
  data = reef)

coef(reef.fit4)
AIC(reef.fit4)

#COMPARE
AICtab(reef.fit, reef.fit2, reef.fit3, reef.fit4,
       weights = TRUE)

#WHAT DOES IT MEAN? WHAT DOES IT MEAN?? AHHH
#Question 1c Explanation: 
#I compared 4 different models to determine which parameters best explain species 
#richness of Florida reef fish surveys. The original Poisson model performed significantly 
#worse than the negative bionomial model, which a deltaAIC of 13,482 (bomboclaat).
#The full negative bionomical model (reef.fit2) had the lowest AIC of all four models
#compared (62,609.90), and the greatest model weight (1.000). Removing MPA status 
#increased AIC by ~36, and removing the quadratic depth term increased AIC by ~193.
#These results suggest that both marine protectio and nonlinear depth relationships
#can help explain recodrded differences in species richness among surveys. 
#The prefered model predicts that richness increases with depth before declining after 17.4 meters,
#and that protected areas are expected to have 6.9% higher species richness.
#However, the higher recorded richness with better visibility may actually refelect the 
#ability of divers to see and detect the fish better, rather than actual differences 
#in fosh diversity. I would choose the full negative bionomial model because it reatains 
#a good balance of model fit and complexity, and best explains the variation in recorded 
#fish species richness among the four models tested.

#-------------------------------------------------------------------------------

#Part 2: Whale Shark Models

#Question: How does SST vary with latitude at recorded whale shark locations?

#work in library robis
library(robis)

#retrieve whale shark records (this takes awhile) 
records <- occurrence(scientificname = "Rhincodon typus")

#only keep records with valid SST and latitude values (bc theres some funky shit in there lowkeyly)
sharks <- records[
  !is.na(records$sst) &
    !is.na(records$decimalLatitude) &
    records$sst >= 0 &
    records$decimalLatitude >= -90 &
    records$decimalLatitude <= 90,
]

#Calculate absolute latitude (distance from equator, bc we dont want negatives vs postive for which hemi its in)
sharks$abs_lat <- abs(sharks$decimalLatitude)

#Check the dataset (what is going on in here)
nrow(sharks)
summary(sharks[, c("sst", "decimalLatitude", "abs_lat")])

#Plot SST against abs latitude
plot(sharks$abs_lat, sharks$sst,
     xlab = "Absolute latitude (degrees)",
     ylab = "Sea surface temperature (°C)",
     main = "Whale shark records: SST vs. latitude",
     pch = 16, col = rgb(0, 0, 0, 0.15))

#lets try some different models here

#Model I: Null model
#SST does not change with abs. latitude
shark.fit1 <- lm(sst ~ 1, data = sharks)

#Model II: Linear model
#SST changes linearly with abs. latitude
shark.fit2 <- lm(sst ~ abs_lat, data = sharks)

#Model III: Quadratic model
#SST changes nonlinearly with abs. latitude
shark.fit3 <- lm(sst ~ abs_lat + I(abs_lat^2), data = sharks)

#Compare all three models
AIC(shark.fit1, shark.fit2, shark.fit3)

AICtab(shark.fit1, shark.fit2, shark.fit3,
       weights = TRUE)

#coefficients of quadratic model
coef(shark.fit3)


#quadratic model residuals (is it really that good?)
par(mfrow = c(1, 2))

#Residuals versus fitted SST
plot(fitted(shark.fit3), resid(shark.fit3),
     xlab = "Predicted SST (°C)",
     ylab = "Residuals",
     main = "Residuals vs. fitted values",
     pch = 16, col = rgb(0, 0, 0, 0.15))
abline(h = 0, col = "magenta")

#Check the distribution of residuals
hist(resid(shark.fit3),
     breaks = 40,
     main = "Distribution of residuals",
     xlab = "Residual")

par(mfrow = c(1, 1))

#i got this idea from the internet thanks google ai!

#Alternative Model IV: Quadratic latitude with hemisphere
#does SST differ between the Northern and Southern Hemispheres

sharks$hemisphere <- factor(
  ifelse(sharks$decimalLatitude < 0, "South", "North")
)

shark.fit4 <- lm(sst ~ abs_lat + I(abs_lat^2) + hemisphere,
                 data = sharks)

#Comparison of all 4 models
AIC(shark.fit1, shark.fit2, shark.fit3, shark.fit4)

AICtab(shark.fit1, shark.fit2, shark.fit3, shark.fit4,
       weights = TRUE)

#check coefficients
coef(shark.fit4)


#check the preferred model before i go to the effort of writing anything

par(mfrow = c(1, 2))

plot(fitted(shark.fit4), resid(shark.fit4),
     xlab = "Predicted SST (°C)",
     ylab = "Residuals",
     main = "Residuals vs. Fitted Values",
     pch = 16, col = rgb(0, 0, 0, 0.15))
abline(h = 0, col = "turquoise")

hist(resid(shark.fit4),
     breaks = 40,
     main = "Distribution of Residuals",
     xlab = "Residual")

par(mfrow = c(1, 1))

#Part 2 Explanation
#For Part 2 I used whale shark (Rhincodon typus) occurence recorcds from the OBIS database
#to investiagte how sea surface temperature (SST) varies with geographic latitude. 
#I started by investigating the data and removing observations with missing/impossible SST/Latistude values.
#After remving these obs. I retained 16,865 whale shark occurence records. 
#Four models were compared: a null model with constant SST, a linear model with absolute 
#Latitude, a quadratic model using absolute latitude, and a quadratic model including global hemisphere. 

#The quadratic model including hemisphere (shark.fit4) had the lowest AIC (52,816.25) 
#and the greatest model weight (approximately 1.0). Removing hemisphere increased AIC by 5,411.7, 
#while the linear and null models performed much worse. These results suggest that both
#a nonlinear relationship with latitude and differences between hemispheres can help explain 
#variation in SST at recorded whale shark locations.

#The preferred model estimated that SST was approximately 1.62°C lower in the Southern Hemisphere 
#than in the Northern Hemisphere at equal absolute latitude values. The quadratic relationship
#also suggests that SST initially increases with absolute latitude, reaches a maximum around 13.4°,
#then decreases again at higher latitudes. These findings suggest that absolute latitude cannot
#fully capture geographic differences in SST on its own.

#However, residual invesitgation showed that the preferred model didn't satisfy the 
#assumptions of a normal linear regression. The residuals exhibited clustering, unequal 
#variation across fitted values, and a distribution with an aggressive peak near zero 
#and an extented negative tail. These patterns suggest that additional environmental variables, 
#seasonal differences, or different sampling methods may explain variation not captured by the current model.
#(this is exactly the same as my econ thesis where I basically learned we don't measure the correct 
#variables to exlain differences in Marine Habitat Protection). 

#I would choose the quadratic model including hemisphere because it has the best balance 
#of model fit and complexity among the four investigated models. However, because OBIS records 
#represent whale shark occurrences rather than standardized habitat surveys, and some 
#observations may not be independent, these results only describe environmental conditions 
#associated with recorded whale shark sightings rather than proving temperature preferences 
#or habitat selection of Rhincodon typus. 

