################################################################################################
##
##
##                                          Class II
##
##
################################################################################################

## Permutations
################################
# the idea here is to randomly reorder the matrix to destroy any association
# between the data. This will allow you to build a new NULL model of your data

data <- iris #should be rerun

# randomize data
permuted_data <- data.frame(data$Sepal.Length[sample(nrow(data))], data$Petal.Length[sample(nrow(data))])
colnames(permuted_data) = c("Sepal.Length", "Petal.Length")

# now that we have reordered the data, making any association between the two random, let us compute some 
# descriptive stats 

cor.test(data$Sepal.Length, data$Petal.Length)
cor.test(permuted_data$Sepal.Length, permuted_data$Petal.Length)


########## Exercise 6
##########################################
# repeat permutations 1000 times, plot a histogram of the effects. What do you see?










## Bootstrapping
# bootstrapping is similar to permutations yet we NEED replacement i.e., everytime we take a marble (data point)
# from the bag, we need to put it back so that we can allow us the possibility of us to take it a second time 

bootstrap_data= data.frame( data$Sepal.Length[sample(nrow(data), replace = TRUE)], data$Petal.Length[sample(nrow(data), replace = TRUE)])
colnames(bootstrap_data) = c("Sepal.Length", "Petal.Length")

apply(bootstrap_data, 2, mean)
apply(bootstrap_data, 2, sd)

########## Exercise 7
##########################################
# repeat bootstrap 1000 times, save the mean and sd every time, plot a histogram of these values. What do you see?
# can we make CI around this distribution? This is what we call bootstrapping an estimate (i.e, bootstrap CI, mean etc)








## ANOVAs
# let us try a one way anova 
#reload data if required, needs to be read as a TSV
hot100=read.csv(
  'C:/Users/Debajyoti Saha/Documents/GitHub/Sta-R-tistics/datasets/Billboard_Hot_weekly_charts/Hot100AudioFeatures.tsv', 
  sep = "\t")

hot100_4anova=hot100[!is.na(hot100$spotify_track_explicit),] # remove nas
hot100_4anova$key <- as.factor(hot100_4anova$key)
levels(hot100_4anova$key) # these are the levels of your first factor! 
anova0=aov(danceability ~ key, hot100_4anova)

summary(anova0) # summary of effects

# now let us look at post-hoc test
TukeyHSD(anova0)
pairwise.t.test(hot100_4anova$danceability, hot100_4anova$key,
                p.adjust.method = "BH")
# as you can see these are A LOT of post-hoc tests that you need to run....

# let us try another anova:
anova0=aov(danceability ~spotify_track_explicit , hot100_4anova)
summary(anova0) # summary of effects

########## Theory Question 
##########################################
# How is a one-way anova with two groups similar to a t-test? Do you need to run pot-hoc tests after you 
# run an one way anova with only two factors? why? 



# compare it to a t-test
sample1=hot100_4anova$danceability[hot100_4anova$spotify_track_explicit==FALSE] # collect sample 1 (non-explicit)
sample2=hot100_4anova$danceability[hot100_4anova$spotify_track_explicit==TRUE] # collect sample 2 (explicit)

# simple two sample t-test (two-sided) assuming equal variance 
t.test(sample1,sample2, var.equal = TRUE)

## assumptions of an anova
########
anova0=aov(danceability ~ key, hot100_4anova)

summary(anova0) # summary of effects
# 1. Homogeneity of variances
# one check you can do is plot the residuals against the fitted values, there should be no relationship 
plot(anova0, 1)

# you can also run a Levne Test
car::leveneTest(danceability ~ key, hot100_4anova)
# despite a seemingly small if any relationship we get a significant Levene test... WHY?

# 2. Normality
# let us plot a q-qplot
plot(anova0, 2)

# Can check the normality of the residuals 
# Extract the residuals
aov_residuals = residuals(object = anova0 )
hist(aov_residuals)
# Run Shapiro-Wilk test
shapiro.test(aov_residuals ) #size problem

## Running a two-way anova is just as easy!
# we will look at how explict tracks and key affect dancibility

hot100_4anova=hot100_4anova[!is.na(hot100_4anova$key),] # remove nas
hot100_4anova$key_binary=as.numeric(hot100_4anova$key)>=6

anova0=aov(danceability ~ spotify_track_explicit + key_binary, hot100_4anova)
summary(anova0)

library(tidyverse) 
#need it for the magrittr
# compute cell means
hot100_4anova %>% group_by(spotify_track_explicit, key_binary) %>% summarise(m=mean(danceability))

# we can also add an interaction term! (note you can use : or * for an interaction, this simply determines if it is fully crossed or not...
# try running a three way interaction and you will notice the difference between the two operators)

anova0=aov(danceability ~ spotify_track_explicit + key_binary + spotify_track_explicit*key_binary, hot100_4anova)
summary(anova0)


##########  Exercise 8
##########################################
# open the mice data we have (https://www.kaggle.com/ruslankl/mice-protein-expression/version/1)
# this experiment uses two type of genotype mice (controls vs DS) across two conditions (i.e., treatment vs saline)
# load the data from this experiment. Is it repeated measures? what kind of experiment is this? How many mice do you have per condition? 
# Pick a protein, and run a t-test across the two levels genotype and treatment
# now run a two way anova, what do you find? How would you test this for multiple proteins? How could you control for 
# multiple comparisons? 











## Regression
## linear Regression
#########################################
#reload data if required, needs to be read as a TSV, remove NAs again?
hot100=read.csv(
  'C:/Users/Debajyoti Saha/Documents/GitHub/Sta-R-tistics/datasets/Billboard_Hot_weekly_charts/Hot100AudioFeatures.tsv', 
  sep = "\t")
# let us run a regression to test the relationship between two variables
lm0=lm(danceability ~ loudness, hot100)
summary(lm0)
install.packages(sjPlot)
sjPlot::tab_model(lm0)
confint(lm0)


########## Theory Question 
##########################################
# What does a significant intercept mean?
# what does a significant slope mean? 
# How does sample size affect our ability to find effects? Try and use a smaller sample and replicate this effect



# we can add multiple predictors to our models
lm0=lm(danceability ~ loudness + valence + liveness, hot100)
summary(lm0)
sjPlot::tab_model(lm0)

# We can add predictors in the model that we do NOT care about but wish to control for their effect
# these are called covariates and we can 'regress' their effect out of the data
# we can add multiple predictors to our models
lm0=lm(danceability ~ loudness + valence + liveness +tempo, hot100)
summary(lm0)
sjPlot::tab_model(lm0)

# we can save the residuals from a regression and use them for other purposes
# this is the equivalent of removing the effect of tempo on loudness for you to then
# use it in another analysis
# Please note that if you run another regression you can simply build ONE large model while covarying out the effect of tempo (see above) 
lm0=lm(danceability ~ tempo, hot100)
corrected4tempo=lm0$residuals


# we can also test for an interaction term!
lm0=lm(danceability ~ loudness + valence + loudness*valence, hot100)
summary(lm0)
sjPlot::tab_model(lm0)

## How are ANOVAs like Regressions?
# first let us run a regression with binary or categorical predictors 

# we will look at how explicit tracks and which key affect danceability
hot100_4anova=hot100[!is.na(hot100$spotify_track_explicit),] # remove nas
hot100_4anova=hot100_4anova[!is.na(hot100_4anova$key),] # remove nas
hot100_4anova$key_binary=as.numeric(hot100_4anova$key)>=6

# Let us dummy code our variables and save them as factors 
hot100_4anova$key_binary=plyr::mapvalues(hot100_4anova$key_binary, c(TRUE, FALSE), c(1,0))
hot100_4anova$spotify_track_explicit=plyr::mapvalues(hot100_4anova$spotify_track_explicit, c(TRUE, FALSE), c(1,0))

hot100_4anova$key_binary=as.factor(hot100_4anova$key_binary)
hot100_4anova$spotify_track_explicit=as.factor(hot100_4anova$spotify_track_explicit)

lm0=lm(danceability ~ spotify_track_explicit + key_binary + spotify_track_explicit*key_binary, hot100_4anova)
summary(lm0)
sjPlot::tab_model(lm0)


# compare the outputs of the lm0 and anov0
# how are they different?

anov0= aov(danceability ~ spotify_track_explicit + key_binary + spotify_track_explicit*key_binary, hot100_4anova)
summary(anov0)

summary(lm0)
# Look at the cell means? How do they relate to the regression?
hot100_4anova %>% group_by(spotify_track_explicit, key_binary) %>% summarise(m=mean(danceability))