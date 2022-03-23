#######################################################################################
#
#
#                                 Class III Statistics intro
#                             
#
#
#######################################################################################

## variable coding
# let us play around with the coding of the effects!
# before we did what is called dummy coding (i.e., 0 and 1s)
# let us try effect coding (i.e., -1 and 1):

library(tidyverse)

#load data 
hot100=read.csv(
  'C:/Users/jasondsc/Documents/GitHub/Sta-R-tistics/datasets/Billboard_Hot_weekly_charts/Hot100AudioFeatures.tsv', 
  sep = "\t")

# Let us dummy code our variables and save them as factors 
hot100_4anova$key_binary=plyr::mapvalues(hot100_4anova$key_binary, c(TRUE, FALSE), c(1,0))
hot100_4anova$spotify_track_explicit=plyr::mapvalues(hot100_4anova$spotify_track_explicit, c(TRUE, FALSE), c(1,0))

hot100_4anova$key_binary=as.factor(hot100_4anova$key_binary)
hot100_4anova$spotify_track_explicit=as.factor(hot100_4anova$spotify_track_explicit)

lm0=lm(danceability ~ spotify_track_explicit + key_binary + spotify_track_explicit*key_binary, hot100_4anova)
summary(lm0)
sjPlot::tab_model(lm0)


# effect coding 
hot100_4anova=hot100[!is.na(hot100$spotify_track_explicit),] # remove nas
hot100_4anova=hot100_4anova[!is.na(hot100_4anova$key),] # remove nas
hot100_4anova$key_binary=as.numeric(hot100_4anova$key)>=6

hot100_4anova$key_binary=plyr::mapvalues(hot100_4anova$key_binary, c(TRUE, FALSE), c(1,-1))
hot100_4anova$spotify_track_explicit=plyr::mapvalues(hot100_4anova$spotify_track_explicit, c(TRUE, FALSE), c(1,-1))


lm0=lm(danceability ~ spotify_track_explicit + key_binary + spotify_track_explicit*key_binary, hot100_4anova)
summary(lm0)

temp=hot100_4anova %>% group_by(spotify_track_explicit, key_binary) %>% summarise(m=mean(danceability))
print(temp)
# Grand mean
mean(temp$m)

# let us try effect coding (i.e., -0.5 and 0.5):

hot100_4anova=hot100[!is.na(hot100$spotify_track_explicit),] # remove nas
hot100_4anova=hot100_4anova[!is.na(hot100_4anova$key),] # remove nas
hot100_4anova$key_binary=as.numeric(hot100_4anova$key)>=6

hot100_4anova$key_binary=plyr::mapvalues(hot100_4anova$key_binary, c(TRUE, FALSE), c(0.5,-0.5))
hot100_4anova$spotify_track_explicit=plyr::mapvalues(hot100_4anova$spotify_track_explicit, c(TRUE, FALSE), c(0.5,-0.5))


lm0=lm(danceability ~ spotify_track_explicit + key_binary + spotify_track_explicit*key_binary, hot100_4anova)
summary(lm0)

temp=hot100_4anova %>% group_by(spotify_track_explicit, key_binary) %>% summarise(m=mean(danceability))
print(temp)

# Grand mean
mean(temp$m)
# look at slopes from table above (diff from explicit to non-explicit)

##Suseful and relevant resource: https://sphweb.bumc.bu.edu/otlt/MPH-Modules/BS/R/R5_Correlation-Regression/index.html
## assumptions of a Regression
########
sample1=hot100_4anova[sample(10000,length(hot100_4anova)),]
lm0=lm(danceability ~  key, sample1)
summary(lm0)
# 1. Linearity of relationship
## this can be checked with a residual vs fitted plot. The data should show no relationship 
plot(lm0,1)

# 2. Normality
## qq-plots 
plot(lm0,2)

# 3. Homogeneity of variance 
## can check if the residuals are spread out equally along the range of the predictors
# one potential solution to both normality and heteroscedasticity is a log or square root transform of the data
# try taking the log or square root of your predictors before fitting the model, see what happens
plot(lm0,3)

# 4. Looking for outliers
## we also want to check that the data does not have any outliers, this may affect if our regression will be significant or not 

# Cook's distance
# the rule of thumb would be an observation might be an outlier if it's Cook's distance exceeds 4/(n - p - 1)(P. Bruce and Bruce 2017), 
# where n is the number of observations and p the number of predictor variables.
# look at the plots below and determine if there is an outlier
plot(lm0, 4)
# Residuals vs Leverage
plot(lm0, 5)

#Install if required
install.packages("see")
library(see)

# alternative way to plot assumptions
performance::check_model(lm0)

## Logistic Regression
#######################
# so far we have looked at predicting continuous y values.... but what happens when the y isn't continuous? 
# let us say we want to predict if a song will be explicit based on some of its properties...

hot100_4anova=hot100[!is.na(hot100$spotify_track_explicit),] # remove nas
hot100_4anova$spotify_track_explicit=plyr::mapvalues(hot100_4anova$spotify_track_explicit, c(TRUE, FALSE), c(1,0))

glm0= glm(spotify_track_explicit ~ danceability + energy + loudness + valence, hot100_4anova, family = 'binomial')
summary(glm0)
sjPlot::tab_model(glm0) # reports odds ratio
sjPlot::tab_model(glm0, transform = NULL) # reports log odds (see slides for explanation)

# we can build a regression model and use it to predict held back data:
ids=sample(length(hot100_4anova$SongID), 2*ceiling(length(hot100_4anova$SongID)/3)) #Would it make more sense to train the model on 2/3 of the data and predict on the remaining 1/3?
sample1=hot100_4anova[ids,]
sample2=hot100_4anova[!(seq(1,length(hot100_4anova$SongID)) %in% ids),]

lm0= lm(danceability ~  energy + loudness + valence, sample1)


########### exercise X
## use the function predict to get the dancibility scores predicted by the lm
# compute error (predicted-observed), plot a histogram of this error
# plot a scatter plot of predicted vs observed data

predicted_labels=predict(lm0, sample2) # predict danceability from regression above
error_predicted=predicted_labels-sample2$danceability # look at the error between true and predicted values 
summary(error_predicted)
hist(error_predicted)
summary(sample2$danceability)
summary(predicted_labels)
plot(predicted_labels, sample2$danceability)


# this also works for glm (logistic regressions return the probability of being a class)
glm0= glm(spotify_track_explicit ~ danceability + energy + loudness + valence, sample1, family = 'binomial')

predicted_labels=predict(glm0, sample2,type="response") 
head(predicted_labels)
range(predicted_labels, na.rm = TRUE) # notice that it returns data between 0 and 1
# we can see how many entires it correctly identifies by looking at the categories
# any data with a prob above 0.5 will be labeled as 1, below 0.5 will be 0
prediction=rep(0, length(predicted_labels))
prediction[predicted_labels>0.5]=1

# compute accuracy 
sum(prediction==sample2$spotify_track_explicit)/length(predicted_labels)


## Hierarchical regressions 
##############################################

# let us now look at epilepsy data where patients, seen multiple times. Number of seizures were recorded per visit. Patients receieved treatment for 
# anti-convulsants

#Install if required
install.packages("brms")

epilepsy=brms::epilepsy
epilepsy$Trt=as.factor(epilepsy$Trt)

lm0=lm(count ~ Age  + Trt, epilepsy)
summary(lm0)
sjPlot::tab_model(lm0)

## The above model (you built) is good, but it can be better.....
# We can take into account how observations are in fact nested within participants (repeated measures design)
# to do this we will build a model where we allow participants to receive a unique intercept 
# this intercept fit per participant will take into account inter-individual differences 

lme0=lme4::lmer(count ~ Age  + Trt + (1| patient), epilepsy)
summary(lme0)
sjPlot::tab_model(lme0)
# now you see that you have a seprate section for random effects!
# Note that you have the ICC which is like an ANOVA in the sense that 
# it measures the ratio of variability within vs between groups
# It ranges from 0 to 1, where the higher the ICC the more
# your data is clustered and thus less independent 



# we now see that the outputs of the regression are broken down into both fixed and random effects
# fixed effects are between and random are repeated (see slides for details)
coef(summary(lme0)) # this will give you the fixed effects reported in summary 

lme4::ranef(lme0)$patient # and these are the values for the random intercepts fit per person
# the random intercepts should have the same number of participants (in order)

# the above multi-level model assumes participants differ in terms of their overall number of seizures, and so to account for this
# each subject was given a different intercept (i.e., they all start at a different point) but their overall effect is consistent 

# what if we wanted to test a different hypothesis where the effect of the treatment may be different for each subject
# but that their starting point (intercept) was similar across everyone?

# here we would run a model with a random SLOPE and INTERCEPT 

lme1=lme4::lmer(count ~ Age  + Trt + (1 + Trt| patient), epilepsy) #Apparently this model does not converge. 
summary(lme1)
sjPlot::tab_model(lme1)
coef(lme1) # notice how both the intercept and slope vary per subject 


# note that you can make these multilevel models (hierarchical models) into logistic ones using the function of glmer. It works
# just like the glm function, see sections above and slides 



# Random Slopes
######################################
## code below adapted from https://github.com/mkfreeman/hierarchical-models/blob/master/generate-data.R
cognitive_control= read.csv('C:/Users/Debajyoti Saha/Documents/GitHub/Sta-R-tistics/datasets/Cognitive_control_figshare/Cognitive_control.csv')
colnames(cognitive_control)[1] <- "Task" #Column name is mangled when read in 
cognitive_control= cognitive_control[cognitive_control$Task=='Stroop',]

cognitive_control$subject=as.factor(cognitive_control$subject)
cognitive_control$current.trial= plyr::mapvalues(cognitive_control$current.trial, c('incongruent', 'congruent'), c(0,1))

cognitive_control=cognitive_control[cognitive_control$subject %in% c(1,2,3,4,5,6),]

# Model without respect to grouping
m0 <- lm(RT ~ current.trial, data=cognitive_control)
predict(m0)
cognitive_control$simple.model <- predict(m0)
coef(m0)

# Model with varying intercept
m1 <- lme4::lmer(RT ~ current.trial + (1|subject), data = cognitive_control)
cognitive_control$random.intercpet.preds <- predict(m1)
coef(m1)


# Model with varying slope and intercept
m3 <- lme4::lmer(RT ~ current.trial + (1 + current.trial|subject), data=cognitive_control)
cognitive_control$random.slope.int.preds <- predict(m3)
coef(m3)

# Some plots using ggplot2
library(ggplot2)

# Visualize simple model
ggplot(data=cognitive_control, aes(x=current.trial, y=random.intercpet.preds, group = subject, colour = subject)) +
  geom_line() + geom_point() + theme_minimal() + #need a plus sign here
labs(x="RT", y="Incongrunet") +
  ggtitle("Varying Intercept") + 
  scale_colour_discrete('Subject')     

# Visualize random intercept
ggplot(data=cognitive_control, aes(x=current.trial, y=simple.model, group = subject, colour = subject)) +
  geom_line() + geom_point() + theme_minimal() + #need a plus sign here
labs(x="RT", y="Incongrunet") +
  ggtitle("Fixed Slope and Intercept") + #Fix label
  scale_colour_discrete('Subject')     


# Visualize random slope + intercept
ggplot(data=cognitive_control, aes(x=current.trial, y=random.slope.int.preds, group = subject, colour = subject)) +
  geom_line() + geom_point() + theme_minimal() + #need a plus sign here
labs(x="RT", y="Incongrunet") +
  ggtitle("Varying slopes and intercept") + 
  scale_colour_discrete('Subject')   


##########  Exercise 8
##########################################
# coming back to the open mice data, let us build on the idea of random slopes
# can we build a single model where we test the effect of the genotype and treatment 
# on all proteins? 
# try using hierarchical models and random slopes to make a complex model that can tackle all our questions at once

# hint use the stack function to reorganize your data

protein = read.csv('~/Documents/GitHub/Sta-R-tistics/datasets/MouseProtein_Kaggle/Data_Cortex_Nuclear.csv')

table(protein$MouseID)
table(protein$Treatment)
table(protein$Genotype)

protein_stacked=cbind(protein$MouseID, protein[,79:82] ,stack(protein[,2:78]))
colnames(protein_stacked)[1]='MouseID'

#tidy alternative for the above:
protein %>% pivot_longer(cols = c(-MouseID, -Genotype, -Treatment, - Behavior, - class), names_to = "ind") %>% protein_stacked

lme0= lme4::lmer(values ~ Genotype + Treatment + Genotype*Treatment + (Genotype | ind), protein_stacked)
summary(lme0)
sjPlot::tab_model(lme0)
coef(lme0)


##########  Log Likelihoods & Model fits
##########################################

# when building regression models sometimes we want to assess what is THE best model
# this will allow us to test what predictors are significant AND which are the most useful

# one way to do this is to build your model in steps:

protein = read.csv('~/Documents/GitHub/Sta-R-tistics/datasets/MouseProtein_Kaggle/Data_Cortex_Nuclear.csv')
lm0=lm(BDNF_N ~ 1, protein) # this is the intercept ONLY model
summary(lm0)

lm1=lm(BDNF_N ~ Genotype, protein) 
summary(lm1)

lm2=lm(BDNF_N ~ Genotype + Treatment, protein) 
summary(lm2)

lm3=lm(BDNF_N ~ Genotype + Treatment + Genotype*Treatment, protein) 
summary(lm3)

anova(lm0,lm1,lm2,lm3) # goodness of fit comparison between models

logLik(lm0) # log likelihood 
logLik(lm1) # log likelihood 
AIC(lm0,lm1,lm2,lm3) # AIC 
BIC(lm0,lm1,lm2,lm3) # BIC 


lme0=lme4::lmer(count ~(1| patient), epilepsy)
summary(lme0)
sjPlot::tab_model(lme0)
lme1=lme4::lmer(count ~ Age + (1| patient), epilepsy)
lme2=lme4::lmer(count ~ Age + Trt+ (1| patient), epilepsy)
lme3=lme4::lmer(count ~ Age + Trt+ Age*Trt+ (1| patient), epilepsy)

anova(lm0,lm1,lm2,lm3)
AIC(lm0,lm1,lm2,lm3) # AIC 
BIC(lm0,lm1,lm2,lm3) # BIC 

# Note that you can compute a Bayes Factor from the BIC
# see Bayesian class for details of interpretations
# cut offs for interpretation: https://en.wikipedia.org/wiki/Bayes_factor
# BF = e^((BIC_Null-BIC_alt)/2)
BF = exp(((BIC(lm0)-BIC(lm3))/2))

