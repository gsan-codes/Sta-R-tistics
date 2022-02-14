#######################################################################################
#
#
#                                 Class XXX Statistics intro
#                             
#
#
#######################################################################################
######
# See slides on GitHub for theory
#####
########## Descriptive stats
library(dplyr)
head(iris)
data=iris
data[55,4]=NaN
## mean
mean(iris$Sepal.Length) # take mean of the entire column
mean(data$Sepal.Length) # take mean of the entire column
mean(data$Sepal.Length, na.rm = TRUE) # take mean of the entire column, you can remove NaN this way
colMeans(iris[,1:4]) # take mean of every coloumn 
rowMeans(iris[,1:4]) # take mean of every row Note that these functions require NUMERIC data

## Median
# Note that median DOES NOT have a function for rows and cols
median(iris$Sepal.Length)
# what if we used apply to help us with this?
lapply(data[1:4], median, na.rm = TRUE)
apply(data[1:4], 1, median, na.rm = TRUE)
apply(data[1:4], 2, median, na.rm = TRUE)

## Other useful descriptive functions
sum(iris$Sepal.Length)
rowSums(data[,1:4])
colSums(data[,1:4])

# remember you can use dplyr and tidyr functions to help you
summary_stats=iris %>% group_by(Species) %>% summarise(mean_length=mean(Sepal.Length), N=n(), mode_length=mode(Sepal.Length))

########## Exercise 1
##########################################
# R does not have an inbuilt Mode function, try building one yourself (note: DescTools pacakge has a function Mode)



## Max, Min, Min k
max(iris$Sepal.Length)
min(iris$Sepal.Length)
tail(order(iris$Sepal.Length), 5) # biggest 5 elements 
head(order(iris$Sepal.Length), 5) # smallest 5 elements 

########## Exercise 2
##########################################
# get the smallest 5 elements from each column


## Data spread
## Standard deviation
# we similarly have to make our own function to apply sd to rows or columns 
sd(iris$Sepal.Length)
apply(data[1:4], 2, sd, na.rm = TRUE)
apply(data[1:4], 1, sd, na.rm = TRUE)

## Other useful descriptive functions
apply(data[1:4], 1, quantile, na.rm = TRUE)

apply(data[1:4], 2, range, na.rm = TRUE)

# inter quantile range (i.e., difference between first and third quantile)
apply(data[1:4], 2, IQR,na.rm = TRUE)



########## Exercise 3
##########################################
# scale columns/ rows by mean and sd. Do NOT use the scale function (you can use the sclae function for this, but I want you to try without it first)
(data[,1:4] - colMeans(data[,1:4]))/apply(data[1:4], 2, sd, na.rm = TRUE)



# Confidence intervals 

# compute 95% confidence intervals of a normal distribution
# this is the equivalent of the following formula: 
#               
#                 CI = mean(x) =/- z* (s/sqrt(n))
#
# where z is the value of the cumulative normal dist of the confidence level 
# s is the standard deviation and n is the sample size
# note that (s/sqrt(n)) is also standard error 

CI = qnorm(0.975) * sd(iris$Sepal.Length)/sqrt(length(iris$Sepal.Length))
CI_upper = mean(iris$Sepal.Length) + CI
CI_lower = mean(iris$Sepal.Length) - CI


# we will look at a different method for computing CI later

########## Exercise 4
##########################################
# use apply to compute CI for each column 

CI = function(data, conf=0.95){
  conf_level=1-((1-conf)/2)
  CI= qnorm(0.975) * sd(data)/sqrt(length(data))
  return(c(mean(data)+CI, mean(data)-CI))
}

apply(iris[1:4],2, CI)


## Correlations
cor.test(iris$Sepal.Length,iris$Petal.Length) # pearson correlation looks at the relationship between two continupus variables 
# it is a simple ratio of the covariance between the two variables normalized by their standard deviation 
# note that the correlation istelf is just a description, the statistic we get is PARAMETRIC 

# when your data is ordinal or when you want to run a non-parametric test
cor.test(iris$Sepal.Length,iris$Petal.Length, method = "spearman")
# this looks at the monotonic relationship between variables by ranking your data
# does increasing rank in one increase the other variable 


########## Inferential stats
## T-tests in R 
# All the different kinds of t-test you would like to run can be done with the t.test() function using different inputs 
# let us compare the loudness of music on spotify between groups of explicit vs non-explicity 

# one sample tests:
hist(iris$Sepal.Length)
mean(iris$Sepal.Length)

# let us test if the mean of the Sepal Length is different than zero!
t.test(iris$Sepal.Length,var.equal = TRUE)
# Now let us test if it is BIGGER than zero
t.test(iris$Sepal.Length,var.equal = TRUE, alternative = "greater")
# Now let us test if it is LESS than zero
t.test(iris$Sepal.Length,var.equal = TRUE, alternative = "less")
# Now let us test if it is LESS than 7
t.test(iris$Sepal.Length,var.equal = TRUE, alternative = "less", mu = 7)


# two sample tests:
hot100=read.csv('/Users/jasondsc/Documents/GitHub/Sta-R-tistics/data/Hot_100_Audio_Features.csv') # read data
hot100=hot100[!is.na(hot100$spotify_track_explicit),] # remove nas
hot100=hot100[!is.na(hot100$loudness),] # remove nas

sample1=hot100$loudness[hot100$spotify_track_explicit==FALSE] # collect sample 1 (non-explicit)
sample2=hot100$loudness[hot100$spotify_track_explicit==TRUE] # collect sample 2 (explicit)

# simple two sample t-test (two-sided) assuming equal variance 
t.test(sample1,sample2, var.equal = TRUE)
# simple two sample t-test (two-sided) un-equal variance 
t.test(sample1,sample2)
# simple two sample t-test (two-sided) assuming equal variance, testing that sample 1 is LOWER than sample 2
t.test(sample1,sample2, var.equal = TRUE, alternative = "less")
# simple two sample t-test (two-sided) assuming equal variance, testing that sample 1 is GREATER than sample 2
t.test(sample1,sample2, var.equal = TRUE, alternative = "greater")

########## Exercise 5
##########################################
# what happens when we lower the N? take a sub sample of data from each group and run a t-test three times. Do this for a N of 5, 10, 25, 50, 100
# what do you notice? 


# Let us try paired tests: REMINDER that this means repeated measures (i.e., every observation comes in a PAIR)
# anxiety dataset: two groups were measured across time, looked at their anxiety across treatment 
# comparing the final scores across the two groups is a UNPAIRED test (see above)
# comparing data within a group across time is a PAIRED test
library(datarium)
data("anxiety")
sample1=anxiety$t1[anxiety$group=="grp1"]
sample2=anxiety$t2[anxiety$group=="grp1"]

t.test(sample1,sample2, var.equal = TRUE, paired = TRUE)
# OR YOU CAN RUN THIS: IT IS THE SAME
t.test(sample1-sample2, var.equal = TRUE)


########## Theory Question 
##########################################
# run some code and see if testing a paired test is the same as running a one-sample t test against zero? Why would this be the case?
# try and reason out your answer 



## assumptions of a t-test
########
# 1. Independence 
# we first assume that the data are independent of one another... this complicated (will discuss in course)
# does a correlation between two variables break this assumption?

# 2. Normality 
# we can test this many ways: first let us look at the data 

hist(iris$Sepal.Length)
hist(hot100$loudness[hot100$spotify_track_explicit==FALSE])
hist(anxiety$t1[anxiety$group=="grp1"])

# we can also run a Shapiro-Wilk test to test for normality 
shapiro.test(iris$Sepal.Length)
shapiro.test(hot100$loudness[hot100$spotify_track_explicit==FALSE]) # is sample size a problem? Why?

# Q-Q plot
# here we plot our datas quantiles against a theortical normal dist quantiles
# we should hopefully see that our data lines up nicely with the theoretical one
# along the x-y diagonal line. Any deviation from this line would indicate some 
# non-normal property of the distribution

qqnorm(iris$Sepal.Length, pch = 1, frame = FALSE)
qqline(iris$Sepal.Length, col = "pink", lwd = 5)

car::qqPlot(iris$Sepal.Length) # same thing different package (possibly 'nicer')

# let us generate some theoretical data
data_sim=rnorm(10,0,1)
qqnorm(data_sim, pch = 1, frame = FALSE)
qqline(data_sim, col = "pink", lwd = 5)
# what happens as you add more samples?
data_sim=runif(10,-5,5)
qqnorm(data_sim, pch = 1, frame = FALSE)
qqline(data_sim, col = "pink", lwd = 5)

data_sim=rpois(10,2)
qqnorm(data_sim, pch = 1, frame = FALSE)
qqline(data_sim, col = "pink", lwd = 5)

# 3. Homogeneity of variance 
# checking that the variances of the two groups are the same 
sample1=anxiety$t1[anxiety$group=="grp1"]
sample2=anxiety$t1[anxiety$group=="grp2"]
var.test(sample1,sample2)

## Effect sizes and Power
################################

# using the pwr package, let us compute a curve to see how many samples we need (for a given effect size)
# to achieve 80% power


cohend=seq(0.1,0.8,0.1)
count=1
sample_sizes= seq(5, 500, 1)                                                                                                                                                                                         color = "black", size=1)
one_group=data.frame(power=c(), effect_size=c(), sample_size=c())

# iterate over possible effect sizes and compute power given sample size 
for (ds in cohend){
  
  for (i in 1:length(sample_sizes)){
    power=pwr::pwr.t.test(n=sample_sizes[i],d=ds, sig.level = 0.05, type= 'paired', alternative = 'two.sided')
    one_group[length(sample_sizes)*(count-1)+i,1]=power$power
    one_group[length(sample_sizes)*(count-1)+i,2]=ds
    one_group[length(sample_sizes)*(count-1)+i,3]=sample_sizes[i]
  }
  count=count+1
}   

one_group$V2 = as.factor(one_group$V2 )

ggplot(data=one_group, aes(x=V3, y=V1, colour=V2, group=V2)) +
  geom_line(size=1.3) + scale_fill_brewer(palette="Pastel1") + theme_minimal() + labs(y= "Power (%)", x ="Sample size (N)", color= "Effect Size")  +
  coord_cartesian(ylim=c(0, 1)) + theme(legend.justification=c(1,1), legend.position=c(1,1)) + geom_hline(yintercept = 0.80, linetype="dashed", 
                                                                                                          color = "black", size=1)

ggplot(data=one_group, aes(x=V3, y=V1, colour=V2, group=V2)) + xlim(c(0,100)) +
  geom_line(size=1.3) + scale_fill_brewer(palette="Pastel1") + theme_minimal() + labs(y= "Power (%)", x ="Sample size (N)", color= "Effect Size")  +
  coord_cartesian(ylim=c(0, 1)) + theme(legend.justification=c(1,1), legend.position=c(1,1)) + geom_hline(yintercept = 0.80, linetype="dashed", color = "black", size=1)
                                                                                                          


# I have provided a list of samples I have extracted from 38 random EEG papers sampled from the literature
# Let us use these sample sizes and different effect sizes to compute how much statitical power the field
# has to find different effects

# For Balanced ONE-WAY-ANOVA Test
cohend=seq(0.1,0.8,0.05)
count=1
num_group=2
data=read.csv("data/sample_sizes.csv")
sample_sizes=data$nsub_after_exc

sample_sizes=as.data.frame(sample_sizes)
# plot histogram of sample sizes in the field
ggplot(sample_sizes, aes(x=sample_sizes)) + 
  geom_histogram(aes(y=..density..),color="black", fill="pink", binwidth = 5, position = "identity") +geom_density(alpha=0.6, fill="pink") + xlab("Sample Size") + ylab("Number of Studies") + theme_minimal() + geom_vline(xintercept = median(sample_sizes[,1]), linetype="dashed", 
                                                                                                                                                                                                                            color = "black", size=1)
one_group=data.frame()

# iterate over possible effect sizes and compute power given sample size 
for (ds in cohend){
  
  for (i in 1:length(sample_sizes[,1])){
    power=pwr::pwr.anova.test(k=num_group,n=sample_sizes[i,], sig.level = 0.05, f = ds)
    print(sample_sizes[i,])
    print(power$power)
    one_group[count,i]=power$power
  }
  count=count+1
}   

# find what porportion of studies have an power greater than X
porportion=seq(0,1,0.01) # make sequence of porportion we care about 
porportions_bigger_than=data.frame()
for (k in 1:length(cohend))
  for (j in 1:length(porportion)){
    
    jj=porportion[j]
    pro=mean(one_group[k,] > jj)
    porportions_bigger_than[k,j]=pro
  }

power_plot=as.data.frame(t(rbind(porportion, porportions_bigger_than)))
colnames(power_plot)[1]= "power"


## FOR ANOVA EFFECT SIZES
rr=rep(c(0.1,0.2,0.3,0.4,0.5),101)
rr=rr[order(rr)]
ploter=cbind(100*rep(porportion,5),rr,c(power_plot$V2 , power_plot$V4, power_plot$V6, power_plot$V8, power_plot$V9))
ploter=as.data.frame(ploter)
ploter$rr=as.factor(ploter$rr)

ggplot(data=ploter, aes(x=V1, y=V3, colour=rr, group=rr)) +
  geom_line(size=1.3) + scale_fill_brewer(palette="Pastel1") + theme_minimal() + labs(y= "Porportion of Studies", x ="Power (%)", color= "Effect Size")  +
  coord_cartesian(ylim=c(0, 1)) + theme(legend.justification=c(1,1), legend.position=c(1,1)) + geom_vline(xintercept = 80, linetype="dashed", 
                                                                                                          color = "black", size=1)



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

# randomize data
permuted_data= data.frame( data$Sepal.Length[sample(nrow(data))], data$Petal.Length[sample(nrow(data))])
colnames(permuted_data) = c("Sepal.Length", "Petal.Length")

# now that we have reordered the data, making any association between the two random, let us compute some 
# descriptive stats 

cor.test(data$Sepal.Length, data$Petal.Length)
cor.test(permuted_data$Sepal.Length, permuted_data$Petal.Length)


########## Exercise 6
##########################################
# repeat permutations 1000 times, plot a histogram of the effects. What do you see?

permuted_r= c()


for (i in 1:1000){
  
  permuted_data= data.frame( data$Sepal.Length[sample(nrow(data))], data$Petal.Length[sample(nrow(data))])
  colnames(permuted_data) = c("Sepal.Length", "Petal.Length")
  temp=cor.test(permuted_data$Sepal.Length, permuted_data$Petal.Length)
  permuted_r[i]= temp$estimate
  
}

hist(permuted_r)
quantile(permuted_r, 0.95)

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

bootstrap_mean_SL =c()
bootstrap_mean_PL =c()
bootstrap_sd_SL =c()
bootstrap_sd_PL =c()

for (i in 1:1000){
  
  bootstrap_data= data.frame( data$Sepal.Length[sample(nrow(data), replace = TRUE)], data$Petal.Length[sample(nrow(data), replace = TRUE)])
  colnames(bootstrap_data) = c("Sepal.Length", "Petal.Length")
  
  bootstrap_mean_SL[i]=apply(bootstrap_data, 2, mean)[1]
  bootstrap_mean_PL[i]=apply(bootstrap_data, 2, mean)[2]
  bootstrap_sd_SL[i]=apply(bootstrap_data, 2, sd)[1]
  bootstrap_sd_PL[i]=apply(bootstrap_data, 2, sd)[2]
  
  
}

hist(bootstrap_mean_SL)
quantile(bootstrap_mean_SL, 0.025)
quantile(bootstrap_mean_SL, 0.975)

## ANOVAs
# let us try a one way anova 
hot100_4anova=hot100[!is.na(hot100$spotify_track_explicit),] # remove nas
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
shapiro.test(aov_residuals )

## Running a two-way anova is just as easy!
# we will look at how explict tracks and key affect dancibility

hot100_4anova=hot100_4anova[!is.na(hot100_4anova$key),] # remove nas
hot100_4anova$key_binary=as.numeric(hot100_4anova$key)>=6

anova0=aov(danceability ~ spotify_track_explicit + key_binary, hot100_4anova)
summary(anova0)

# compute cell means
hot100_4anova %>% group_by(spotify_track_explicit, key_binary) %>% summarise(m=mean(danceability))

# we can also add an interaction term! (note you can use : or * for an interaction, this simply determines if it is fully corssed or not...
# try running a three way interaction and you will notice the difference between the two operators)

anova0=aov(danceability ~ spotify_track_explicit + key_binary + spotify_track_explicit*key_binary, hot100_4anova)
summary(anova0)


##########  Exercise 8
##########################################
# open the mice data we have (https://www.kaggle.com/ruslankl/mice-protein-expression/version/1)
# this experiment uses two type of genotype mice (controls vs DS) across two conditions (i.e., treatment vs saline)
# load the data from this experiment. Is it repeated measures? what kind of experiment is this? How many mice do you have per condition? 
# Pick a protein, and run a t-test across the two levels geneotype and treatment
# now run a two way anova, what do you find? How would you test this for multiple proteins? How could you control for 
# multiple comparisons? 

protein = read.csv('~/Documents/GitHub/Sta-R-tistics/data/Data_Cortex_Nuclear.csv')

table(protein$MouseID)
table(protein$Treatment)
table(protein$Genotype)

t.test(protein$BDNF_N[protein$Genotype=='Control'], protein$BDNF_N[protein$Genotype=='Ts65Dn'])
t.test(protein$BDNF_N[protein$Treatment=='Memantine'], protein$BDNF_N[protein$Treatment=='Saline'])

anova0=aov(BDNF_N ~ Genotype + Treatment + Genotype*Treatment, protein)
summary(anova0)



## Regression
## linear Regression
#########################################
hot100=read.csv('/Users/jasondsc/Documents/GitHub/Sta-R-tistics/data/Hot_100_Audio_Features.csv') # read data

# let us run a regression to test the relationship between two variables
lm0=lm(danceability ~ loudness, hot100)
summary(lm0)
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


## variable coding
# let us play around with the coding of the effects!
# before we did what is called dummy coding (i.e., 0 and 1s)
# let us try effect coding (i.e., -1 and 1):

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
sjPlot::tab_model(glm0, transform = NULL) # reports log odds (see slides for explination)

# we can build a regression model and use it to predict held back data:
ids=sample(length(hot100_4anova$SongID),ceiling(length(hot100_4anova$SongID)/3))
sample1=hot100_4anova[ids,]
sample2=hot100_4anova[!(seq(1,length(hot100_4anova$SongID)) %in% ids),]

lm0= lm(danceability ~  energy + loudness + valence, sample1)

predicted_labels=predict(lm0, sample2) # predict danceability from regression above
error_predicted=predicted_labels-sample2$danceability # look at the error between true and predicted values 


## Hierarchical regressions 
##############################################

# let us now look at epilepsy data where patients, seen multiple times. Number of seizures were recorded per visit. Patients recieved treatment for 
# anti-convulsants

epilepsy=brms::epilepsy
epilepsy$Trt=as.factor(epilepsy$Trt)

lm0=lm(count ~ Age  + Trt, epilepsy)
summary(lm0)


## The above model (you built) is good, but it can be better.....
# We can take into account how observations are in fact nested within participants (repeated measures design)
# to do this we will build a model where we allow participants to receive a unique intercept 
# this intercept fit per particpant will take into account inter-individual differences 

lme0=lme4::lmer(count ~ Age  + Trt + (1| patient), epilepsy)
summary(lme0)
sjPlot::tab_model(lme0)
# we now see that the outputs of the regression are broken down into both fixed and random effects
# fixed effects are between and random are repeated (see slides for details)
coef(summary(lme0)) # this will give you the fixed effects reported in summary 

ranef(lme0)$patient # and these are the values for the random intercepts fit per person
# the random intercepts should have the same number of participants (in order)

# the above multi-level model assumes participants differ in terms of their overall number of seizures, and so to account for this
# each subject was given a different intercept (i.e., they all start at a different point) but their overall effect is consistent 

# what if we wanted to test a different hypothesis where the effect of the treatment may be different for each subject
# but that their starting point (intercept) was similar across everyone?

# here we would run a model with a random SLOPE and INTERCEPT 

lme1=lme4::lmer(count ~ Age  + Trt + (1 + Trt| patient), epilepsy)
summary(lme1)
sjPlot::tab_model(lme1)
coef(lme1) # notice how both the intercept and slope vary per subject 


# note that you can make these multilevel models (hierarchical models) into logistic ones using the function of glmer. It works
# just like the glm function, see sections above and slides 





##########  Exercise 8
##########################################
# coming back to the open mice data, let us build on the idea of random slopes
# can we build a single model where we test the effect of the geneotype and treatment 
# on all proteins? 
# try using hierarchical models and random slopes to make a complex model that can tackel all our questions at once

# hint use the stack function to reorganize your data

protein = read.csv('~/Documents/GitHub/Sta-R-tistics/data/Data_Cortex_Nuclear.csv')

table(protein$MouseID)
table(protein$Treatment)
table(protein$Genotype)

protein_stacked=cbind(protein$MouseID, protein[,79:82] ,stack(protein[,2:78]))
colnames(protein_stacked)[1]='MouseID'

lme0= lme4::lmer(values ~ Genotype + Treatment + Genotype*Treatment + (Genotype | ind), protein_stacked)
summary(lme0)
sjPlot::tab_model(lme0)
coef(lme0)


##########  Log Likelihoods & Model fits
##########################################

# when building regression models sometimes we want to assess what is THE best model
# this will allow us to test what predictors are significant AND which are the most useful

# one way to do this is to build your model in steps:

protein = read.csv('~/Documents/GitHub/Sta-R-tistics/data/Data_Cortex_Nuclear.csv')
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


# Random Slopes
######################################
## code below adapted from https://github.com/mkfreeman/hierarchical-models/blob/master/generate-data.R
cognitive_control= read.csv('~/Documents/GitHub/Sta-R-tistics/data/Cognitive_control.csv')

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
  geom_line() + geom_point() + theme_minimal()
labs(x="RT", y="Incongrunet") +
  ggtitle("Varying Intercept") + 
  scale_colour_discrete('Subject')     

# Visualize random intercept
ggplot(data=cognitive_control, aes(x=current.trial, y=simple.model, group = subject, colour = subject)) +
  geom_line() + geom_point() + theme_minimal()
  labs(x="RT", y="Incongrunet") +
  ggtitle("Varying Intercept") + 
  scale_colour_discrete('Subject')     


# Visualize random slope + intercept
  ggplot(data=cognitive_control, aes(x=current.trial, y=random.slope.int.preds, group = subject, colour = subject)) +
    geom_line() + geom_point() + theme_minimal()
  labs(x="RT", y="Incongrunet") +
    ggtitle("Varying slopes and intercept") + 
    scale_colour_discrete('Subject')   
  
  
# Expercise XX
######################################
# Use the above code as a template and run i) a simple model ii) random intercept model iii) random slopes 
# and iv) random slopes and random intercpet model on the AD provided (https://www.kaggle.com/hyunseokc/detecting-early-alzheimer-s/data?select=oasis_cross-sectional.csv)
#
  