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
data[55,1]=NaN #To demonstrate what happens when you have an NA while calculating
## mean
mean(iris$Sepal.Length) # take mean of the entire column
mean(data$Sepal.Length) # take mean of the entire column
mean(data$Sepal.Length, na.rm = TRUE) # take mean of the entire column, you can remove NaN this way
colMeans(iris[,1:4]) # take mean of every column 
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
colSums(data[,1:4], na.rm = TRUE)#Will get a NaN otherwise

# remember you can use dplyr and tidyr functions to help you

summary_stats=iris %>% group_by(Species) %>% summarise(mean_length=mean(Sepal.Length), N=n(), mode_length=mode(Sepal.Length))
summary_stats #to look at what we get out

########## Exercise 1
##########################################
# R does not have an inbuilt Mode function, try building one yourself (note: DescTools pacakge has a function Mode)





## Max, Min, Min k
max(iris$Sepal.Length)
min(iris$Sepal.Length)
tail(iris$Sepal.Length[order(iris$Sepal.Length)], 5) # biggest 5 elements #To explain that it returns the indices of the elements and not the elements themselves
head(iris$Sepal.Length[order(iris$Sepal.Length)], 5) # smallest 5 elements 

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
# scale columns/ rows by mean and sd. Do NOT use the scale function (you can use the scale function for this, but I want you to try without it first)






# Confidence intervals 

# compute 95% confidence intervals of a normal distribution
# this is the equivalent of the following formula: 
#               
#                 CI = mean(x) +/- z* (sd/sqrt(n))
#
# where z is the value of the cumulative normal dist of the confidence level 
# s is the standard deviation and n is the sample size
# note that (s/sqrt(n)) is also standard error 

CI = qnorm(0.975) * sd(iris$Sepal.Length)/sqrt(length(iris$Sepal.Length)) #typos in CI formula in above comment
CI_upper = mean(iris$Sepal.Length) + CI
CI_lower = mean(iris$Sepal.Length) - CI


# we will look at a different method for computing CI later

########## Exercise 4
##########################################
# use apply to compute CI for each column 








## Correlations
cor.test(iris$Sepal.Length,iris$Petal.Length) #typo in this comment # pearson correlation looks at the relationship between two continuous variables 
# it is a simple ratio of the covariance between the two variables normalized by their standard deviation 
# note that the correlation itself is just a description, the statistic we get is PARAMETRIC 

# when your data is ordinal or when you want to run a non-parametric test
cor.test(iris$Sepal.Length,iris$Petal.Length, method = "spearman")
# this looks at the monotonic relationship between variables by ranking your data
# does increasing rank in one increase the other variable 


########## Inferential stats
## T-tests in R 
# All the different kinds of t-test you would like to run can be done with the t.test() function using different inputs 
# let us compare the loudness of music on spotify between groups of explicit vs non-explicit 

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
hot100=read.csv(
  'C:/Users/jasondsc/Documents/GitHub/Sta-R-tistics/datasets/Billboard_Hot_weekly_charts/Hot100AudioFeatures.tsv', 
  sep = "\t") 
hot100=hot100[!is.na(hot100$spotify_track_explicit),] # remove nas
hot100=hot100[!is.na(hot100$loudness),] # remove nas

sample1=hot100$loudness[hot100$spotify_track_explicit==FALSE] # collect sample 1 (non-explicit)
sample2=hot100$loudness[hot100$spotify_track_explicit==TRUE] # collect sample 2 (explicit)

# simple two sample t-test (two-sided) assuming equal variance 
t.test(sample1,sample2, var.equal = TRUE) #could be nice to make box plots of the loudness here for explicit and non-explicit songs
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
install.packages("datarium") #needed if they don't have it installed 
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
#If the p-value is above 0.05, the data is considered to be not significantly different from a normal distribution. 
shapiro.test(iris$Sepal.Length) #Shapiro Wilk
# The Shaprio Wilk test is a test where the null-hypothesis is that your data is normally distributed
# The p-value, if lower than 0.05 for example, would reflect that you have evidence that your data
# IS NOT normally distributed, it belongs to some other kind of distribution
# since this test confuses a lot of people, always plot your data to double check 
shapiro.test(hot100$loudness[hot100$spotify_track_explicit==FALSE]) # is sample size a problem? Why?

# Q-Q plot
# here we plot our data's quantiles against a theoretical normal dist quantiles
# we should hopefully see that our data lines up nicely with the theoretical one
# along the x-y diagonal line. Any deviation from this line would indicate some 
# non-normal property of the distribution

qqnorm(iris$Sepal.Length, pch = 1, frame = FALSE)
qqline(iris$Sepal.Length, col = "pink", lwd = 5)

install.packages("car") #will need this line if the package is not installed
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


cohend=seq(0.1,0.8,0.1) # Let us pick some effect sizes to loop over
sample_sizes= seq(5, 500, 1)  # Let us also pick some sample sizes to try out                                                                                                                                                                                   
one_group=data.frame() #making a blank data.frame here
install.packages("pwr") #will need to install this if package not already installed

# iterate over possible effect sizes and compute power given sample size 
for (ds in cohend){ #I think the for loop is a little bit cleaner this way, up to you :)
  for (i in 1:length(sample_sizes)){
    power=pwr::pwr.t.test(n=sample_sizes[i],d=ds, sig.level = 0.05, type= 'paired', alternative = 'two.sided')
    one_group <- rbind(one_group, data.frame(power = power$power, effect_size = ds, sample_size = sample_sizes[i]))
  }
}   

one_group$effect_size = as.factor(one_group$effect_size) #now it has a name

library(ggplot2) #need to run this first (should be installed along with tidyverse during session 2) You may also want to explain that we have a module later on that focuses on plotting :)
ggplot(data=one_group, aes(x=sample_size, y=power, colour=effect_size, group=effect_size)) +
  geom_line(size=1.3) + scale_fill_brewer(palette="Pastel1") + theme_minimal() + labs(y= "Power (%)", x ="Sample size (N)", color= "Effect Size")  +
  coord_cartesian(ylim=c(0, 1)) + theme(legend.justification=c(1,1), legend.position=c(1,1)) + geom_hline(yintercept = 0.80, linetype="dashed", 
                                                                                                          color = "black", size=1)

ggplot(data=one_group, aes(x=sample_size, y=power, colour=effect_size, group=effect_size)) + xlim(c(0,100)) +
  geom_line(size=1.3) + scale_fill_brewer(palette="Pastel1") + theme_minimal() + labs(y= "Power (%)", x ="Sample size (N)", color= "Effect Size")  +
  coord_cartesian(ylim=c(0, 1)) + theme(legend.justification=c(1,1), legend.position=c(1,1)) + geom_hline(yintercept = 0.80, linetype="dashed", color = "black", size=1)


########## Exercise 6
##########################################
# But this is all theoretical..... instead let us go to the literature and pick some sample sizes at random
# I have provided a list of samples I have extracted from 38 random EEG papers sampled from the literature
# Let us use these sample sizes and different effect sizes to compute how much statistical power the field
# has to find different effects for a ONE-WAY ANOVA

# first let us pick some effect sizes to iterate over and how many groups in the ANOVA
cohend=seq(0.1,0.8,0.05)
num_group=2
# Now let us read the data in


# plot histogram of sample sizes in the field
ggplot(sample_sizes, aes(x=data$nsub_after_exc)) + 
  geom_histogram(aes(y=..density..),color="black", fill="pink", binwidth = 5, position = "identity") +
  geom_density(alpha=0.6, fill="pink") + xlab("Sample Size") + ylab("Number of Studies") + 
  theme_minimal() + geom_vline(xintercept = median(sample_sizes[,1]), linetype="dashed", 
color = "black", size=1)

# make a data frame to hold the data


# iterate over possible effect sizes and compute power given sample size 





# find what proportion of studies have an power greater than X
cohend=seq(0.1,0.8,0.2) # first let us select a range of effect sizes to iterate over
power_iter=seq(0,1,0.01) # make sequence of power we care about going from 0.0 to 1.0
proportions_bigger_than=data.frame() # make a data frame to hold output 

# iterate over cohen D's selected and the sequence of powers we care about
# compute how many EEG studies have the sample size to achieve that power for a given effect size 






# need to make sure that the effect size is a factor before plotting




# plot results 
ggplot(data=proportions_bigger_than, aes(x=power, y=porportion, colour=effect_size, group=effect_size)) +
  geom_line(size=1.3) + scale_fill_brewer(palette="Pastel1") + theme_minimal() + 
  labs(y= "Porportion of Studies", x ="Power (%)", color= "Effect Size")  +
  coord_cartesian(ylim=c(0, 1)) + theme(legend.justification=c(1,1), legend.position=c(1,1)) + 
  geom_vline(xintercept = 0.80, linetype="dashed", color = "black", size=1)

save.image('~/Documents/GitHub/Sta-R-tistics/workspace_stats_1.RData')

