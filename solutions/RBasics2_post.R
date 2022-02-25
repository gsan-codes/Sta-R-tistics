library(tidyverse)

setwd("C:/Users/<username>/Downloads/Sta-R-tistics/datasets")

#Slides 8-10
#Example 1: filtering, grouping, and summarizing tibbles with dplyr
#Functions: read_csv, glimpse, slice_sample, filter, n, count, summarise_if, group_by, etc. 

#Read in the mental health survey data
mental_health_survey <- read_csv(file = "MentalHealthTech_Kaggle/survey.csv")

#look at the data
#without piping
glimpse(mental_health_survey)

#with piping
mental_health_survey %>% glimpse()

#look at a few entries
mental_health_survey %>% slice_sample(n = 10)

#See which countries the respondents are from 
mental_health_survey %>% group_by(Country) %>% summarise(country_count = n())

#Get the age range
mental_health_survey %>% summarise_if(is.numeric, range)

#Combining filtering with conditionals
#Filter by age
mental_health_survey %>% filter(Age >= 18 & Age <= 100) -> mental_health_survey

#How many respondents are over 30?
mental_health_survey %>% filter(Age > 30) %>% count()

#How many respondents have a family history of mental illness and are under the age of 45?
mental_health_survey %>% filter(family_history == "Yes" & Age < 45) %>% count()

#How can you get only the data for those who have sought treatment for mental health conditions?
mental_health_survey %>% filter(treatment == "Yes") -> sought_treatment

sought_treatment

#Exercise 1: practice filtering, grouping, summarizing tibbles with dplyr
#Read in the mouse protein dataset
mouse_proteins <- read_csv(file = "MouseProtein_Kaggle/Data_Cortex_Nuclear.csv")

#How many mice are in each genotype?
mouse_proteins %>% group_by(Genotype) %>% summarise(genotype_count = n())

#How can you get the data for all control mice treated with saline?
mouse_proteins %>% filter(Genotype == "Control" & Treatment == "Saline") -> control_saline

#Slides 11-12
#Example 2 adding and removing columns, writing functions
#Functions: switch, if_else, mutate, map_chr, transmute

#Write a function to convert the gender information to a concise format
gender_short <- function(gender_original) {
  short_gender <- switch(tolower(gender_original), 
        "m" = "M", 
        "f" = "F",
        "female" = "F", 
        "male" = "M"
        )
        short_gender <- if_else(is.null(short_gender), "other", short_gender)
  short_gender
}

#save the above function to "gender_short.R"
#load the function
source("gender_short.R")

#Add a new column called Gender_short to map the gender information to short form, 
#then remove the original Gender column 
mental_health_survey %>% mutate(Gender_short = map_chr(Gender, gender_short)) -> mental_health_survey

#Slide 13
#Example 3 adding and removing columns, writing functions
#Functions: str_replace

#Add a new column to the mouse data to convert the drug name in the "Class" column to long form
mouse_proteins %>% mutate(Class_long = str_replace(class, pattern = "m$", replacement = "Memantine"), 
                          .keep = "unused") %>%
                  mutate(Class_long_final = str_replace(Class_long, pattern = "s$", replacement = "Saline"), 
                         .keep = "unused") ->
                    mouse_proteins 

#Slide 14
#Example 3.1 changing elements of a vector or factor 
#functions mapvalues, recode 
#install.packages("plyr")
#Convert the 5 types of responses for "leave" to 3 types of responses
mental_health_survey %>% group_by(leave) %>% count()
mental_health_survey %>% mutate(leave = recode(leave, 
                                "Somewhat easy" = "Easy", 
                                "Very easy" = "Easy", 
                                "Somewhat difficult" = "Difficult", 
                                "Very difficult" = "Difficult")) -> mental_health_survey
mental_health_survey %>% group_by(leave) %>% count()

table(mental_health_survey$work_interfere)
mental_health_survey %>% mutate(work_interfere = plyr::mapvalues(work_interfere, 
                                from = c("Never", "Rarely", "Often", "Sometimes"), 
                                to = c("No", "Yes", "Yes", "Yes"))) -> mental_health_survey
table(mental_health_survey$work_interfere)

#Exercise 2: practice writing a function and creating new columns
#functions rowMeans, round
#Create a new column in the mouse dataset containing the average across all numeric values 
#rounded to 2 digits after the decimal point 

#Function to find the average across rows and round
rowmeans_round <- function(values, digits) {
  values %>% rowMeans(na.rm = TRUE) %>% round(digits) -> rounded_means
  rounded_means
}

mouse_protein_dataset %>% mutate(rounded_mean = rowmeans_round(select_if(mouse_protein_dataset, is.numeric),2)) -> mouse_protein_dataset

#Slides 15-16
#Example 4: joins and bind_rows 
#install.packages("dragracer")
#look at data, give some suggestions for how we might do that
library(dragracer)
rpdr_contestants
rpdr_ep

glimpse(rpdr_contestants)
glimpse(rpdr_ep)

rpdr_ep %>% filter(season == "S03") -> rpdr_ep_S03

left_join(rpdr_ep_S03, rpdr_contestants) -> var_1 %>% group_by(contestant) %>% group_keys()
rpdr_contestants %>% filter(season == "S03") %>% select(contestant)

full_join(rpdr_ep_S03, rpdr_contestants, keep = TRUE) %>% tail() %>% glimpse()

set.seed(33)
rpdr_contestants %>% slice_sample(n=20) -> rpdr_contestants_random

rpdr_contestants_random %>% inner_join(rpdr_ep, by = c("season" = "season"))-> var_2
rpdr_contestants_random %>% right_join(rpdr_ep) %>% tail() %>% glimpse()

#Example 4.1 other mehthods for combining dataframes
#functions rbind (cbind is the column equivalent)
mouse_protein_dataset %>% filter(Genotype == "Control" & Treatment == "Memantine") -> control_memantine

#won't work
rbind(control_saline, control_memantine)

control_saline %>% mutate(Class_long = str_replace(class, pattern = "m$", replacement = "Memantine"), 
                          .keep = "unused") %>%
                    mutate(Class_long_final = str_replace(Class_long, pattern = "s$", replacement = "Saline"), 
                          .keep = "unused") ->
                    control_saline

control_saline %>% mutate(rounded_mean = rowmeans_round(select_if(control_saline, is.numeric),2)) -> control_saline

rbind(control_saline, control_memantine)

#Exercise 3
#Select all contestants younger than 25
#Create a new table by joining with all rows of the episodes table where any of these contestants were eliminated
#This would be equivalent to fining the contestants name in the "eliminated1" or "eliminated2" column.
#You can run two joins
#How would you specify the "by" argument for each join?
#Finally use bind_rows to create a single final table 

rpdr_contestants %>% filter(age < 25) -> rpdr_contestants_under25

inner_join(rpdr_contestants_under25, rpdr_ep, by = c("contestant" = "eliminated1"), keep = TRUE) -> 
          contestants_eliminated1
inner_join(rpdr_contestants_under25, rpdr_ep, by = c("contestant" = "eliminated2"), keep = TRUE) -> 
          contestants_eliminated2

contestants_elimintated_under25 <- bind_rows(contestants_eliminated1, contestants_eliminated2)

#Slide 18
#Example 5: for and while loops
#Functions ls, typeof, class, get, boxplot, length, which
for(i in 1:10){
  print(i*25)
}

list_of_variables <- ls()

for(variable_name in list_of_variables) {
  variable <- get(variable_name)
  print(variable_name)
  print(typeof(variable))
  print(class(variable))
}

#Simple, but not super useful example
#Print out all number below 200 which are divisible by 5

limit <- 200
number <- 1

while(number < limit) {
  if(number %% 5 == 0) print(number)
  number <- number+1
}

#Cool, but complicated example
#Iteratively remove outliers (not necessarily recommended by statisticians, just an example)

mouse_protein_dataset$BDNF_N -> subset_mouse_proteins_BDNF_N
dim(mouse_protein_dataset)

boxplot(subset_mouse_proteins_BDNF_N)$out -> BDNF_N_outliers

while(length(BDNF_N_outliers) > 0) {
  print(length(BDNF_N_outliers))
  subset_mouse_proteins_BDNF_N <- subset_mouse_proteins_BDNF_N[-which(subset_mouse_proteins_BDNF_N %in% BDNF_N_outliers)]
  boxplot(subset_mouse_proteins_BDNF_N)$out -> BDNF_N_outliers
} 

#Exercise 4
#print all perfect squares less than 100
#print out the dimensions (number of rows and columns) of all variables in your workspace which are tibbles
#Use the dim function. Also print out the names of the variables. 
#You may also want to look up the %in% functionality in R

#Contributed solution!
for(i in 1:100){
     if((sqrt(i) %% 1 == 0)) print(i)}
  
limit < 100
number <- 1
square <- number^2

while(square < 100) {
  print(square)
  number <- number+1
  square <- number^2
}

list_of_variables <- ls()

for(variable_name in list_of_variables) {
  variable <- get(variable_name)
  if("tbl" %in% class(variable)) {
    print(variable_name)
    print(dim(variable))
  }
}

#Slide 19
#Example 6: lapply (no exercise)
#Using lapply instead of for 
classes_of_variables <- lapply(ls(), function(x) {x %>% get() %>% class()})

#Slide 20
#Example 7: group and map (no exercise)
#Make some plots for each group in the data (more details will be covered in the visualization session)
density_plot_function <- function(numbers, name) {
  density_plot <- plot(density(numbers), main = name)
}

mouse_protein_dataset %>% group_by(Class_long_final) %>% 
            select(BDNF_N, Class_long_final) %>% 
            drop_na() %>% 
            group_map(~density_plot_function(.x$BDNF_N, .y$Class_long_final)) -> density_plots_list

#Exercise 3 (long)
#Read in the two tables in the Billboard_Hot_weekly_charts folder (hint: read_csv, read_delim)
#Functions: read_delim, as.Date

Hot_100_Audio_Features <- read_delim(file = "Billboard_Hot_weekly_charts/Hot100AudioFeatures.tsv")
Hot_Stuff <- read_csv(file = "Billboard_Hot_weekly_charts/HotStuff.csv")

#Look at the data using any functions of your choice.

slice_sample(Hot_Stuff)
glimpse(Hot_100_Audio_Features)

#Now answer the following questions:
#How large is each dataset (number of rows and cols)? 

dim(Hot_100_Audio_Features)
dim(Hot_Stuff)

#What are the data types of each column of each dataset? (hint: spec function)
spec(Hot_100_Audio_Features)
spec(Hot_Stuff)

#Convert WeekID in the HotStuff.csv table to date format with the as.Date function and format = "$m/%d/%y"
Hot_Stuff %>% mutate(WeekID_date = as.Date(WeekID, format = "%m/%d/%y")) -> Hot_Stuff

#What is the date range for each performer in the HotStuff table?
#Use summarise_at and select the appropriate column
#for range use the min and max functions (provide as a list to summarise_at)
Hot_Stuff %>% group_by(Performer) %>% summarise_at("WeekID_date", c("min", "max"))

#How many artists are represented in each dataset? 
#Which artists feature most frequently in each dataset? (hint: use the arrange function to order)

Hot_Stuff %>% summarise_at("Performer", n_distinct)
Hot_100_Audio_Features %>% summarise_at("Performer", n_distinct)

Hot_Stuff %>% group_by(Performer) %>% count() %>% arrange(-n) -> Hot_Stuff_tops
Hot_100_Audio_Features %>% group_by(Performer) %>% count() %>% arrange(-n) -> Hot_100_Audio_Features_tops

Hot_Stuff_tops %>% filter(n > 500) -> Hot_Stuff_tops

#Make a new table containing only the songs in the Hot100AudioFeatures for performers featuring in HotStuff 
#with a frequency more than 500
#filter to only keep songs which reached peak position of < 10
#get the unique SongIDs of the Songs which remain

Hot_Stuff %>% filter(Performer %in% Hot_Stuff_tops$Performer) -> Hot_Stuff_top_subset

inner_join(Hot_100_Audio_Features, Hot_Stuff_top_subset, by = c("SongID" = "SongID")) %>% 
          group_by(SongID) %>%
          filter(`Peak Position` < 10) %>% select(SongID) %>% distinct() -> remaining_songs

#Now filter the original Hot100AudioFeatures table to only include these songs
#For each Performer present in the resulting table, plot the dancebility of their songs in a boxplot 
#You can use group_map or any other looping device of your choice!

Hot_100_Audio_Features %>% filter(SongID %in% remaining_songs$SongID) %>%
          group_by(Performer) %>% group_map(~boxplot(.x$danceability, main = .y$Performer))
