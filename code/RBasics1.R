###Slide 8--------------------------------------------------
##variable and data types
#numeric
weight <- 60.5    # doesn't print anything but assinging number to variable
weight          # typing the name of the object
class(weight)   # type of the object
#numeric and numeric type
typeof(weight)

#character
sex <- "Female"
sex          # typing the name of the object
class(sex)   # type of the object

#boolean
result <- weight > 50
result

###Slide 9--------------------------------------------------
##converting type of variables
#character to numbers
new_number <- "30"
class(new_number)
new_number <- as.numeric(new_number)
class(new_number)
new_number
#number to character
new_number <- as.character(new_number)
class(new_number)
new_number

#this will not work properly
class(sex)
sex <- as.numeric(sex)
class(sex)

#double to integer
weight <- as.integer(weight)
weight

#boolean to numeric
as.numeric(result)

###Slide 11--------------------------------------------------
#vector with numeric variables
age <- c(21, 34, 39, 54, 55)
class(age)
length(age)
#matric with numeric variables
new_matrix <- matrix(1:9, nrow = 3, ncol = 3)
new_matrix
class(new_matrix)

###Slide 12--------------------------------------------------
#list containing, strings, numbers, logical 
subject_a <- list("Female", c(21, 33), weight = 60, TRUE)
subject_a
class(subject_a)
length(subject_a)
subject_a[["weight"]]
subject_a[[2]]

#dataframe containing strings and numbers
age <- c(21, 34, 39, 54, 55) 
group <- c("MDD", "Ctrl", "MDD", "Ctrl", "Ctrl")
meta.data <- data.frame(age, group) #create dataframe
meta.data
typeof(meta.data$age)
meta.data[3, "group"]

###Slide 13--------------------------------------------------
#Check the current directory for Windows and Mac
getwd()
#Example of setting a new directory for Windows
setwd("C:/Users/harukamitsuhashi/Desktop/R_Workshop_2022")
#Example of setting a new directory for Macs
setwd("/Users/harukamitsuhashi/Desktop/R_Workshop_2022")
#List files in the current direcotry
list.files()

##Exercise1--set your working directory to a folder named "R_Workshop_2022" (You have to create the folder first)

###Slide 14--------------------------------------------------
#set directory
setwd("/Users/harukamitsuhashi/OneDrive\ -\ McGill\ University/staRtistics_2021-2022/")
#Read files seperated by comma
data1 <- read.csv(file = "Datasets/Billboard_Hot_weekly_charts/HotStuff.csv", sep = ',')
data1

#Read files seperated by tab
data2<- read.csv(file = "Datasets/Billboard_Hot_weekly_charts/Hot100AudioFeatures.tsv", sep = '\t', header = TRUE) 
head(data2)
data2[33:37,]

##Exercise2- read "MentalHealthTech_Kaggle/survey.csv" as "my.my.data1" 
my.data1 <- read.csv(file = "Datasets/MentalHealthTech_Kaggle/survey.csv", sep = ',')

###Slide 15--------------------------------------------------

###Slide 17--------------------------------------------------
#Expore data
head(my.data1) #check first few rows of data
dim(my.data1) #check number of rows and columns
colnames(my.data1) #list column names
rownames(my.data1) #list row names
dim(my.data1)

#List specific part of data using [row:row,column:column]
my.data1[1:5,] #list 1-5 rows of data
my.data1[,1:2] #list 1-2 columns of data

#check unique value
unique(my.data1$Country)
table(my.data1$Country)

#check for NA in a list
weights <- c(63, 69, 60, 65, NA, 68, 61, 59, 64, 69, 63, 63, NA)
sum(is.na(weights))
#example1
is.na(weights)
weights[!is.na(weights)]
#example2
mean(weights)
mean(weights, na.rm = TRUE)

#Exercise3 - check row and column names for "my.data1"
#check class of my.data1
#check number of column
#check number of row
#list column names
#check data type of age
#check data type of gender

#Exercise4 - list 1-10 rows of "my.data1"

###Slide 18--------------------------------------------------
##lets test package
install.packages("tidyverse") #install a library package
library(ggplot2) #callinga a library
ggplot2::geom_boxplot#check ggplot function 
ggplot2::geom_boxplot()
?geom_boxplot

###Slide 19--------------------------------------------------
##explore two lists
x <- c(1,3,5,8,13,42,67,88,93) #create list x
y <- c(5,4,21,8,78,65,42) #create list y
union(x,y) #check union of list x and list y
intersect(x,y) #check overlap of list x and list y
setdiff(x,y) #number in list x but not in list y
setdiff(y,x)

#Exercise5 using country_mydata1 and country_mydata2 
country_mydata1 <- unique(my.data1$Country)
country_mydata2 <- c("Bulgaria", "Switzerland", "Italy", "France", "Finland", "India", "Vietnam", "Indonesia", "Malaysia","China")
#Which county is included in both lists?
#Which county is unique to country_mydata2
#Which county is unique to country_mydata1

###Slide 20--------------------------------------------------
#arithmetic operator
a <- 1 + 58
b <- 100/5*3
a
b
#relational operator
a < b
a != b

#operator can be also applied to list
x <- c(1,2,3,4,5) #create listx
y <- c(1,5,10,15,20) #create listy
x+y
x<y
x>2

#logical operators can be be used to combine boolean results
(a > b) & (a != b)
(a > b) | (a != b)


####Slide 20-------------------------------------------------
#Review class and typeof
class(my.data1$Age)
class(my.data1$Gender)
class(my.data1$family_history)

#Excercise6- check the type and class of each column on iris dataset
my.data2 <- iris

#Example of mean, median, and SD 
mean(my.data1$Age)
median(my.data1$Age)
sd(my.data1$Age)

#Exercise7- check mean, median and SD for iris Petal.Length

###Slide 22--------------------------------------------------
x <- 3
if(x > 0){
  print("value is positive")
} else if (x < 0){
  print("value is negative")
} else{
  print("value is neither positive nor negative")
}
#if else statment using list
country_mydata1 <- unique(my.data1$Country)
if("Canada" %in% country_mydata1){
  print("Canada is included")
} else{
  print("Canada is not included")
}

#Exercise8- create if else statement which returns "Finland is included" if Finland is included in country_mydata1

###Slide 23--------------------------------------------------
#Save data
save(my.data1, file = "/Users/harukamitsuhashi/Desktop/R_Workshop_2022/workshop_my.data1.Rds")
write.csv(my.data1, "/Users/harukamitsuhashi/Desktop/R_Workshop_2022/workshop_my.data1.csv")

#Exercise9 try saving your own variables and workspace (hint: use save.image)

#Example - save your command history
savehistory(file = "/Users/harukamitsuhashi/Desktop/R_Workshop_2022/Workshop_commands.Rhistory")
