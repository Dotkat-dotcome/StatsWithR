###############
### Cleaning Data
###############

# Please do the "Cleaning Data with R" exercise that was assigned in dataCamp.
# We recommend that you take notes during the DataCamp tutorial, so you're able to use the commands 
# you learned there in the exercise below.
# This week, the exercise will be about getting data into a good enough shape to start analysing. 
# Next week, there will be a tutorial on running t tests for this data.
## You need to provide a serious attempt to each exercise in order to have
## the assignment graded as complete. 

# 1. Download the data file "digsym.csv" from the moodle and save it in your working directory. 

# 2. Read in the data into a variable called "dat".
library(readr)
dat <- read_csv("D:/Course-States exercises/digsym.csv")
View(dat)

# 3. Load the libraries languageR, stringr, dplyr and tidyr.
library("languageR")
library("stringr")
library("dplyr")
library("tidyr")

# 4. How many rows, how many columns does that data have?
dim(dat)
# 3700 row ; 11 columns

# 5. Take a look at the structure of the data frame using "glimpse"
glimpse(dat)


# 6. View the first 20 rows, view the last 20 rows
head(dat, n = 20)


# 7. Is there any missing data in any of the columns?
summary(dat)
# Yes, StimulDS1.RT
sum(is.na(dat))


# 8. Get rid of the row number column
dat <- dat[, -1]
head(dat)


# 9. Put the Sub_Age column second
dat <- dat[c(1,10,2,3,4,5,6,7,8,9)]


# 10. Replace the values of the "ExperimentName" column with something shorter, more legible
# Since there is only value: "Digit Symbol - Kopie"
dat$ExperimentName <- "DSK"


# 11. Keep only experimental trials (encoded as "Trial:2" in List), get rid of practice trials 
# (encoded as "Trial:1"). When you do this, assign the subset of the data to a variable "data2", 
# then assign data2 to dat and finally remove data2.
data2 <- subset(dat, List == 'Trial:2')
dat <- data2


# 12. Separate Sub_Age column to two columns, "Subject" and "Age", using the function "separate"
dat <- separate(dat, Sub_Age, c("Subject", "Age"), sep = "_")


# 13. Make subject a factor
dat$Subject <- factor(dat$Subject)


# 14. Extract experimental condition ("right" vs. "wrong") from the "File" column:
# i.e. we want to get rid of digit underscore before and the digit after the "right" and "wrong".
dat$File <- gsub('[0-9]+', '', dat$File)
dat$File <- str_replace(dat$File, "_", "")
head(dat$File)


# 15. Using str_pad to make values in the File column 8 chars long, by putting 0 on the end  (i.e., 
# same number of characters, such that "1_right" should be replaced by "1_right0" etc)
dat$File <- str_pad(dat$File, 8, side = "right", pad="0")


# 16. Remove the column "List"
dat$List <- NULL


# 17. Change the data type of "Age" to integer
as.integer(dat$Age) 



# 18. Missing values, outliers:
# do we have any NAs in the data, and if so, how many and where are they?
sum(is.na(dat))


# 19. Create an "accuracy" column using if-statement
# if actual response (StimulDS1.RESP) is the same as the correct response (StimulDS1.CRESP), put 
# in value 1, otherwise put 0
dat <- mutate(dat, accuracy = ifelse(StimulDS1.RESP == StimulDS1.CRESP, 1, 0))


# 20. How many wrong answers do we have in total?
sum(dat$accuracy == 0)


# 21. Whats the percentage of wrong responses?
sum((dat$accuracy == 0) / nrow(dat)) * 100


# 22. Create a subset "correctResponses" that only contains those data points where subjects responded correctly. 
correctResponses <- subset(dat, accuracy == 1)


# 23. Create boxplot of StimulDS1.RT - any outliers?
boxplot(dat$StimulDS1.RT)
# Yes, many dots are falling out of the box


# 24. Create histogram of StimulDS1.RT with bins set to 50
hist(dat$StimulDS1.RT, breaks = 50)


# 25. Describe the two plots - any tails? any suspiciously large values?
# These two plots both shows an tail which outliers fall in. 
# Especially there is one data falls aorund 14000+ far away from others.


# 26. View summary of correct_RT
summary(correctResponses)


# 27. There is a single very far outlier. Remove it and save the result in a new dataframe named "cleaned".
cleaned <- dat[-which.max(dat$StimulDS1.RT),]
summary(cleaned$StimulDS1.RT)


## EXTRA Exercises:
##You can stop here for your submission of this week's assignment,
##but you are encouraged to try it now. 
##All these exercises will be discussed and solved in the tutorial!

# 28. Dealing with the tail of the distribution: outlier removal
# Now, remove all correct_RT which are more than 2.5. SD away from the grand mean


# 29. Create new "correct_RT_2.5sd" column in data which prints NA if an RT value is below/above the cutoff


# 30. Take a look at the outlier observations
# any subjects who performed especially poorly?


# 31. How many RT outliers in total?


# 32. Plot a histogram and boxplot of the correct_RT_2.5sd columns again - nice and clean eh?
hist(correct_RT_2.5sd)


# 33. Next, we'd like to take a look at the avrg accuracy per subject
# Using the "cast" function from the library "reshape", create a new data.frame which shows the 
# average accuracy per subject. Rename column which lists the average accuracy as "avrg_accuracy".


# 34. Sort in ascending order or plot the average accuracies per subject.


# 35. Would you exclude any subjects, based on their avrg_accuracy performance?


# 36. Congrats! Your data are now ready for analysis. Please save the data frame you created into a new 
# file "digsym_clean.csv".