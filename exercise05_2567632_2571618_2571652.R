### Stats with R Exercise sheet 5

##########################
#Week6: t-test and friends
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, November 25. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are allowed to work together in group up to three students, but everybody 
## needs to submit the group version of the homework via moodle.


## Please write below your (and your teammates) name, matriculation number. 
## Name:
## Matriculation number:

## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

###########################################################################################
###########################################################################################

###############
### Cleaning Data
###############

library(lsr)
library(tidyr)
library(effsize)


# set your wd and load the data frame digsym_clean.csv
library(readr)
digsym_clean <- read_csv("R/digsym_clean.csv")
View(digsym_clean)


# get rid of the column "X"
digsym_clean <- subset( digsym_clean, select = -X )


# Say you're interested in whether people respond with different accuracy to 
# right vs wrong picture-symbol combinations.
# In other words, you want to compare the average accuracy for the digsym-right 
# and digsym-wrong condition.
# Like the conscientious researcher you are, you want to take a look at the data 
# before you get into the stats.
# Therefore, you will need to create a barplot of the mean accuracy data 
# (split out by condition) using ggplot and the summarySE function (given below).
# Let's do it step by step.

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95) {
  # data: an input dataframe
  # measurevar: a column name of <data> (as string), on which we would like to calculate standard 
  #             deviation (SD), standard error (SE) and confidence interval (CI).
  # groupvars: categorical columns of <data> (as vector of strings ) which we would like to use
  #            to make all possible combinations for which we calculate SD, SE, CI based 
  #            on <measurevar>.
  # na.rm: should we remove NA
  # conf.interval: confidence interval
  library(doBy)
  
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # Collapse the data
  formula <- as.formula(paste(measurevar, paste(groupvars, collapse=" + "), sep=" ~ "))
  datac <- summaryBy(formula, data=data, FUN=c(length2,mean,sd), na.rm=na.rm)
  
  # Rename columns
  names(datac)[ names(datac) == paste(measurevar, ".mean",    sep="") ] <- measurevar
  names(datac)[ names(datac) == paste(measurevar, ".sd",      sep="") ] <- "sd"
  names(datac)[ names(datac) == paste(measurevar, ".length2", sep="") ] <- "N"
  
  # Calculate standard error of the mean
  datac$se <- datac$sd / sqrt(datac$N)  
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

# apply the function summarySE on the accuracy data grouping by right/wrong condition
# (use the provided documentation inside the function for the arguments description)


summarySE(digsym_clean, measurevar = "accuracy", groupvars = "condition" , na.rm=FALSE, conf.interval=.95)
#summarySE(digsym_clean, measurevar = "accuracy", groupvars = condition =="wrong" , na.rm=FALSE, conf.interval=.95)
  

# Create the barplot (use ggplot2 for this and all tasks below) with error bars 
# (which the function summarySE readily provided).
# Gauging from the plot, does it look like there's a huge difference in accuracy 
# for responses to the right and wrong condition?

library(ggplot2)
ggplot(data = digsym_clean, aes(x = condition, y = accuracy))+
  geom_bar(stat="identity", position=position_dodge())
#  geom_errorbar(aes(ymin=len-sd, ymax=len+sd), width=.2,
#                position=position_dodge(.9))


# Let's go back to our data frame "data", which is still loaded in your console
# Now that you've taken a look at the data, you want to get into the stats.
# You want to compute a t-test for the average accuracy data in the right and 
# wrong condition.
# Why can't you compute a t-test on the data as they are now? 
# Hint: which assumption is violated?

summary(digsym_clean)
head(digsym_clean)
# The samples of the two groups are not aggregated to their mean

# we need to reshape( - cast) the data to only one observation (average accuracy)
# per subject and right/wrong condition 
# Collapse the data, using 
# cast(data, var1 + var2 + var3 ... ~, function, value = var4, na.rm = T)

library(reshape)
str(digsym_clean)
acc = cast(digsym_clean, formula = Subject + condition ~ . , fun.aggregate = "mean", value="accuracy", na.rm = T)
acc$accuracy <- acc$`(all)`



# Create a histogram of the accuracy data depending on the right and wrong 
# condition and display them side by side

ggplot(data = acc, aes(x = accuracy, fill = condition))+
  geom_histogram(position ="identity", binwidth = 0.01)


# Display the same data in a density plot 

ggplot(data = acc, aes(x = accuracy, fill = condition))+
  geom_density()


# Based on the histograms and the density plots - are these data normally 
# distibuted?
#No. 
#The bell shapes in the density graph does not have symmetry with respect to the bell peak.



# Create a boxplot of the accuracy data

boxplot(accuracy ~ condition, data = acc)

# Compute the t-test to compare the mean accuracy between wrong and right picture
# combinations.
# Do you need a paired t-test or independent sample t-test? why?

library(dplyr)
cr <- (subset(acc, condition == "right"))$accuracy
cl <- (subset(acc, condition == "wrong"))$accuracy
t.test(cr, cl, paired=TRUE)
#paired t-test because the subjects are being tested few times so it is not an independent sample

# What does the output tell you? What conclusions do you draw?
#p-value is really small so it indicates strong evidence against null hypothesis
#so we can reject null hypothesis.

# Compute the effect size using CohensD 
cohensD(cr,cl, method = "paired")

# How big it is? How do you interpret this result?
#It's a medium value. It's essentially mean difference in terms of standard deviation units.
#So we can grap a feeling how far are these two means in terms of sd.

# In addition to the long-format data we've just been working on, you may also 
# encounter data sets in a wide format 
# (this is the format we have been using in class examples.)
# Let's do a transformation of our data set to see how it would like in a wide 
# format.
# Use "spread" in tidyr.

library(tidyr)
wide <- spread(data = digsym_clean, key = condition, value = accuracy)
wide[is.na(wide)] <- 0


# Compute the t test again on the wide format data - note that for wide-format 
# data you need to use a different annotation for the t-test.
#wide-format
t.test(wide$right, wide$wrong, paired=TRUE, conf.level=0.95)


# Compare the t-test results from the wide-format and the long-format data.
#long-format
t.test(accuracy ~ condition, data = acc, paired=TRUE, conf.level=0.95)
#long-format has sufficiently small p-value to reject null hypothesis;
#on the other hand, wide-format, has large p-value which suggest no evidenve to reject null hypothesis.


# Compute CohensD on the wide format data.
cohensD(wide$right, wide$wrong, method = "paired")




# Let's try the t-test again, but for a different question:
# Suppose you are interested in whether reaction times in the digit symbol 
# task differ depending on gender.
# In other words, you want to test whether or not men perform significantly 
# faster on average than women, or vice versa.
# Collapse the data again, using 
# cast(data, var1 + var2 + var3 ... ~ ., function, value = var4, na.rm = T)
tg <- cast(digsym_clean, formula = Gender ~ . , fun.aggregate = "mean", value="StimulDS1.RT", na.rm = T)

# Take a look at the resulting data frame using head()
head(tg)

# Compute the t-test to compare the accuracy means of female and male 
# participants.
# Which t-test do you need and why? How do you interpret the result?

tm <- (subset(digsym_clean, Gender == "male"))
tf <- (subset(digsym_clean, Gender == "female"))
t.test(tm$StimulDS1.RT , tf$StimulDS1.RT , var.equal = TRUE)
#We need independent t-test because male and female are two seperate samples.
#p-value is also really small so we reject null hypothesis here.

