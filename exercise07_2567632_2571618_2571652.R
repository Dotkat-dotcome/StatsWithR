### Stats with R Exercise sheet 7

##########################
#Week 8: ANOVA
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, December 9. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are allowed to work together in group up to three students, but everybody 
## needs to submit the group version of the homework via moodle.
## You need to provide a serious attempt to each exercise in order to have
## the assignment graded as complete. 

## Please write below your (and your teammates) name, matriculation number. 
## Name: Safura Isayeva, Hui-Syuan Yeh, Divyanshu Bhardwaj
## Matriculation number: 2567632, 2571618, 2571652

## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

###########################################################################################



#######################
### PART 1: Preparation
#######################

library(boot)
library(ggplot2)
library(reshape)

# This time we will be working with the "amis" data frame (package 'boot') that has 
# 8437 rows and 4 columns.

# In a study into the effect that warning signs have on speeding patterns, Cambridgeshire
# County Council considered 14 pairs of locations. The locations were paired to account 
# for factors such as traffic, volume and type of road. One site in each pair had a sign 
# erected warning of the dangers of speeding and asking drivers to slow down. No action 
# was taken at the second site. Three sets of measurements were taken at each site. 
# Each set of measurements was nominally of the speeds of 100 cars but not all sites 
# have exactly 100 measurements. These speed measurements were taken before the erection 
# of the sign, shortly after the erection of the sign, and again after the sign had been 
# in place for some time.

# 1. For the further reference please use ?amis. It may take some time to understand the dataset. 
?amis
# speed
# warning: 1 sign presense, 2 not
# period: 1-3 the reading is taken before, immediately after and after some time of the sign
# pair: 1-14 locations

# 2. Load the dataset and briefly inspect it. 
# Feel free to make some plots, calculate some statistics in order to understand the data.
data(amis)
str(amis)

mean(amis$speed[amis$warning == 1])
mean(amis$speed[amis$warning == 2])

sd(amis$speed[amis$warning == 1])
sd(amis$speed[amis$warning == 2])

# 3. All our columns have numeric type. Convert the categorial columns to factors.
amis$warning <- as.factor(amis$warning)
amis$period <- as.factor(amis$period) 
amis$pair <- as.factor(amis$pair)

# 4. Build a boxplot for the distribution of `speed` for each of `period` values 
# (before, immediately after and after some time). Build 2 plots side by side
# depending on the `warning` variable.
# (for all plots here and below please use ggplot)

ggplot(amis, aes(x = period, y = speed))+
  geom_boxplot()+
  ggtitle("period-speed distribution")

ggplot(amis, aes(x = period, y = speed))+
  geom_boxplot()+
  ggtitle("period-speed distribution with warning")+
  facet_wrap(~warning, nrow=1) 


# 5. What can you conclude according this plot? What can you say about people behaviour in
# different periods: before, immediately after and after some time?

#People seeing the warning sign do lower the speed comparing to the no-warning group, 
#especially immediately after(period==2) the erected sign. 


# 6. What are your ideas about why the data with warning==2 (which correspond to the
# measurements in different times on sites where no sign was erected) was collected?

#Data with warning==2 serves as a control group to observe what is the effect of warning sign on drivers


#######################
### PART 2: 1-way ANOVA
#######################

#1. First let's create a new data frame which will be used for all PART 2.
# For 1-way ANOVA we will be working with the subset of `amis` where the 
# warning sign was erected, which corresponds to warning==1, therefore first
# subset your data to filter out warning==2 and then apply cast() to average
# speed over each "pair" and "period. Assign this new data frame to the variable casted_data.
casted_data <- cast(subset(data, warning ==1), pair+ period~. ,mean, value.var = 'speed')
colnames(casted_data)[3] <- c('avg_speed') 


# 2. Build a boxplot of the average speed depending on period
ggplot(data = casted_data, aes(x = period, y = avg_speed))+
  geom_boxplot()


# 3. Is there a difference between the periods?
# The average speed decreases at the start in the first period, but it gradually smoothens 
#  out as time progresses. The median is higher during the later period.

# 4. Now, let's check each ANOVA assumptions and whether they are violated or not and why.

# a) Independence assumption
# (you need to figure out the best way to do it and give a detailed justified answer)
	
	t.test(casted_data$avg_speed, casted_data$period)
#	As the p value < 0.05, the two means are independent.

# b) Normality of residuals
# (you need to figure out the best way to do it and give a detailed justified answer)

	shapiro.test(casted_data$avg_speed)
#	As the p value > 0.05, the residual (average speed) is normal.

# c) Homogeneity of variance of residuals
# (you need to figure out the best way to do it and give a detailed justified answer)

	leveneTest(avg_speed~period, data = casted_data)
#	As the p value > 0.05, the residual's (average speed)  variance is homogenous.


# 5.Now we are ready to perform 1-way ANOVA: please use the function aov() on the speed
# depending on the period,report p-value and interpret the result in details
summary(aov(data = casted_data, formula = avg_speed~period))
# Here p value = 0.405.  As the p value > 0.05, there is no significanct difference between the two groups.

# 6. Please do a pairwise t-test with pairwise.t.test()
pairwise.t.test(casted_data$avg_speed, casted_data$period, p.adjust.method = 'none')

# 7. Report pair-wise p-values and interpret the result in details


# As the p value > 0.05, no significance between the two groups can be reported.

# 8. Try to use no adjustment for pairwise testing and then Bonferroni correction.
# Does the result change?

pairwise.t.test(casted_data$avg_speed, casted_data$period, p.adjust.method = 'bonferroni')

# This changes the values((3 times) but has no impact on the result.


#######################
### PART 3: 2-way ANOVA
#######################
# Now we want to analyze the influence of 2 categorial variables (period and warning) on the speed.
# So let's turn back to our initial dataset amis (not its subset with warning==1)
# First, we need to again average the speed over each `pair`, `warning` and `period`.
amis_cast2 <-dcast(amis_data, period+pair+warning~"ave_speed",mean, value.var = "speed")


# Calculate the mean for each of 6 pairs of `period` and `warning`
mean11 <-mean(subset(amis_cast2, period ==1 & warning ==1)$ave_speed)
mean12 <-mean(subset(amis_cast2, period ==1 & warning ==2)$ave_speed)
mean21 <-mean(subset(amis_cast2, period ==2 & warning ==1)$ave_speed)
mean22 <-mean(subset(amis_cast2, period ==2 & warning ==2)$ave_speed)
mean31 <-mean(subset(amis_cast2, period ==3 & warning ==1)$ave_speed)
mean32 <-mean(subset(amis_cast2, period ==3 & warning ==2)$ave_speed)
# Do you think there is a significant difference in some of the groups?
##Visually there isn't a significant difference seen between the groups

# Now apply 2-way ANOVA: please use the function aov() on the speed depending on the period and warning
# report p-value and interpret the result in details
aov_2way <- aov(ave_speed~period+warning, data = amis_cast2)
anova(aov_2way)
## p value for period = .329105
## p value for warning = .004531
## It shows that the Null hypothesis for the period is not rejected, while for the warning it is too small and 
## under the threshold.
##In the interaction plot we can see that period and warning both have effects on the ave_speed.
interaction.plot(amis_cast2$period,amis_cast2$warning, amis_cast2$ave_speed)


# What do you conclude about the behaviour of drivers based on the 2-way ANOVA?
## when the signs are new, it has a dramatic effect on the ave_speed, while the roads without
##  the signs show the increase in ave_speed in perio = 2. 


