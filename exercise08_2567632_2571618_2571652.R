### Stats with R Exercise sheet 8

##########################
#Week9: Checking Assumptions underlying ANOVA and linear regression
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, December 16. Write the code below the questions. 
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

###############################################################################
###############################################################################

########
### Exercise 1
########

########
### Please, use ggplot to make plots in all exercises below!
########


# a) Read in the data kidiq.txt (available in the Moodle) and take a look
#    at the data summary. It contains information about the mum's iq and 
#    their child's iq. 
#    mom_hs indicates whether the mother has a high school degree
#    1= high school education, 0= no high school degree.
 

  setwd("~/R")
  iq <- read.table("kidiq.txt")
	summary(iq)
	head(iq)


# b) Plot kid_score against mom_iq in a scatter plot, and add a regression line 
#    (kid_score should be the response variable and mom_iq the predictor 
#    variable) to the plot. 
#    Name the plot and the axis in sensible ways.
	library(ggplot2)
	ggplot(iq, aes(x = mom_iq, y= kid_score))+
  	geom_point()+
  	geom_smooth(method='lm',se = FALSE)+
  	ggtitle( "Kid_Score vs Mom_IQ")+
  	xlab("Mom_IQ")+ ylab("Kid_Score")


# c) Calculate a simple regression model for kid_score with mom_hs as a 
#    predictor and interpret the results.
	regmod1<- lm(kid_score ~ mom_hs, iq)
	summary(regmod1)
#	The minimum value for kid score is 77.548. For every unit increase in mom_hs, kid_score increases by
#	11.771.



# d) Next, fit a regression model with two predictors: mom_hs and mom_iq. 
#    Interpret the model and compare to the previous model.

model2 <- lm(iq$kid_score ~ iq$mom_hs + iq$mom_iq)
model2
## We got the b0 = 25.7315 , b1= 5.9501 and b2 = 0.5639 in kid_score = b0 + b1*mom_hs + b2*mom_iq,
##  this shows that kid_score is rarely related to his/her mom's iq, while more related to the mother's
##  education.


# e) Now plot a model where both predictors are shown. Do this by plotting 
#    data points for mothers with high school degree==1 in one color and those 
#    without one in another color. Then also fit two separate regression lines such 
#    that these lines reflect the model results.
#	   HINT: One solution in ggplot is to calculate fitted values of the regression model 
#    and then plot them along with the original data points:
#    pred = data.frame(mom_iq=kidiq$mom_iq, mom_hs=kidiq$mom_hs, kid_score_pred=fitted(your_model))

library(ggplot2)
kidiq_momHs_0 <- subset(iq,mom_hs ==0)
kidiq_momHs_1 <- subset(iq,mom_hs ==1)
coef0<-coef(lm(kidiq_momHs_0$kid_score~kidiq_momHs_0$mom_iq))
coef0#intercept = -11.4820211, slope = 0.9688892
coef1<-coef(lm(kidiq_momHs_1$kid_score~kidiq_momHs_1$mom_iq))
coef1#intercept = 39.7862023, slope = 0.4846145

ggplot(iq, aes(x=mom_iq, y = kid_score, colour = factor(mom_hs)))+
  geom_point()+
  geom_abline(intercept = -11.4820211, slope = 0.9688892, colour = rgb(1,0,0))+##coef0
  geom_abline(intercept = 39.7862023, slope = 0.4846145, colour = rgb(0,.6,.6))##coef1


# f) Next, we will proceed to a model including an interaction between mom_hs
#    and mom_iq. Interpret your results.

model_with_interaction <- lm(iq$kid_score ~ iq$mom_hs * iq$mom_iq)
model_with_interaction


# g) Next, let's plot the results of this model.
plot(model_with_interaction)
## It displays 4 sequential plots:
##  1- Residuals vs Fitted
##  2- Normal QQplot
##  3- Fitted values agains the square root of Residuals
##  4- Leverage against residuals


# h) Next, let's explore the "predict.lm" function. Please first generate
#    a new dataframe with one datapoint (a mother with high school degree
#    and iq of 100). Then, use the predict function to predict the child's iq. 
#    Please specify the predict function to also give you the 0.95 confidence 
#    interval.

newdata <- (subset(iq, mom_hs == 1 & mom_iq == 100))
predict.lm(model_with_interaction, newdata, interval="confidence", level=.95)



# i) Meaning of confidence intervals for regression line.
#    Let's go back to the exercise b) and plot again the data points with the 
#    regression line. By default, there should also be displayed the borders of 
#    the confidence interval. What is the meaning of the confidence interval?

library(ggplot2)
ggplot(iq, aes(x = mom_iq, y= kid_score))+
  geom_point()+
  geom_smooth(method='lm')+
  ggtitle( "Kid_Score vs Mom_IQ")+
  xlab("Mom_IQ")+ ylab("Kid_Score")

#A confidence interval addresses this issue:
#How well the sample statistic estimates the underlying population value is always an issue
#because it provides a range of values which is likely to contain the population parameter of interest
#constructed under certain corresponding confidence level


# j) Finally, do model checking on your model with the interaction, i.e. inspect 
#    the standard model plots provided by R, and interpret what you see.

plot(model_with_interaction)

#the standard model plots provided have 4 plots which analysis the model in different aspect respetively 

#The Normal QQ plot helps us to assess whether the residuals are roughly normally distributed. 
#If the residuals look far from normal we may be in trouble. 
#In particular, if the residual tend to be larger in magnitude than what we would expect from the normal distribution, 
#then our p-values and confidence intervals may be too optimisitic. 


#The Residuals vs Leverage plot are used to detect possible outlier. 
#, where possible outlier is any point that isn't approximated well by the model (has a large residual) 
#and which significantly influences model fit (has large leverage). 

