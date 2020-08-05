##########################
#Week 12: Model Families and Logistic Regression
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, January 27. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are allowed to work together in group up to three students, but everybody 
## needs to submit the group version of the homework via moodle.


## Please write below your (and your teammates) name, matriculation number. 
## Name: Safura Isayeva, Hui-Syuan Yeh, Divyanshu Bhardwaj
## Matriculation number: 2567632, 2571618, 2571652

## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

######################################################################################################################


####
#Part 1
####
# The folder speed.dating
# contains data from an experiment on a few hundred students that randomly assigned
# each participant to 10 short dates with participants of the opposite sex
# (Fisman et al., 2006). For each date, each person recorded several subjective
# numerical ratings of the other person (attractiveness, compatibility, and some 
# other characteristics) and also wrote down whether he or she would like to meet
# the other person again. Label and rij1, . . . , rij6 as person i’s numerical ratings of person j 
# on the dimensions of attractiveness, compatibility, and so forth.



#(1) Fit a classical logistic regression predicting Pr(yij = 1) given person i's 
#    ratings of person j. For ratings, use the features attr, sinc, intel, fun; see the documentation for what exactly these
#    abbreviations stand for.
#    Also, please plot the data in order to inspect it, and discuss the importance of attractiveness, compatibility, and so 
#    forth in this predictive model.
library(readr)
library(ggplot2)
library(broom)
dat <- read.csv("D:/stats_assignment/Speed Dating Data.csv")
str(dat)

#The attribute regarding whether he or she would like to meet the other person again is not found in the dataset.
#The closest or the obvious counfounding attribute we can find is the rating.
#So we rescale the rating(dat$attr_o) into [0,1] so that we can use it as probability of rij
#attr_o: rating
#attr: attractiveness 

dat$attr_o <- dat$attr_o/10
dat$attr_o

m1 <- glm(attr_o~attr+sinc+intel+fun,data=dat,na.action=na.exclude,family="binomial")
summary(m1)
plot_m1 <- ggplot(data = dat, aes(y = attr_o, x = attr+sinc+intel+fun)) + 
  geom_jitter(width = 0, height = 0.05, alpha = 0.5)+
  geom_smooth(method = "glm", se = FALSE)
plot_m1

#According to the coef(m1), the influnce ranking: attr>intel>sinc>fun, however, 
#the scale of measurement might be not completely comparable as desired.
#Say some people might always rate attr range 7-10 instead of 1-10. 


#(2) Expand this model to allow varying intercepts for the persons making the
#    evaluation; that is, some people are more likely than others to want to meet
#    someone again. Discuss the fitted model.

dat$id <- as.factor(dat$id)
class(dat$id)
m2 <- glm(attr_o~attr+sinc+intel+fun+id,data=dat,na.action=na.exclude,family="binomial")
summary(m2)
coef(m2)
sort(coef(m2))
#According to m2, id 16,6,11,14 tend to rate the partner higher

#(3) Expand further to allow varying intercepts for the persons being rated. Discuss
#    the fitted model.

dat$partner <- as.factor(dat$partner)
class(dat$partner)
m3 <- glm(attr_o~attr+sinc+intel+fun+id:partner,data=dat,na.action=na.exclude,family="binomial")
summary(m3)
sort(coef(m3))
#According to m3, id16:partner22  id5:partner22  id22:partner2  id8:partner22 have good compatibility,
# we can see id22 is quite popular.

#(4) Now fit some models that allow the coefficients for attractiveness, compatibility, and the 
#    other attributes to vary by person.  Fit a multilevel model, allowing the intercept and the 
#    coefficients for the 6 ratings to vary by the rater i. (Hint: The model will not converge when you 
#    include many predictors as random slopes; see with how many predictors you can get the model to converge;
#    and try out some of the tricks we have seen to see whether they affect convergence for this dataset.)

m3<-glm(dec~attr+sinc+intel+fun+(1+attr+sinc+intel+fun|pid)+(1+attr+sinc+intel+fun|iid), data=data, family = binomial)
#(5) compare the output for the different models that you calculated - did the model design affect your conclusions?

##m -> AIC= 8319
##m1-> AIC= 8319
##m2-> AIC= 8319
##m3-> AIC= 8319
## They have performed equally powerful.
####

#Part 2
####

# In this example, num_awards is the outcome variable and indicates the number of awards earned by students at
# a high school in a year, math is a continuous predictor variable and represents students' scores on their 
# math final exam, and prog is a categorical predictor variable with three levels indicating the type of program 
# in which the students were enrolled. It is coded as 1 = "General", 2 = "Academic" and 3 = "Vocational". 
# Let's start with loading the data and looking at some descriptive statistics.

p = read.csv("poisson_sim.csv", sep=";")
p <- within(p, {
  prog <- factor(prog, levels=1:3, labels=c("General", "Academic", "Vocational"))
  id <- factor(id)
})
summary(p)

#(6) Plot the data to see whether program type and math final exam score seem to affect the number of awards.

ggplot(p,aes(x=math, y = num_awards, color=factor(prog)))+
  geom_point()
  
#(7) Run a generalized linear model to test for significance of effects.

m4 <- glm(num_awards ~ math + prog, data = p, family = "poisson")
summary(m4)

#(8) Do model comparisons do find out whether the predictors significantly improve model fit.
#	From comparing the models, we see that the predictors do not significantly improve the model.


#(9) Compare to a model that uses a gaussian distribution (normal lm model) for this data.

m5 <- glm(num_awards ~ math + prog, data = p, family = "gaussian")
summary(m5)
