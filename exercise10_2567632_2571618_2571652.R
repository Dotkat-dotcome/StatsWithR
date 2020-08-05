### Stats with R Exercise sheet 10

##########################
#Week 11: Model Selection, Transformations, Power
##########################


## This exercise sheet contains the exercises that you will need to complete and 
## submit by 23:55 on Sunday, January 20. Write the code below the questions. 
## If you need to provide a written answer, comment this out using a hashtag (#). 
## Submit your homework via moodle.
## You are allowed to work together in group up to three students, but everybody 
## needs to submit the group version of the homework via moodle.


## Please write below your (and your teammates) name, matriculation number. 
## Name: Safura Isayeva, Hui-Syuan Yeh, Divyanshu Bhardwaj
## Matriculation number: 2567632, 2571618, 2571652

## Change the name of the file by adding your matriculation numbers
## (exercise0N_firstID_secondID_thirdID.R)

###########################################################################################
###########################################################################################


#Play around with the simulation code. The code simulates how a dataset may be generated. 
#The advantage over using a real data set is that we know exactly how the data was generated, 
#and can observe whether the model manages to correctly identify the original model structure and coefficients.
# IMPORTANT! Run each model simulation code several times to see how stable the results are -- this is necessary
#because we are sampling the data randomly, so it could be that we sometimes get more or less "lucky" draws.

library(lme4)
library(car)

n <- 200 # number of observations to be simulated
predA <- rnorm(n, 100, 20)
predB <- rnorm (n, 60, 30)
interact <- 0.002*(predA*predB) 
error <- rnorm (n, 0, 30)

resp <- 25 + predA + 1.2*predB - interact + error

d <- data.frame(predA, predB, resp)
# 1. Write down what values you would hope for the model to estimate in the ideal case:
# a)intercept= 25
# b)predA= 1
# c)predB= 1.2
# d)predA:predB = 0.0024

m1<- lm(resp~predA*predB, data=d)
# Ignore the warning message about rescaling for now, we'll get to that below.
summary(m1)  

# 2. Can the model recover the original model structure and estimate correct coefficients for the predictors?
#	The model cannot recover the original model structure and estimate correct coefficients for the predictors, as the number of
#	observations in the dataset is small.


# 3. What happens if you change the number of subjects?
#	Change in the number of subjects brings the estimated values of the coefficients closer to the real values.

# 4. What happens if you change the variance of the error term?
#	The change in variance of the error term is directly proportional to the change in the standard error of the residuals.

# 5. What happens if you change the effect sizes?
# 	Change in effect sizes brings about increased variance in the response variable with respect to the predictors.


# Next, we want to observe the effect of scaling the predictors. 
# By hand, we could do: normpredA <- (predA - mean(predA)) / sd(predA)
# this is the same as calling "normpredA <- scale(predA)"
# we can do this for the whole data frame:
nd <- as.data.frame(scale(d))
sm1<- lm(resp~predA*predB, data=nd)
summary(sm1)
vif(m1)
vif(sm1)

# 6. Are the predictors currently correlated? What does the vif value mean?

# For model m1, vif is high for predB and the interaction term which results in high correlation.
# For model sm1, vif is -1 which means that there is no correlation.

# The variance inflation factor (VIF) measures how much the variance of a regression coefficient is inflated due to multicollinearity in the model. 

# 7. Check whether normalization also has a large effect when there is no interaction present in the model

sm2<- lm(resp~predA+predB, data=nd)
m2<- lm(resp~predA+predB, data=d)
summary(sm2)
summary(m2)
vif(m2)
vif(sm2)


### When there is no interaction in included in the model, the predA and predB values
###   are already, almost equal to 1, and normalizing does not have a dramatic effect.

# Try out what happens if there was originally no interaction in the data.
### In m2 there was no interaction included in the model and the values for predA and predB
###   are equally 1.017.

# 8. Try out what happens if there was originally no interaction in the data.
### In m2 there was no interaction included in the model and the values for predA and predB
###   are equally 1.017.


#Next, we want to calculate interpretable estimates
names(sm1)
coef(sm1)

denormPredA <- coef(sm1)[2] *sd(d$resp)/ sd(d$predA) 
denormPredA


# 10. Explain in your own words, why the denormalization for predictor A works this way.
### For the coefficients we have the following relation: Coef(x)= correlation* SDy/SDx, and so:
###   We have the following: correlation = Coef(predA)*SD(resp)/SD(predA)
###   What we have done, is denormalizing the predA with the response value.

denormPredB <- coef(sm1)[3] * sd(d$resp)/ sd(d$predB)
denormPredB
# expected: 1.2


denormIntercept<-coef(sm1)[1] * sd(d$resp)+mean(d$resp)-
  (denormPredA*mean(d$predA) + denormPredB* mean(d$predB))
denormIntercept
# expected: 25

denormInteract <- coefficients(sm1)[4] / (sd(d$predA)*sd(d$predB)) * sd(d$resp)
denormInteract




# Next, we create correlated variables 
n <- 200 # number of observations to be simulated
predA <- rnorm(n, 100, 20)
predB<- rnorm (n, 60, 30)
predC <- -1* predA + rnorm(n,0,10)
error <- rnorm (n, 0, 30)
respcor <- 25 + predA + 3* predB+ 2*predC - (0.02*(predA*predC)) + error
d2<-data.frame(predA, predB, predC, respcor)
summary(lm(respcor ~ predA * predC + predB, data=d2))

sd2 <-as.data.frame(scale(d2))
summary(lm(respcor ~ predA * predC + predB, data=sd2))

# 11. What do you observe regarding the results from the models? Do the models obtain the same or different results 
# with / without normalization?
### What I observed was the model d2 has better and almost acceptable estimates and normalizing the models did not leave a positive affect on 
###   the results, after normalizing in sd2 model, the estimated values are far away from the expected ideal values.

# 12. Denormalize the coefficients.
sd2 <- lm(respcor ~ predA * predC + predB, data=sd2)
summary(sd2)
coef(sd2)
denormIntercept<-coef(sd2)[1] * sd(d$resp)+mean(d$resp)-
  (denormPredA*mean(d$predA) + denormPredB* mean(d$predB) + denormPredC* mean(d$predC))
denormIntercept

denormPredA <- coef(sd2)[2] *sd(d$resp)/ sd(d$predA) 
denormPredC <- coef(sd2)[3] *sd(d$resp)/ sd(d$predC) 
denormPredB <- coef(sd2)[4] *sd(d$resp)/ sd(d$predB) 

denormInteract <- coef(sd2)[5] / (sd(d$predA)*sd(d$predC)) * sd(d$resp)
denormInteract


# Finally, we will generate repeated measures!
# For this, we will use the dataframe d; for simplicity of interpretation, we will do no normalization here.
n<-400
predA <- rnorm(n, 100, 20)
predB <- rnorm (n, 60, 30)
error <- rnorm (n, 0, 30)
subjno <- 20 #number of subjects; 
itemno<- 20 # number of items;
subj<-as.factor(rep(1:subjno,itemno))
item<-as.factor(rep(1:itemno, each=subjno))
lmerd <- data.frame(predA, predB, resp, subj, item)
# basic data frame done; now on to by subject and by item random effects:
subjid<-as.factor(1:subjno)
subjint<- rnorm(subjno, 0, 8)
subjeffdf<-data.frame(subjid, subjint)
itemid<-as.factor(1:itemno)
itemint<-rnorm(itemno, 0, 4)
itemeffdf<-data.frame(itemid, itemint)
newd <- merge(lmerd, itemeffdf, by.x="item", by.y="itemid")
newd<- merge(newd, subjeffdf, by.x="subj", by.y = "subjid")
lmerd <- newd
# add by-subject and by-item effects here!
lmerd$respr <- 25+newd$subjint + newd$itemint + newd$predA + 1.2*newd$predB + error

m0<-lm(respr ~ predA + predB , data=lmerd)
summary(m0)
m1<-lmer(respr ~ predA + predB + (1|subj) + (1| item), data=lmerd)
summary(m1)

lmerd$resp <- 25 + newd$predA + 1.2*newd$predB + error
m2<-lmer(resp ~ predA + predB + (1|subj) + (1| item), data=lmerd)
summary(m2)

#13. Explain the difference between models m0 m1 and m2
##m0 includes fixed effect slope: predA and predB
##m1 includes fixed effect slope: predA and predB; both fixed and random effect intercepts: subj and item
##m2 includes fixed effect slope: predA and predB; random effect intercepts: subj and item


#14. Play around with the size of the by item and by subject effects (here: intercepts only)
m3<-lmer(resp ~ predA + predB + (1|item), data=lmerd)
summary(m3)

m4<-lmer(resp ~ predA + predB + (1|subj), data=lmerd)
summary(m4)


#15. Generate the data such that subjects differ in terms of how much predictor A affects them.
w<- rnorm(n, 1000, 50)
subjint<- predA*w

#16. Then build a mixed effects model that includes a random slope for subjects.
m5<-lmer(resp ~ predA + predB + (predA|subj), data = lmerd)
##singular fit may be from the overfitting



