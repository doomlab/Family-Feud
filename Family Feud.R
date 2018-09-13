####Family Fued Study####
#upload data sets
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
AB = read.csv("AB_FF_Data.csv")
#View(AB)
CD = read.csv("CD_FF_Data.csv")
#View(CD)
newdata = read.csv("Points.csv")

# Load Libraries ----------------------------------------------------------
library(moments)
library(reshape)
library(nlme)
library(lme4)

# Data Screening ----------------------------------------------------------
###AB Set###
##Accuracy
#Categorical Variables
table(AB$Number.of.People) #Includes One Group of One? (delete)
table(AB$Condition, AB$Number.of.People)
AB$Condition[AB$Number.of.People == 1] = "A.1 Single"
table(AB$Condition) #Uneven But No Typos
table(AB$Equal.Participation) #*Will Make No Missing Data
notyposAB = AB

#check for continuous problems, making sure only options are 1 and 0
summary(notyposAB) #Looks Good

#Missing Data
#none

#Outliers
#Don't See How we could do an outlier ananlysis (0 and 1 only options)

##Assumptions
#Linearity
random = rchisq(nrow(notyposAB), 7)
fakeAB = lm(random~., data=notyposAB[ , -c(1,2,3,4)])
standardizedAB = rstudent(fakeAB)
qqnorm(standardizedAB)
abline(0,1) 

#Normality
library(moments)
skewness(notyposAB[ , -c(1,2,3,4)], na.rm=TRUE)
kurtosis(notyposAB[ , -c(1,2,3,4)], na.rm=TRUE)
hist(standardizedAB, breaks=15) #A Bit Skewed, but CLT

##homogeneity and homoscedaticity
fitvalues = scale(fakeAB$fitted.values)
plot(fitvalues, standardizedAB) 
abline(0,0)
abline(v = 0)


###CD Set###
#Accuracy
#Categorical Variables
table(CD$Number.of.People) #Looks Good
table(CD$Condition) #Fairly Even, Look Good
table(CD$Condition, CD$Number.of.People)
table(CD$Equal.Participation) #Random Little y, will change
CD$Equal.Participation[CD$Equal.Participation == "y"] = "Y"
CD$Equal.Participation = droplevels(CD$Equal.Participation)
notyposCD = CD

#check for continuous problems, looking for extreme  numbers, and no negatives
summary(notyposCD) # Looks Good

#Missing Data
#none

#Outliers
#I don't think taking out someones guess as an outlier is appropriate analysis

#Assumptions
#Linearity
randomCD = rchisq(nrow(notyposCD), 7)
fakeCD = lm(randomCD~., data=notyposCD[ , -c(1,2,3,4)])
standardizedCD = rstudent(fakeCD)
qqnorm(standardizedCD)
abline(0,1) #Good

#Normality
library(moments)
skewness(notyposCD[ , -c(1,2,3,4)], na.rm=TRUE)
kurtosis(notyposCD[ , -c(1,2,3,4)], na.rm=TRUE)
hist(standardizedCD, breaks=15) #A Bit Skewed, but CLT

#homogeneity and homoscedaticity
fitvaluesCD = scale(fakeCD$fitted.values)
plot(fitvaluesCD, standardizedCD) 
abline(0,0)
abline(v = 0)



# Merging -----------------------------------------------------------------

master = rbind(notyposAB, notyposCD)
View(master)


# Melting -----------------------------------------------------------------

library(reshape)
longmaster = melt(master, 
                   id = c("Group.Name", "Number.of.People", "Condition", "Equal.Participation"), 
                   measured = c("Cigarette", "Smoke", "Ash", "Butt", "Stomach", 
                                "Button", "Ache", "Dancer", "Fat", "Hair", "Tooth", 
                                "Comb", "Paint", "Store", "Food", "Shopping","Bag", "List", 
                                "Shampoo", "Hair.1", "Air", "Cold", "Cool", "Washer","Hair.2", 
                                "Clothes", "Heat", "Laundry", "Jump", "Ski", "Rabbit", "Bunny", 
                                "Scotch", "Paper", "Bed", "Blanket", "Music", "White", "Pillow", 
                                "Cover"))
#Rename Columns
colnames(longmaster)[5] = "Word"
colnames(longmaster)[6] = "Correct"
View(longmaster)

#read in the other data
#make sure the newdata has a column called Word
#make sure Hair and Hair.1 are both rows 

colnames(newdata)[1] = "Word"
longmaster = merge(longmaster, newdata,
                   by.x = "Word")






# C Versus D --------------------------------------------------------------
hyp1 = subset(longmaster, Condition == "C Singles" | Condition == "D Single")

#Intercept Only Model
model1 = gls(Correct ~ 1, 
             data = hyp1, 
             method = "ML", 
             na.action = "na.omit")
summary(model1)

#Random Intercept Only Model
model2 = lme(Correct ~ 1, 
             data = hyp1, 
             method = "ML", 
             na.action = "na.omit",
             random = ~1|Group.Name)
summary(model2)

model2.1 = lme(Correct ~ 1, 
               data = hyp1, 
               method = "ML", 
               na.action = "na.omit",
               random = list(~1|Group.Name, ~1|Word))
summary(model2.1)

model2.2 = lme(Correct ~ 1, 
               data = hyp1, 
               method = "ML", 
               na.action = "na.omit",
               random = list(~1|Word))
summary(model2.2)

#Compare One and Two
anova(model1, model2, model2.1) ## both is better than just group name
anova(model1, model2.1) ##both better than nothing
anova(model1, model2.2, model2.1) ##model 2.1 is actually worse than model 2.2 so just by word

#Add Fixed Effects to the Model
model3 = lme(Correct ~ Condition + Points, 
             data = hyp1, 
             method = "ML", 
             na.action = "na.omit",             
             random = list(~1|Group.Name, ~1|Word))
summary(model3)

#Compare One, Two and Three
anova(model2.2, model3)

##Interactions
model4 = lme(Correct ~ Condition * Points, 
             data = hyp1, 
             method = "ML", 
             na.action = "na.omit",
             random = list(~1|Group.Name, ~1|Word))
summary(model4)

anova(model3, model4)

##simple slopes
#group c slope
#run model 3 on just group c
hyp2 = subset(longmaster, Condition == "C Singles")
model3.c = lme(Correct ~ Points,
             data = hyp2, 
             method = "ML", 
             na.action = "na.omit",
             random = list(~1|Group.Name, ~1|Word))
summary(model3.c)

#group d slope 
#run model 3 on just group d
hyp3 = subset(longmaster, Condition == "D Single")
model3.d = lme(Correct ~ Points,
               data = hyp3, 
               method = "ML", 
               na.action = "na.omit",
               random = list(~1|Group.Name, ~1|Word))
summary(model3.d)

##group c
plot(hyp2$Points, hyp2$Correct)
##group d
plot(hyp3$Points, hyp3$Correct)







# A versus B --------------------------------------------------------------


hyp3 = subset(longmaster, Condition == "B Single" | Condition == "A.1 Single")
model5 = glm(Correct ~ 1, 
             data = hyp3, 
             family = binomial(), 
             na.action = "na.omit")
summary(model5)

##Model 2a = random intercept (participant) 
model6 = glmer(Correct ~ 1 + (1|Group.Name), 
             data = hyp3, 
             na.action = "na.omit",
             family = binomial,
             control = glmerControl(optimizer = "bobyqa"),
             nAGQ = 1)
summary(model6)

model6.1 = glmer(Correct ~ 1 + (1|Group.Name) + (1|Word), 
               data = hyp3, 
               na.action = "na.omit",
               family = binomial,
               control = glmerControl(optimizer = "bobyqa"),
               nAGQ = 1)
summary(model6.1)

model6.2 = glmer(Correct ~ 1 + (1|Word), 
               data = hyp3, 
               na.action = "na.omit",
               family = binomial,
               control = glmerControl(optimizer = "bobyqa"),
               nAGQ = 1)
summary(model6.2)

#Compare
anova(model5, model6, model6.1) 
anova(model5, model6.1)
anova(model5, model6.2, model6.1)

##main effects
model7 = glmer(Correct ~ Condition + Points + (1|Group.Name) + (1|Word), 
             data = hyp3, 
             na.action = "na.omit",
             family = binomial,
             control = glmerControl(optimizer = "bobyqa"),
             nAGQ = 1)
summary(model7)

##interactions
model8 = glmer(Correct ~ Condition * Points + (1|Group.Name) + (1|Word), 
             data = hyp3, 
             na.action = "na.omit",
             family = binomial,
             control = glmerControl(optimizer = "bobyqa"),
             nAGQ = 1)
summary(model8)

anova(model7, model8)

###simple slopes 
#A Single Slope
hyp4 = subset(longmaster, Condition == "A.1 Single")
model3.c = glmer(Correct ~ Points + (1|Group.Name) + (1|Word),
               data = hyp4, 
               na.action = "na.omit",
               family = binomial,
               control = glmerControl(optimizer = "bobyqa"),
               nAGQ = 1)
summary(model3.c)

#B Single Slope
hyp5 = subset(longmaster, Condition == "B Single")
model3.d = glmer(Correct ~ Points + (1|Group.Name) + (1|Word),
               data = hyp5, 
               na.action = "na.omit",
               family = binomial,
               control = glmerControl(optimizer = "bobyqa"),
               nAGQ = 1)
summary(model3.d)








# A Singles Versus Groups -------------------------------------------------

hyp6 = subset(longmaster, Condition == "A Group" | Condition == "A.1 Single")
model9 = glm(Correct ~ 1, 
             data = hyp6, 
             family = binomial(), 
             na.action = "na.omit")
summary(model9) #AIC is 10049

##Model 2a = random intercept (participant) 
model10 = glmer(Correct ~ (1|Group.Name), 
                data = hyp6,
                family = binomial,
                control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 1)
summary(model10) #AIC is 10003.7

model10.1 = glmer(Correct ~ (1|Group.Name) + (1|Word), 
                  data = hyp6,
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa"),
                  nAGQ = 1)
summary(model10.1) #AIC is 7491.9 ****, both is best model

model10.2 = glmer(Correct ~ (1|Word), 
                  data = hyp6,
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa"),
                  nAGQ = 1)
summary(model10.2) #AIC is 7631

#Compare, Doesn't give much info
anova(model9, model10, model10.1) 
anova(model9, model10.1)
anova(model9, model10.2, model10.1)

##main effects
model11 = glmer(Correct ~ Condition + Points + (1|Group.Name) + (1|Word) ,
                data = hyp6,
                family = binomial,
                control = glmerControl(optimizer = "bobyqa"),
                nAGQ = 1)
summary(model11) #AIC is 7373.2, all significant 

##interactions
model11.1 = glmer(Correct ~ Condition * Points + (1|Group.Name) + (1|Word), 
                  data = hyp6,
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa"),
                  nAGQ = 1)
summary(model11.1) #AIC is 7368.7, all significant

anova(model11, model11.1) ##significant

###simple slopes 
#A Single Slope
hyp7 = subset(longmaster, Condition == "A.1 Single")
model11.c = glmer(Correct ~ Points + (1|Group.Name) + (1|Word),
                  data = hyp7,
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa"),
                  nAGQ = 1)
summary(model11.c) #AIC is 2650.5, all significant

#A Group
hyp8 = subset(longmaster, Condition == "A Group")
model11.d = glmer(Correct ~ Points + (1|Group.Name) + (1|Word),
                  data = hyp8,
                  family = binomial,
                  control = glmerControl(optimizer = "bobyqa"),
                  nAGQ = 1) 
summary(model11.d) #AIC is 4775.7, significant








# D Groups Versus D Single ------------------------------------------------
hyp9 = subset(longmaster, Condition == "D Groups" | Condition == "D Single")

#Intercept Only Model
model12 = gls(Correct ~ 1, 
             data = hyp9, 
             method = "ML", 
             na.action = "na.omit")
summary(model12) #AIC is 17228.86

#Random Intercept Only Model
model13 = lme(Correct ~ 1, 
             data = hyp9, 
             method = "ML", 
             na.action = "na.omit",
             random = ~1|Group.Name)
summary(model13) #AIC is 17206.12

model13.1 = lme(Correct ~ 1, 
               data = hyp9, 
               method = "ML", 
               na.action = "na.omit",
               random = list(~1|Group.Name, ~1|Word))
summary(model13.1) #AIC is 17208.12, really close to model13

model13.2 = lme(Correct ~ 1, 
               data = hyp9, 
               method = "ML", 
               na.action = "na.omit",
               random = list(~1|Word))
summary(model13.2) #AIC is 16694.06 ****, only word is best model

#Compare Thirteen and Twelve
anova(model12, model13, model13.1) 
anova(model12, model13.1) 
anova(model12, model13.2, model13.1) 

#Add Fixed Effects to the Model
model14 = lme(Correct ~ Condition + Points, 
             data = hyp9, 
             method = "ML", 
             na.action = "na.omit",             
             random = list(~1|Group.Name, ~1|Word)) #Doesn't Run
summary(model14)

#Compare One, Two and Three
anova(model, model14)

##Interactions
model15 = lme(Correct ~ Condition * Points, 
             data = hyp9, 
             method = "ML", 
             na.action = "na.omit",
             random = list(~1|Group.Name, ~1|Word)) #Doesn't Run
summary(model15)

anova(model14, model15)

###Simple Slopes
#D Single
hyp10 = subset(longmaster, Condition == "D Single")
model16 = lme(Correct ~ Points,
               data = hyp10, 
               method = "ML", 
               na.action = "na.omit",
               random = list(~1|Group.Name, ~1|Word))
summary(model16) #AIC is 16735.43, significant

#D Group
hyp11 = subset(longmaster, Condition == "D Group")
model17 = lme(Correct ~ Points,
               data = hyp11, 
               method = "ML", 
               na.action = "na.omit",
               random = list(~1|Group.Name, ~1|Word))
summary(model17) #AIC is 21488.97, significant


