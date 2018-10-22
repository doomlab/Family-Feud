####Family Fued Study####
#upload data sets
ta





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
hyp9 = subset(longmaster, Condition == "D Group" | Condition == "D Single")

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
anova(model12, model13, model13.1) # model 3 is significant
anova(model12, model13.1) #model 3.1 is significant
anova(model12, model13.2, model13.1) #All significant

#Add Fixed Effects to the Model
model14 = lme(Correct ~ Condition + Points, 
             data = hyp9, 
             method = "ML", 
             na.action = "na.omit",             
             random = list(~1|Group.Name, ~1|Word))
summary(model14)

#Compare One, Two and Three
anova(model, model14)

##Interactions
model15 = lme(Correct ~ Condition * Points, 
             data = hyp9, 
             method = "ML", 
             na.action = "na.omit",
             random = list(~1|Group.Name, ~1|Word)) 
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


