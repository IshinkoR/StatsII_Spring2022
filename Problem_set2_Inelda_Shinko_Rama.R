#Loading the dataset

climateSupport <- read.csv("C:/Users/hp/Downloads/climateSupport.csv")
View(climateSupport)
str(climateSupport)

climateSupport$choice <-  as.factor(climateSupport$choice)
climateSupport$countries <- as.factor(climateSupport$countries)
climateSupport$sanctions <- as.factor(climateSupport$sanctions)
str(climateSupport)


?relevel

levels(climateSupport$choice)
relevel(climateSupport$choice, ref = "Not supported")


###Question 1

# Logistic regression model

logmodel <- glm(formula = choice~ countries + sanctions, data = climateSupport, family = binomial)
summary(logmodel)

#To test the significance for the overall model, we use the p- value
1-pchisq((logmodel$null.deviance-logmodel$deviance), (logmodel$df.null-logmodel$df.residual))

# Model with Interactions

logmodel2 <- glm(formula = choice~ countries * sanctions, data = climateSupport, family = binomial)
summary(logmodel2)
