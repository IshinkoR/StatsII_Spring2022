#Question 1
#We’re interested in modeling the historical causes of infant mortality. We have data from
#5641 first-born in seven Swedish parishes 1820-1895. Using the ”infants” dataset in the
#eha library, fit a Cox Proportional Hazard model using mother’s age and infant’s gender as
#covariates. Present and interpret the output.

# un-comment the following command to install package eha.
# install.packages("eha")

# load the package
require(eha)
require(survival)
require(survminer)
require(dplyr)

data("infants")


glimpse(infants)


# create a time variable by subtracting exit and enter
infants <- infants %>% mutate(time = exit - enter)

# Fit survival data a Cox proportional hazards model
cox_model <- coxph(Surv(time, event) ~ age + sex, data = infants)

# summarize the model
summary(cox_model)

# visualize the results 
ggforest(cox_model, data = infants)