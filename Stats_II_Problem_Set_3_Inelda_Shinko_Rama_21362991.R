

setwd("D:/R-Codes")

library(dplyr)
library(nnet)
require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)




#*Loading the datasets for the two problems and checking the structure*
  
  
odt1=read.csv('odt1.csv',header = T)

odt1 %>% head(10)



str(odt1)




odt2=read.csv('odt2.csv',header = T)

odt2 %>% head(10)


str(odt2)


#**PROBLEM 1.1**
  
odt2$OIL=as.factor(odt2$OIL)

odt2$REG=as.factor(odt2$REG)

str(odt2)


# Recode GDPWdiff correctly
odt2 %>% mutate(GDPWdiff = case_when(
  GDPWdiff > 1 ~ 'positive',
  GDPWdiff < -1 ~ 'negative',
  GDPWdiff == 0 ~ 'no_change',
  GDPWdiff == " " ~ 'no_change'
)) -> odt22

odt22 %>% str()






#**PROBLEM 1.2**
  
 # *unordered multinom*
  

# Use "no change" as the reference category
#odt22$GDPWdiff <- relevel(odt22$GDPWdiff, ref = "no_change")

model <- multinom(GDPWdiff ~ OIL+REG,data = odt22 )
summary(model)


#*ordered multinom*
  
  
odt22$GDPWdiff=as.factor(odt22$GDPWdiff)


m <- polr(GDPWdiff ~ OIL+REG,data = odt22)
summary(m)


odt22$GDPWdiff=as.factor(odt22$GDPWdiff)






#**QUESTION TWO**
  
  
str(odt1)

odt1 %>% head()


#**Problem 2.a**
  
  
odt1$competitive.district=as.factor(odt1$competitive.district)

odt1$PAN.governor.06=as.factor(odt1$PAN.governor.06)


odt1 %>% str()


output <-glm(formula = PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06,
             data = odt1, family = poisson)

print(summary(output)) 
output





