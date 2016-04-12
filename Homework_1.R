#### 37301 Data Driven Marketing ######
#### Homework 1 #######

setwd("/Users/ben/Dropbox/Chicago Booth/37103 Data Driven Marketing/Homework 1/37103-HW1")
load("Mayo.RData")

###########################################################
######################### Question 1 ######################

summary(mayo_DF)

table(mayo_DF$market, mayo_DF$product)

summary(subset(mayo_DF, product=="Hellmans Mayo 32oz" & market=="Jewel"))
summary(subset(mayo_DF, product=="Kraft Mayo 32oz" & market=="Jewel"))
summary(subset(mayo_DF, product=="Hellmans Mayo 32oz" & market=="Kraft Central"))
summary(subset(mayo_DF, product=="Kraft Mayo 32oz" & market=="Kraft Central"))

library(psych)

# Use one variable to create groups:
describeBy(mayo_DF, mayo_DF$product)
# Use multiple variables to create groups:
describeBy(mayo_DF, list(mayo_DF$product, mayo_DF$market))
describeBy(mayo_DF$sales_units, mayo_DF$product)

###########################################################
######################### Question 2 ######################

mayo_DF$price = mayo_DF$sales_dollars/mayo_DF$sales_units

### 2.1 ###
# The price variable represents the average sale price over the week

stats_1 <- describeBy(mayo_DF$price, list(mayo_DF$product, mayo_DF$market), mat=TRUE)

