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

markets <- unique(mayo_DF$market)
products <- unique(mayo_DF$product)

stats <- data.frame()
k = 0
for (i in 1:2){
  for (j in 1:2){
    k = k + 1
    stats[k,1] <- markets[i]
    stats[k,2] <- products[j]
    stats[k,3] <- mean(mayo_DF$price[which((mayo_DF$market == markets[i] & mayo_DF$product == products[j]))])
    stats[k,4] <- median(mayo_DF$price[which((mayo_DF$market == markets[i] & mayo_DF$product == products[j]))])
    stats[k,5] <- sd(mayo_DF$price[which((mayo_DF$market == markets[i] & mayo_DF$product == products[j]))])
  }
}

