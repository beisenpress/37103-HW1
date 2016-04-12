#### 37301 Data Driven Marketing ######
#### Homework 1 #######

setwd("/Users/ben/Dropbox/Chicago Booth/37103 Data Driven Marketing/Homework 1/37103-HW1")
load("Mayo.RData")

###########################################################
########################### Part 1 ########################

# Summarize data overall
summary(mayo_DF)

# Create summary table of data
table(mayo_DF$market, mayo_DF$product)

Jewel.H <- subset(mayo_DF, product=="Hellmans Mayo 32oz" & market=="Jewel")
Jewel.K <- subset(mayo_DF, product=="Kraft Mayo 32oz" & market=="Jewel")
Central.H <- subset(mayo_DF, product=="Hellmans Mayo 32oz" & market=="Kraft Central")
Central.K <- subset(mayo_DF, product=="Kraft Mayo 32oz" & market=="Kraft Central")

library(psych)


# Use multiple variables to create groups:
describeBy(mayo_DF, list(mayo_DF$product, mayo_DF$market))
describeBy(mayo_DF$sales_units, mayo_DF$product)

###########################################################
########################### Part 2 ########################

mayo_DF$price = mayo_DF$sales_dollars/mayo_DF$sales_units

### 2.1 ###
# The price variable represents the average sale price over the week

stats_1 <- describeBy(mayo_DF$price, list(mayo_DF$product, mayo_DF$market), mat=TRUE)
write.csv(stats_1,"Price Statistics.csv")

# The means are fairly similar across all markets.
# Jewel-Osco prices are higher than the Kraft Central region as a whole.
# There is more price variation in Jewel-Osco than the whole region. This is likely due to the law of large numbers.
# The full region has more stores than Jewel-Osco does, so their standard deviation is lower.
# It will be easier to estimate elasticity for the  whole region because we have more data

###########################################################
########################### Part 3 ########################



plot(mayo_DF$week, mayo_DF$price, type = "o")

library(lattice)

xyplot(price ~ week | product + market, data = mayo_DF,
       type = "o",
       pch = 21, lwd = 0.5, cex = 0.75, col = "gray40", fill = "blue",
       par.settings = list(strip.background = list(col = "gray95")),
       xlab = "Week", ylab = "Price")

# Question 3 #
# The price at Jewl is more variable.  This is likely because of store specific sales or promotions

###########################################################
########################### Part 4 ########################

xyplot(sales_units ~ price | product + market, data = mayo_DF,
       pch = 21, lwd = 0.5, cex = 0.75, col = "gray40", fill = "blue",
       scales = list(relation = "free"),
       par.settings = list(strip.background = list(col = "gray95")),
       xlab = "Price", ylab = "Units")

# Question 3 #
# There appears to be a slight negative relationship
# The data is bunched in the lower right corner, so it is hard to see a relationship.

###########################################################
########################### Part 5 ########################
mayo_DF$log_price = log(mayo_DF$price)
mayo_DF$log_sales_units = log(mayo_DF$sales_units)

reg1.combined <- lm(log_price ~ log_sales_units*product*market, data = mayo_DF)
summary(reg1)

reg1.Jewel.H <- lm(log_price ~ log_sales_units, data = Jewel.H)
summary(reg1.Jewel.H)

reg1.Jewel.K <- lm(log_price ~ log_sales_units, data = Jewel.K)
summary(reg1.Jewel.K)

reg1.Central.H <- lm(log_price ~ log_sales_units, data = Central.H)
summary(reg1.Central.H)

reg1.Central.K <- lm(log_price ~ log_sales_units, data = Central.K)
summary(reg1.Central.K)

###########################################################
########################### Part 6 ########################

mayo_DF_extract = mayo_DF[, c("market", "product", "week", "sales_units", "price")]


mayo_DF_wide = reshape(mayo_DF_extract,
                       timevar = "product",
                       idvar = c("market", "week"),
                       direction = "wide")

colnames(mayo_DF_wide) = c("market", "week", "sales_H", "price_H", "sales_K", "price_K")

mayo_DF_wide$log_sales_H = log(mayo_DF_wide$sales_H)
mayo_DF_wide$log_price_H = log(mayo_DF_wide$price_H)
mayo_DF_wide$log_sales_K = log(mayo_DF_wide$sales_K)
mayo_DF_wide$log_price_K = log(mayo_DF_wide$price_K)

reg2.Jewel.H <- lm(log_sales_H ~ log_price_H + log_price_K, data = mayo_DF_wide[which(mayo_DF$market == "Jewel"),])
summary(reg2.Jewel.H)

reg2.Jewel.K <- lm(log_sales_K ~ log_price_K + log_price_H, data = mayo_DF_wide[which(mayo_DF$market == "Jewel"),])
summary(reg2.Jewel.K)

reg2.Central.H <- lm(log_sales_H ~ log_price_H + log_price_K, data = mayo_DF_wide[which(mayo_DF$market == "Kraft Central"),])
summary(reg2.Central.H)

reg2.Central.K <- lm(log_sales_K ~ log_price_K + log_price_H, data = mayo_DF_wide[which(mayo_DF$market == "Kraft Central"),])
summary(reg2.Central.K)

## Question 8 ###
# In Jewel stores, Heinz sales increase 1.76% for each % decrease in Kraft prices.  Kraft sales increase 
# In Central Region stores, Heinz sales decrease 2.10% for each 1% decrease in Kraft prices.  Kraft sales increase 



