#### 37301 Data Driven Marketing ######
#### Homework 1 #######

setwd("/Users/ben/Dropbox/Chicago Booth/37103 Data Driven Marketing/Homework 1/37103-HW1")
load("Mayo.RData")

###########################################################
########################### Part 1 ########################

# Summarize data each variable overall
summary(mayo_DF)

# Create summary table of data
table(mayo_DF$market, mayo_DF$product)

library(psych)


# Summarize data by market and product
describeBy(mayo_DF, list(mayo_DF$product, mayo_DF$market))

# Summarize sales by product
describeBy(mayo_DF$sales_units, mayo_DF$product)

###########################################################
########################### Part 2 ########################

# Create price variable
mayo_DF$price = mayo_DF$sales_dollars/mayo_DF$sales_units

### 2.1 ###
# The price variable represents the average of actual sale prices over the week. It includes discounts.

stats_1 <- describeBy(mayo_DF$price, list(mayo_DF$product, mayo_DF$market), mat=TRUE)
write.csv(stats_1,"Price Statistics.csv")

# The means are fairly similar across all markets.
# Jewel-Osco prices are higher than the Kraft Central region for both products.
# There is more price variation in Jewel-Osco than the whole region. This is because the whole region has multiple chains.
# If one chain runs a promotion, it will have a small effect on the overall data.
# It will be easier to estimate elasticity for the Jewel Osco because we have more variation to observe.

###########################################################
########################### Part 3 ########################



library(lattice)

xyplot(price ~ week | product + market, data = mayo_DF,
       type = "o",
       pch = 21, lwd = 0.5, cex = 0.75, col = "gray40", fill = "blue",
       par.settings = list(strip.background = list(col = "gray95")),
       xlab = "Week", ylab = "Price")

# Question 3 #
# The price at Jewl is more variable.  This is likely because of store specific sales or promotions.  

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

# Subset data by product and market
Jewel.H <- subset(mayo_DF, product=="Hellmans Mayo 32oz" & market=="Jewel")
Jewel.K <- subset(mayo_DF, product=="Kraft Mayo 32oz" & market=="Jewel")
Central.H <- subset(mayo_DF, product=="Hellmans Mayo 32oz" & market=="Kraft Central")
Central.K <- subset(mayo_DF, product=="Kraft Mayo 32oz" & market=="Kraft Central")


reg1.Jewel.H <- lm(log_sales_units ~ log_price, data = Jewel.H)
summary(reg1.Jewel.H)

reg1.Jewel.K <- lm(log_sales_units ~ log_price, data = Jewel.K)
summary(reg1.Jewel.K)

reg1.Central.H <- lm(log_sales_units ~ log_price, data = Central.H)
summary(reg1.Central.H)

reg1.Central.K <- lm(log_sales_units ~ log_price, data = Central.K)
summary(reg1.Central.K)

## Question 5 ##
# Demand is more elastic at Jewel Osco.  This is because it is easier to substitute to or away from Jewel: simply go to another store.
# It is harder to substitute to or away from all stores in the Central region.  Most people will not drive out of state for cheaper Mayo.

## Question 6 ##

# We cannot model a simultanious price increase because these equations do not include cross price elasticities.
# We can only estimate the impact of each price increse in isolation.

# The beta coefficient for Helmans at Jewel is -4.584. 
# Using the log approximation, this implies a price increase of 10% will decrease sales by 10 * 4.584 = 45.84%
# Using the exact formula, we estimate a price increase of 10% will decrease sales by 1.1^-4.584 - 1 = -35.39%

# The beta coefficient for Kraft at Jewel is -4.167
# Using the log approximation, this implies a price increase of 10% will decrease sales by 10 * -4.167 = -41.67%
# Using the exact formula, we estimate a price increase of 10% will decrease sales by 1.1^-4.167 - 1 = -32.78%

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

reg2.Jewel.H <- lm(log_sales_H ~ log_price_H + log_price_K, data = mayo_DF_wide[which(mayo_DF_wide$market == "Jewel"),])
summary(reg2.Jewel.H)

reg2.Jewel.K <- lm(log_sales_K ~ log_price_K + log_price_H, data = mayo_DF_wide[which(mayo_DF_wide$market == "Jewel"),])
summary(reg2.Jewel.K)

reg2.Central.H <- lm(log_sales_H ~ log_price_H + log_price_K, data = mayo_DF_wide[which(mayo_DF_wide$market == "Kraft Central"),])
summary(reg2.Central.H)

reg2.Central.K <- lm(log_sales_K ~ log_price_K + log_price_H, data = mayo_DF_wide[which(mayo_DF_wide$market == "Kraft Central"),])
summary(reg2.Central.K)

## Question 8 ###
# In Jewel stores, Helmanns sales increase 0.20% for each 1% increase in Kraft prices.  
# Kraft sales increase 1.78% for each 1% increase in Hellmans prices.

# In Central Region stores, Helmanns sales actually decrease 0.07% for each 1% increase in Kraft prices. 
# However, this effect is not statistically significantly different from zero. 
# Kraft sales increase 2.58% for each 1% increase in Hellmans prices.

# Therefore, Kraft is more vulnerable to changes competitor prices than Hellman's

### Question 9 ###
# The coefficient on Hellman's price in Kraft sales at Jewel is -1.871.
# Therefore, a Hellman's price cut of 10% will decrease Kraft sales by 0.9^1.871 - 1 = -17.89%
# To offset this, Kraft must decrease price by:
# (1/(1-.1789))^(1/-4.44) - 1
# (1/.8212)^(1/-4.44) - 1
# (1.2179)^(-0.2253) - 1
# -4.34%

