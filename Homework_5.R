#### 37301 Data Driven Marketing ######
#### Homework 5 #######

library(psych)
library(lattice)
library(plyr)
library(mfx)

setwd("/Users/ben/Dropbox/Chicago Booth/37103 Data Driven Marketing/Homework 5/37103-HW5")
load("Detailing.RData")

########################################################
################## Question 1 ##########################

histogram(detail_DF$det, main = "Histogram of Detailing Visits in Last 30 Days")

describeBy(detail_DF$det, group = detail_DF$type)
# There is a lot more detailing for doctors that perscribe more

########################################################
################## Question 2 ##########################
reg1 <- glm(choice ~ det, family = binomial(link = "logit"), data = detail_DF)
summary(reg1)

logitmfx(choice ~ det, data = detail_DF)

# Each detailing visit increases the probability of a perscription by 3.4%

########################################################
################## Question 3 ##########################

# Create blank vectors for the deltas and resulting AIC values of regressions
deltas <- numeric()
AICs <- numeric()

# Loop through all delta values with regressions
for (i in 1:5){
  # Calculate Delta
  delta = 0.7 + 0.05*i
  # Use delta to calculate the detail stock variable
  detail_DF$detstock = detail_DF$det + delta*detail_DF$lag_det
  # Run regression using the detail stock
  reg2 <- glm(choice ~ detstock, family = binomial(link = "logit"), data = detail_DF)
  # Store the delta value in a vector of deltas
  deltas[i] <- delta
  # Store the AIC from the regression in a vector of AIC values
  AICs[i] <- AIC(reg2)
}

# Combine the deltas and AIC values into a data frame
AIC_DF <- data.frame(deltas, AICs)

# Pull delta that corresponds to minimum AIC
delta_min = AIC_DF$deltas[which.min(AIC_DF$AICs)]

# Minimum AIC at delta = 0.8
# Detailing from 31 to 60 days ago has 80% of the impact of detailing in the past 30 days.
# The impact of detailing declines by 20% over the 30 days

detail_DF$detstock = detail_DF$det + delta_min*detail_DF$lag_det
reg3 <- glm(choice ~ detstock, family = binomial(link = "logit"), data = detail_DF)
summary(reg3)
logitmfx(choice ~ detstock, data = detail_DF)

# Our coefficient says one detailing visit or 1.25 lagged detailing visit increases 
# the chance of a perscription by 2.4%

########################################################
################## Question 4 ##########################

# ROI formula is Increase in Profit / Cost
# (Increase in Revenue - Cost) / Cost
# (Increase in Probility of Perscription Per Visit * Visits * Revenue per Perscription - Cost) / Cost
# (Delta P * 15.7 * $100 - $60) / $60

# Method 1: Marginal effect on prescription probability

# Run mfx package
mfx1 <- logitmfx(choice ~ detstock, data = detail_DF, atmean = FALSE)

# Extract the marginal effects extimates from the mfx output
P_mfx <- mfx1$mfxest[1]

# Calculate ROI
ROI_MFX <- (P_mfx * 15.7 * 100 - 60) / 60


# Method 2: Exact effect on prescription probability

