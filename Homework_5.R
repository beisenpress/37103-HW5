#### 37301 Data Driven Marketing ######
#### Homework 5 #######

# Load libraries
library(psych)
library(lattice)
library(plyr)
library(mfx)

# Set working directory
setwd("/Users/ben/Dropbox/Chicago Booth/37103 Data Driven Marketing/Homework 5/37103-HW5")
# Load Data
load("Detailing.RData")

########################################################
################## Question 1 ##########################

histogram(detail_DF$det, main = "Detailing Visits in Last 30 Days", type = "count")

histogram(~factor(det)|factor(type), data = detail_DF, main = "Detailing Visits in Last 30 Days by Type", type = "count")

table(detail_DF$det, detail_DF$type)

describeBy(detail_DF$det, group = detail_DF$type)
# There is a lot more detailing for doctors that prescribe more

########################################################
################## Question 2 ##########################
reg1 <- glm(choice ~ det, family = binomial(link = "logit"), data = detail_DF)
summary(reg1)

logitmfx(choice ~ det, data = detail_DF, atmean = FALSE)

# Each detailing visit increases the probability of a prescription by 3.4%

########################################################
################## Question 3 ##########################

# Create blank vectors for the deltas and resulting AIC values of regressions
deltas <- numeric()
AICs <- numeric()

# Loop through all delta values with regressions
for (i in 1:5){
  # Calculate Delta
  delta = 0.65 + 0.05*i
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
logitmfx(choice ~ detstock, data = detail_DF, atmean = FALSE)

# Our coefficient says one detailing visit or 1.25 lagged detailing visit increases 
# the chance of a prescription by 2.4%

########################################################
################## Question 4 ##########################

# ROI formula is Increase in Profit / Cost
# (Increase in Revenue - Cost) / Cost
# (Increase in Probility of Prescription Per Visit * Visits * Revenue per Prescription - Cost) / Cost
# (Delta P * 15.7 * $100 - $60) / $60

# Method 1: Marginal effect on prescription probability

# Run mfx package
mfx1 <- logitmfx(choice ~ detstock, data = detail_DF, atmean = FALSE)

# Extract the marginal effects extimates from the mfx output
P_mfx1 <- mfx1$mfxest[1]

# Calculate ROI using marginal effects method
ROI_MFX1 <- (P_mfx1 * 15.7 * 100 - 60) / 60


# Method 2: Exact effect on prescription probability

# Calculate the predicted probabiliy of writing a prescription for each observation
Pr = predict(reg3, type = "response")

# Create summary statistics and histogram for predicted probabilities
describe(Pr)
histogram(Pr, xlab = "Predicted Probability of Writing a Prescription", main = "Histogram of Predicted Probabilities")

# Create a new data frame
detail_new_DF = detail_DF

# In the new data frame, increase detail stock by 1 for every observation
detail_new_DF$detstock = detail_new_DF$detstock + 1

# Calculate the predicted probabiliy of writing a prescription for each modified observation
Pr_1 = predict(reg3, newdata = detail_new_DF, type = "response")

# Calculate the average difference in predictions.  This yields the marginal probability for 1 more detailing
P_pred1 <- mean(Pr_1 - Pr)

# Calculate ROI using prediction method
ROI_Pred1 <- (P_pred1 * 15.7 * 100 - 60) / 60

# ROI using both methods is negative 

########################################################
################## Question 5 ##########################

# ROI formula is Increase in Profit / Cost
# (Increase in Revenue - Cost) / Cost
# (Increase in Probility of Prescription Per Visit * Visits * Revenue per Prescription - Cost) / Cost
# (Delta P * 15.7 * $100 - $60) / $60

# Method 1: Marginal effect on prescription probability

# Calculate ROI.  The probability from the MFX output is simply multipled by the delta lag factor 
ROI_MFX2 <- (P_mfx1 * (1+delta_min) * 15.7 * 100 - 60) / 60


# Method 2: Exact effect on prescription probability

# Calculate the predicted probabiliy of writing a prescription for each observation
detail_new_DF = detail_DF

# In the new data frame, increase detail stock by 1 for every observation
detail_new_DF$detstock = detail_new_DF$detstock + delta_min

# Calculate the predicted probabiliy of writing a prescription for each modified observation
Pr_2 = predict(reg3, newdata = detail_new_DF, type = "response")

# Calculate the average difference in predictions.  This yields the marginal probability for 1 more detailing
P_pred2 <- mean(Pr_2 - Pr)

# Calculate ROI
ROI_Pred2 <- ( (P_pred1 + P_pred2) * 15.7 * 100 - 60) / 60

# ROI under both methods is now positive

########################################################
################## Question 6 ##########################

# Data frame containing marginal effects and (exact) predicted incremental prescription probabilities
type_DF = data.frame(type = 1:3,
                     visits = c(1.9, 7.2, 30.2),
                     delta_Pr_MFX = rep(0, times = 3), 
                     delta_Pr = rep(0, times = 3), 
                     delta_Pr_next = rep(0, times = 3))

# Loop over all physician types
for (k in 1:3) {
  # Extract data for type k
  detail_k_DF = subset(detail_DF, type == k)
  
  # Run the marginal effects package on teh 
  mfx_k <- logitmfx(choice ~ detstock, data = detail_k_DF, atmean = FALSE)
  # Extract the marginal effects extimates from the mfx output
  type_DF[k, "delta_Pr_MFX"] = mfx_k$mfxest[1]
  
  # Run logistic regression on data for type k
  fit_type_k = glm(choice ~ detstock, family = binomial(link = "logit"), data = detail_k_DF)
  
  # Exact effect on prescription probability from a one unit increase in detailing
  # Duplicate the dataset
  detail_k_new_DF = detail_k_DF
  # Add 1 to the detail stock for each observation
  detail_k_new_DF$detstock = detail_k_DF$detstock + 1
  # Predict prescription probabilities for the original data
  Pr = predict(fit_type_k, type = "response")
  # Predict prescription probabilities for the data with stock increased by 1 
  Pr_1 = predict(fit_type_k, newdata = detail_k_new_DF, type = "response")
  # Mean exact effect on prescription probability
  type_DF[k, "delta_Pr"] = mean(Pr_1 - Pr)
  # Exact effect of a one unit increase in detailing on the prescription probability in the next period
  # Set the stock in the new data frame to be the original stock plus delta
  detail_k_new_DF$detstock = detail_k_DF$detstock + delta_min
  # Predict prescription probabilities with a delta increase on detail stock
  Pr_1_next = predict(fit_type_k, newdata = detail_k_new_DF, type = "response")
  # Calcualte the mean increase in probability
  type_DF[k, "delta_Pr_next"] = mean(Pr_1_next - Pr)
}

# Calculate ROI for the marginal effects estimate
type_DF$ROI_MFX <- (type_DF$delta_Pr_MFX * (1+delta_min) * type_DF$visits * 100 - 60) / 60

# Calculate ROI for the effects using the average prediction
type_DF$ROI_Pred <- ( (type_DF$delta_Pr + type_DF$delta_Pr_next) * type_DF$visits * 100 - 60) / 60

# ROI is only positive for type 3.  Focus efforts on those doctors.