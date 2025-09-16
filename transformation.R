# Simulate skewed data (like income)
set.seed(123)
income <- rexp(1000, rate = 0.1)  # Exponential distribution (right skew)

# Plot original
par(mfrow=c(2,3))  # layout for multiple plots
hist(income, main="Original (Skewed)", col="skyblue")

# Log transformation
hist(log(income), main="Log Transformation", col="skyblue")

# Square root transformation
hist(sqrt(income), main="Square Root Transformation", col="skyblue")

# Reciprocal transformation
hist(1/income, main="Reciprocal Transformation", col="skyblue")

# Box-Cox transformation (requires MASS package)
library(MASS)
bc <- boxcox(lm(income ~ 1), lambda = seq(-2,2,0.1))
title("Box-Cox Transformation")



###################################################################
# Load dataset
data("airquality")
head(airquality)

# Take the Ozone column (remove missing values)
ozone <- na.omit(airquality$Ozone)

# Plot original
par(mfrow=c(3,3))  # layout 3x3 grid
hist(ozone, main="Original Ozone", col="skyblue")
hist(log(ozone), main="Log Transformation", col="skyblue")
hist(sqrt(ozone), main="Square Root Transformation", col="skyblue")
hist(1/ozone, main="Reciprocal Transformation", col="skyblue")
library(MASS)

# Fit a simple linear model (Ozone ~ 1) for Box-Cox
bc <- boxcox(lm(ozone ~ 1), lambda=seq(-2,2,0.1))
# Example: proportion of days with Ozone > 100
prop_days <- airquality$Ozone / max(airquality$Ozone, na.rm=TRUE)
prop_days <- na.omit(prop_days)

hist(asin(sqrt(prop_days)), main="Arcsine Transformation", col="skyblue")


z_ozone <- scale(ozone)  # (x - mean)/sd
hist(z_ozone, main="Z-score Transformation", col="skyblue")
rank_ozone <- rank(ozone)
hist(rank_ozone, main="Rank Transformation", col="skyblue")

