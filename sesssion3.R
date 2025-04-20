##fit a linear model in statistics ##interpreting the statisitcal outputs ##investigating the model assumptions
##example: investigating the relationship between environmental temperature and body size
## Hypothesis: Larger body size is observed in colder climates (Bergmann's rule)

# Load required libraries
library(ggplot2)
library(car)  # For checking model assumptions
data(iris)
# Simulated dataset: Temperature (°C) vs. Body Size (grams)
data <- data.frame(
  Temperature = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50),
  Body_Size = c(500, 450, 420, 390, 360, 340, 320, 310, 290, 270)
)

# View dataset
print(data)
ggplot(data, aes(x = Temperature, y = Body_Size)) +
  geom_point(size = 3, color = "blue") +
  labs(title = "Temperature vs. Body Size",
       x = "Temperature (°C)", y = "Body Size (grams)") +
  theme_minimal()
# Fit the model
model <- lm(Body_Size ~ Temperature, data = data)

# Print the model summary
summary(model)

#The summary(model) output includes:
#Intercept [𝛽0]: expected body size when temperature = 0°C.
#Slope (𝛽1): Change in body size per 1°C increase in temperature.
#R-squared: Proportion of variance in body size explained by temperature.
#p-value: Whether the predictor (Temperature) significantly affects body size.

#Interpretation
#The slope (-4.8) suggests that for every 1°C increase in temperature, body size decreases by 4.8 grams.
#The p-value < 0.001 indicates this effect is statistically significant.
#The R-squared = 0.92 suggests 92% of variation in body size is explained by temperature.

##checking the model assumptions# (residual diagnostics)
#Linear regression assumes: Linearity – Relationship between X and Y is linear.
#Independence – Residuals are independent.
#Normality – Residuals should be normally distributed.
#Homoscedasticity – Residual variance should be constant.

#linearity check
ggplot(data, aes(x = Temperature, y = Body_Size)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_minimal()

#If a curve appears, consider transformations (log, sqrt).

#normality check (QQ Plot)
qqnorm(resid(model))
qqline(resid(model), col = "red")
#Residuals should fall close to the line.

# Homoscedasticity (Equal Variance)
plot(model, which = 3)  # Scale-Location plot
#Points should be randomly scattered; no clear pattern.


#checking for outliers
influencePlot(model)
#Identifies influential data points affecting regression.


# Linking to Evolutionary Hypotheses
#Using this regression model, we can support Bergmann’s Rule by showing a negative relationship between temperature and body size.
#Further research could:Incorporate additional predictors (e.g., humidity, altitude).
#Use phylogenetic corrections if working with related species.
#Test interaction effects (e.g., does climate affect body size differently in different taxa?).


##ANOVA is used to compare the means of two or more groups to determine if there is a significant difference between them.
#One way ANOVA
#example: We compare gene expression levels across three different treatments.

set.seed(123)
data <- data.frame(
  Treatment = rep(c("Control", "DrugA", "DrugB"), each = 10),
  Gene_Expression = c(rnorm(10, mean = 50, sd = 5), 
                      rnorm(10, mean = 55, sd = 5),
                      rnorm(10, mean = 60, sd = 5))
)

head(data)

anova_model <- aov(Gene_Expression ~ Treatment, data = data)
summary(anova_model)
#If the p-value < 0.05, at least one group mean is significantly different.

#post hoc analysis (tukeys test)
#If ANOVA is significant, we check which groups differ.
TukeyHSD(anova_model)
#If p < 0.05, the two groups have a statistically significant difference.



#two way anova: interaction effects
#We test Gene Expression affected by both Treatment and Time.

set.seed(123)
data2 <- data.frame(
  Treatment = rep(c("Control", "DrugA", "DrugB"), each = 10),
  Time = rep(c("Day1", "Day2"), each = 5, times = 3),
  Gene_Expression = c(rnorm(5, 50, 5), rnorm(5, 52, 5),
                      rnorm(5, 55, 5), rnorm(5, 57, 5),
                      rnorm(5, 60, 5), rnorm(5, 62, 5))
)

# Convert categorical variables to factors
data2$Treatment <- as.factor(data2$Treatment)
data2$Time <- as.factor(data2$Time)

head(data2)
anova_model2 <- aov(Gene_Expression ~ Treatment * Time, data = data2)
summary(anova_model2)
##The interaction term (Treatment:Time) tells us if the effect of Treatment depends on Time.
#If p < 0.05, interaction is significant.

##checking assumptions
#Before interpreting ANOVA results, check assumptions:
#normality of residuals
shapiro.test(residuals(anova_model2))
qqnorm(residuals(anova_model2))
qqline(residuals(anova_model2), col = "red")
#p > 0.05 → Normality holds.
#p < 0.05 → Consider transformations (e.g., log(Gene_Expression)).
#Homogeneity of Variance (Levene’s Test)
library(car)
leveneTest(Gene_Expression ~ Treatment * Time, data = data2)
#p > 0.05 → Variances are equal.
#p < 0.05 → Consider Welch’s ANOVA (handles unequal variance).

##welch's anova
set.seed(123)
data_welch <- data.frame(
  Treatment = rep(c("Control", "DrugA", "DrugB"), each = 10),
  Gene_Expression = c(rnorm(10, mean = 50, sd = 5), 
                      rnorm(10, mean = 55, sd = 10),  # Larger variance
                      rnorm(10, mean = 60, sd = 15))  # Even larger variance
)

head(data_welch)
library(car)
#Check Variance Assumption (Levene’s Test)
leveneTest(Gene_Expression ~ Treatment, data = data_welch)
#If p < 0.05, variances are unequal, so Welch’s ANOVA is needed.

#Perform Welch’s ANOVA
oneway.test(Gene_Expression ~ Treatment, data = data_welch, var.equal = FALSE)
#If p < 0.05, at least one group is significantly different.
#Post-hoc Test: Games-Howell
#Since Tukey’s test assumes equal variance, we use Games-Howell instead.
library(rstatix)
games_howell_test(data_welch, Gene_Expression ~ Treatment)
#This tells us which specific groups are significantly different.



### Multiple Linear Regression
#Example: Predicting Gene Expression Levels
#In biological studies, gene expression (𝑌) may be influenced by multiple factors ((𝑋1,X2,𝑋3). For instance, let's examine how temperature, pH, and nutrient concentration affect gene expression levels.
#
# Load necessary libraries
library(ggplot2)
library(car)

# Create a dataset with 30 samples
set.seed(42)
data <- data.frame(
  Temperature = runif(30, min = 10, max = 40),  # Temperature in °C
  pH = runif(30, min = 6, max = 8),  # pH values
  Nutrient = runif(30, min = 10, max = 50),  # Nutrient concentration
  Gene_Expression = 5 + 2 * runif(30, min = 10, max = 40) - 3 * runif(30, min = 6, max = 8) + 0.5 * runif(30, min = 10, max = 50) + rnorm(30, 0, 5) # Random noise
)

# View the dataset
print(head(data))
# Fit the multiple regression model
model <- lm(Gene_Expression ~ Temperature + pH + Nutrient, data = data)

# Model summary
summary(model)

#Key Interpretation
#Each coefficient represents the change in gene expression per unit increase in the variable, keeping others constant.
#Significant predictors (low p-values) influence gene expression significantly.
#Adjusted R² tells us how much variation is explained by all predictors

#check the model assumption
# Checking residuals
par(mfrow=c(2,2))  # Arrange plots
plot(model)

#interpretation
#If residuals are randomly scattered, assumptions are met.
#If a clear pattern appears, consider transformations


#1. Checking for Linearity
#Why? Multiple regression assumes a linear relationship between predictors and the response variable. If the relationship is non-linear, transformation (e.g., log, sqrt) may be needed.
# Scatter plots of predictors vs. response
par(mfrow=c(1,3))
plot(data$Temperature, data$Gene_Expression, main="Gene Exp vs. Temperature", xlab="Temperature", ylab="Gene Expression")
plot(data$pH, data$Gene_Expression, main="Gene Exp vs. pH", xlab="pH", ylab="Gene Expression")
plot(data$Nutrient, data$Gene_Expression, main="Gene Exp vs. Nutrient", xlab="Nutrient", ylab="Gene Expression")

#interpretation
#If points follow a curved pattern, transformation may help.
#If spread is random, a transformation may not be necessary.

#2. Checking for Normality of Residuals
#Why? The residuals should be normally distributed.
#If they are skewed, transformations like log(Y), sqrt(Y), or Box-Cox may be needed.
qqnorm(resid(model))
qqline(resid(model), col = "red")  # Should be roughly straight
hist(resid(model), main="Histogram of Residuals", xlab="Residuals", col="lightblue", breaks=10)
shapiro.test(resid(model))
#p > 0.05 → Residuals are normal (no transformation needed).
#p < 0.05 → Residuals deviate from normality (consider transformations).

#3. Checking for Homoscedasticity (Equal Variance)
#Why? If residual variance is not constant, transformation can help stabilize it.
#Residual vs. Fitted Plot:
plot(model, which=3)  # Scale-Location plot
#If the spread widens or narrows, log(Y) or sqrt(Y) transformation can help.

#Breusch-Pagan Test:
library(lmtest)
bptest(model)
#p > 0.05 → No heteroscedasticity (equal variance).
#p < 0.05 → Unequal variance (transformation needed

#. Checking for Multicollinearity
#Why? High correlation between predictors can affect model interpretation.
#vairance inflation factor (VIF)
library(car)
vif(model)
#VIF < 5 → No multicollinearity, transformation not needed.
#VIF > 10 → Strong multicollinearity, consider removing/restructuring variables.

#Should We Apply a Transformation?
#If normality is violated → Try log(Gene_Expression), sqrt(Gene_Expression), or Box-Cox transformation.
#If residual variance is unequal → Apply log(Y), sqrt(Y), or power transformations.
#If relationship is non-linear → Try transformations on predictors (log(Temperature), sqrt(Nutrient), etc.).
#If multicollinearity is high → Remove highly correlated predictors or use Principal Component Regression (PCR).


##example: log transformation
data$Gene_Expression_log <- log(data$Gene_Expression)

# Fit the transformed model
model_log <- lm(Gene_Expression_log ~ Temperature + pH + Nutrient, data = data)

# Check assumptions again
par(mfrow=c(2,2))
plot(model_log)

#If residuals now appear normal and homoscedastic, log transformation was effective.
#If not, try other transformations (sqrt(), inverse(), etc.)

##Interaction effect in multiple regression
#Why Consider Interaction Effects?
#Sometimes, two variables together influence the response in a way that neither does alone.
#Example: The effect of Temperature on Gene Expression might depend on pH.
# Fit model with interaction effects
model_interaction <- lm(Gene_Expression ~ Temperature * pH + Nutrient, data = data)

# Summary of the model
summary(model_interaction)

# Interpretation of Interaction Terms:
#The coefficient of Temperature:pH tells us how the effect of Temperature on Gene Expression changes with pH.
#If significant (p < 0.05), it means there’s a strong interaction effect.

#library(ggplot2)
ggplot(data, aes(x = Temperature, y = Gene_Expression, color = as.factor(pH))) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(title = "Interaction Between Temperature and pH on Gene Expression")

##If slopes are different for different pH values, interaction is present.
#If lines are parallel, no interaction is needed.

##box-cox transformation for automatic selection
#Why Use Box-Cox?
#It finds the best power transformation (log, sqrt, etc.) automatically.
#Only applicable when Y > 0 (response variable must be positive)
##appplying the box-cox test
library(MASS)
boxcox(model, lambda = seq(-2, 2, 0.1))  # Lambda = 0 suggests log transformation
##The best transformation is at the peak of the curve.
#λ = 0 suggests a log transformation (log(Y)).
#λ = 0.5 suggests a square root transformation (sqrt(Y)).
#λ = 1 means no transformation is needed.

#fiting the model with optimal transformation
data$Gene_Expression_transformed <- log(data$Gene_Expression)  # Example if λ = 0

# Re-fit the model
model_transformed <- lm(Gene_Expression_transformed ~ Temperature + pH + Nutrient, data = data)

# Check assumptions again
par(mfrow=c(2,2))
plot(model_transformed)
#If residuals improve, transformation was effective!


###polynomial regression
#Example: Modeling Enzyme Activity vs. Substrate Concentration
#In enzyme kinetics, activity (Y) often follows a non-linear pattern with substrate concentration (X).We fit a polynomial regression to capture this curved relationship.
# Simulate an enzyme kinetics dataset
set.seed(123)
Substrate_Conc <- seq(0, 100, by = 5)
Enzyme_Activity <- 50 + 2 * Substrate_Conc - 0.05 * Substrate_Conc^2 + rnorm(length(Substrate_Conc), 0, 5)

# Create a dataframe
data_poly <- data.frame(Substrate_Conc, Enzyme_Activity)

# View data
print(head(data_poly))

##fit the polynomial regression model
# Fit a quadratic model (degree = 2)
poly_model <- lm(Enzyme_Activity ~ poly(Substrate_Conc, 2), data = data_poly)

# Model summary
summary(poly_model)
ggplot(data_poly, aes(x = Substrate_Conc, y = Enzyme_Activity)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red") +
  labs(title = "Enzyme Activity vs. Substrate Concentration (Quadratic Model)") +
  theme_minimal()
##Interpretation
#The polynomial term captures the curvature.
#If a higher degree term (cubic, quartic) is needed, increase the polynomial order.



###logistic regression
#Example: Predicting Cancer Cell Response to Drug Treatment
#When the outcome is binary (e.g., responsive vs. non-responsive), we use logistic regression.

# Simulated data: Drug Dose and Cell Response
set.seed(42)
data_logistic <- data.frame(
  Drug_Dose = runif(100, 0, 50),
  Response = as.factor(ifelse(runif(100) < plogis(-2 + 0.15 * runif(100, 0, 50)), "Resistant", "Sensitive"))
)

# View dataset
print(head(data_logistic))

##fit the logistic regression model
# Fit logistic model
log_model <- glm(Response ~ Drug_Dose, data = data_logistic, family = "binomial")

# Model summary
summary(log_model)
#Interpret Coefficients: The log-odds coefficient tells how response probability changes with dose.
#Odds ratio is computed using exp(coef(log_model)).
exp(coef(log_model))  # Convert log-odds to odds ratio
##Predict and plot
# Prediction for new doses
new_data <- data.frame(Drug_Dose = seq(0, 50, length.out = 100))
new_data$Predicted_Prob <- predict(log_model, newdata = new_data, type = "response")

# Plot
ggplot(data_logistic, aes(x = Drug_Dose, y = as.numeric(Response) - 1)) +
  geom_point(alpha = 0.5) +
  geom_line(data = new_data, aes(x = Drug_Dose, y = Predicted_Prob), color = "red") +
  labs(title = "Drug Dose vs. Probability of Cancer Cell Sensitivity") +
  theme_minimal()

##IRIS dataset as an example to explain the concenpts of regression
data(iris)
head(iris)
#Does Sepal.Width predict Sepal.Length?
#linear regression(Sepal.Length vs. Sepal.Width)
#Use if you expect a linear relationship between two continuous variables.
model_lm <- lm(Sepal.Length ~ Sepal.Width, data = iris)
summary(model_lm)

#Do Sepal.Width, Petal.Length, and Petal.Width together predict Sepal.Length?
##Multiple regression (Sepal.Length vs. Multiple Predictors)
#Use when multiple continuous variables influence the response.
model_multi <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = iris)
summary(model_multi)

## Use when residual plots suggest a curved (non-linear) relationship.
#Polynomial Regression (If Relationship is Non-linear)
model_poly <- lm(Sepal.Length ~ poly(Sepal.Width, 2), data = iris)
summary(model_poly)


#Can we classify Species using Sepal and Petal measurements?
##LOGISTIC regression (predicting species)
# Use when predicting a categorical outcome (Species).
install.packages("nnet")   # Only run if not installed
library(nnet)

model_logistic <- multinom(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris)
summary(model_logistic)
exp(coef(model_logistic))  # Convert log-odds to odds ratios

#Each coefficient represents the log-odds of one species compared to the baseline (e.g., setosa).
#The exp(coef(model_logistic)) converts these log-odds to odds ratios.
#If an odds ratio > 1, the predictor increases the likelihood of that species.
#If an odds ratio < 1, the predictor decreases the likelihood

ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point(size = 3) +
  labs(title = "Petal Size and Iris Species Classification") +
  theme_minimal()
#Setosa (Red) is well-separated from the other species.
#Versicolor (Green) and Virginica (Blue) overlap, meaning classification is harder between these two.



#ANOVA (Comparing Sepal.Length Across Species)
#Do different species have significantly different Sepal.Length values?
anova_model <- aov(Sepal.Length ~ Species, data = iris)
summary(anova_model)
#Use when comparing the mean Sepal.Length across multiple species groups.
TukeyHSD(anova_model)
#If p < 0.05, species differ significantly in Sepal.Length.
ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot() +
  labs(title = "Sepal Length Distribution Across Species") +
  theme_minimal()

#If boxes do not overlap, the species have statistically different Sepal.Length values.
#If ANOVA p < 0.05, at least one group has a significant difference








###stepwise model selection (AIC, BIC) for optimal predictor selection
#Why Use Stepwise Selection?
#Helps automate model selection by adding or removing predictors based on AIC (Akaike Information Criterion) or BIC (Bayesian Information Criterion).
#Lower AIC/BIC values indicate better models (while balancing complexity).
#AIC is more flexible (penalizes complexity less), while BIC prefers simpler models.

##fit a full model
#Using our Gene Expression ~ Temperature + pH + Nutrient example:
# Full model with all predictors
full_model <- lm(Gene_Expression ~ Temperature + pH + Nutrient, data = data)
summary(full_model)

#Step 2: Stepwise Selection Using AIC
#Automatic Selection (step() function)
# Stepwise selection using AIC (both directions: forward & backward)
stepwise_model_aic <- step(full_model, direction = "both", trace = TRUE)
summary(stepwise_model_aic)
#direction = "both" allows both forward selection (adding variables) and backward elimination (removing variables).

#Stepwise Selection Using BIC
#BIC penalizes more complex models stronger than AIC.
# Stepwise selection using BIC
stepwise_model_bic <- step(full_model, direction = "both", k = log(nrow(data)), trace = TRUE)
summary(stepwise_model_bic)
##Setting k = log(n) applies BIC instead of AIC

##compare models
AIC(full_model, stepwise_model_aic, stepwise_model_bic)
BIC(full_model, stepwise_model_aic, stepwise_model_bic)
#The model with the lowest AIC or BIC is the best.


