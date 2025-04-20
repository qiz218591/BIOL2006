set.seed(123)  # For reproducibility
sample_heights <- rnorm(50, mean = 170, sd = 10)  # Generate sample heights
mean(sample_heights)  # Estimate population mean

#point estimaiton
sample_mean <- mean(sample_heights)  # Sample mean
sample_var <- var(sample_heights)  # Sample variance
sample_sd <- sd(sample_heights)  # Sample standard deviation

cat("Sample Mean:", sample_mean, "\nSample Variance:", sample_var, "\nSample SD:", sample_sd)

#interval estimation
n <- length(sample_heights)
alpha <- 0.05  # 95% confidence level
t_value <- qt(1 - alpha/2, df = n-1)  # critical t-score for 95% CI
se <- sample_sd / sqrt(n)  # Standard error

lower_bound <- sample_mean - t_value * se
upper_bound <- sample_mean + t_value * se

cat("95% Confidence Interval: [", lower_bound, ",", upper_bound, "]")

#unbiasedness
pop_heights <- rnorm(10000, mean = 170, sd = 10)  # Large population
sample_means <- replicate(1000, mean(sample(pop_heights, 50)))  # Repeated samples ## Take 1000 samples of size 50
mean(sample_means)  # Should be close to 170  #the sample mean is an unbiased estimator.

#consistency
small_sample <- mean(sample(pop_heights, 1))   # Small sample (n=1)
large_sample <- mean(sample(pop_heights, 500))   # Large sample (n=500)

cat("Small Sample Mean:", small_sample, "\nLarge Sample Mean:", large_sample)  #The mean of the small sample will fluctuate more.#The mean of the large sample will be closer to 170, showing that larger samples lead to more accurate estimates.

##effeciency
var_small <- var(replicate(1000, mean(sample(pop_heights, 10))))   ## Small sample (n=10)
var_large <- var(replicate(1000, mean(sample(pop_heights, 500))))   ### Large sample (n=500)

cat("Variance of small sample mean:", var_small, "\nVariance of large sample mean:", var_large)   #The variance for the small sample will be higher than that of the large sample.

###sample distribution
sample_means <- replicate(1000, mean(sample(pop_heights, 50)))
hist(sample_means, breaks = 30, main = "Sampling Distribution of Mean", col = "blue")

alpha <- 0.01  # 99% confidence level
t_value <- qt(1 - alpha/2, df = n-1)
lower_bound <- sample_mean - t_value * se
upper_bound <- sample_mean + t_value * se

cat("99% Confidence Interval: [", lower_bound, ",", upper_bound, "]")

library(MASS)  ##for maximum likelihood estimation
fit <- fitdistr(sample_heights, "normal")  ##fit a normal distribution to the data
print(fit)   ##print the estimated parameters


se <- sd(sample_heights) / sqrt(n)
cat("Standard Error:", se)

sample_small <- mean(sample(pop_heights, 10))
sample_large <- mean(sample(pop_heights, 500))

cat("Small Sample Bias:", abs(sample_small - mean(pop_heights)), 
    "\nLarge Sample Bias:", abs(sample_large - mean(pop_heights)))

##Lesson1 ##university of queensland
##coin flip   ## you can take an example of num_heads as 40, 30 etc..
##PBL1
# Simulate flipping a fair coin 50 times
define_fip <- function(n_flips = 50, prob_heads = 0.5) {
  flips <- sample(c("Heads", "Tails"), size = n_flips, replace = TRUE, prob = c(prob_heads, 1 - prob_heads))
  num_heads <- sum(flips == "Heads")
  return(num_heads)
}

# Run the simulation
set.seed(123)  # For reproducibility
num_heads <- define_fip()

# Define expected range based on normal approximation
expected_mean <- 50 * 0.5  # 25
expected_sd <- sqrt(50 * 0.5 * 0.5)  # ~3.54
lower_bound <- expected_mean - 2 * expected_sd  # ~18
upper_bound <- expected_mean + 2 * expected_sd  # ~32

# Output results
cat("Number of heads:", num_heads, "\n")
if (num_heads < lower_bound || num_heads > upper_bound) {
  cat("The result is outside the expected range (", lower_bound, ",", upper_bound, "). The coin may be biased.\n")
} else {
  cat("The result is within the expected range. The coin appears fair.\n")
}

# 1. Binomial Probability: Calculate P(X = num_heads) under a fair coin assumption
binom_prob <- dbinom(num_heads, size = 50, prob = 0.5)
cat("Binomial Probability of exactly", num_heads, "heads:", binom_prob, "\n")

# 2. Confidence Interval for proportion of heads
prop_heads <- num_heads / 50    
se <- sqrt((prop_heads * (1 - prop_heads)) / 50)  # Standard error
conf_int <- prop_heads + c(-1.96, 1.96) * se  # 95% Confidence Interval
cat("95% Confidence Interval for proportion of heads:", conf_int[1], "to", conf_int[2], "\n")

# 3. Hypothesis Test: Binomial test for fairness
binom_test <- binom.test(num_heads, 50, p = 0.5, alternative = "two.sided")
cat("Hypothesis Test p-value:", binom_test$p.value, "\n")
if (binom_test$p.value < 0.05) {
  cat("The p-value is less than 0.05, suggesting the coin may be biased.\n")
} else {
  cat("The p-value is greater than 0.05, suggesting no strong evidence of bias.\n")
}


###Exercise B: Dice Simulation
set.seed(101)  # Ensures reproducibility
n <- 30  # Number of rolls
rolls <- sample(1:6, n, replace = TRUE)  # Simulate rolling a fair die 30 times
table(rolls)  # Count occurrences of each face


die1 <-sample(1:6, n, replace=TRUE)
die2 <-sample(1:6, n, replace=TRUE)
sums <-die1 + die2
table(sums)

##PBL2

demonstrate_clt <- function(n_samples=500, sample_size=200){
sample_means <- replicate(n_samples, mean(rnorm(sample_size))
)
hist(sample_means, probability=TRUE,
     main=paste("CLT Demo: n=", sample_size),
     xlab="Sample Mean")
}
demonstrate_clt()





#######normal and discrete distribution
# Load necessary library
library(ggplot2)

# Set seed for reproducibility
set.seed(101)

# Generate data for Discrete Uniform Distribution (rolling a fair 6-sided die)
n_uniform <- 10000  # Number of simulations
uniform_data <- sample(1:6, n_uniform, replace = TRUE)  # Simulating dice rolls

# Generate data for Normal Distribution
n_normal <- 10000
normal_data <- rnorm(n_normal, mean = 3.5, sd = 1.5)  # Mean 3.5, SD 1.5 to match dice range

# Create data frames for plotting
df_uniform <- data.frame(Value = uniform_data, Distribution = "Discrete Uniform (Die Rolls)")
df_normal <- data.frame(Value = normal_data, Distribution = "Normal Distribution")

# Combine data
df_combined <- rbind(df_uniform, df_normal)

# Plot Discrete Uniform vs. Normal Distribution
ggplot() +
  # Histogram for discrete uniform distribution (die rolls)
  geom_histogram(data = df_uniform, aes(x = Value, y = ..density.., fill = Distribution), 
                 bins = 6, color = "black", alpha = 0.5, position = "identity") +
  # Density plot for normal distribution
  geom_density(data = df_normal, aes(x = Value, y = ..density.., color = Distribution), 
               adjust = 1.2, size = 1.2) +  
  labs(title = "Comparison of Discrete Uniform and Normal Distributions",
       x = "Value",
       y = "Density") +
  theme_minimal() +
  scale_fill_manual(values = c("Discrete Uniform (Die Rolls)" = "blue")) +
  scale_color_manual(values = c("Normal Distribution" = "red"))


##to test the CLT###
# Load necessary library
install.packages("ggplot2")
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Generate a non-normal population (Exponential Distribution)
population <- rexp(10000, rate = 1)  # Highly skewed

# Function to test CLT by taking many samples
test_clt <- function(sample_size, n_samples = 1000) {
  sample_means <- replicate(n_samples, mean(sample(population, sample_size, replace = TRUE)))
  
  # Plot histogram of sample means
  ggplot(data.frame(means = sample_means), aes(x = means)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
    geom_density(color = "red", size = 1) +  # Overlay density curve
    ggtitle(paste("Sample Size:", sample_size)) +
    xlab("Sample Mean") + ylab("Density") +
    theme_minimal()
}

# Compare CLT for different sample sizes
test_clt(5)    # Small sample, not normal
test_clt(30)   # Larger sample, closer to normal
test_clt(100)  # Even larger sample, almost perfectly normal


##a) data transformation- used when data is highly skewed, and helps meets normality assumption
# Original data (right-skewed)
skewed_data <- rexp(1000, rate = 1)

# Apply log transformation
log_data <- log(skewed_data)

# Plot before and after transformation
par(mfrow = c(1, 2))  # Split plot window
hist(skewed_data, main = "Original Data", col = "red")
hist(log_data, main = "Log Transformed Data", col = "blue")
##this making CLT more likely to hold


#b) bootstrapping  (resampling to approximate CLT)
##instead of assuming normality, we resample many times to estimate the distribution
# Load library
library(boot)

# Function to compute mean
mean_func <- function(data, indices) {
  return(mean(data[indices]))
}

# Apply bootstrapping
boot_results <- boot(skewed_data, mean_func, R = 1000)

# Plot bootstrap distribution
hist(boot_results$t, main = "Bootstrap Distribution of the Mean", col = "purple")
##bootstrapping avoids parametric assumptions and holds approximate CLT

#c) mixed-effects models (for non-independent data) ## used when datapoints are correlated (eg repeated measures)
##Instead of assuming independece, it models variation within groups
# Load library
library(lme4)

# Simulated dataset
data <- data.frame(
  patient = rep(1:10, each = 5),  # 10 patients, 5 measurements each
  response = rnorm(50, mean = 5, sd = 1) + rep(rnorm(10, mean = 0, sd = 1), each = 5)
)

# Fit a mixed-effects model
model <- lmer(response ~ 1 + (1 | patient), data = data)

# Display results
summary(model)

##What to do if data is not normal?
#1) check normality first!
#Before deciding what to do, check if your data is actually  non-normal using:
# Load data (Example: Skewed Exponential Distribution)
set.seed(123)
data <- rexp(1000, rate = 1)

# Histogram
hist(data, main = "Histogram of Data", col = "blue")

# QQ Plot (Check Normality)
qqnorm(data)
qqline(data, col = "red")

# Shapiro-Wilk Test (p < 0.05 means data is NOT normal)
shapiro.test(data)
## If the Shapiro-Wilk p-value < 0.05, data is significantly non-normal.

##2) Apply data transformation!
#If data is skewed, apply log, square root, or Box-Cox transformations to make it normal.
# Log transformation
log_data <- log(data)

# Square root transformation
sqrt_data <- sqrt(data)

# Histogram after transformation
hist(log_data, main = "Log Transformed Data", col = "red")

#3) Use Non-Parametric Tests (If Transformation Fails)! #(do not assume normality)
#Parametric Test	#Non-Parametric Alternative	#When to Use?
  #t-test (mean comparison)	#Wilcoxon Rank-Sum Test	#Data is skewed or has outliers
#ANOVA (group comparison)	#Kruskal-Wallis Test	#Data is non-normal
#Pearson Correlation	#Spearman Rank Correlation	#Data is not linear
#Linear Regression	#Generalized Linear Model (GLM)	#Errors are non-normal

#Example: Wilcoxon Rank-Sum Test (Instead of t-test)
# Generate two non-normal groups
group1 <- rexp(30, rate = 1)
group2 <- rexp(30, rate = 1.2)

# Perform Wilcoxon Test
wilcox.test(group1, group2)

#4) Use Bootstrapping (If Sample Size is Small)
#If you cannot assume normality, use bootstrapping to estimate the sampling distribution.

# Load boot library
library(boot)

# Function to calculate mean
mean_func <- function(data, indices) {
  return(mean(data[indices]))
}

# Apply bootstrapping
boot_results <- boot(data, mean_func, R = 1000)

# Plot bootstrap distribution
hist(boot_results$t, main = "Bootstrap Distribution of the Mean", col = "purple")


###PBL3
set.seed(202)  # Ensure reproducibility
n <- 30  # Sample size
leaves <- rnorm(n, mean = 10, sd = 2)  # Simulate leaf sizes
mean(leaves)  # Calculate sample mean
sd(leaves)  # Calculate sample standard deviation

##CI
se <- sd(leaves)/sqrt(n)  ##standard error of the mean
ci <- mean(leaves) + c(-1,1)*qt(0.975, df=n-1)*se   ##95% CI using t-distribution

#non-normal distributions (challenging)
set.seed(202)
leaves_skewed <- rexp(n, rate = 0.2)  # Skewed (Exponential) data
hist(leaves_skewed, main = "Histogram of Skewed Leaf Sizes", col = "lightblue")

se_skewed <- sd(leaves_skewed) / sqrt(n)
ci_skewed <- mean(leaves_skewed) + c(-1, 1) * qt(0.975, df = n-1) * se_skewed
ci_skewed  # Print confidence interval
##use bootstrapping CI for non-normal data
library(boot)  # Load bootstrapping package

# Function to compute the sample mean
mean_func <- function(data, indices) {
  return(mean(data[indices]))
}

# Bootstrap resampling
boot_results <- boot(leaves_skewed, mean_func, R = 1000)

# Bootstrap confidence interval
boot.ci(boot_results, type = "perc")  # Percentile method CI

