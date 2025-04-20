###Binomial distribution###Example: Simulating the probability of getting heads in a coin toss.
# Simulate 1000 trials of flipping a fair coin 10 times (p=0.5)
set.seed(123)  # For reproducibility
binom_data <- rbinom(1000, size = 10, prob = 0.5)

# Visualizing the distribution
hist(binom_data, breaks = 10, col = "blue", main = "Binomial Distribution (n=10, p=0.5)",
     xlab = "Number of Heads", ylab = "Frequency")
#Dataset Insight: It shows how many times we get heads in 10 coin tosses across 1000 simulations.

###2. Poisson Distribution in R
#Example: Simulating the number of customer arrivals at a bank per hour.
# Simulating 1000 observations with average rate (lambda) of 5 arrivals per hour
set.seed(123)
poisson_data <- rpois(1000, lambda = 5)

# Visualizing the distribution
hist(poisson_data, breaks = 20, col = "red", main = "Poisson Distribution (λ=5)",
     xlab = "Number of Arrivals", ylab = "Frequency")
#Dataset Insight: Most hours have around 5 arrivals, with fewer extreme cases.

###3. Normal Distribution in R
#Example: Simulating the heights of 1000 people
# Generating normal data (mean=175 cm, sd=7 cm)
set.seed(123)
normal_data <- rnorm(1000, mean = 175, sd = 7)

# Visualizing the distribution
hist(normal_data, breaks = 30, col = "green", main = "Normal Distribution (Mean=175, SD=7)",
     xlab = "Height (cm)", ylab = "Frequency")

# Checking summary statistics
summary(normal_data)
## Dataset Insight: The histogram will show a bell curve, centered around 175 cm.

###4. Exponential Distribution in R
#Example: Simulating waiting time for the next bus.
# Simulating waiting times (mean waiting time of 10 minutes)
set.seed(123)
exp_data <- rexp(1000, rate = 1/10)

# Visualizing the distribution
hist(exp_data, breaks = 30, col = "purple", main = "Exponential Distribution (λ=1/10)",
     xlab = "Waiting Time (Minutes)", ylab = "Frequency")
##Dataset Insight: Most buses arrive within shorter waiting times, but some take longer.


##5. Uniform Distribution in R
#Example: Simulating random numbers between 1 and 100
# Generating 1000 random values from a uniform distribution between 1 and 100
set.seed(123)
uniform_data <- runif(1000, min = 1, max = 100)

# Visualizing the distribution
hist(uniform_data, breaks = 30, col = "orange", main = "Uniform Distribution (1-100)",
     xlab = "Value", ylab = "Frequency")
#Dataset Insight: All numbers between 1 and 100 are equally likely.

###6. Chi-Square Distribution in R
#Example: Simulating test statistics from chi-square distribution.
# Simulating 1000 chi-square values with df=4
set.seed(123)
chi_data <- rchisq(1000, df = 4)

# Visualizing the distribution
hist(chi_data, breaks = 30, col = "cyan", main = "Chi-Square Distribution (df=4)",
     xlab = "Chi-Square Value", ylab = "Frequency")

##Dataset Insight: Used in statistical tests like goodness-of-fit and independence tests.


###7. t-Distribution in R
#Example: Simulating a small-sample t-distribution.
# Simulating 1000 values from t-distribution with df=10
set.seed(123)
t_data <- rt(1000, df = 10)

# Visualizing the distribution
hist(t_data, breaks = 30, col = "pink", main = "t-Distribution (df=10)",
     xlab = "t-Value", ylab = "Frequency")
###Dataset Insight: Looks like a normal distribution but with heavier tails (useful for small sample sizes).




#####chi square distribution with iris dataset
#A Chi-Square test can be used to check if there is an association between categorical variables.

#For example:
  
 # We can check if the species of the flower is independent of the sepal length category (short, medium, long).
#The null hypothesis (H₀) states that species and sepal length category are independent.
#The alternative hypothesis (H₁) states that species and sepal length category are dependent.


# Load the dataset
data(iris)

# View the first few rows
head(iris)
#Since Chi-Square works with categorical data, we need to categorize Sepal.Length into bins (Short, Medium, Long).
# Convert Sepal.Length into categorical bins
iris$Sepal_Category <- cut(iris$Sepal.Length,
                           breaks = quantile(iris$Sepal.Length, probs = c(0, 0.33, 0.67, 1)),
                           labels = c("Short", "Medium", "Long"),
                           include.lowest = TRUE)

# View the new column
table(iris$Sepal_Category)
##A contingency table is needed to check how Species and Sepal_Category relate.
# Create a contingency table
contingency_table <- table(iris$Species, iris$Sepal_Category)

# Print the table
print(contingency_table)   # This table shows the frequency of each Species for each Sepal Length category.

##Now, we apply the Chi-Square Test for Independence to check if Sepal length and Species are related.
### Apply the Chi-Square test
chi_test <- chisq.test(contingency_table)

# Print the test result
print(chi_test)
##Chi-Square Statistic (χ²): Measures how different the observed data is from expected if they were independent.
##Degrees of Freedom (df): Calculated as  =(rows-1) * (columns-1)
##p-value: If p < 0.05, we reject the null hypothesis (significant relationship); otherwise, we fail to reject (no evidence of relationship).


##We can visualize the contingency table using a mosaic plot.
# Install vcd package if not installed
if (!require(vcd)) install.packages("vcd", dependencies=TRUE)
library(vcd)

# Plot mosaic plot
mosaicplot(contingency_table, col = c("red", "blue", "green"),
           main = "Mosaic Plot of Sepal Length Category vs Species")
##conclusion from this test:
#If p < 0.05: Sepal length category and species are dependent, meaning certain species tend to have specific sepal length categories.
#If p > 0.05: No significant relationship, meaning species do not strongly determine sepal length category.

