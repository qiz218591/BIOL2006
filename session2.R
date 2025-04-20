# Load dataset 
data(iris)  ##150 observations of three species of flowers (setosa, versicolor, and virginica) with four features: 
#sepal.length # sepal width # petal length ## petal width

# View first few rows
head(iris)

# Summary statistics
summary(iris)

#Let's test if there is a significant difference in the Sepal.Length between setosa and versicolor species.

#H₀ (Null Hypothesis): No difference in mean Sepal.Length between setosa and versicolor.
#H₁ (Alternative Hypothesis): A difference exists in mean Sepal.Length between setosa and versicolor.

#Data preprocessing ##We extract data only for setosa and versicolor species
# Subset data for two species
iris_subset <- subset(iris, Species %in% c("setosa", "versicolor"))

# Check the structure
str(iris_subset)

#4. Check Assumptions for t-test
#Before performing a t-test, we check:
  
  #Normality (using Shapiro-Wilk test)
#Equal Variance (using F-test)

# Check normality
shapiro.test(iris_subset$Sepal.Length[iris_subset$Species == "setosa"])
shapiro.test(iris_subset$Sepal.Length[iris_subset$Species == "versicolor"])

# Check equal variance using F-test
var.test(Sepal.Length ~ Species, data = iris_subset)

#If p-value > 0.05, normality assumption is met.
#If p-value > 0.05, equal variance assumption is met 
#if equal variance then we reject null hypothesis
#p-value < 0.05 in F-test → Strong evidence that variances are significantly different.
#Equal variance assumption is violated → Use Welch’s t-test instead of a standard t-test.

#5. Perform t-test
#Based on assumption checks, we run a t-test.
# Perform two-sample t-test
t.test(Sepal.Length ~ Species, data = iris_subset, var.equal = FALSE)

#pvalue-  Probability of obtaining this result if H₀ is true.
#If p < 0.05, we reject H₀ and conclude a significant difference

##Welch’s t-test gives a more reliable result than the standard t-test when variances are unequal


#6. Calculate Degrees of Freedom (df)
#For a two-sample t-test:
n1 <- sum(iris_subset$Species == "setosa")
n2 <- sum(iris_subset$Species == "versicolor")
df <- (n1 + n2 - 2)
df
#7. Critical Value & Rejection Zone
#Using qt(), we find the critical t-value at α = 0.05 (two-tailed).

# Critical t-value
alpha <- 0.05
t_critical <- qt(1 - alpha/2, df)
t_critical

#Rejection Region: If t-statistic > ± t_critical, reject H₀

#8. Type I & Type II Errors
#Type I Error (α): Rejecting H₀ when it's true (false positive).
#Type II Error (β): Failing to reject H₀ when it's false (false negative).
# Type I Error probability (fixed at 0.05)
alpha <- 0.05

# Type II Error & Power (using power.t.test)
power.t.test(n = min(n1, n2), delta = mean(iris_subset$Sepal.Length[iris_subset$Species == "setosa"]) - mean(iris_subset$Sepal.Length[iris_subset$Species == "versicolor"]),
             sd = sd(iris_subset$Sepal.Length), sig.level = alpha, type = "two.sample", alternative = "two.sided")

#Power (1 − β): Probability of correctly rejecting false H₀.
#Higher power (≥0.8) is preferred. If too low, we increase sample size.


#9. Visualizing Hypothesis Testing
#To visualize the distribution of Sepal.Length, use boxplots and density plots.
# Boxplot
boxplot(Sepal.Length ~ Species, data = iris_subset, col = c("lightblue", "lightgreen"),
        main = "Sepal Length Comparison", ylab = "Sepal Length")

# Density plot
library(ggplot2)
ggplot(iris_subset, aes(x = Sepal.Length, fill = Species)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Density Plot of Sepal Length")



###vole population analysis
# Set random seed for reproducibility
set.seed(123)

# Define parameters
n_island <-50 # Sample size for island population
n_mainland <-50 # Sample size for mainland population
island_mean <-3.75 # Mean dorsal size (cm)
mainland_mean <-3.57 # Mean dorsal size (cm)
common_sd <-0.5 # Common standard deviation

# Generate data
island_voles <-rnorm(n_island, mean = island_mean, sd = common_sd)
mainland_voles <-rnorm(n_mainland, mean = mainland_mean, sd =
                            common_sd)

# Create data frame

vole_data <-data.frame(
  dorsal_size = c(island_voles, mainland_voles),
  population = factor(c(rep("Island", n_island),
                           rep("Mainland", n_mainland)))
  )

# Check normality
shapiro.test(vole_data$dorsal_size[vole_data$population == "Island"])
shapiro.test(vole_data$dorsal_size[vole_data$population == "Mainland"])

# Check equal variance using F-test
var.test(dorsal_size ~ population, data = vole_data)

#for both p values >0.05 means Ho rejected.

# Perform t-test
t_result <-t.test(dorsal_size ~ population,
                     data = vole_data,
                     var.equal = TRUE)
# Print results
print(t_result)
# Calculate effect size
effect_size <-(mean(island_voles)-mean(mainland_voles)) /
  common_sd
print(paste("Cohen’s d =", round(effect_size, 3)))


