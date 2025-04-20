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

