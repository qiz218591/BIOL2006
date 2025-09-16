set.seed(123)

# Null hypothesis: mean = 120
true_mean <- 120  

# Case 1: Null is TRUE (Type I error can happen)
sample1 <- rnorm(30, mean = 120, sd = 10)
t1 <- t.test(sample1, mu = 120)

t1$p.value   # Sometimes < 0.05 by chance (Type I error)

# Case 2: Null is FALSE (Type II error can happen)
# True mean is slightly higher (122 instead of 120)
sample2 <- rnorm(30, mean = 122, sd = 10)
t2 <- t.test(sample2, mu = 120)

t2$p.value   # If > 0.05, we failed to detect difference → Type II error



##################################################################
set.seed(42)

alpha <- 0.05   # significance level
n <- 30         # sample size
sd <- 10

# ---- Type I Error (H0 true: mean=120) ----
type1 <- replicate(10000, {
  sample <- rnorm(n, mean = 120, sd = sd)
  pval <- t.test(sample, mu = 120)$p.value
  pval < alpha   # Reject H0?
})
mean(type1)   # ≈ 0.05 (Type I error rate)

# ---- Type II Error (H0 false: true mean=122) ----
type2 <- replicate(10000, {
  sample <- rnorm(n, mean = 122, sd = sd)
  pval <- t.test(sample, mu = 120)$p.value
  pval > alpha   # Fail to reject H0?
})
mean(type2)   # Type II error rate (depends on effect size, n, α)

# Power of test
1 - mean(type2)


#####################################################################
install.packages("pwr")

library(pwr)

# Effect size (Cohen's d) = difference / SD
d <- 2 / 10   # small effect size

# Sample sizes
n_values <- seq(10, 200, by = 10)

# Calculate power for each sample size
power_values <- sapply(n_values, function(n) {
  pwr.t.test(n = n, d = d, sig.level = 0.05, type = "one.sample", alternative = "two.sided")$power
})

# Plot power curve
plot(n_values, power_values, type = "b", pch = 19, col = "blue",
     xlab = "Sample Size (n)", ylab = "Power (1 - β)",
     main = "Power Curve for Detecting Mean Difference = 2")

abline(h = 0.8, col = "red", lty = 2)  # 80% power threshold

