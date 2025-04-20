##imagine you’re testing the effect of three drug formulations 
#on tumor volume in mice. You have 3 treatments (A, B, C), and 
#you're measuring tumor volume after treatment.

#Biological Scenario
#You have 18 mice, 6 per treatment group.
#Mice come from 3 litters (possible source of variation, so we’ll use blocking).
#Each mouse receives one treatment (experimental unit = mouse).
#Tumor volume is measured (measurement unit = mouse).

set.seed(123)

# Factors
treatment <- factor(rep(c("A", "B", "C"), each = 6))
litter <- factor(rep(1:3, times = 6))  # blocking factor

# Simulate tumor volume (different means per treatment + random noise)
tumor_volume <- c(rnorm(6, mean = 50, sd = 5),
                  rnorm(6, mean = 40, sd = 5),
                  rnorm(6, mean = 30, sd = 5))

# Combine into data frame
data <- data.frame(treatment, litter, tumor_volume)
head(data)


#Linear model including categorical predictor and blocking
model <- lm(tumor_volume ~ treatment + litter, data = data)
summary(model)

#treatment is a categorical predictor.
#litter is a blocking factor.
#The reference level for treatment is A. Coefficients show the effect of B and C relative to A.


##anova 
anova(model)

#This will output the ANOVA table, showing:
#SS (sum of squares)
#DF (degrees of freedom)
#F value and p-value
#Use this to test whether treatment or litter significantly affects tumor volume.


library(ggplot2)

ggplot(data, aes(x = treatment, y = tumor_volume)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  labs(title = "Tumor Volume by Treatment Group",
       y = "Tumor Volume (mm³)", x = "Treatment")

# Fit model without blocking for Tukey's test simplicity
model_tukey <- aov(tumor_volume ~ treatment, data = data)
#Once ANOVA shows that the treatment effect is significant, Tukey's Honest Significant Difference (HSD) test tells you which groups differ.

# Tukey's HSD
TukeyHSD(model_tukey)

plot(TukeyHSD(model_tukey))


##concept 3- independent replication

#Explanation:
#Experimental unit: What receives the treatment → e.g., a mouse.
#Measurement unit: Where measurement is taken → e.g., tumor volume from each mouse.
#Incorrect Example (pseudo-replication):
#If we measure tumor volume 3 times from the same mouse and treat them as separate replicates:

# BAD EXAMPLE: multiple measurements from same mouse, treated as independent
mouse_id <- rep(1:6, each = 3)
treatment <- rep(c("A", "B"), each = 9)
tumor_volume <- rnorm(18, mean = 50, sd = 5)

df_wrong <- data.frame(mouse_id, treatment, tumor_volume)

# Wrong model – ignores nesting
lm_wrong <- lm(tumor_volume ~ treatment, data = df_wrong)
summary(lm_wrong)
##Correct way: account for mouse as a random effect or use mean per mouse.


##concept 4: randomization:In practice, assign treatments randomly.
set.seed(123)

# Create a vector of treatments
treatments <- rep(c("A", "B", "C"), times = 6)

# Randomize order of assignment
random_treatments <- sample(treatments)

# Assign to 18 mice
mouse_id <- 1:18
randomized_data <- data.frame(mouse_id, treatment = random_treatments)
head(randomized_data)

#concept 4: blocking
#Let’s say the mice come from 3 litters, and you want to block for that.

# Add blocking factor (3 litters, 6 mice per litter)
randomized_data$litter <- rep(1:3, each = 6)

# Simulate response with block effect
randomized_data$tumor_volume <- rnorm(18, mean = 40 + as.numeric(randomized_data$treatment) * 5, sd = 4) +
  rep(c(0, 3, -2), each = 6)  # litter effect

# Fit model with blocking
model_blocked <- lm(tumor_volume ~ treatment + factor(litter), data = randomized_data)
anova(model_blocked)


###randomizatin
set.seed(123)

# Create 18 mouse IDs
mouse_id <- 1:18

# Create 6 mice per treatment group, randomly assigned
treatment <- sample(rep(c("A", "B", "C"), each = 6))

# Make treatment a factor
treatment <- factor(treatment, levels = c("A", "B", "C"))

# Blocking factor: 3 litters (6 mice each)
litter <- factor(rep(1:3, each = 6))

#Now that treatment is a factor with 3 levels, we can assign numeric values manually (e.g., A = 0, B = -5, C = -10), and add litter effects.
#simulating tumor volume with treatment and blocking effects
# Define treatment effect manually
treatment_effect <- ifelse(treatment == "A", 0,
                           ifelse(treatment == "B", -5, -10))

# Define block (litter) effect: litter 1 = 0, 2 = +3, 3 = -2
block_effect <- ifelse(litter == 1, 0,
                       ifelse(litter == 2, 3, -2))

# Simulate tumor volume
tumor_volume <- rnorm(18, mean = 40 + treatment_effect + block_effect, sd = 4)


##put it all together
randomized_data <- data.frame(mouse_id, treatment, litter, tumor_volume)
head(randomized_data)


#fit model and perform anova
model_blocked <- lm(tumor_volume ~ treatment + litter, data = randomized_data)
anova(model_blocked)


#anova output###The ANOVA table will show:
#A significant treatment effect if means differ (A vs B vs C).
#Whether blocking (litter) helps explain variation.

#If you're working with nested or random effects, consider lmer() from lme4:
library(lme4)
model_mixed <- lmer(tumor_volume ~ treatment + (1 | litter), data = randomized_data)
summary(model_mixed)


ggplot(randomized_data, aes(x = treatment, y = tumor_volume, fill = treatment)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Tumor Volume by Treatment Group",
       x = "Treatment",
       y = "Tumor Volume (mm³)") +
  scale_fill_brewer(palette = "Set2")


ggplot(randomized_data, aes(x = treatment, y = tumor_volume, fill = treatment)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ litter) +
  theme_minimal() +
  labs(title = "Tumor Volume by Treatment across Litters (Blocks)",
       x = "Treatment",
       y = "Tumor Volume (mm³)") +
  scale_fill_brewer(palette = "Set3")

ggplot(randomized_data, aes(x = treatment, y = tumor_volume, fill = treatment)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.7) +
  facet_wrap(~ litter) +
  theme_minimal() +
  labs(title = "Tumor Volume by Treatment across Litters (with Jittered Points)",
       x = "Treatment",
       y = "Tumor Volume (mm³)") +
  scale_fill_brewer(palette = "Pastel1")

#publication
ggboxplot(randomized_data, x = "treatment", y = "tumor_volume",
          color = "treatment", palette = "Set2",
          add = "jitter", shape = "litter") +
  stat_compare_means(method = "anova", label.y = max(randomized_data$tumor_volume) + 2) +
  labs(title = "Tumor Volume by Treatment",
       x = "Treatment", y = "Tumor Volume (mm³)") +
  theme_minimal()

ggboxplot(randomized_data, x = "treatment", y = "tumor_volume",
          color = "treatment", palette = "Dark2",
          add = "jitter") +
  stat_compare_means(method = "anova", label.y = max(randomized_data$tumor_volume) + 2) +  # ANOVA p-value
  stat_compare_means(comparisons = list(c("A", "B"), c("A", "C"), c("B", "C")),
                     method = "t.test", label = "p.signif", hide.ns = TRUE) +
  labs(title = "Tumor Volume by Treatment with Pairwise Comparisons") +
  theme_minimal()


ggboxplot(randomized_data, x = "treatment", y = "tumor_volume",
          color = "treatment", palette = "Set3",
          add = "jitter") +
  facet_wrap(~ litter) +
  labs(title = "Tumor Volume by Treatment within Litters (Blocked Design)",
       x = "Treatment", y = "Tumor Volume (mm³)") +
  theme_minimal()

ggsave("tumor_volume_plot.png", width = 8, height = 6, dpi = 300)

