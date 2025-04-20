# Simulate data
set.seed(123)
n <- 90
diet <- factor(rep(c("A", "B", "C"), each = 30))
initial_weight <- rnorm(n, mean = 70, sd = 10)
weight_loss <- 5 + 
  ifelse(diet == "B", 2, ifelse(diet == "C", 3, 0)) +
  0.3 * initial_weight + rnorm(n, 0, 2)

data <- data.frame(diet, initial_weight, weight_loss)
head(data)

# ANCOVA model: weight_loss ~ diet + initial_weight
ancova_model <- lm(weight_loss ~ diet + initial_weight, data = data)
summary(ancova_model)

##Interpretation:
#diet: shows the effect of diet on weight loss, adjusting for initial weight.
#initial_weight: indicates how weight loss changes with initial weight.
#Look at the p-values to determine significance.
#R-squared tells how much variability in weight loss is explained.




#To check if the relationship between weight loss and initial weight differs by diet group:
##check for interaction
interaction_model <- lm(weight_loss ~ diet * initial_weight, data = data)
anova(ancova_model, interaction_model)  # Compare models
#interpretation:If interaction is significant, you should not use ANCOVA directly and consider stratified analyses.


library(ggplot2)

ggplot(data, aes(x = initial_weight, y = weight_loss, color = diet)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "ANCOVA: Weight Loss by Diet Adjusted for Initial Weight",
       x = "Initial Weight", y = "Weight Loss") +
  theme_minimal()



###examples 
set.seed(123)
n <- 140  # 10 replicates for 14 excipients
excipient <- factor(rep(paste0("E", 1:14), each = 10))
initial_conc <- runif(n, 5, 10)  # mg/mL
# Simulated monomer retention with slight effects
monomer_retention <- 90 + 
  rnorm(14, 0, 2)[as.numeric(excipient)] +
  0.8 * initial_conc +
  rnorm(n, 0, 1.5)

data <- data.frame(excipient, initial_conc, monomer_retention)
head(data)
# ANCOVA: Effect of excipient on monomer retention adjusting for concentration
model <- lm(monomer_retention ~ excipient + initial_conc, data = data)
summary(model)

#interpretation of results
#excipient: Tests differences in stability between excipients after adjusting for initial concentration.
#initial_conc: Tells how the concentration affects stability.


##check for interaction
interaction_model <- lm(monomer_retention ~ excipient * initial_conc, data = data)
anova(model, interaction_model)

#interpretation 
#If interaction is not significant, your ANCOVA model is valid.
#If interaction is significant, then each excipient may behave differently across concentration, and you might analyze separately or model differently.

#visualisation 
library(ggplot2)

ggplot(data, aes(x = initial_conc, y = monomer_retention, color = excipient)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "ANCOVA: Monomer Retention by Excipient Adjusted for Initial Concentration",
       x = "Initial Protein Concentration (mg/mL)",
       y = "Percent Monomer Retention") +
  theme_minimal()


#ANCOVA
#ANCOVA is powerful in this context:
#It removes unwanted variability (e.g., initial protein amount).
#It isolates the true effect of the excipient on stability.
#Useful in screening studies where baseline conditions vary slightly.