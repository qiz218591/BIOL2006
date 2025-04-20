#









#Import the data 
library(readr)
setwd("C:/Users/Divya Agrawal/OneDrive/Documents/")
getwd()
DserImm_ <- read_csv("DserImm.csv")
pairs(DserImm_[, 1:4], main = "Scatterplot Matrix of DserImm Data")


install.packages("tidyr")
library(tidyr)

# Reshape to long format
df_long <- DserImm_ %>%
  pivot_longer(cols = 2:5, names_to = "Variable", values_to = "Value")

# Boxplot
ggplot(df_long, aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot() +
  labs(title = "Boxplots of 4 Variables",
       x = "Variable", y = "Value") +
  theme_minimal()     
##interpretation: distribution, central tendency and variability is definied by four variables in your dataset.
#gene_expr has the highest values and widest spread, indicating large variation in gene expression levels among individuals. Its median is also relatively high.
#mass values are close to zero and show no variation — this could indicate either all observations have the same mass, or it may need to be checked for errors or scaling issues.
#reproduction has moderate values with a narrow spread, suggesting reproduction rates are relatively consistent among individuals.
#survival_time has a higher spread than reproduction, with a median around 200 and several values extending further, indicating greater variability in survival outcomes.


##assumptions for multiple linear regression and multiplicate effect

#Check multicollinearity
plot(model)
library(car)
vif(model)  # VIF should be < 5 (or < 10, conservative)

##assumptions for linear regression
plot(model)  ### all 4 graphs generated are assumptions plots, and the interpretation of them is written below. 
#analyse the plot and check whether the below interpretation of assumptions are fitting in or not.
#Residuals vs Fitted: if No clear pattern = good (linearity, homoscedasticity)
#Normal Q-Q: if Points close to the line = good (normality)
#Scale-Location: if Horizontal spread = good (homoscedasticity)
#Residuals vs Leverage: if Watch for high-leverage points (outliers)





#Import the data 
library(readr)
setwd("C:/Users/Divya Agrawal/OneDrive/Documents/")
getwd()
DserImm_ <- read_csv("DserImm.csv")
View(DserImm_)
#Fit full model with three-way interaction 
model <- lm(survival_time ~ mass*reproduction*gene_expr, data = DserImm_)
#view model summary 
summary(model)
#Assess for no multicolinearity 
#load the library 'car'
library(car)
#fit linear mdoel with just main effects of the 3 predicators & get Variance Inflation factor
vif(with(DserImm_,lm(survival_time ~ mass+reproduction+gene_expr)))
#calculate correlation coefficients
round(cor(DserImm_),3)
#Scatterplot of all variables 
with(DserImm_,pairs(survival_time~mass+reproduction+gene_expr))
#Fit model 2 with interaction 
lm2<-with(DserImm_,lm(survival_time~mass*reproduction*gene_expr))
#check residuals for model assumptions:
#Figure 2- qqplot
##Check if the regression residuals are normally distributed
qqnorm(lm2$residuals,main="",ylab="Residuals from Model 2")
#Add the 1:1 line for visualisation 
qqline(lm2$residuals)
#Figure 3- homogeneity of variances plot 
plot(lm2$fitted.values, lm2$residuals, ylab = "Residuals from Model 2", xlab = "Fitted values from Model 2")
## add horizontal line to indicate zero
abline(h=0, col="red")
#Figure S1 -leverage plot 
plot(lm2,which=5,main="")
#Get results from Model 2
#get overall model fit F-value and R^2
summary(lm2) 
# view coefficient estimates and extract to make a formatted table in word.doc.
anova(lm2) 
#Load the library "plotly"
library(plotly)

#Add the new column to the data set
DserImm_$mass_by_reproduction<-DserImm_$mass*DserImm_$reproduction
#Check that the new variable has been added
head(DserImm_)
#Analyzing the data with the mass_by_reproduction
m3_manual_interaction<-lm(survival_time ~ mass + reproduction + mass_by_reproduction, data = DserImm_)
#look at lm summary
summary(m3_manual_interaction) 
#Replacing the m3_manual_interaction with the R Code for an interaction term (:) between mass and reproduction
m3<-lm(survival_time ~ mass + reproduction + mass:reproduction, data = DserImm_)
#look at lm summary
summary(m3) 
#fit the model using the asterix to tell R to fit a model with each predictor as a main effect and the interaction between the mass*reproduction 
m3_by_stars <- lm(survival_time ~ mass * reproduction, data = DserImm_)
#look at lm summary
summary(m3_by_stars)  
# Create interactive 2D plot

# define some colours. 
cols_to_use<-c("black", "grey", "lightgreen")

# First make a categorical version of reproduction
reproduction_cut<-cut(DserImm_$reproduction, breaks = 3, labels = c("low", "med", "high"))
# the cut function here "cuts" the reproduction variable into thirds (breaks = 3)
#and we tell R to call the categories "low", "med" and "high"

# Next choose values of reproduction to plot the curves by grabbing low, medium and high values of reproduction
reproduction_vals_to_plot<-quantile(DserImm_$reproduction, p = c(0.17, 0.5, 0.83))

#make the plot
with(DserImm_, plot(survival_time~mass,col=cols_to_use[reproduction_cut], pch=16))
#here we use our 'cut' variable to colour the points

#Now add the fit lines for mass for each of the 3 levels of reproduction
# line for low reproduction
curve(cbind(1,x,reproduction_vals_to_plot[1], x*reproduction_vals_to_plot[1])%*%coef(m3), add=T, col=cols_to_use[1], lwd=3)

# line for medium reproduction
curve(cbind(1,x,reproduction_vals_to_plot[2], x*reproduction_vals_to_plot[2])%*%coef(m3), add=T, col=cols_to_use[2], lwd=3)

# line for high reproduction
curve(cbind(1,x,reproduction_vals_to_plot[3], x*reproduction_vals_to_plot[3])%*%coef(m3), add=T, col=cols_to_use[3], lwd=3)

# add a legend
legend("bottomleft", col=cols_to_use, lwd=2, legend = c("low reproduction", "medium reproduction", "high reproduction"))

#Create interactive 3D plot


#each with fifteen evenly spaced values ranging from the lowest and highest values of each variable
mass<- seq(min(DserImm_$mass), max(DserImm_$mass), length.out=15)
reproduction<- seq(min(DserImm_$reproduction), max(DserImm_$reproduction), length.out=15)

#Next, define a function that we can use to generate the full interaction surface using the "smooth" variables
surface_func<-function(var1, var2) (cbind(1, var1, var2, var1*var2)%*%coef(m3))

#now use "outer" to make the surface
survival_time<-outer(X = mass, Y=reproduction,  FUN=surface_func) 
#Here, outer() takes smooth_mass and smooth_reproduction and applies them to the surface function we made

#now plot the surface using "persp"
persp(y = reproduction, x = mass, z=survival_time, 
      theta = 45, phi = 30, col="lightblue")



#####format 1 for 3d plot
install.packages("plotly")
library(plotly)

# Generate the grid of values
mass <- seq(min(DserImm_$mass), max(DserImm_$mass), length.out = 15)
reproduction <- seq(min(DserImm_$reproduction), max(DserImm_$reproduction), length.out = 15)

# Define the function for the interaction surface
surface_func <- function(var1, var2) (cbind(1, var1, var2, var1 * var2) %*% coef(m3))

# Create grid using expand.grid
grid <- expand.grid(mass = mass, reproduction = reproduction)
grid$survival_time <- surface_func(grid$mass, grid$reproduction)

# Reshape for plotly
z_matrix <- matrix(grid$survival_time, nrow = length(mass), ncol = length(reproduction))

# Create the interactive plot
plot_ly(x = ~mass, y = ~reproduction, z = ~z_matrix) %>%
  add_surface(colorscale = 'Viridis') %>%
  layout(
    title = "Interactive 3D Surface Plot",
    scene = list(
      xaxis = list(title = "Mass"),
      yaxis = list(title = "Reproduction"),
      zaxis = list(title = "Survival Time")
    )
  )

################# format2 for 3d plot
# Define sequences for grid
mass_seq <- seq(min(DserImm_$mass), max(DserImm_$mass), length.out = 30)
repro_seq <- seq(min(DserImm_$reproduction), max(DserImm_$reproduction), length.out = 30)

# Create grid
grid <- expand.grid(mass = mass_seq, reproduction = repro_seq)

# Predict survival time using your model (m3)
grid$survival_time <- predict(m3, newdata = grid)

# Create interactive 3D scatter plot with regression surface
library(plotly)

# Scatter points from real data
p <- plot_ly(DserImm_, x = ~mass, y = ~reproduction, z = ~survival_time,
             type = "scatter3d", mode = "markers",
             marker = list(size = 4, color = ~survival_time, colorscale = "Viridis"),
             name = "Observed Data")

# Add regression surface
p <- p %>% add_trace(data = grid,
                     x = ~mass, y = ~reproduction, z = ~survival_time,
                     type = "surface",
                     opacity = 0.6,
                     colorscale = list(c(0,1), c("lightblue", "lightblue")),
                     showscale = FALSE,
                     name = "Regression Plane")

# Final layout
p <- p %>% layout(
  title = "3D Regression: Survival Time ~ Mass * Reproduction",
  scene = list(
    xaxis = list(title = "Mass"),
    yaxis = list(title = "Reproduction"),
    zaxis = list(title = "Survival Time")
  )
)

p

