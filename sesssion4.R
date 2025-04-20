# Load the dataset
data(mtcars)
head(mtcars)


#Predict miles per gallon (mpg) using horsepower (hp), weight (wt), and their interaction.
# Model with interaction between hp and wt
#Fit a multiple linear regression model
model <- lm(mpg ~ hp * wt, data = mtcars)
plot(mtcars)  ##scattermatrix #assumptions: linearity, outliers, collinearity
plot(model$residuals~model$fitted.values,xlab="fittedvalues",ylab="residuals")  ##assumptions: #Homoscedasticity – Are residuals spread evenly across all fitted values?
#Linearity – Should be no obvious curve in the plot.
#Independence – No clustering pattern.
#Mean of residuals = 0 – Checked via the horizontal line.
#If residuals fan out or show a curve, assumptions are violated.

abline(h=0,lty=3)
mtext(text="Fig.2",at=-3,cex=1.8,line=1.5)
summary(model)
##interpretation
#Each unit increase in hp decreases mpg by ~0.12, depending on weight.
#Each unit increase in wt decreases mpg by ~8.216, depending on horsepower.
#The positive interaction term (hp:wt) means that the effect of hp on mpg becomes less negative as weight increases.


# Load the car package to check VIF
install.packages("car")  # run only once
library(car)


##use variance inflation factor (VIF)
vif(model)
##assumptions
#VIF (Variance Inflation Factor) helps detect multicollinearity—whether predictors are too correlated.
#rule of thumb:
#VIF > 5: Moderate collinearity
#VIF > 10: Serious collinearity issue
##interpretation:
#VIF > 5 or 10 suggests high collinearity.
#If hp and wt are highly collinear, interpreting them individually becomes difficult.







##Interactions need careful interpretation—draw interaction plots if needed:
library(ggplot2)
ggplot(mtcars, aes(x = hp, y = mpg, color = factor(wt > 3))) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x)
#Collinearity doesn’t violate regression assumptions, but it affects coefficient stability.


##3D
# Install and load plotly if needed
install.packages("plotly")
library(plotly)

# Create a 3D scatter plot
plot_ly(mtcars, x = ~hp, y = ~wt, z = ~mpg,
        type = "scatter3d", mode = "markers",
        marker = list(size = 5, color = ~mpg, colorscale = "Viridis")) %>%
  layout(title = "3D Scatterplot: mpg ~ hp + wt",
         scene = list(
           xaxis = list(title = "Horsepower (hp)"),
           yaxis = list(title = "Weight (wt)"),
           zaxis = list(title = "Miles per Gallon (mpg)")
         ))
##3D Static plot with fitted plane
install.packages("scatterplot3d")
library(scatterplot3d)

# 3D scatter plot
s3d <- scatterplot3d(mtcars$hp, mtcars$wt, mtcars$mpg,
                     xlab = "Horsepower",
                     ylab = "Weight",
                     zlab = "MPG",
                     pch = 16,
                     highlight.3d = TRUE,
                     type = "h",
                     angle = 55,
                     main = "3D Plot with Regression Plane")

# Fit linear model
model <- lm(mpg ~ hp + wt, data = mtcars)

# Add regression plane
s3d$plane3d(model, draw_polygon = TRUE, draw_lines = TRUE, lty = "dotted", col = "skyblue")



##interactive 3d plot with regression plane:
model <- lm(mpg ~ hp + wt, data = mtcars)
summary(model)  # Optional, to view coefficients
# Create grid
hp_seq <- seq(min(mtcars$hp), max(mtcars$hp), length.out = 30)
wt_seq <- seq(min(mtcars$wt), max(mtcars$wt), length.out = 30)
grid <- expand.grid(hp = hp_seq, wt = wt_seq)

# Predict mpg over the grid
grid$mpg <- predict(model, newdata = grid)

library(plotly)

# Scatter plot of original data
p <- plot_ly(mtcars, x = ~hp, y = ~wt, z = ~mpg,
             type = "scatter3d", mode = "markers",
             marker = list(size = 4, color = ~mpg, colorscale = "Viridis"),
             name = "Data points")

# Add the regression plane
p <- p %>% add_trace(data = grid,
                     x = ~hp, y = ~wt, z = ~mpg,
                     type = "surface",
                     opacity = 0.6,
                     colorscale = list(c(0,1), c("lightblue", "lightblue")),
                     showscale = FALSE,
                     name = "Regression Plane")

# Layout
p <- p %>% layout(
  title = "3D Regression: mpg ~ hp + wt",
  scene = list(
    xaxis = list(title = "Horsepower (hp)"),
    yaxis = list(title = "Weight (wt)"),
    zaxis = list(title = "Miles per Gallon (mpg)")
  )
)

p
