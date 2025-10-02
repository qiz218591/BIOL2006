###machine learning
packages <- c(
  "tidyverse", "caret", "randomForest", "e1071", "pROC", "rpart", "rpart.plot",
  "xgboost", "glmnet", "factoextra", "pheatmap", "rattle.data", "MASS", "titanic"
)


install_if_missing <- function(pkgs){
  for(p in pkgs){
    if(!requireNamespace(p, quietly = TRUE)) install.packages(p)
  }
}
install_if_missing(packages)


# Load libraries
library(tidyverse)
library(caret)
library(randomForest)
library(e1071) # SVM, Naive Bayes
library(pROC) # ROC/AUC
library(rpart)
library(rpart.plot)
library(xgboost)
library(glmnet)
library(factoextra)
library(pheatmap)
library(rattle.data)
library(MASS)
library(titanic)
library(MASS)
library(caret)
data(Boston)
head(Boston)

# Set seed
set.seed(123)

###Goal: Predict Species from sepal/petal measurements. Explore clustering & PCA


##preprocessing
data(iris)
head(iris)
summary(iris)


# Basic ggplot
library(ggplot2)
ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) +
  geom_point(size=2) + theme_minimal()


install.packages("easystats")
library(easystats)
# Correlation matrix (numeric variables only)

# Drop Species column (factor) and calculate correlation
cor_mat <- cor(iris[, -5])   # 5th column is Species
round(cor_mat, 2)     ###no missing values, features are on the same scale.

iris_pca <- prcomp(iris[,-5], center = TRUE, scale. = TRUE)
summary(iris_pca)
# Biplot
factoextra::fviz_pca_biplot(iris_pca, geom.ind = "point", habillage = iris$Species,
                            addEllipses = TRUE, repel = TRUE)
  ####first two PCs usually sperate species well, especially setosa.



##k means unsupervised modeling
set.seed(123)
km <- kmeans(scale(iris[,-5]), centers = 3, nstart = 50)
# Compare clusters vs species
table(km$cluster, iris$Species)   


# Plot clusters on PC1 & PC2
pc_df <- data.frame(iris_pca$x[,1:2], Species = iris$Species, Cluster = factor(km$cluster))
ggplot(pc_df, aes(PC1, PC2, color = Species, shape = Cluster)) + geom_point(size=2)


##K-means often finds clusters matching species closely.


##supervised model (train/test, cross validation)

set.seed(123)
trainIndex <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
train <- iris[trainIndex, ]
test <- iris[-trainIndex, ]


ctrl <- trainControl(method = "cv", number = 10)


# Random Forest
model_rf <- train(Species ~ ., data = train, method = "rf", trControl = ctrl)
model_rf


# SVM (radial)
model_svm <- train(Species ~ ., data = train, method = "svmRadial", trControl = ctrl, tuneLength = 6)
model_svm


# k-NN
model_knn <- train(Species ~ ., data = train, method = "knn", trControl = ctrl, tuneLength = 8)
model_knn


# Evaluate on test set
pred_rf <- predict(model_rf, test)
pred_svm <- predict(model_svm, test)
pred_knn <- predict(model_knn, test)


confusionMatrix(pred_rf, test$Species)
confusionMatrix(pred_svm, test$Species)
confusionMatrix(pred_knn, test$Species)


##multi class ROC (one vs all), and class probabilites
# Get class probabilities from the rf model
probs_rf <- predict(model_rf, test, type = "prob")
# Example: multiclass AUC using pROC (one-vs-all approach)
multiclass_roc <- multiclass.roc(test$Species, probs_rf)
multiclass_roc


###Petal measurements are the strongest discriminators (Petal.Length, Petal.Width).

##Random forest and SVM typically perform best. k-NN also works well with k tuning.

##PCA is useful to visualize separability; don't use PCA before tree-based methods necessarily.


###Task: Predict whether a car is automatic or manual based on features like mpg, hp, wt, etc.


data(mtcars)
mtcars <- mtcars %>% rownames_to_column(var = "model")
summary(mtcars)


# Visualizations
ggplot(mtcars, aes(wt, mpg)) + geom_point() + geom_smooth(method='lm') + theme_minimal()
ggplot(mtcars, aes(hp, mpg)) + geom_point() + geom_smooth(method='lm')


# Correlation heatmap
num_vars <- mtcars[,-1]
corr <- cor(num_vars)
pheatmap::pheatmap(corr, main = "mtcars correlation")


###2nd october
library(class)
data(iris)

set.seed(123)
train_index <- sample(1:nrow(iris), 0.7*nrow(iris))
train <- iris[train_index, ]
test <- iris[-train_index, ]

pred_knn <- knn(train[, -5], test[, -5], train$Species, k = 5)
table(Predicted = pred_knn, Actual = test$Species)



###svm
library(e1071)

data(iris)
svm_model <- svm(Species ~ ., data = iris, kernel = "radial")
pred_svm <- predict(svm_model, iris)
table(Predicted = pred_svm, Actual = iris$Species)


###rf
library(randomForest)

set.seed(123)
rf_model <- randomForest(Species ~ ., data = iris, ntree = 100)
pred_rf <- predict(rf_model, iris)
table(Predicted = pred_rf, Actual = iris$Species)



###titanic ##rf
library(titanic)
library(randomForest)

data("titanic_train")
titanic_train$Survived <- factor(titanic_train$Survived)

rf_model <- randomForest(Survived ~ Pclass + Sex + Age + Fare, 
                         data = titanic_train, ntree = 100, na.action = na.omit)
print(rf_model)


###svm
library(e1071)

data(mtcars)
svm_model <- svm(mpg ~ wt + hp + disp, data = mtcars, kernel = "radial")
pred_svm <- predict(svm_model, mtcars)
cor(pred_svm, mtcars$mpg)  # Check correlation




#########comparing svm rf and knn
# Load libraries
library(caret)
library(randomForest)
library(e1071)
library(class)
library(ggplot2)
library(GGally)
install.packages("GGally")

set.seed(123)

# Load data
data(iris)

# Train/test split
trainIndex <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
train <- iris[trainIndex, ]
test  <- iris[-trainIndex, ]

### 1. Random Forest
rf_model <- randomForest(Species ~ ., data = train, ntree = 100)
rf_pred <- predict(rf_model, test)
rf_acc <- mean(rf_pred == test$Species)

### 2. SVM
svm_model <- svm(Species ~ ., data = train, kernel = "radial")
svm_pred <- predict(svm_model, test)
svm_acc <- mean(svm_pred == test$Species)

### 3. k-NN
knn_pred <- knn(train[, -5], test[, -5], train$Species, k = 5)
knn_acc <- mean(knn_pred == test$Species)

# Accuracy comparison
acc_df <- data.frame(
  Model = c("Random Forest", "SVM", "k-NN"),
  Accuracy = c(rf_acc, svm_acc, knn_acc)
)
print(acc_df)

# ---- Plotting ----
# Use only 2 features for easy visualization: Petal.Length & Petal.Width
plot_data <- iris[, c("Petal.Length", "Petal.Width", "Species")]

# Function for plotting decision boundaries
plot_decision_boundary <- function(model, model_name) {
  grid <- expand.grid(
    Petal.Length = seq(min(plot_data$Petal.Length), max(plot_data$Petal.Length), length.out = 200),
    Petal.Width  = seq(min(plot_data$Petal.Width), max(plot_data$Petal.Width), length.out = 200)
  )
  grid$Species <- predict(model, newdata = grid)
  
  ggplot(plot_data, aes(Petal.Length, Petal.Width, color = Species)) +
    geom_point(size = 2) +
    geom_tile(data = grid, aes(fill = Species), alpha = 0.2) +
    ggtitle(paste("Decision Boundary -", model_name)) +
    theme_minimal()
}

# Train models only on Petal.Length & Petal.Width for visualization
rf_model_2 <- randomForest(Species ~ Petal.Length + Petal.Width, data = train)
svm_model_2 <- svm(Species ~ Petal.Length + Petal.Width, data = train, kernel = "radial")

# For kNN we need a wrapper
knn_pred_grid <- function(grid) {
  knn(train[, c("Petal.Length","Petal.Width")],
      grid[, c("Petal.Length","Petal.Width")],
      train$Species, k = 5)
}

grid <- expand.grid(
  Petal.Length = seq(min(plot_data$Petal.Length), max(plot_data$Petal.Length), length.out = 200),
  Petal.Width  = seq(min(plot_data$Petal.Width), max(plot_data$Petal.Width), length.out = 200)
)
grid$Species <- knn_pred_grid(grid)

# ---- Plots ----
p1 <- plot_decision_boundary(rf_model_2, "Random Forest")
p2 <- plot_decision_boundary(svm_model_2, "SVM")

p3 <- ggplot(plot_data, aes(Petal.Length, Petal.Width, color = Species)) +
  geom_point(size = 2) +
  geom_tile(data = grid, aes(fill = Species), alpha = 0.2) +
  ggtitle("Decision Boundary - k-NN") +
  theme_minimal()

# Display plots
print(p1)
print(p2)
print(p3)


############################
##random forest##############

install.packages("randomForest")

library(randomForest)
data(iris)
head(iris)
set.seed(42)

trainIndex <- sample(1:nrow(iris), 0.8 * nrow(iris))

trainData <- iris[trainIndex, ]
testData <- iris[-trainIndex, ]
rf_model <- randomForest(Species ~ ., data = trainData)
print(rf_model)


install.packages("caret")
library(caret)

predictions <- predict(rf_model, testData)
confusionMatrix(predictions, testData$Species)


rf_tuned <- randomForest(Species ~ ., data = trainData, ntree = 500, mtry = 2)
print(rf_tuned)


importance(rf_model)
varImpPlot(rf_model)


###svm###################3
install.packages('e1071') 
install.packages('caTools')
install.packages('ggplot2')
install.packages('caret')

library(caret)
library(e1071) 
library(caTools)
library(ggplot2)

data = read.csv("C:/Users/Lenovo/Downloads/social.csv")

head(data)
summary(data)

set.seed(123)

data$Gender <- as.numeric(factor(data$Gender, levels = c("Male", "Female"), labels = c(0, 1)))

data[, c("Age", "EstimatedSalary")] <- scale(data[, c("Age", "EstimatedSalary")])

split <- sample.split(data$Purchased, SplitRatio = 0.75)
training_set <- subset(data, split == TRUE)
test_set <- subset(data, split == FALSE)


classifier <- svm(Purchased ~ Age + EstimatedSalary + Gender, 
                  data = training_set, 
                  type = 'C-classification', 
                  kernel = 'radial', 
                  gamma = 0.1)

y_pred <- predict(classifier, newdata = test_set)

table(test_set$Purchased, y_pred)

accuracy <- sum(diag(table(test_set$Purchased, y_pred))) / sum(table(test_set$Purchased, y_pred))
cat("Accuracy: ", accuracy)

confusionMatrix(table(test_set$Purchased, y_pred))
X1 = seq(min(training_set$Age) - 1, max(training_set$Age) + 1, by = 0.01)
X2 = seq(min(training_set$EstimatedSalary) - 1, max(training_set$EstimatedSalary) + 1, by = 0.01)

grid_set <- expand.grid(Age = X1, EstimatedSalary = X2)

grid_set$Gender = median(training_set$Gender)  # Default Gender value for grid

y_grid = predict(classifier, newdata = grid_set)

ggplot() +
  geom_tile(data = grid_set, aes(x = Age, y = EstimatedSalary, fill = as.factor(y_grid)), alpha = 0.3) +
  geom_point(data = training_set, aes(x = Age, y = EstimatedSalary, color = as.factor(Purchased)), size = 3, shape = 21) +
  scale_fill_manual(values = c('coral1', 'aquamarine')) +
  scale_color_manual(values = c('green4', 'red3')) +
  labs(title = 'SVM Decision Boundary (Training set)', x = 'Age', y = 'Estimated Salary') +
  theme_minimal() +
  theme(legend.position = "none")



###knn
install.packages("caTools") 
install.packages("class") 
install.packages("ggplot2")


library(caTools) 
library(class)
library(ggplot2)

data(iris)
str(iris)


split <- sample.split(iris, SplitRatio = 0.7) 
train_cl <- subset(iris, split == "TRUE") 
test_cl <- subset(iris, split == "FALSE") 

train_scale <- scale(train_cl[, 1:4]) 
test_scale <- scale(test_cl[, 1:4])


classifier_knn <- knn(train = train_scale, 
                      test = test_scale, 
                      cl = train_cl$Species, 
                      k = 1)

cm <- table(test_cl$Species, classifier_knn) 
cm

library(ggplot2)

k_values <- c(1, 3, 5, 7, 15, 19)

accuracy_values <- sapply(k_values, function(k) {
  classifier_knn <- knn(train = train_scale, 
                        test = test_scale, 
                        cl = train_cl$Species, 
                        k = k)
  1 - mean(classifier_knn != test_cl$Species)
})

accuracy_data <- data.frame(K = k_values, Accuracy = accuracy_values)

ggplot(accuracy_data, aes(x = K, y = Accuracy)) +
  geom_line(color = "lightblue", size = 1) +
  geom_point(color = "lightgreen", size = 3) +
  labs(title = "Model Accuracy for Different K Values",
       x = "Number of Neighbors (K)",
       y = "Accuracy") +
  theme_minimal()
