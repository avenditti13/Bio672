##to be separated out into individual codes for each numbered section/question:

##AnnVenditti_Unit2.1_Bio672
#using Wine data set in R with numerical values and chemical properties of wine (found online)

install.packages(c("e1071", "mclust", "rattle"))

library(class)      # for KNN
library(e1071)      # for Naive Bayes
library(MASS)       # for LDA and QDA
library(mclust)     # for EM clustering
library(rattle)     # for the Wine dataset
library(ggplot2)

data(wine, package = "rattle")
set.seed(123) # For reproducibility

#Shuffle the data set randomly
wine <- wine[sample(1:nrow(wine)), ]

#Set the parameters
k <- 5  # 5 is number folds for cross-validation
folds <- cut(seq(1, nrow(wine)), breaks = k, labels = FALSE)

# Storage for accuracy scores
results <- data.frame(KNN = numeric(), NaiveBayes = numeric(), LDA = numeric(), QDA = numeric())

# Loop through each fold
for(i in 1:k){
  # Create training and test sets
  testIndex <- which(folds == i, arr.ind = TRUE)
  testData <- wine[testIndex, ]
  trainData <- wine[-testIndex, ]
  
  # Separate features and labels
  trainX <- trainData[, -1]
  trainY <- trainData$Type
  testX <- testData[, -1]
  testY <- testData$Type
  
  # Apply KNN
  knn_pred <- knn(train = trainX, test = testX, cl = trainY, k = 3)
  knn_acc <- mean(knn_pred == testY)
  
  # Apply Naive Bayes
  nb_model <- naiveBayes(Type ~ ., data = trainData)
  nb_pred <- predict(nb_model, testData)
  nb_acc <- mean(nb_pred == testY)
  
  # Apply LDA
  lda_model <- lda(Type ~ ., data = trainData)
  lda_pred <- predict(lda_model, testData)$class
  lda_acc <- mean(lda_pred == testY)
  
  # Apply QDA
  qda_model <- qda(Type ~ ., data = trainData)
  qda_pred <- predict(qda_model, testData)$class
  qda_acc <- mean(qda_pred == testY)
  
  # Storing results in predetermined "results"
  results <- rbind(results, data.frame(KNN = knn_acc, NaiveBayes = nb_acc, LDA = lda_acc, QDA = qda_acc))
}

# Visualize correct/incorrect predictions for KNN
# Apply KNN on the entire wine dataset to get the predictions
wine$Predictions <- ifelse(knn(train = trainX, test = wine[, -1], cl = trainY, k = 3) == wine$Type, "Correct", "Incorrect")
# Plot using ggplot2
KNNplot <- ggplot(wine, aes(x = Alcohol, y = Malic, color = Predictions)) +
  geom_point() +
  labs(title = "KNN Prediction Results (Correct vs Incorrect)") +
  theme_minimal()
print(KNNplot)

# Summarizing  results
sink("PreClassified_Data.txt")
print("Accuracy across folds:\n")
print(results)


# Average accuracy
avg_results <- colMeans(results)
print("Average accuracy for each method:\n")
print(avg_results)

print(KNNplot)

sink()



##AnnVenditti_Unit2.2_Bio672
##Second section of assignment 2

install.packages(c("kernlab", "caret"))

library(e1071)    # for svm
library(kernlab)  # for ksvm
library(rattle)   # for the Wine dataset
library(caret)    # for confusion matrix and cross-validation
library(ggplot2)

#Wine data again:
data(wine, package = "rattle")
set.seed(123) # For reproducibility

#Split data into training (70%) and testing (30%) sets
trainIndex <- createDataPartition(wine$Type, p = 0.7, list = FALSE)
trainData <- wine[trainIndex, ]
testData <- wine[-trainIndex, ]

# Separate features and labels
trainX <- trainData[, -1]
trainY <- trainData$Type
testX <- testData[, -1]
testY <- testData$Type

# Function to evaluate model and its accuracy
evaluate_model <- function(model, testX, testY) {
  predictions <- predict(model, testX)
  confusion <- confusionMatrix(predictions, as.factor(testY))
  accuracy <- confusion$overall['Accuracy']
  print(confusion)  # Print the confusion matrix
  return(list(confusion = confusion, accuracy = accuracy))
}

# Train and evaluate SVM models with different kernels using e1071::svm
sink("Kernel_Model_Results.txt")
cat("e1071 SVM Results:\n")

cat("\nLinear Kernel:\n")
svm_linear <- svm(Type ~ ., data = trainData, kernel = "linear")
svm_linear_eval <- evaluate_model(svm_linear, testX, testY)

cat("\nPolynomial Kernel:\n")
svm_poly <- svm(Type ~ ., data = trainData, kernel = "polynomial", degree = 3)
svm_poly_eval <- evaluate_model(svm_poly, testX, testY)

cat("\nRBF Kernel:\n")
svm_rbf <- svm(Type ~ ., data = trainData, kernel = "radial")
svm_rbf_eval <- evaluate_model(svm_rbf, testX, testY)

# Train and evaluate SVM models with different kernels using kernlab::ksvm
cat("\nkernlab SVM Results:\n")

cat("\nLinear Kernel:\n")
ksvm_linear <- ksvm(Type ~ ., data = trainData, kernel = "vanilladot")
ksvm_linear_eval <- evaluate_model(ksvm_linear, testX, testY)

cat("\nPolynomial Kernel:\n")
ksvm_poly <- ksvm(Type ~ ., data = trainData, kernel = "polydot", kpar = list(degree = 3))
ksvm_poly_eval <- evaluate_model(ksvm_poly, testX, testY)

cat("\nRBF Kernel:\n")
ksvm_rbf <- ksvm(Type ~ ., data = trainData, kernel = "rbfdot")
ksvm_rbf_eval <- evaluate_model(ksvm_rbf, testX, testY)

print(KernelPlot)

cat("The accuracy of these models did perform better than the simpler methods from Question 1.\n")
cat("The average accuracy was higher for models combined in this section compared to the average accuracy of the models previously used.\n")

sink()

# Visualize results for the best-performing model with a scatterplot
# Assuming RBF kernel performed best; use Alcohol and Malic as example axes
best_model <- if (svm_rbf_eval$accuracy > ksvm_rbf_eval$accuracy) svm_rbf else ksvm_rbf
wine$Predicted <- predict(best_model, wine[, -1])

KernelPlot <- ggplot(wine, aes(x = Alcohol, y = Malic, color = as.factor(Predicted))) +
  geom_point(alpha = 0.7) +
  labs(title = "SVM Classification Results (RBF Kernel)", color = "Predicted Type") +
  theme_minimal()
print(KernelPlot)



##AnnVenditti_Unit2.3_Bio672
##Third section of assignment 2

#install neural net packages
install.packages("neuralnet")

##libraries needed
library(neuralnet)
library(rattle)    #Wine data
library(caret)     # train-test splitting and evaluation
library(ggplot2)

##loading in wine data to use in neural net
data(wine, package = "rattle")

# Data prep = Normalize dataset
normalize <- function(x) (x - min(x)) / (max(x) - min(x))
wine_norm <- as.data.frame(lapply(wine[, -1], normalize))
wine_norm$Type <- wine$Type

# Train-test split
set.seed(123) #reproducibility
trainIndex <- createDataPartition(wine_norm$Type, p = 0.7, list = FALSE)
trainData <- wine_norm[trainIndex, ]
testData <- wine_norm[-trainIndex, ]

# Training neural network
nn_model <- neuralnet(Type ~ ., data = trainData, hidden = c(5, 3), linear.output = FALSE)

# Predict on test data
testX <- testData[, -ncol(testData)]
predictions <- compute(nn_model, testX)$net.result
predicted_labels <- apply(predictions, 1, function(x) which.max(x))

# Confusion matrix
testY <- testData$Type
confusion <- confusionMatrix(as.factor(predicted_labels), as.factor(testY))
cat("Neural Network Accuracy:", confusion$overall['Accuracy'], "\n")
print(confusion)

# Plot neural network
plot(nn_model)

cat("The Neural Net does perform a better job; there is as high accuracy as 1.00 and only goes down to 0.97, which is better than the SVM accuracy.")

#installing packages for deep neural networks

########I could not download proper python to get tensorflow to work
########I decided to do the neural net without tensorflow since i tried for 4 days to find a way to download and use it proeprly
#install.packages("keras")
#install.packages()
#library(keras)
#library(tensorflow)
#tensorflow::install_tensorflow(delete_env = TRUE)
#tensorflow::install_tensorflow(version = "2.12.0")
###I not sure what this is but i looked it up and it said i have to run in in a fresh R session
#and that i cannot run the deep neural network without having this python thing ????
##the first line is suppsoed to let R handle the set up i guess

# Install h2o if not already installed
if (!requireNamespace("h2o", quietly = TRUE)) {
  install.packages("h2o")
}

library(h2o)
library(ggplot2)
# Start H2O instance
h2o.init(nthreads = -1, max_mem_size = "1G")

# Load Diamonds dataset preloaded in ggplot2
data("diamonds")

# Initialize H2O
h2o.init()
 #########copied to install latest h20 package becuase it was not running properly
# Step 1: Install the latest version of the H2O package
install.packages("h2o", repos = "https://cloud.r-project.org/")

# Step 2: Restart the R session (this step is automatically done once you execute the following)
.rs.restartR()

# Load required libraries
library(h2o)
library(ggplot2) # for the diamonds dataset

# Step 1: Initialize H2O cluster
h2o.init()

# Step 2: Load the diamonds dataset
data(diamonds)

# Step 3: Convert ordered factors to regular factors
diamonds$cut <- as.factor(as.character(diamonds$cut))
diamonds$color <- as.factor(as.character(diamonds$color))
diamonds$clarity <- as.factor(as.character(diamonds$clarity))

# Step 4: Convert the dataset to H2O format
diamonds_h2o <- as.h2o(diamonds)

# Step 5: Verify the structure of the H2O frame
print(h2o.describe(diamonds_h2o))

## Summary of the H2O frame
h2o.describe(diamonds_h2o)

# Analysis with H2O
# Split data into train and test sets
splits <- h2o.splitFrame(data = diamonds_h2o, ratios = 0.8, seed = 1234)
train <- splits[[1]]
test <- splits[[2]]

# Train a simple H2O GBM model to predict 'price' using other variables
gbm_model <- h2o.gbm(
  x = c("carat", "cut", "color", "clarity", "depth", "table", "x", "y", "z"),
  y = "price",
  training_frame = train,
  validation_frame = test,
  ntrees = 50,
  max_depth = 5,
  seed = 1234
)
print(gbm_model)

#Evaluate the model
#Model performance on 'test' data
performance <- h2o.performance(model = gbm_model, newdata = test)
print(performance)


#shutting down H2O cluster 
h2o.shutdown(prompt = FALSE)


##AnnVenditti_Unit2.3_Bio672
##Fourth section of assignment 2

# Install libraries
install.packages("randomForest")
install.packages("ada")

# Load libraries
library(randomForest)
library(ada)
library(caret)
library(ggplot2)

data(diamonds)

# Create a binary target variable
diamonds$price_category <- ifelse(diamonds$price > median(diamonds$price), "High", "Low")
diamonds$price_category <- as.factor(diamonds$price_category)

# Split the data into training and test sets
set.seed(234)
train_index4 <- createDataPartition(diamonds$price_category, p = 0.8, list = FALSE)
train_data4 <- diamonds[train_index4, ]
test_data4 <- diamonds[-train_index4, ]

# Build the Random Forest model
rf_model <- randomForest(
  price_category ~ carat + cut + color + clarity + depth + table + x + y + z,
  data = train_data4,
  ntree = 500,
  importance = TRUE
)

# Evaluate Random Forest on the test set
rf_predictions <- predict(rf_model, newdata = test_data4)
rf_confusion <- confusionMatrix(rf_predictions, test_data4$price_category)
print(rf_confusion)


# Ensure column names are syntactically valid
##adaboost confusion was not creating because "y.1" not found
##looked it up said re define data names
# Ensure 'price_category' is a factor
# Ensure price_category is a factor
# Ensure price_category is a factor
library(ada)
library(caret)

# Ensure price_category is a factor in both train and test datasets
train_data4$price_category <- as.factor(train_data4$price_category)
test_data4$price_category <- as.factor(test_data4$price_category)

# Match levels of price_category in test data to those in train data
test_data4$price_category <- factor(test_data4$price_category, levels = levels(train_data4$price_category))

# Ensure column alignment between train and test datasets
required_columns <- colnames(train_data4)
test_data4 <- test_data4[, required_columns, drop = FALSE]

# Train AdaBoost Model 
##this take like over 10 minutes to run fully
ada_model <- ada(
  price_category ~ carat + cut + color + clarity + depth + table + x + y + z,
  data = train_data4,
  iter = 500
)

###I KEEP GETTING AN "Error in eval(predvars, data, env) : object 'y.1' not found" error when i try and run my ada predictions
## i have tried everything that i can think of and what online is telling me
#####if its i am able to predict and eval my ada model below here i fixed it

# Output AdaBoost model details
print(ada_model)

# Make predictions on the test dataset
ada_predictions <- predict(ada_model, newdata = test_data4)

#ok i canNOT figure out what is wrong but here is the code anyways
####ive been working on this for 5 days and i need to be over this error message that makes no sense

# Evaluate AdaBoost model performance
ada_confusion <- confusionMatrix(ada_predictions, test_data4$price_category)

# Print the confusion matrix
print("AdaBoost Confusion Matrix:")
print(ada_confusion)

###I hope that maybe randomly the error will go away :(
