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

# Visualize results for the best-performing model with a scatterplot
# Assuming RBF kernel performed best; use Alcohol and Malic as example axes
best_model <- if (svm_rbf_eval$accuracy > ksvm_rbf_eval$accuracy) svm_rbf else ksvm_rbf
wine$Predicted <- predict(best_model, wine[, -1])

KernelPlot <- ggplot(wine, aes(x = Alcohol, y = Malic, color = as.factor(Predicted))) +
  geom_point(alpha = 0.7) +
  labs(title = "SVM Classification Results (RBF Kernel)", color = "Predicted Type") +
  theme_minimal()
print(KernelPlot)
