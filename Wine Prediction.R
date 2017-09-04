# Libraries
library(ggplot2)
library(corrplot)
library(caTools)
library(class)
library(caret)

# Importing the dataset
dataset <- read.csv('Wine.csv', header=TRUE)

# Changing dependent variable to factor
dataset$Wine = as.factor(dataset$Wine) 

# Renaming a column
colnames(dataset)[13] <- 'OD_280_315'

# Since, all the rows are ordered by the class of the wine, we will randomize the dataset row-wise
dataset <- dataset[sample(nrow(dataset)),]

# Summary statistics
summary(dataset)

# Correlation matrix
M <- cor(dataset[2:14])
corrplot(M, method='ellipse')

# The plot shows high positive correlation between total phenols-flavanoids, OD_280_315-flavanoids
# However, most other variables do not have a strong correlation with each other

# Visualizing the data to detect patterns
g <- ggplot(data=dataset)
g + geom_point(aes(x=OD_280_315, y=Total.phenols, color=Wine))

g + geom_point(aes(x=Total.phenols, y=Flavanoids, color=Wine))

g + geom_point(aes(x=Hue, y=Color.intensity, color=Wine))

g + geom_histogram(aes(Proline, fill=Wine),binwidth = 100, position='dodge')
g + geom_boxplot(aes(x=Wine, y=Proline))

g + geom_boxplot(aes(x=Wine, y=Magnesium))

g + geom_point(aes(x=Ash, y=Alcalinity.of.ash, color=Wine), alpha = 0.7)

g + geom_point(aes(x=Malic.acid, y=Alcohol, color=Wine), alpha = 0.7)

g + geom_point(aes(x=Flavanoids, y=Proanthocyanins, color=Wine), alpha = 0.7)

# Splitting the dataset into training and test set
set.seed(11)
data_split = sample.split(dataset$Wine, SplitRatio = 0.70)
training_set = subset(dataset, data_split == TRUE)
test_set = subset(dataset, data_split == FALSE)

# Scale the training and test set
training_set[-1] = scale(training_set[-1])
test_set[-1] = scale(test_set[-1])

# 5 fold cross validation
folds = createFolds(training_set$Wine, k = 5)

set.seed(11)
performance = lapply(folds, function(x) {
  
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  classifier = knn(train = training_fold[, -1],
                   test = test_fold[, -1], 
                   cl = training_fold[, 1],
                   k = 5)
  
  confusion_matrix <- table(test_fold[, 1],  classifier)
  accuracy <- (confusion_matrix[1,1] + confusion_matrix[2,2] + confusion_matrix[3,3])/
    (confusion_matrix[1,1] + confusion_matrix[1,2] + confusion_matrix[1,3] +
     confusion_matrix[2,1] + confusion_matrix[2,2] + confusion_matrix[2,3] +
     confusion_matrix[3,1] + confusion_matrix[3,2] + confusion_matrix[3,3])
})


# k-fold cross validation using a for loop

# loop_acc <- list()


# for (x in 1:length(folds)) {
# 
#   training_fold = training_set[-folds[[x]], ]
#   test_fold = training_set[folds[[x]], ]
#   classifier = knn(train = training_fold[, -1],
#                    test = test_fold[, -1],
#                    cl = training_fold[, 1],
#                    k = 5)
# 
#   confusion_matrix <- table(test_fold[, 1],  classifier)
#   loop_acc[[x]] <- (confusion_matrix[1,1] + confusion_matrix[2,2] + confusion_matrix[3,3])/
#       (confusion_matrix[1,1] + confusion_matrix[1,2] + confusion_matrix[1,3] +
#        confusion_matrix[2,1] + confusion_matrix[2,2] + confusion_matrix[2,3] +
#        confusion_matrix[3,1] + confusion_matrix[3,2] + confusion_matrix[3,3])
# }



# Model a k-nn classifier
knn_classifier = knn(train = training_set[, -1],
                     test = test_set[, -1], 
                     cl = training_set[, 1],
                     k = 5)

# Confusion Matrix
conmatrix = table(test_set[, 1], knn_classifier)



# Scale the data before we perform PCA
wine_scaled <- scale(dataset[,-1])

# PCA using the prcomp available in base R
wine_PCA_1 <- prcomp(wine_scaled)

# A scatter plot using the first two PCA gives us a much better view of the three different types of wines in the dataset
plot(x=wine_PCA_1$x[,1], y=wine_PCA_1$x[,2], type='p', col=dataset$Wine, xlab='PC1', ylab='PC2')

# The screeplot tells us how many principal components explain majority of the variance in the data
screeplot(wine_PCA_1, npcs = 13, type=c('lines'), ylim = c(0, 5))


