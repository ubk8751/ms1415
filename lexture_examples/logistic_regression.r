set.seed(10)
library(datasets)
data(iris)
data <- iris
summary(data)
head(data)
# data frame with 150 observations and 5 variables
# the first 4 variables give information about plant
# attributes in centimeters
# the last one give us the name of plant species
levels(data$Species) # 3 levels
par(mar = c(7, 5, 1, 1)) # more space to labels
boxplot(data, las = 2)
sum(is.na(data)) # checking if the data set has NA
# No missing data in this data set!
data1 <- data[1:100, ] # excluding the last
# 50 observations (virginica)
# binary study
samp <- sample(1:100, 80)
data_test <- data1[samp, ] # creating test data (80%)
data_control <- data1[-samp, ]
y <- data_test$Species
x <- data_test$Sepal.Length
fit1 <- glm(y ~ x, family = "binomial")
summary(fit1)
plot(fit1)
newdata <- data.frame(x = data_control$Sepal.Length)
predicted_val <- predict(fit1, newdata,
    type = "response"
)
prediction <- data.frame(
    data_control$Sepal.Length,
    data_control$Species, predicted_val
)
prediction
x_bi <- rep(0, 20)
for (i in 1:20)
{
    if (data_control$Species[i] ==
        "versicolor") {
        x_bi[i] <- 1
    }
}
plot(round(prediction[, 3]))
points(x_bi, pch = 3)
