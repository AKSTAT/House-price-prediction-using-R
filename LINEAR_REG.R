## linear regression

library(MASS)

?Boston

Boston = MASS::Boston
names(Boston)

set.seed(2) # Randomization
library(caTools)

split = sample.split(Boston$medv, SplitRatio = 0.7) ## dividing the data into 70:30 Ratio
split

training_data = subset(Boston, split =="TRUE")
testing_data = subset(Boston, split =="FALSE")
str(training_data)
str(testing_data)

library(lattice) ## Scatter plot matrix
splom(~Boston[c(1:6,14)], groups = NULL, data = Boston, axis.line.tck = 0, axis.text.alpha = 0)

cr = cor(Boston)

library(corrplot)
corrplot(cr, type = "full")


##VIF
library(car)
model = lm(medv~. , data = training_data)
vif(model)

#create vector of VIF values
vif_values <- vif(model)

#create horizontal bar chart to display each VIF value
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")
abline(v = 5, lwd = 3, lty = 2)


summary(model)

## implementation of model 

model = lm(medv~crim+zn+chas+nox+rm+dis+rad+ptratio+black+lstat, data = training_data)
summary(model)

model = lm(medv~crim+zn+chas+nox+rm+dis+ptratio+black+lstat, data = training_data)
summary(model)

prd = predict(model, testing_data)


plot(testing_data$medv, type = "l", col = "green" )
lines(prd, type = "l", col = "blue")

## predicting the sample data

# Random value
sample_data = data.frame(crim = 0.23899, zn = 13.7,chas = 0, 
                         nox = 0.479, rm = 6.9989, dis = 5.976, ptratio = 16.9, 
                         black = 390.6, lstat = 6.1)
sample_data

pred = predict(model, sample_data)
pred



