getwd()
## problem Statement: Predict the Mpg value.
######################################################
# Read the data
######################################################
carmpg_data <- read.csv('carmpg.csv')
View(carmpg_data)

######################################################
# Exploratory Data Analysis
######################################################

######################################################
#       Understand the Structure of the data
######################################################
summary(carmpg_data)
str(carmpg_data)
carmpg_data$cylinders = factor(carmpg_data$cylinders) #Converting from Int to Factors since its not a continuous variable
carmpg_data$horsepower = as.numeric(carmpg_data$horsepower) # HP should be a numeric
carmpg_data$model.year = as.factor(carmpg_data$model.year) # It should be factor
carmpg_data$origin = factor(carmpg_data$origin) # should be factor
carmpg_data$car.name = as.character(carmpg_data$car.name) # should be character
str(carmpg_data) # Checking structure of the data again

######################################################
#      Univariate Analysis
######################################################
#Here, Mpg is our target variable. Lets do univariate analysis for this.
hist(carmpg_data$mpg) # can be observed that Right-skewed. Longer tale in the right. 
summary(carmpg_data$mpg)
# Checking Skewness & Kurtosis to find mpg variable is normally distributed or not
# install.packages('e1071')
library(e1071)
skewness(carmpg_data$mpg) # +ve value indicates that it is right skewed
kurtosis(carmpg_data$mpg) # -ve value indicates that, the peak is less than ideal normal distribution for mpg


######################################################
#      Bivariate Analysis
######################################################
# Relationship b/w dependent and each of the independent variables
# mpg vs cylinders.. Boxplot is better option, if we have categorical/factor as independent variable
library(ggplot2)
ggplot(data = carmpg_data, aes(x=cylinders, y= mpg, color=cylinders))+
  geom_boxplot()  ## Potential Outliers Present 

# mpg vs displacement
ggplot(data = carmpg_data, aes(x=displacement, y= mpg))+
  geom_point(color = 'blue')+
  geom_smooth(se=FALSE,method='lm', color='red' ) ## Non linear trend, might be some outliers, may need some data transformations
 
# mpg vs horsepower
ggplot(data = carmpg_data, aes(x=horsepower, y= mpg, color=cylinders))+
  geom_point()+
  geom_smooth(se=FALSE,method='lm', color='red' ) # May need to divide the hp (<50 or >50) before feeding the model

# mpg vs weight
ggplot(data = carmpg_data, aes(x=weight, y = mpg))+
  geom_point(color = 'blue')+
  geom_smooth(se=FALSE, method='lm', color='red') # Non Linearity. Exponentially decreasing trend. May need data transformation

# mpg vs accelaration
ggplot(data = carmpg_data, aes(x=acceleration, y = mpg))+
  geom_point(color = 'blue')+
  geom_smooth(se=FALSE, method='lm', color='red') # Heteroscadacity, Non Linearity, Fanning effect

# mpg vs model.year
ggplot(data = carmpg_data, aes(x=model.year, y = mpg))+
  geom_boxplot(color = 'blue') # Increasing trend, nothing much to worry

# mpg vs origin
ggplot(data = carmpg_data, aes(x=origin, y = mpg, color = origin ))+
  geom_boxplot() # Some potential outliers for origin = 2


######################################################
#      Objective Bivariate Analysis
######################################################

str(carmpg_data)
names(carmpg_data)
# Identify numeric variables - mpg, displacement, horsepower, weight, acceleration
sub_dataset = carmpg_data[, c('mpg', 'displacement', 'horsepower', 'weight', 'acceleration')]
# Pairplot
library(GGally)
ggpairs(sub_dataset)
corrmat = cor(sub_dataset)
library(corrplot)
corrplot.mixed(corrmat)
# High Correlation observed between displacement & Weight
# Mild correlation between displacement & acceleration

######################################################
# Data Preprocessing
######################################################

######################################################
#       Scaling
######################################################
# R has taken care of this. So no need to do anything here.

sum(is.na(carmpg_data$mpg))
sapply(carmpg_data, function(x) sum(is.na(x))) # Checking Missing values for each column. No null values present.

######################################################
# Splitting data into training & test sets
######################################################
library(caTools)
set.seed(567)
split <- sample.split(carmpg_data$mpg, SplitRatio = 0.7)
train_data <- subset(carmpg_data, split == TRUE)
test_data <- subset(carmpg_data, split == FALSE)

# Car.name has only car names. It doesn't help us to find any metrics. So, we can ignore this.
train_data$car.name = NULL
test_data$car.name = NULL

######################################################
# First Cut Model
######################################################
first_regressor <- lm(mpg ~ ., data = train_data)
y_pred = predict(first_regressor, newdata = test_data)

######################################################
# Test the Assumptions
######################################################

######################################################
#      Assumption 1 - Linearity(residuals vs fits)
######################################################
# Here, y_pred is on Test data. But first_regressor is for train data. So, draw a plot
# b/w residuals(test data) vs fits(y_pred) which is on same data(here, test data we considered)
test_data_regressor <- lm(mpg ~ ., data = test_data)
residuals <- resid(test_data_regressor)
ggplot(aes(x=y_pred, y = residuals), data = test_data)+
  geom_point()+
  geom_smooth(se=FALSE, method='lm',color='red')
# We can use standardized residuals VS fits plot
library(MASS)
test_data$standres = stdres(test_data_regressor)
ggplot(aes(x=y_pred, y = standres), data = test_data)+
  geom_point()+
  geom_smooth(se=FALSE, method='lm',color='red')
# Non -Linear distribution, heteroscedacity, Potential outliers
# Since we have the above issues, we need to perform data transformations.

######################################################
# Data Transformation
######################################################
# Lets transform the mpg
hist(carmpg_data$mpg)
skewness(carmpg_data$mpg) # Normality issue
kurtosis(carmpg_data$mpg) # Normality Issue
# Perform Log transformation (Y-transformation) since we have normality problem
carmpg_data$mpg = log(carmpg_data$mpg)
hist(carmpg_data$mpg)
skewness(carmpg_data$mpg)
kurtosis(carmpg_data$mpg)

###########################################################################################
###########################################################################################


######################################################
# Repeat Bivariate Analysis  (Post Transformation)
######################################################
# Relationship b/w dependent and each of the independent variables
# mpg vs cylinders.. Boxplot is better option, if we have categorical/factor as independent variable
library(ggplot2)
ggplot(data = carmpg_data, aes(x=cylinders, y= mpg, color=cylinders))+
  geom_boxplot()  ## Potential Outliers Present 

# mpg vs displacement
ggplot(data = carmpg_data, aes(x=displacement, y= mpg))+
  geom_point(color = 'blue')+
  geom_smooth(se=FALSE,method='lm', color='red' ) 

# mpg vs horsepower
ggplot(data = carmpg_data, aes(x=horsepower, y= mpg, color=cylinders))+
  geom_point()+
  geom_smooth(se=FALSE,method='lm', color='red' ) # May need to divide the hp (<50 or >50) before feeding the model

# mpg vs weight
ggplot(data = carmpg_data, aes(x=weight, y = mpg))+
  geom_point(color = 'blue')+
  geom_smooth(se=FALSE, method='lm', color='red') # better Linearity than before. Exponentially decreasing trend. 

# mpg vs accelaration
ggplot(data = carmpg_data, aes(x=acceleration, y = mpg))+
  geom_point(color = 'blue')+
  geom_smooth(se=FALSE, method='lm', color='red') # Heteroscadacity, Non Linearity, Fanning effect

# mpg vs model.year
ggplot(data = carmpg_data, aes(x=model.year, y = mpg))+
  geom_boxplot(color = 'blue') # Increasing trend, nothing much to worry

# mpg vs origin
ggplot(data = carmpg_data, aes(x=origin, y = mpg, color = origin ))+
  geom_boxplot() # Some potential outliers for origin = 2

######################################################
# Splitting data into training & test sets 
# since there is a data transformation for mpg variable
######################################################
library(caTools)
set.seed(567)
split <- sample.split(carmpg_data$mpg, SplitRatio = 0.7)
train_data <- subset(carmpg_data, split == TRUE)
test_data <- subset(carmpg_data, split == FALSE)

train_data$car.name = NULL
test_data$car.name = NULL

######################################################
# Second Cut Model
######################################################
second_regressor <- lm(mpg ~ ., data = train_data)
y_pred2 = predict(second_regressor, newdata = test_data)
y_pred_train = predict(second_regressor,newdata = train_data)
######################################################
# Re-Test the Assumptions
######################################################

######################################################
#      Assumption 1 - Linearity(residuals vs fits)    -- Valid
######################################################
# We can perform the residulas vs fits plot on train data too. But be consistent with the axis
# If we are using y_pred from train data, then we should use residuals from traindata. 

# We can use standardized residuals VS fits plot
library(MASS)
# (residuals vs fits) on Train Data
standres2 = stdres(second_regressor)
ggplot(aes(x=y_pred_train, y = standres2), data = train_data)+
  geom_point() +
  geom_smooth(se=FALSE, method='lm',color='red')
# Linear distribution, No - heteroscedacity, No Potential outlier

######################################################
#      Assumption 2 - Independence(residuals vs Order plot)  -- Valid
######################################################
# Durbin Watson test
# Ideal range of D-w Statistic value must be 1.5 to 2.5
library(car)
durbinWatsonTest(second_regressor)

######################################################
#      Assumption 3 - Normality(QQ Plot)              -- Valid
######################################################
# Multiple ways of checking whether it has normality or not.
# Plot Histogram, Check Skewness, kurtosis , Or plot QQPLOT
hist(resid(second_regressor))
skewness(resid(second_regressor))
kurtosis(resid(second_regressor))
# Otherway to check the normality using qqPlot
qqPlot(resid(second_regressor))

######################################################
#      Assumption 4 - Equal Variance                   -- Valid
######################################################
# Using Stdres vs Fitted plot and check any fanning or funneling effect
standres2 = stdres(second_regressor)
ggplot(aes(x=y_pred_train, y=standres2), data=train_data)+
  geom_point()+
  geom_smooth(se=FALSE, method='lm',color='red')
# No Prominent heteroscedacity, fanning & funneling effect observed

######################################################
# Multicolinearity 
# VIF >= 4, Moderate Multicolinearity
# VIF >= 10, Serious Multicolinearity
######################################################
vif(second_regressor) # Displacement & Weight has Multicollinearity.

# So we need to drop one of the column. In this case, we are dropping Displacement.
carmpg_data$displacement <- NULL
carmpg_data$car.name <- NULL
# Resplit into training and test sets
library(caTools)
set.seed(567)
split <- sample.split(carmpg_data$mpg, SplitRatio = 0.7)
train_data <- subset(carmpg_data, split == TRUE)
test_data <- subset(carmpg_data, split == FALSE)

# Rebuild the model
third_regressor <-  lm(mpg ~ ., data=train_data)
vif(third_regressor) # Now, the values are <=4


######################################################
# Influential Points
# Cooks Distance: < 0.5
# DFFITs
######################################################
plot(third_regressor)

train_data$leverages = cooks.distance(third_regressor)
max(train_data$leverages) # No Major influential points

# Predict on test data
y_pred3 = predict(third_regressor, newdata = test_data)
summary(third_regressor)
# The multiple R-squared value is > 90%. Which is quite good.
# horsepower is junk. To make our model better, we can remove this variable.
anova(third_regressor)

