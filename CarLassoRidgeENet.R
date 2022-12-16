library(MASS)  # Package needed to generate correlated precictors
library(glmnet)  # Package to fit ridge/lasso/elastic net models
library(fastDummies)
set.seed(1)

#read data
data <- read.csv("CarPrice_Assignment.csv")
library(stringr)

#print number of columns
print(ncol(data))
#Get the column names
colnames(data)

#delete first column(Delete the Id's)
data <- data[,2:26]

#Get the column names
colnames(data)
#print the first couple of column names
print(head(data))
#delete first column
data <- data[,2:25]
#print the first couple of column names
print(head(data))


#Check for missing values
sum(is.na(data))
print(head(data))

#Check for duplicated rows
sum(duplicated(data))
#Check the summary in the dataset

summary(data)
#We notice that the columns are character type?


str(data)



#Delete all characters after whitespace in CarName
data$CarName = sub(" .*", "", data$CarName)


#check for unique values in CarName
table(data["CarName"])

#We notice that some variables are misspelled.
#data$CarName <- ifelse(data$CarName == "maxda", "mazda",ifelse(data$CarName == "nissan","Nissan",ifelse(data$CarName == "porcshce",))
data$CarName <- str_replace(data$CarName, "maxda", "mazda")
data$CarName <- str_replace(data$CarName, "nissan", "Nissan")
data$CarName <- str_replace(data$CarName, "porcshce", "porsche")
data$CarName <- str_replace(data$CarName, "toyouta", "toyota")
data$CarName <- str_replace(data$CarName, "vokswagen", "volkswagen")
data$CarName <- str_replace(data$CarName, "vw", "volkswagen")


#Get the column names
table(data["CarName"])
str(data)

data <- dummy_cols(data, select_columns = c('fueltype',"aspiration","doornumber","carbody","drivewheel"
                                            ,"enginelocation","enginetype","cylindernumber","fuelsystem"
                                            ,"CarName"),remove_selected_columns = TRUE)

#######################################
#Multiple Linear Regression 70/30 split
#######################################

#make this example reproducible
set.seed(1)

#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.7,0.3))
train  <- data[sample, ]
test   <- data[!sample, ]


model = lm(price~.,data = train)
summary(model)


library(car)


#Use stepwiseRegression backwards
BestModel = step(model,direction = "backward")
plot(BestModel)
summary(BestModel)

vif(BestModel)
# library(glmtoolbox)
# gvif(BestModel)



test["predict"] = predict
mean((test$price - test$predict)^2)
#MSE=8442140


#SHapiroWilkDoe not work(possibly because of dummy variables)
# > shapiro.test(data)
# Error in shapiro.test(data) : is.numeric(x) is not TRUE



#Problem to fix next time: 
#VIF(handling categorical data, having categorical and numeric in the data mess up the VIF)
#Answer: We can ignore the VIF since we are just focusing on optimizing prediction.
#######################################
#Multiple Linear Regression k-fold
#######################################
library(caret)

set.seed(100)
train_control <- trainControl(method = "cv",
                              number = 10)
library(car)

model = lm(price~.,data = data)


# training the model by assigning sales column
# as target variable and rest other column
# as independent variable
model <- train(price ~., data = data,
               method = "lm",
               trControl = train_control)

# printing model performance metrics
# along with other details
print(model)
summary(model)
# RMSE      Rsquared  MAE     
# 2615.282  0.910171  1833.692
model$coefnames
model$finalModel
#Shows RMSE of each fold.
model$resample
#   RMSE  Rsquared      MAE Resample
# 1  2571.949 0.9600345 1409.852   Fold01
# 2  1592.792 0.9167473 1314.786   Fold02
# 3  2144.187 0.9316640 1635.998   Fold03
# 4  2596.428 0.9451005 1831.288   Fold04
# 5  2499.282 0.9275968 1917.402   Fold05
# 6  1803.568 0.9619681 1365.023   Fold06
# 7  2708.943 0.8997333 2031.603   Fold07
# 8  3308.298 0.8988438 2119.807   Fold08
# 9  3529.826 0.8103091 2241.447   Fold09
# 10 3397.549 0.8497128 2469.718   Fold10

################################################################################
#RUn before MUltiple Linear Regression and after Split data into train and test
################################################################################





###############################
#Split data into train and test
###############################

#####
#y
#####

y= data$price

#####
#x
#####
#Syntax to drop columns using %in% operator
x =data[ , !names(data) %in% c("price")]
x = data.matrix(x)
class(x)
n= 205
set.seed(100)
# Split data into train (2/3) and test (1/3) sets
train_rows <- sample(1:n, .66*n)
#choose a random vector of 660 elements from all rows in matrix dataset

x.train <- x[train_rows, ]
#Save that in x.train.
x.test <- x[-train_rows, ]
#those that weren't choosen put them in x.test


y.train <- y[train_rows]
y.test <- y[-train_rows]




# Fit models 
# (For plots on left):
#alpha=1 is the lasso penalty, and alpha=0 the ridge penalty
fit.lasso <- glmnet(x.train, y.train, family="gaussian", alpha=1)
fit.ridge <- glmnet(x.train, y.train, family="gaussian", alpha=0)
fit.elnet <- glmnet(x.train, y.train, family="gaussian", alpha=.5)







#We look at MSE to decide which model has the smallest mse(the smaller mse, means the better predicted).

##################################
#Here we test ou different alpha levels(the automatic version of the top)
set.seed(100)
list.of.fits <- list()
for (i in 0:10) {
  ## Here's what's going on in this loop...
  ## We are testing alpha = i/10. This means we are testing
  ## alpha = 0/10 = 0 on the first iteration, alpha = 1/10 = 0.1 on
  ## the second iteration etc.
  
  ## First, make a variable name that we can use later to refer
  ## to the model optimized for a specific alpha.
  ## For example, when alpha = 0, we will be able to refer to 
  ## that model with the variable name "alpha0".
  fit.name <- paste0("alpha", i/10)
  
  ## Now fit a model (i.e. optimize lambda) and store it in a list that 
  ## uses the variable name we just created as the reference.
  list.of.fits[[fit.name]] <-
    cv.glmnet(x.train, y.train, type.measure="mse", alpha=i/10, 
              family="gaussian")
}

## Now we see which alpha (0, 0.1, ... , 0.9, 1) does the best job
## predicting the values in the Testing dataset.
results <- data.frame()
for (i in 0:10) {
  fit.name <- paste0("alpha", i/10)
  
  ## Use each model to predict 'y' given the Testing dataset
  predicted <- 
    predict(list.of.fits[[fit.name]], 
            s=list.of.fits[[fit.name]]$lambda.1se, newx=x.test)
  
  ## Calculate the Mean Squared Error...
  mse <- mean((y.test - predicted)^2)
  
  ## Store the results
  temp <- data.frame(alpha=i/10, mse=mse, fit.name=fit.name)
  results <- rbind(results, temp)
}

## View the results
results  
  

# alpha     mse fit.name
# 1    0.0 5624242   alpha0
# 2    0.1 5647437 alpha0.1
# 3    0.2 6817455 alpha0.2
# 4    0.3 5843358 alpha0.3
# 5    0.4 6453820 alpha0.4
# 6    0.5 7793312 alpha0.5
# 7    0.6 6211833 alpha0.6
# 8    0.7 5543633 alpha0.7
# 9    0.8 6750061 alpha0.8
# 10   0.9 6033428 alpha0.9
# 11   1.0 5569688   alpha1

#Here we can see Elastic Net Regression did a better job at predicting the price of a car when
#Compared to MLR, Lasso, and ridge Regression.
