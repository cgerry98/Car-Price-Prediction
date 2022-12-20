###############################
#Random Forest
###############################
#read data
set.seed(123)
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





#We are going to find unique values in R columns.
table(data["fueltype"])


# Create dummy variable
data$fueltype <- ifelse(data$fueltype == "gas", 0, 1)
#Check to see if it worked
table(data["fueltype"])



#We look for the unique values


#check for unique values in aspiration
table(data["aspiration"])
#Baseline is:
# Create dummy variable
data$aspiration <- ifelse(data$aspiration == "std", 0, 1)

#Check if everything was done correctly

table(data["aspiration"])


#check for unique values in doornumber
table(data["doornumber"])

# Create dummy variable
data$doornumber <- ifelse(data$doornumber == "four", 0, 1)

#Check if everything was done correctly

table(data["doornumber"])


#check for unique values in aspiration
table(data["carbody"])


# Create dummy variable for cardoor
# Transforming feature cardoor from text to its numeric equivalent
data$carbody <- ifelse(data$carbody == "sedan", 0, ifelse(data$carbody == "hatchback",1,
                                                          ifelse(data$carbody == "wagon", 2,ifelse(data$carbody == "hardtop",3,4))))
#Check if everything was done correctly

table(data["carbody"])


#check for unique values in drivewheel
table(data["drivewheel"])
data$drivewheel <- ifelse(data$drivewheel == "fwd", 0, ifelse(data$drivewheel == "rwd",1,2))
#Check if everything was done correctly
table(data["drivewheel"])



#check for unique values in drivewheel
table(data["enginelocation"])
data$enginelocation <- ifelse(data$enginelocation == "front", 0, 1)
#Check if everything was done correctly

table(data["enginelocation"])


#check for unique values in enginetype
table(data["enginetype"])
data$enginetype <- ifelse(data$enginetype == "ohc", 0,ifelse(data$enginetype == "ohcf",1,ifelse(data$enginetype == "ohcv",2,ifelse(data$enginetype == "dohc",3,ifelse(data$enginetype == "l",4,ifelse(data$enginetype == "rotor",5,6))))))
#Check if everything was done correctly

table(data["enginetype"])




#check for unique values in cylindernumber
table(data["cylindernumber"])
data$cylindernumber <- ifelse(data$cylindernumber == "four", 0,ifelse(data$cylindernumber == "six",1,ifelse(data$cylindernumber == "five",2,ifelse(data$cylindernumber == "eight",3,ifelse(data$cylindernumber == "two",4,ifelse(data$cylindernumber == "twelve",5,6))))))
#Check if everything was done correctly

table(data["cylindernumber"])




#check for unique values in fuelsystem
table(data["fuelsystem"])
data$fuelsystem <- ifelse(data$fuelsystem == "mpfi", 0,ifelse(data$fuelsystem == "2bbl",1,ifelse(data$fuelsystem == "idi",2,ifelse(data$fuelsystem == "1bbl",3,ifelse(data$fuelsystem == "spdi",4,ifelse(data$fuelsystem == "4bbl",5,ifelse(data$fuelsystem == "mfi",6,7)))))))
#Check if everything was done correctly
table(data["fuelsystem"])


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




data$CarName <- ifelse(data$CarName == "alfa-romero", 18,
                       ifelse(data$CarName == "audi",12,ifelse(data$CarName == "bmw",11,
                                                               ifelse(data$CarName == "buick",10,ifelse(data$CarName == "chevrolet",16,
                                                                                                        ifelse(data$CarName == "dodge",9,ifelse(data$CarName == "honda",3,
                                                                                                                                                ifelse(data$CarName == "isuzu", 17,ifelse(data$CarName == "jaguar",19,
                                                                                                                                                                                          ifelse(data$CarName == "mazda",2,
                                                                                                                                                                                                 ifelse(data$CarName == "mitsubishi",4,ifelse(data$CarName == "Nissan",1,
                                                                                                                                                                                                                                              ifelse(data$CarName == "peugeot",7,ifelse(data$CarName == "plymouth", 13,
                                                                                                                                                                                                                                                                                        ifelse(data$CarName == "porsche",15,ifelse(data$CarName == "renault",20,
                                                                                                                                                                                                                                                                                                                                   ifelse(data$CarName == "saab",14,ifelse(data$CarName == "subaru",5,
                                                                                                                                                                                                                                                                                                                                                                           ifelse(data$CarName == "toyota",0,ifelse(data$CarName == "volkswagen",6,
                                                                                                                                                                                                                                                                                                                                                                                                                    ifelse(data$CarName == "volvo",8,21)))))))))))))))))))))


#Here I represented the car brands with a numeric value.

# toyota-0
# Nissan-1
# mazda-2
# honda- 3
# mitsubishi-4
# subaru-5
# volkswagen-6
# peugeot-7
# volvo-8
# dodge-9
# buick-10
# bmw-11
# audi-12
# plymoth-13
# saab-14
# porsche-15
# isuzu-16
# chevrolet-17
# alfa-romero-18
# jaguar-19
# renault-20
# mercury-21

#Factor
fueltype= factor(data$fueltype)
aspiration= factor(data$aspiration)
doornumber=factor(data$doornumber)
carbody=factor(data$carbody)
drivewheel=factor(data$drivewheel)
enginelocation = factor(data$enginelocation)
enginetype = factor(data$enginetype)
cylindernumber = factor(data$cylindernumber)
fuelsystem = factor(data$fuelsystem)
CarName =factor(data$CarName)

#Check to see if it worked
table(data["CarName"])



library(ggplot2)
library(cowplot)
library(randomForest)
#We need to specify the train(should be 2/3 of total data), the 1/3 gets used to use
#for the OOB

#####
# The above Mean Squared Error and Variance explained are calculated using Out of 
# Bag Error Estimation.In this 2/3 of Training data is used for training and the 
# remaining 1/3 is used to Validate the Trees. 



####




set.seed(123)

train<-sample(1:nrow(data),(2/3)*nrow(data))
model.test=data[-train ,"price"]


model <- randomForest(price ~ ., data=data,subset = train, proximity=TRUE)
model

# Call:
#   randomForest(formula = price ~ ., data = data, proximity = TRUE,      subset = train) 
# Type of random forest: regression
# Number of trees: 500
# No. of variables tried at each split: 7
# 
# Mean of squared residuals: 5192916
# % Var explained: 92.37


#notice here that there were 7 variables used, how would we know that that't the optimal 



#number of variables used to achieve the best % Var explained?
#Answer: we don't, we need to check:

plot(model)



yhat.bag = predict (model , newdata=data[-train ,])


plot(yhat.bag , model.test)
abline (0,1)
mean((yhat.bag -model.test)^2)
#Calculate MSE when we have 7 variables and 500 trees


#Check:

oob.err<-double(18)
test.err<-double(18)

#mtry is no of Variables randomly chosen at each split
for(mtry in 1:18) 
{
  rf=randomForest(price ~ . , data = data , subset = train,mtry=mtry,ntree=400) 
  oob.err[mtry] = rf$mse[400] #Error of all Trees fitted
  
  pred<-predict(rf,data[-train,]) #Predictions on Test Set for each Tree
  test.err[mtry]= with(data[-train,], mean( (price - pred)^2)) #Mean Squared Test Error
  
  cat(mtry," ")
  
}
test.err  
oob.err

matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))
# Now what we observe is that the Red line is the Out of Bag Error Estimates and the 
# Blue Line is the Error calculated on Test Set.Both curves are quiet smooth and the 
# error estimates are somewhat correlated too. The Error Tends to be minimized at around 
# mtry=4.


#Check which variables are important
varImpPlot(model)
library(caret)
varImp(model)

#From varImpPlot on Random Forest model, we can tell that engine size plays a significant
#role on our model.

# Overall
# CarName           135875906
# fueltype            7731345
# aspiration         17215635
# doornumber          6540625
# carbody            31033223
# drivewheel         60358778
# enginelocation     11825003
# wheelbase         289141821
# carlength         383530575
# carwidth          300677898
# carheight          51507057
# curbweight       1268534163
# enginetype          9538306
# cylindernumber    816671294
# enginesize       2628664571
# fuelsystem         15768031
# boreratio         110231501
# stroke             30966531
# compressionratio   79183325
# horsepower        944094197
# peakrpm            41074741
# citympg          1002241026
# highwaympg        714685237




#Check multicollinearity
#Recall that we move multicollinearity to improve interpretability
#city mpg and highwaympg are highlycorrelated
with(data,cor(citympg,highwaympg))

set.seed(123)
#Now we try each tree having 4 variables
model2 <- randomForest(price ~ ., data=data,subset = train,mtry=4, proximity=TRUE)
model2

# Call:
#   randomForest(formula = price ~ ., data = data, mtry = 4, proximity = TRUE,      subset = train) 
# Type of random forest: regression
# Number of trees: 500
# No. of variables tried at each split: 4
# 
# Mean of squared residuals: 5454399
# % Var explained: 91.99
#Varianced decreased

#We try the second mse
set.seed(123)
model3 <- randomForest(price ~ ., data=data,subset = train,mtry=9, proximity=TRUE)
model3

# Call:
#   randomForest(formula = price ~ ., data = data, mtry = 9, proximity = TRUE,      subset = train) 
# Type of random forest: regression
# Number of trees: 500
# No. of variables tried at each split: 9
# 
# Mean of squared residuals: 5072384
# % Var explained: 92.55


#The variability explained is better with 9 variables
#which is close to p/3 which is theoretically the optimal mtry for regression.
#see reference: https://arxiv.org/pdf/1804.03515.pdf


