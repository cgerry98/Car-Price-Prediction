set.seed(123)
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
# cross - validation methods
library(gbm)
library(caret)
###########
#Train and test
###########


set.seed(123)           # set seed for generating random data.

# createDataPartition() function from the caret package to split the original dataset into a training and testing set and split data into training (80%) and testing set (20%)
parts = createDataPartition(data$price, p = .7, list = F)
train = data[parts, ]
test = data[-parts, ]


test_x = test[, -24] # feature and target array
test_y = test[, 24] 



model_gbm = gbm(train$price ~.,
                data = train,
                distribution = "gaussian",
                cv.folds = 10,
                shrinkage = .01,
                n.minobsinnode = 10,
                n.trees = 500)

print(model_gbm)

# gbm(formula = train$price ~ ., distribution = "gaussian", data = train, 
#     n.trees = 500, n.minobsinnode = 10, shrinkage = 0.01, cv.folds = 10)
# A gradient boosted model with gaussian loss function.
# 500 iterations were performed.
# The best cross-validation iteration was 500.
# There were 23 predictors of which 18 had non-zero influence.

summary(model_gbm)

# var     rel.inf
# enginesize             enginesize 37.54572227
# curbweight             curbweight 17.52868291
# carwidth                 carwidth 10.84081338
# horsepower             horsepower  7.86436514
# citympg                   citympg  6.64150186
# CarName                   CarName  4.08601848
# highwaympg             highwaympg  3.88020382
# cylindernumber     cylindernumber  3.01859650
# wheelbase               wheelbase  2.59586550
# carlength               carlength  2.01994427
# drivewheel             drivewheel  1.64545395
# boreratio               boreratio  1.34802925
# peakrpm                   peakrpm  0.54216272
# compressionratio compressionratio  0.16354983
# carbody                   carbody  0.13003488
# stroke                     stroke  0.09690767
# fuelsystem             fuelsystem  0.02896848
# carheight               carheight  0.02317910
# fueltype                 fueltype  0.00000000
# aspiration             aspiration  0.00000000
# doornumber             doornumber  0.00000000
# enginelocation     enginelocation  0.00000000
# enginetype             enginetype  0.00000000

pred_y = predict.gbm(model_gbm, test_x)
pred_y

residuals = test_y - pred_y
RMSE = sqrt(mean(residuals^2))
print(RMSE)
#2442.834
