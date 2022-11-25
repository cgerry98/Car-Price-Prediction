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


library(GGally)
#ggpairs(data)
#Check multicollinearity: If two variables what pattern that shows us that two variables have multicollinearity





#load reshape2 package

library(reshape2)

# creating correlation matrix
corr_mat <- round(cor(data),2)

# reduce the size of correlation matrix
melted_corr_mat <- melt(corr_mat)
head(melted_corr_mat)

# plotting the correlation heatmap
library(ggplot2)
p.dia=ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                         fill=value)) +
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value),
            color = "black", size = 4)

p <- p.dia +theme(axis.text.x= element_text(angle=90, size=9))
p
model <- lm(price ~., data = data)
summary(model)
#Highly correlated predictors:(compression ratio,fueltype)
#(citympg,highwaympg),(curveweight,horsepower),
#load the car library
library(car)


#Use stepwiseRegression backwards
step(model,direction = "backward")
#AIC=3241.3

model2 = lm(price ~ CarName + fueltype + drivewheel + enginelocation + 
              wheelbase + carwidth + enginetype + cylindernumber + enginesize + 
              stroke + compressionratio + horsepower + peakrpm, data = data)

summary(model2)
#Adjusted R-squared:  0.8921
#model2 is our best model so far. 






# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      -5.353e+04  1.083e+04  -4.943 1.68e-06 ***
#   CarName           1.320e+02  3.675e+01   3.592 0.000417 ***
#   fueltype          9.242e+03  4.385e+03   2.108 0.036360 *  
#   drivewheel        1.149e+03  4.009e+02   2.867 0.004613 ** 
#   enginelocation    1.119e+04  1.855e+03   6.035 8.11e-09 ***
#   wheelbase         1.926e+02  6.040e+01   3.189 0.001670 ** 
#   carwidth          5.208e+02  1.994e+02   2.611 0.009735 ** 
#   enginetype       -6.094e+02  1.706e+02  -3.572 0.000448 ***
#   cylindernumber    1.377e+03  2.421e+02   5.688 4.77e-08 ***
#   enginesize        9.123e+01  1.124e+01   8.117 5.74e-14 ***
#   stroke           -2.147e+03  7.372e+02  -2.913 0.004013 ** 
#   compressionratio -5.056e+02  3.161e+02  -1.600 0.111314    
# horsepower        2.362e+01  1.219e+01   1.937 0.054250 .  
# peakrpm           1.749e+00  5.748e-01   3.043 0.002672 ** 










# We want to check interaction terms

library(MASS)


# We want to check if the model2 check the LINE checkmarks.
#plot(model2)


#we can check for potential significant interaction terms before doing any transformations.


add1(model2, ~. + fueltype*CarName + drivewheel*CarName + enginelocation*CarName +
       wheelbase*CarName + carwidth*CarName + enginetype*CarName + cylindernumber*CarName + enginesize*CarName +
       stroke*CarName + compressionratio*CarName + horsepower*CarName + peakrpm*CarName, test = 'F')
#The results are amazing to see, I would have expected for all interactions to be significant
#but based on the interaction terms only wheelbase,carwidth,enginetype,cylindernumber,enginesize,horsepower are
#significant. 

#SHort: We expected carname to have significant interation with all predictors.

#We decide to keep all the significant ones

# model3 = update(model2, ~.+CarName:wheelbase+CarName:carwidth +CarName:enginetype+CarName:cylindernumber+CarName:enginesize+CarName:horsepower)
# summary(model3)
#We had higher adjR^2 but more p-values that are less signigficant
#Adjusted R-squared:  0.9095 

# model3 = update(model2, ~.+CarName:wheelbase+CarName:carwidth +CarName:enginetype+CarName:cylindernumber+CarName:enginesize+CarName:horsepower)
# summary(model3)
#Adjusted R-squared:  0.9095, more significant individual values
#9 predictors are not significatn

# model3 = update(model2, ~.+CarName:wheelbase+CarName:carwidth +CarName:enginetype+CarName:cylindernumber+CarName:horsepower)
# summary(model3)
#Adjusted R-squared:  0.9095 , but more p-values that are less signigficant
##9 predictors are not significatn

model3 = update(model2, ~.+CarName:cylindernumber)
summary(model3)
#We decide to keep this one, not much difference and we have more significant values.
#We can remove compression ratio
#model4 = update(model3, ~.-compressionratio)
#model5 = update(model4,~.-horsepower)
#summary(model4)
#summary(model5)
#Our adj R^2 went down, we keep compression ratio.
#we keep model3 as our best model since we think compression ration is a good predictor of price.
summary(model3)


plot(model3)


#In the residual vs Fitted plot we see a fanning pattern
#In the QQ plot we see a non normality.

library(broom)
#Another way to plot these graphs are:
mod_table = augment(model3)
names(model3)
ggplot(mod_table, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, colour = 'blue') +
  labs(x = 'Fitted Values', y = 'Residuals') +
  ggtitle('Residual vs Fit') +
  theme_bw()
#Here we see a fanning pattern
#We need to transform the y-variable
ggplot(mod_table, aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle('Normal Q-Q Plot') +
  theme_bw()

shapiro.test(resid(model3))
#The shapiro test has a 𝑝-value smaller than 0.05, and we reject 𝐻0
# . We conclude that the residuals don’t
# follow a normal distribution.


#We can try box-cox method

library(car)
mod.boxcox = boxCox(model3, lambda = seq(-1, 1, length=10))
lambda.opt = mod.boxcox$x[which.max(mod.boxcox$y)] #find the optimal lambda
lambda.opt

# model4 = lm((price^lambda.opt) ~ CarName + fueltype + drivewheel + enginelocation +
#               data$wheelbase + data$carwidth + enginetype + cylindernumber + data$enginesize +
#               data$stroke + data$compressionratio + data$horsepower + data$peakrpm + CarName:cylindernumber,
#             data = data)



model4 = lm((price^lambda.opt) ~ CarName + fueltype + drivewheel + enginelocation +
              wheelbase + carwidth + enginetype + cylindernumber + enginesize +
              stroke + compressionratio + horsepower + peakrpm + CarName:cylindernumber,
            data = data)
summary(model4)
plot(model4)



  
#The adj-R^2 slightly decreased but it fixed our nonconstant variance and non-normality.
#Adjusted R-squared:  0.8926 

#ck.dist = cooks.distance(mod.4)
plot(model4, which = 4)
rs = rstandard(model4)
which(rs > 3)
#We see no influential points






#Second research question. Does the effect of carname number on the cylinder number
#effectiveness depend on carname?
  
reduced = lm((price^lambda.opt) ~ CarName + fueltype + drivewheel + enginelocation +
               wheelbase + carwidth + enginetype + cylindernumber + enginesize +
               stroke + compressionratio + horsepower + peakrpm ,
             data = data)

anova(reduced,model4)

#yes, since p-value is <.05

#Confidence Interval.

new = data.frame(enginesize = 100,cylindernumber=4)
ans = predict(model4, data, se.fit = TRUE, interval = "confidence", level = 0.99)
ans



#Prediction interval

new = data.frame(cylindernumber=4)
ans = predict(model4, data, se.fit = TRUE, interval = "prediction", level = 0.99)
ans






#On average, what is the most expensive car brand?
  
# Here we notice that on average the most expensive carbrand seems to be buick.
# Second seems to be jaguar.
# 
# The least expensive known brand is volkswagen 




# how much variation in (car price)^(boxcox) is explained by our final model?

summary(model4)$r.squared

#ans:0.9000149

#How will the change in enginesize affect the change in car price after controlling with carname, fueltype, drivewheel, enginelocation,wheeelbase,carwidth,enginetype,cylindernumber,stroke,compressionaration,horsepowe,peakrpm,and the interaction between carname and cylindernumber

  
# Explanation: after controlling with carname, fueltype, drivewheel, enginelocation,wheeelbase,carwidth,enginetype,cylindernumber,stroke,compressionaration,horsepowe,peakrpm,and the interaction between carname and cylindernumber, when the enginesize increases by 1 unit, then car price is expected to change by 0.00059. We are 95% confident that the amount of change is
# between 0.728 cm and 1.317 cm.


