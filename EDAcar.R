data <- read.csv("CarPrice_Assignment.csv")
library(stringr)
# Library
library(ggplot2)
library(dplyr)
#print number of columns
print(ncol(data))
#Get the column names
colnames(data)

#delete first column(Delete the Id's)
#data <- data[,-c(1,2)]
data <- data[,3:26]
#Delete all characters after whitespace in Brand
data$Brand = sub(" .*", "", data$CarName)
data$Brand <- str_replace(data$Brand, "maxda", "mazda")
data$Brand <- str_replace(data$Brand, "nissan", "Nissan")
data$Brand <- str_replace(data$Brand, "porcshce", "porsche")
data$Brand <- str_replace(data$Brand, "toyouta", "toyota")
data$Brand <- str_replace(data$Brand, "vokswagen", "volkswagen")
data$Brand <- str_replace(data$Brand, "vw", "volkswagen")

#Erase brands:
data <- data[,-1]


#In our dataset which car brand did we have most of?
ggplot(data = data, 
       mapping = aes(x = Brand, fill = Brand)) +
  geom_bar() +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

#It seems that it is toyota


#Q: Which Brand Makes the most sports car? We consider a sports car to have two doors. 
ggplot(data = data, 
       mapping = aes(x = Brand, fill = doornumber)) +
  geom_bar() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# library
library(ggplot2)

# The mtcars dataset is natively available in R
#head(mpg)

#Which carbody has the best hwympg
ggplot(data, aes(x=carbody, y=highwaympg)) + 
  geom_boxplot(color="blue", fill="red")


#Which carbody has the best citympg
ggplot(data, aes(x=carbody, y=citympg)) + 
  geom_boxplot(color="red", fill="blue")

