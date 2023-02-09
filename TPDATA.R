#installing packages
install.packages("tidyverse")
install.packages("neuralnet")
install.packages("GGally")
install.packages("keras")
install.packages("ggplot2")
install.packages("base")
install.packages("stringr")
install.packages("corrplot")


# load libs

library(plyr)
library(MASS)
library(readr)
library(dplyr)
library(caret)
library(tidyverse)
library(neuralnet)
library(GGally)
library(keras)
library(ggplot2)

#Loading data
setwd("C:/Users/hp/Desktop/archive (2)")
dtrain = read.csv("train.csv")

#this data set used for visualization
dtrain1=dtrain

#Data'S Informations
glimpse(dtrain1)
str(dtrain)
head(dtrain)
dim(dtrain)
class(dtrain)
summary(dtrain)

#sum of missing values
sum(is.na(dtrain)) # The result is 0

#plots
#correlation
a <-cor(dtrain)
corrplot(a, method="circle", main ="correlation plot")


#Numeric variables to factors
dtrain1$price_range <- factor(dtrain1$price_range, levels = c(0,1,2,3),
                                      labels = c("low cost", "medium cost", "high cost", "very high cost"))

dtrain1$blue <- factor(dtrain1$blue, levels = c(0,1),
                               labels = c("not", "yes"))
dtrain1$dual_sim <- factor(dtrain1$dual_sim, levels = c(0,1),
                                   labels = c("not", "yes"))
dtrain1$four_g <- factor(dtrain1$four_g, levels = c(0,1),
                                 labels = c("not", "yes"))
dtrain1$three_g <- factor(dtrain1$three_g, levels = c(0,1),
                                  labels = c("not", "yes"))
dtrain1$touch_screen <- factor(dtrain1$touch_screen, levels = c(0,1),
                                       labels = c("not", "yes"))
dtrain1$wifi <- factor(dtrain1$wifi, levels = c(0,1),
                               labels = c("not", "yes"))
# change to numeric 
dtrain1$fc <- as.numeric(dtrain1$fc)
dtrain1$battery_power <- as.numeric(dtrain1$battery_power)
dtrain1$clock_speed <- as.numeric(dtrain1$clock_speed)
dtrain1$m_dep <- as.numeric(dtrain1$m_dep)
dtrain1$int_memory <- as.numeric(dtrain1$int_memory)
dtrain1$mobile_wt <- as.numeric(dtrain1$mobile_wt)
dtrain1$n_cores <- as.numeric(dtrain1$n_cores)
dtrain1$px_height <- as.numeric(dtrain1$px_height)
dtrain1$px_width <- as.numeric(dtrain1$px_width)
dtrain1$ram <- as.numeric(dtrain1$ram)
dtrain1$sc_h <- as.numeric(dtrain1$sc_h)
dtrain1$sc_w <- as.numeric(dtrain1$sc_w)
dtrain1$talk_time <- as.numeric(dtrain1$talk_time)

#

ggplot(data=dtrain1, aes(x = price_range,fill = price_range, colors=price_range)) +
  geom_bar()
ggplot(data = dtrain1, aes(y=price_range)) +
  geom_boxplot() +
  labs(title = "Mobile Price Classification", y="Price Range")

##he amount of data in dataset has the same amount in each class that consist 
##of low cost, medium cost, high cost, and very high cost.

#bivariate analysis
#BlueTooth
ggplot(dtrain1, aes(x=price_range, fill = blue, colors=blue)) +
  geom_bar(position = "dodge")
#dualsim
ggplot(dtrain1, aes(x=price_range, fill = dual_sim, colors=dual_sim)) +
  geom_bar(position = "dodge")
#4G
ggplot(dtrain1, aes(x=price_range, fill = four_g, colors=four_g)) +
  geom_bar(position = "dodge")
#3G
ggplot(dtrain1, aes(x=price_range, fill = three_g, colors=three_g)) +
  geom_bar(position = "dodge")
#wifi
ggplot(dtrain1, aes(x=price_range, fill = wifi, colors=wifi)) +
  geom_bar(position = "dodge")
#touch screen
ggplot(dtrain1, aes(x=price_range, fill = touch_screen, colors=touch_screen)) +
  geom_bar(position = "dodge")
ggplot(dtrain1, aes(x=price_range, fill = touch_screen, colors=touch_screen)) +
  geom_bar(position = "dodge")

####
ggplot(dtrain1, aes(x=sc_h, y=sc_w,
                                 shape=price_range, color=price_range)) +
  geom_point() +
  labs(title = "Mobile Price Classification", x="Screen Height of mobile in cm", y="Screen Width of mobile in cm")
####
ggplot(dtrain1, aes(x=ram, y=battery_power,
                    shape=price_range, color=price_range)) +
  geom_point() +
  labs(title = "Mobile Price Classification", x="Ram", y="Battery Power")

########
#Ram plot
p <- ggplot(dtrain1, aes(x=ram, y = price_range, fill = as.factor(price_range)))+
  geom_histogram(aes(y=..density..), position = 'identity', alpha=0.5) +
  labs(title="ram histogram plot",x="RAM", y = "Density")

p

#battery power
p <- ggplot(dtrain1, aes(x=battery_power, y = price_range, fill = as.factor(price_range)))+
  geom_histogram(aes(y=..density..), position = 'identity', alpha=0.5) +
  labs(title="Battery Power histogram plot",x="Battery Power", y = "Density")

p

#clock speed
p <- ggplot(dtrain1, aes(x=clock_speed, y = price_range, fill = as.factor(price_range)))+
  geom_histogram(aes(y=..density..), position = 'identity', alpha=0.5) +
  labs(title="Clock speed histogram plot",x="Clock speed", y = "Density")

p

#Front Camera
p <- ggplot(dtrain1, aes(x=fc, y = price_range, fill = as.factor(price_range)))+
  geom_histogram(aes(y=..density..), position = 'identity', alpha=0.5) +
  labs(title="Front Camera histogram plot",x="Front Camera", y = "Density")

p

#Pixel resolution width
p <- ggplot(dtrain1, aes(x=px_width, y = price_range, fill = as.factor(price_range)))+
  geom_histogram(aes(y=..density..), position = 'identity', alpha=0.5) +
  labs(title="Pixel Resolution Width histogram plot",x="Px Width", y = "Density")

p

#Pixel Resolution Height
p <- ggplot(dtrain1, aes(x=px_height, y = price_range, fill = as.factor(price_range)))+
  geom_histogram(aes(y=..density..), position = 'identity', alpha=0.5) +
  labs(title="Pixel Resolution Height plot",x="Px Height", y = "Density")

p

#Screen Height of mobile in cm
p <- ggplot(dtrain1, aes(x=sc_h, y = price_range, fill = as.factor(price_range)))+
  geom_histogram(aes(y=..density..), position = 'identity', alpha=0.5) +
  labs(title="Screen Height of mobile in cm plot",x="sc Height", y = "Density")

p


#n core
p <- ggplot(dtrain1, aes(x=n_cores, y = price_range, fill = as.factor(price_range)))+
  geom_histogram(aes(y=..density..), position = 'identity', alpha=0.5) +
  labs(title="n cores plot",x="nCORES", y = "Density")

p


#Screen width of mobile in cm
p <- ggplot(dtrain1, aes(x=sc_w, y = price_range, fill = as.factor(price_range)))+
  geom_histogram(aes(y=..density..), position = 'identity', alpha=0.5) +
  labs(title="Screen width of mobile in cm plot",x="sc width", y = "Density")

p

#talk time
p <- ggplot(dtrain1, aes(x=talk_time, y = price_range, fill = as.factor(price_range)))+
  geom_histogram(aes(y=..density..), position = 'identity', alpha=0.5) +
  labs(title="talk time plot",x="Talk Time", y = "Density")

p

#Mobile
p <- ggplot(dtrain1, aes(x=mobile_wt, y = price_range, fill = as.factor(price_range)))+
  geom_histogram(aes(y=..density..), position = 'identity', alpha=0.5) +
  labs(title="mobile wt plot",x="Mobile Wt", y = "Density")

p

#int Memory
p <- ggplot(dtrain1, aes(x=int_memory, y = price_range, fill = as.factor(price_range)))+
  geom_histogram(aes(y=..density..), position = 'identity', alpha=0.5) +
  labs(title="Int memory plot",x="int Memory", y = "Density")

p

#m dep
p <- ggplot(dtrain1, aes(x=m_dep, y = price_range, fill = as.factor(price_range)))+
  geom_histogram(aes(y=..density..), position = 'identity', alpha=0.5) +
  labs(title="m dep plot",x="M dep", y = "Density")

p

#Scaling data (Normalization)
for (i in 1:20)
{
  dtrain[,i] = (dtrain[,i]-min(dtrain[,i]))/(max(dtrain[,i])-min(dtrain[,i]))
}

glimpse(dtrain)

# Convert the response variable to a factor
dtrain$price_range <- as.factor(dtrain$price_range)
dtrain$price_range <- as.factor(dtrain$price_range)
glimpse(dtrain$price_range)
#Converting Binary inputs to Logical inputs
is.factor(dtrain$blue)
is.factor(dtrain$dual_sim)
is.factor(dtrain$four_g)
is.factor(dtrain$three_g)
is.factor(dtrain$touch_screen)
is.factor(dtrain$wifi)
glimpse(dtrain$wifi)
prop.table(table(dtrain$blue)) # cell percentages
prop.table(table(dtrain$dual_sim)) # cell percentages
prop.table(table(dtrain$four_g)) # cell percentages

# Splitting data into train
# and test data
#split dataset to test and train set
index <- sample(1:nrow(dtrain), 
                round(0.8 * nrow(dtrain)))
trainset <-dtrain[index, ]
testset <- dtrain[-index, ]

#build the model for neural network USING NEURALNET function
#I used logistic activation function for this one (it is a default function)
#one hidden Layer with 5 neurones
nn <- neuralnet(price_range ~., data = trainset, hidden = c(5), linear.output = F, 
                      rep=1, threshold = 0.01, err.fct = "ce", learningrate = 0.05,
                      algorithm = "rprop+")
summary(nn)
plot(nn)
weights <- nn$weights
weights
biases <- nn$bias
biases
#making predictions on the testset
output=compute(nn,testset[,-21])
pred=output$net.result #predictors

#predicting classes
predictions_class <- apply(round((pred)), 1,function(x)which.max(x)-1)
predictions_class

#contingency table
tab= table(testset$price_range,predictions_class)
tab

#matching the levels of predicted values and the actual values
predictions_class <- factor(predictions_class, levels = levels(trainset$price_range))
cm=confusionMatrix(tab)
cm

#calculating the metrics
#1 mean squarred error

predictions_class <- as.numeric(predictions_class)
testset$price_range <- as.numeric(testset$price_range)
mse <- mean((testset$price_range - predictions_class)^2)
mse

# 2 accuracy
acc <- sum(testset$price_range == predictions_class)/nrow(testset)
cat("Accuracy:", acc*100, "%\n")


#Loading data
setwd("C:/Users/hp/Desktop/archive (2)")
dtest = read.csv("test.csv")

for (i in 1:20)
{
  dtest[,i] = (dtest[,i]-min(dtest[,i]))/(max(dtest[,i])-min(dtest[,i]))
}

#making predictions on the testset
output1=compute(nn,dtest)
pred1=output1$net.result #predictors

#predicting classes
predicted_pricerange <- apply(round((pred1)), 1,function(x)which.max(x)-1)
predicted_pricerange
dtest$predicted_pricerange = predicted_pricerange

