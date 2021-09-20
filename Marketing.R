#import dependencies
library(ggplot2)
library(Hmisc)
library(readxl)
options(warn=-1) #to ignore warnings

##set working directory
setwd("C:/Users/Saurabh/Desktop/630/Week 3")

#load datasets
df <- read.csv("dodgers.csv")
df1 <- read.csv("dodgers_new.csv")
describe(df1)

#Evaluate attendance by weather and bobblehead
ggplot(df, aes(x=temp, y=attend/1000, color=bobblehead)) + 
  geom_point() + 
  facet_wrap(day_night~skies) + 
  ggtitle("Dodgers Attendance By Temperature By Time of Game and Skies") +
  theme(plot.title = element_text(lineheight=2, color="black", size=8)) +
  xlab("Temperature") +
  ylab("Attendance (in thousands)")

#Evaluate attendance by weather and fireworks
ggplot(df, aes(x=temp, y=attend/1000, color=fireworks)) + 
  geom_point() + 
  facet_wrap(day_night~skies) + 
  ggtitle("Dodgers Attendance By Temperature By Time of Game and Skies") +
  theme(plot.title = element_text(lineheight=2, color="black", size=8)) +
  xlab("Temperature") +
  ylab("Attendance (in thousands)")

#Evaluate attendance by weather and cap
ggplot(df, aes(x=temp, y=attend/1000, color=cap)) + 
  geom_point() + 
  facet_wrap(day_night~skies) + 
  ggtitle("Dodgers Attendance By Temperature By Time of Game and Skies") +
  theme(plot.title = element_text(lineheight=2, color="black", size=8)) +
  xlab("Temperature") +
  ylab("Attendance (in thousands)")

#Evaluate attendance by weather and shirt
ggplot(df, aes(x=temp, y=attend/1000, color=shirt)) + 
  geom_point() + 
  facet_wrap(day_night~skies) + 
  ggtitle("Dodgers Attendance By Temperature By Time of Game and Skies") +
  theme(plot.title = element_text(lineheight=2, color="black", size=8)) +
  xlab("Temperature") +
  ylab("Attendance (in thousands)")


#Build model

# Train and Test Data

train <- round(nrow(df1) * 0.8)
test <- nrow(df1) - train

## Reseed for repeatability
set.seed(127)

##sample training test
train_index <- sample(seq_len(nrow(df1)),size=train)
trainSet <- df1[train_index, ]
testSet <- df1[-train_index, ]

# Create model parameters
attendance_model <- (attend ~ month + day_of_week)

# Model for training
train_model <- lm(attendance_model, data = trainSet)
summary(train_model)
trainSet$prediction <- predict(train_model)

# Model for testing
testSet$prediction <-predict(train_model, newdata=testSet)

# Visualize testing data
month_pred <- ggplot2::ggplot(testSet, aes(x=testSet$month, y=testSet$prediction)) + ggplot2::geom_point()

month_pred + facet_grid(. ~ testSet$day_of_week) + ggplot2::theme(axis.text.x = element_text(angle = 90)) + ggplot2::xlab('Months') + ggplot2::ylab('Attendance') + ggplot2::ggtitle('Prediction of Attendance',subtitle = 'Day of the Week')


day_pred <- ggplot2::ggplot(testSet, aes(x=testSet$day_of_week, y=testSet$prediction)) + ggplot2::geom_point()

day_pred + facet_grid(. ~ testSet$month) + ggplot2::theme(axis.text.x = element_text(angle = 90)) + ggplot2::xlab('Days of the Week') + ggplot2::ylab('Attendance') + ggplot2::ggtitle('Prediction of Attendance',subtitle = 'Months')








