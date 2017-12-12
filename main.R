# Load packages
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm


train <- read.csv('./train.csv', stringsAsFactors = F)
test  <- read.csv('./test.csv', stringsAsFactors = F)

full  <- bind_rows(train, test) # bind training & test data

str(full)