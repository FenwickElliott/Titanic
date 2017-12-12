# Gender submission

train <- read.csv('./train.csv')
test <- read.csv('./test.csv')

# prop.table(table(train$Sex, train$Survived))

test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1

summary(test$Sex)
summary(test$Survived)

res <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(res, 'gender.csv', row.names = FALSE)