# 1° Descision Tree submission
library(rpart)

train <- read.csv('./train.csv')
test <- read.csv('./test.csv')

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train,method="class")

res <- predict(fit, test, type="class")

submission <- data.frame(PassengerId = test$PassengerId, Survived = as.integer(res) - 1)

write.csv(submission, 'firstOrderTree.csv', row.names = FALSE)
