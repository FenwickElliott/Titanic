# predicting Age submission
library(rpart)

train <- read.csv('./train.csv')
test <- read.csv('./test.csv')
test$Survived <- NA
total <- rbind(train, test)

total$FamilySize <- total$SibSp + total$Parch + 1

total$Name <- as.character(total$Name)

total$Title <- sapply(total$Name, FUN=function(x) { strsplit(x, split='[,.]')[[1]][2]})
total$Title <- sub(' ', '', total$Title)
total$Title[total$Title %in% c('Mlle', 'Mme')] <- 'Miss'
total$Title[total$Title %in% c('Don', 'Dona', 'Lady', 'Sir', 'Jonkheer', 'the Countess')] <- 'Knighthoods'
total$Title[total$Title %in% c('Capt', 'Col', 'Major')] <- 'Military'
total$Title <- factor(total$Title)

total$SurName <- sapply(total$Name, FUN=function(x) { strsplit(x, split='[,.]')[[1]][1]})
total$Family <- factor(paste(total$SurName, as.character(total$FamilySize), sep='_'))
total$Family[total$FamilySize == 1 ] <- NA

age <- rpart(Age ~ Survived + Title + Pclass + Sex + Fare + Embarked, data=total[!is.na(total$Age),], method='anova')
total$Age[is.na(total$Age)] <- predict(age, total[is.na(total$Age),])

train <- total[1:891,]
test <- total[892:1309,]

fit <- rpart(Survived ~ Family + Title + Pclass + Sex + Age + SibSp + Parch + FamilySize + Fare + Embarked, data=train, method="class")
res <- predict(fit, test, type="class")
submission <- data.frame(PassengerId = test$PassengerId, Survived = as.integer(res) - 1)
write.csv(submission, 'predictingAge.csv', row.names = FALSE)
