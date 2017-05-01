##Kaggle Titanic Date 

#install.packages("rpart")
library(rpart)
library(randomForest)
library(ggplot2)
library(scales)
library(dplyr)
library(rattle)
library(caret)

##Set working directory 

setwd('D:/Classes/Kaggle/Titanic')

Train <- read.csv("train.csv",stringsAsFactors = FALSE)
Test <-  read.csv("test.csv",stringsAsFactors = FALSE)

summary(Train)

##Step 1 - Do some data cleaning and preparation if required

#Check the data types of all variables

str(Train)
str(Test)

#For code optimization for Data Preparation we combine will combine both Test and Training

#we will use the response variable to distinguish Training and Test

Test$Survived <- "NA"
Train$Flag <- "Train"
Test$Flag <- "Test"

Maindf <- rbind(Train,Test)

#Step -1 , Find if there are missing values in the data

MissingCounts <- as.data.frame(sapply(Maindf, function(x) sum(is.na(x) | x == "")))

MissingCounts

##Observations
##1. Age has 263 missing values, which is around ~20% of the total rows, We can group the 
##rows with similar attribute values to predict the age

##2. Cabin has 1014 missing values or say it is blank - it is possible that not all passengers 
## have assigned cabins for thier travel, so it can be blank and cant be imputed
## However we can check if there are any missing cabin values for passenger of the same family
##who are most likely to travel together.Since it has more than 77% of missing values we can ignore
##this column

##3.Embraked has two missing values which we can ignore as it accounts to negligible amount

##4. Fare has one missing value 

sapply(Maindf, function(x) class(x))

#We observe the some the character variables we should be Factor so we change their data tye

Maindf$Sex <- factor(Maindf$Sex, levels = c("female","male"))

levels(Maindf$Sex)

#Change the data type for the cols response variable Survived ,Pclass, Embarked

Maindf$Survived <- factor(Maindf$Survived, levels = c(1,0)) 
levels(Maindf$Survived)

Maindf$Pclass <- as.factor(Maindf$Pclass)

Maindf$Embarked <- factor(Maindf$Embarked, levels = c("C","Q","S"))

Maindf$Fare <- as.numeric(Maindf$Fare)

sapply(Maindf, function(x) class(x))

#Lets do some data imputation

Maindf[(Maindf$Embarked == ''),]
##First class pasengers with Fare 80$, let see if there is a particular embarkment which 
## has 80$ fare for first class

##To plot the data using ggplot remove the NAs

ggplot(Maindf[Maindf$Embarked != '',], aes(x = Embarked,y = Fare,fill = Pclass)) +
   geom_boxplot() + geom_hline(aes(yintercept = 80), colour = 'red',linetype = 'dashed',lwd=2)+
    scale_y_continuous(labels = dollar_format()) + theme_bw() +
    labs( x = "Different Embarkments", y = "Fare", title = "Fare for diff. class of tickets across diff. Embarkment")

#Since the 80$ matches the median first class ticket fare from Embarkment C, so we would 
#replace the missing value of embarkment by 'C'

Maindf$Embarked[is.na(Maindf$Embarked)] <- 'C'

#Check for the missing value of Fare if we can replace it,
Maindf[is.na(Maindf$Fare),]

#Travelling Third class and Emarked from 'S', lets find the median fare of this kind of ticket

median(Maindf$Fare[Maindf$Pclass == '3' & Maindf$Embarked == 'S'], na.rm = TRUE)
##8.05 is the median fare, lets replace that with the median fare

Maindf$Fare[is.na(Maindf$Fare)] <- 8.05

#Now we would treat the missing values in Age column using predictive modeling

#Lets try first linear regression  to predict age and validate its accuracy

hist(Maindf$Age, col = "blue", xlab = "Spread for Age variable", main = "Histogram for the Age variable")

D <- plot(density(Maindf$Age,na.rm = TRUE), col= "red",xlab = "Spread for Age variable", main = "Histogram for the Age variable")##doesn't looks normal

N <- density(rnorm(1309,mean = mean(Maindf$Age,na.rm= TRUE), sd = sd(Maindf$Age,na.rm= TRUE)))

lines(N, col = "magenta")

abline(v= mean(Maindf$Age,na.rm= TRUE),col = "green",lwd = 3)

abline(v=median(Maindf$Age,na.rm=TRUE), col ="yellow",lwd =3)

#Age doesn't have a pure normal distribution

#First assumption of linear regression: Multivariate collinearity
head(Maindf)

cor(Maindf[,c(6,7,8,10)]) 
#As we dont have all numeric variables the correlation values are not really helpful

pairs(~Age+SibSp+Parch+Fare, data = Maindf)

##We dont observe linear relation b/n Age and other numeric variables
##This makes us not to predict Age using linear regression

#Decision Tree:

DTFit <- rpart(Age ~ Sex + Pclass + Parch + SibSp + Fare  + Embarked,
                data = Maindf[!is.na(Maindf$Age),], method = "anova" )##anova used since the
                                                                      ## y is numeric 

Age_DT <- predict(DTFit,Maindf[!is.na(Maindf$Age),])

Age <- Maindf$Age[!is.na(Maindf$Age)]

plot(Age, Age_DT)

compare1 <- as.data.frame(cbind(Age,Age_DT))

MSE1 <- mean((compare1$Age - compare1$Age_DT)^2) ##144.8184

#Now Predict the Age missing values

Age_DT1 <- predict(DTFit,newdata = Maindf[is.na(Maindf$Age),])

summary(DTFit)

rsq.val <- 1- (printcp(DT_Age))[,c(3,4)]

##We can also try random forest to see the accuracy 

RFFit <- randomForest(Age ~ Sex +  Pclass + Parch + SibSp + Fare + Embarked,
                       data = Maindf,na.action = na.omit, ntree = 100, set.seed(111), importance = TRUE)

RF_Age <- predict(RFFit, newdata = Maindf[!is.na(Maindf$Age),])

#Check the accuracy

compare2 <- as.data.frame(cbind(Age,RF_Age))

MSE2 <- mean((Age - compare2$RF_Age)^2)## 103.633

#Random forest predicts best in terms of MSE, So we impute Age using Random Forest

#For imputation purpose we are not considering overfitting 

Maindf$Age[is.na(Maindf$Age)] <- predict(RFFit, newdata = Maindf[is.na(Maindf$Age),])

##Feature Engineering - on the variable name

Maindf$Title <- sapply(Maindf$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})

Maindf$Title <- sub(' ','',Maindf$Title)

#reduce the level of the title

table(Maindf$Title,Maindf$Sex)

Maindf$Title[Maindf$Title %in% c('Ms')] <- 'Miss'
Maindf$Title[Maindf$Title %in% c('Capt','Don','Col','Rev','Major','Jonkheer')] <- 'Sir'
Maindf$Title[Maindf$Title %in% c('Dona','Lady','Mme','Mlle','the Countess')] <- 'Lady'

table(Maindf$Title,Maindf$Sex)

#Keep the titles as factor
Maindf$Title <- as.factor(Maindf$Title)

#Lets Work on the Surname

Maindf$LastName <- sapply(Maindf$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})

#Lets work on the family size with an assumption that Parch and Sibsp would tell us that

Maindf$FamilySize <- Maindf$SibSp + Maindf$Parch + 1

#To find if the Family has a relation with the Survival

Maindf$Family <- paste(Maindf$LastName,Maindf$FamilySize, sep = "_")

#Visualize the relationship

ggplot(Maindf[1:891,], aes(x = FamilySize, fill = factor(Survived))) + 
  geom_bar(stat = 'count',position = 'dodge') + scale_x_continuous(breaks = c(1:11)) + 
  theme_bw() + labs(title = "Dist of Family size", x = " Family size")

#There is definitely a relationship between Family size and the probablity of Survival
##We can observe Singletons and large family size > 4 have lower chance of survival

#So, we would differentiate the family size

Maindf$FmlyType[Maindf$FamilySize == 1] <- 'Singleton'
Maindf$FmlyType[Maindf$FamilySize < 5 & Maindf$FamilySize > 1] <- 'Small'
Maindf$FmlyType[Maindf$FamilySize > 4] <- 'Large'

Maindf$FmlyType <- as.factor(Maindf$FmlyType)
levels(Maindf$FmlyType)

#Family size by survival 

mosaicplot(table(Maindf$FmlyType,Maindf$Survived),main = 'Family Size by Survival',shade = TRUE)

##Split the Main data Frame into Train and Test

Train.new <- Maindf[(Maindf$Flag == 'Train'),]
Test.new <- Maindf[(Maindf$Flag == 'Test'),]

#Lets try to fit diff logistic model 

head(Train.new)

plot(Train.new$Sex,Train.new$Survived)

chisq.test(Train.new$Sex,Train.new$Survived)

plot(Train.new$FamilySize,Train.new$Survived)

#Lets try with the simple logistic regression

logfit <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Title + Embarked + Fare
                        + FmlyType + Family,
                    family = binomial(link ="logit"),data = Train.new)
#Warning message:
#glm.fit: fitted probabilities numerically 0 or 1 occurred
summary(logfit)##AIC: 1858.5
#This is due to the overfitting probably we need to remove some variable

head(Train.new)

logfit2 <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Title + Embarked + Fare
              + FmlyType,
              family = binomial(link ="logit"),data = Train.new)
#This time we dont get the err, because we have elminitated the variable with so many values

Response1 <- predict(logfit2,newdata = Train.new[,c(3,5:8,10,12,14,18)], type = 'response')

table(Train.new$Survived,round(Response1))
##The misclassification index 
##  0   1
##1 257  85
##0  64 485

confusionMatrix(round(Response1),Train$Survived)
#The Accuracy is only .1672

#lets try some other model : Decision Tree

DTfit2 <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Title  + Embarked  
                   + Fare + FmlyType,
                data = Train.new, method = "class" )

summary(DTfit2)

fancyRpartPlot(DTfit2)

Response2 <- predict(DTfit2,newdata = Train.new, type = "class")

length(Response2)

confusionMatrix(Response2,Train$Survived)
##Accuracy : 0.8496 

table(Response2,Train.new$Survived)
#Misclassification 
##Response2   1   0
##        1 242  34
##        0 100 515
##This model performs better than the Logistic model

#We also fit a Random forest and check the accuracy

RFfit2 <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Title  + Embarked  
                       + Fare + FmlyType,
                       data = Train.new, ntree = 100, set.seed(111), importance = TRUE)

str(Train.new)
Train.new$Survived <- factor(Train.new$Survived, levels = c(0,1))


#Since RF is throwing err we would check if there is any NA in our data
sapply(Train.new, function(x) sum(is.na(x)| x ==""))

sapply(Train.new, function(x) class(x))

##Found out that FamilyType is character which needs to factor, Family also as factor

unique(Train.new$FmlyType)
unique(Train.new$Family)

Train.new$FmlyType <- as.factor(Train.new$FmlyType)
Train.new$Family   <- as.factor(Train.new$Family)

Response3 <- predict(RFfit2,newdata = Train.new)

confusionMatrix(Response3,Train.new$Survived)
##Accuracy : 0.9327  

table(Response3,Train.new$Survived)
##Response3   1   0
##        1 299  17
##        0  43 532

##Random forest performs best in terms of Training accuracy, which might be due to overfitting

summary(RFfit2)

# Get importance
importance    <- importance(RFfit2)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_bw()

##Lets do some cross validation on the random forest modelling to reduce overfitting

k.folds <- function(k) {
  folds <- createFolds(Train.new$Survived, k = k, list = TRUE, returnTrain = TRUE)
  accuracies.rf <- c()
  for (i in 1:k) {
    model <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Title  + Embarked  
                    + Fare + FmlyType, 
                    data = Train.new[folds[[i]],], ntree = 100, set.seed(111), importance = TRUE)
    predictions <- predict(object = model, newdata = Train.new[-folds[[i]],])
    accuracies.rf <- c(accuracies.rf, 
                       confusionMatrix(predictions, Train.new[-folds[[i]], ]$Survived)$overall[[1]]) 
    
    
    }
  accuracies.rf
}

set.seed(567)

accuracies.rf <- k.folds(5)
accuracies.rf
## 0.8379888 0.8100559 0.7966102 0.8539326 0.8426966

mean(accuracies.rf)##0.8282568, reduced quite a but from the accuracy on the complete training set

##But is still better than Decision Tree

##If 5 looks small to decide if the model accuracy is good or not, we can use repeated 
##k fold cross validation,

v <- c()
v <- replicate(200, k.folds(5))
accuracies.rf1 <- c()
for (i in 1 : 200) { 
  accuracies.rf1 <- c(accuracies.rf1, v[,i])
}

mean.accuracies <- mean(accuracies.rf1)##0.8317077
lci <- mean(accuracies.rf1) - sd(accuracies.rf1) * 1.96 ## 0.7848956
uci <- mean(accuracies.rf1) + sd(accuracies.rf1) * 1.96 ## 0.8785198

##Now, predict response for the test data

predict.test <- predict(object = RFfit2, newdata = Test.new)

# Create a data frame with just PassengerId and Survived to submit to Kaggle. Note that I assign "predict.test" to "Survived"
titanic_solution <- data.frame(PassengerId = Test.new$PassengerId, Survived = predict.test)

# Write your solution to a csv file with the name my_solution.csv
write.csv(titanic_solution, file = "titanic_solution.csv", row.names = FALSE) ##This submission
##resulted to score of .7799

##To improve the score, would try adding one more variable

Maindf$Child <- ifelse(Maindf$Age <18, 1,0)
Maindf$Child <- factor(Maindf$Child, levels = c(1,0))
levels(Maindf$Child)

Train.new <- Maindf[Maindf$Flag == 'Train',]
Test.new <- Maindf[Maindf$Flag == 'Test',]

#Again fit the random forest model

RFfit3 <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Title  + Embarked  
                       + Fare + FmlyType + Child, 
                       data = Train.new, ntree = 100, set.seed(111), importance = TRUE)

Response4  <- predict(RFfit3, newdata = Train.new)
confusionMatrix(Response4,Train.new$Survived) ##0.9282, no improvements in terms of predicting
                                              ##capability

Reference
#Prediction   1   0
#1          289  11
#0          53  538

#Lets try a submssion with DT
##Now, predict response for the test data

predict.test1 <- predict(object = DTfit2, newdata = Test.new, type = "class")

# Create a data frame with just PassengerId and Survived to submit to Kaggle. Note that I assign "predict.test" to "Survived"
titanic_solution1 <- data.frame(PassengerId = Test.new$PassengerId, Survived = predict.test1)

# Write your solution to a csv file with the name my_solution.csv
write.csv(titanic_solution1, file = "titanic_solution1.csv", row.names = FALSE) ##This submission
##resulted to score of .79904, performs better than random forest

##Add the child variable to the Decsion tree

DFfit3 <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Title  + Embarked  
                + Fare + FmlyType + Child, 
                data = Train.new, method = "class")

fancyRpartPlot(DFfit3)

Response5 <- predict(DFfit3,newdata = Train.new, type = "class")


confusionMatrix(Response5,Train$Survived) ##0.8496, no improvements in accuracy.

