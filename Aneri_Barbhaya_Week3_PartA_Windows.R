#install.packages("C50")
#install.packages("gtools")
#install.packages("gmodels")
library(gmodels)
library(C50)

#STEP1: Data collection
#German credit dataset from UCI Machine Learning Repository is being used for Decision Tree classification. The dataset contacts about 1000 observations of loan with numeric and categorical features of loan and the applicant. There are 21 features describing the loan application and loan details which can be used to build a credit model to predict/classify whether the applicant would default on loan or not. 

#STEP2 :explore data
credit<-read.csv("german.csv")

#examining the structure of the data
str(credit)

#table for features that are likely to predict a default
table(credit$Status.of.existing.checking.account)
table(credit$Savings.account.bonds)
table(credit$default.yes.no)

#exploring some numerical attribues of the dataset
summary(credit$Duration.in.month)
summary(credit$Credit.amount)

#recoding credit variable to yes/no
credit$default.yes.no<-ifelse(credit$default.yes.no=="1","no","yes")
table(credit$default.yes.no)

#Data preparation-creating random training and test datasets
#Creating training and testing data
set.seed(123)
train_sample<-sample(1000,900)
str(train_sample)

credit_train<-credit[train_sample,]
credit_test<-credit[-train_sample,]

prop.table(table(credit_train$default.yes.no))
prop.table(table(credit_test$default.yes.no))

#step 3: training the model
credit_model<-C5.0(credit_train[-21],as.factor(credit_train$default.yes.no))
credit_model
summary(credit_model)

#Step 4 : Evaluating the model performance
credit_predict<-predict(credit_model,credit_test)

CrossTable(credit_test$default.yes.no, credit_predict,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

#step 5: improving performance of the model
credit_boost10 <- C5.0(credit_train[-21], as.factor(credit_train$default.yes.no),
                       trials = 10)
credit_boost10
summary(credit_boost10)

credit_predict10<-predict(credit_boost10,credit_test)

CrossTable(credit_test$default.yes.no,credit_predict10,prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE,dnn = c("default","predicted"))


matrix_dimensions <- list(c("no", "yes"), c("no", "yes"))
names(matrix_dimensions) <- c("predicted", "actual")

error_cost <- matrix(c(0, 1, 4, 0), nrow = 2,
                     dimnames = matrix_dimensions)

credit_with_cost<-C5.0(credit_train[-21], as.factor(credit_train$default.yes.no),
                       cost=error_cost)
credit_with_cost
credit_cost_pred <- predict(credit_with_cost, credit_test)

CrossTable(credit_test$default.yes.no, credit_cost_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

