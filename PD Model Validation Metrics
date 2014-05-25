# ===========================================================================
# Validation Metric for a PD Model using Logistic Regression
# Author - Abir Datta
# Date - 25th May, 2014
# OS Details - Ubuntu 13.10 Saucy Salamander 64bit
# RAM - 4 GB (3.7 GB usable)
# Processor - Intel(R) Pentium(R) CPU 2020M @ 2.40GHz
# Cache Size - 2048 KB
# ===========================================================================
# Change the path as per the data location
setwd("~/Desktop/Personal/Office/Projects/Financial_Services")
source("Information Statistics.R")
source("KL Statistics.R")
# ===========================================================================

# Importing the German credit score data
data<-read.csv("German_Credit_data.csv",
               header = TRUE)
summary(data)

# Changing the dependent variable to 0-1
data$goodbad <- data$goodbad-1

# Recoding the variables into factor
data$ID <- as.factor(data$ID)
data$checking <- as.factor(data$checking)
data$history <- as.factor(data$history)
data$purpose <- as.factor(data$purpose)
data$employ <- as.factor(data$employ)
data$rate <- as.factor(data$rate)
data$status <- as.factor(data$status)
data$debtors <- as.factor(data$debtors)
data$residence <- as.factor(data$residence)
data$property <- as.factor(data$property)
data$other_plans <- as.factor(data$other_plans)
data$housing <- as.factor(data$housing)
data$job <- as.factor(data$job)
data$phone <- as.factor(data$phone)
data$foreign <- as.factor(data$foreign)
data$goodbad <- as.factor(data$goodbad)

# ===========================================================================

# Creating the dummy variables for the categorical variables for each classes
k <- model.matrix(goodbad ~ 
                    checking +
                    history +
                    purpose +
                    employ +
                    rate +
                    status +
                    debtors +
                    residence +
                    property +
                    other_plans +
                    housing +
                    job +
                    phone +
                    savings +
                    foreign, 
                  data)
k1 <- k[,-1]
data <- cbind(data,k1)

# ===========================================================================

# Splitting the data into training & test data sets
d = sort(sample(nrow(data), nrow(data)*.6))
#select training sample
train<-data[d,]
test<-data[-d,]

# ===========================================================================

# Running a GLM (logistic regression)
install.packages("glm2", quiet = TRUE)
library(glm2,
        quietly=TRUE)

modelFormula = 
  goodbad ~ checking +
  duration +
  history +
  purpose +
  amount +
  savings +
  employ +
  rate +
  status +
  debtors +
  residence +
  property +
  age +
  other_plans +
  housing +
  exist_cr +
  job +
  provide +
  phone +
  foreign
  
modelLR <- glm(modelFormula,
               data = train,
               family = binomial(link="logit")
               )
summary(modelLR)

# ===========================================================================

# ===========================================================================

# Running GLM for the new variables
modelFormula = 
  goodbad ~ 
  checkingA12 +
  checkingA13 +
  checkingA14 +
  duration +
#   historyA33 +
  historyA34 +
#   historyA35 +
  purposeA41 +
#   purposeA410 +
#   purposeA42 +
  purposeA43 +
#   purposeA49 +
  amount +
  savingsA64 +
  savingsA65 +
#   rate3 +
  rate4 +
  statusA93 +
  debtorsA103 +
  residence2 +
#   propertyA124 +
  other_plansA143 +
  housingA152 +
  foreignA202

modelLR_New = glm(modelFormula,
                  data= data,
                  family = binomial(link='logit'))
summary(modelLR_New)

# Testing the model results
modelLR_Red = glm(goodbad ~ 1,
                  data= data,
                  family = binomial(link='logit'))

anova(modelLR_Red, 
      modelLR_New,
      test = "Chisq")

# ===========================================================================
# Predicting the scores
data$score <- predict(modelLR_New,
                      type="response",
                      data)
# Probability segment defined based on Optimal Accuracy CutOff
data$score_new <- ifelse(data$score > 0.589742,1,0)

# ===========================================================================

# Calculating the metrics for validating the Logistic Regression
# Invoking the required libraries

install.packages("ROCR", quiet = TRUE)
install.packages("verification", quiet = TRUE)
install.packages("AUC", quiet = TRUE)
install.packages("ResourceSelection", quiet = TRUE)
install.packages("dtw", quiet = TRUE)
install.packages("proxy", quiet = TRUE)
install.packages("FNN", quiet = TRUE)
install.packages("flexmix", quiet = TRUE)
install.packages("sqldf", quiet = TRUE)
install.packages("ineq", quiet = TRUE)

library(ROCR, quietly=TRUE)
library(verification, quietly=TRUE)
library(AUC, quietly=TRUE)
library(ResourceSelection, quietly=TRUE)
library(proxy, quietly=TRUE)
library(dtw, quietly=TRUE)
library(FNN, quietly=TRUE)
library(flexmix, quietly=TRUE)
library(sqldf, quietly=TRUE)
library(ineq, quietly = TRUE)
# ===========================================================================

# 1. Plotting the ROC Curve
    pred <- prediction(data$score, data$goodbad)
    perf <- performance(pred, "tpr", "fpr")
    plot(perf, colorize = TRUE)

  # Finding the performance measures for ROC Curve
  # a. AUC for the ROC Curve
    auc.tmp <- performance(pred,"auc")
    print("AUC for the Model");auc <- as.numeric(auc.tmp@y.values); auc

  # b. Gini Coefficient
    print("Gini Coefficient"); 2*auc - 1;

  # c. Accuracy
    acc.tmp <- performance(pred,"acc")
    cutoff.list.acc <- unlist(acc.tmp@x.values[[1]])
    print("Accuracy of the model:");max(acc.tmp@y.values[[1]])
    print("Optimal Cutoff point for Accuracy"); cutoff.list.acc[which.max(acc.tmp@y.values[[1]])]

  # d. Error Rate
    err.tmp <- performance(pred, "err")
    print("Error rate for the model:"); max(err.tmp@y.values[[1]])

  # e. Precision v/s Recall Curve
    prec.tmp <- performance(pred, "prec", "rec")
    plot(prec.tmp, colorize = TRUE)

  # f. Sensitivity
    print("Sensitivity for the model:");auc(sensitivity(data$score, data$goodbad))
  # g. Specificity
    print("Specificity for the model:");auc(specificity(data$score, data$goodbad))

  # h. Lift charts
    lift <- performance(pred, "lift", "rpp")
    plot(lift, colorize = TRUE)

  # i. Kendall's Tau-b
    x <- as.numeric(data$goodbad)
    y <- as.numeric(data$score)
    cor.test(x,y, method="kendall", alternative="greater")

  # j. Contingency Table
    table(data$goodbad, data$score_new)
# ===========================================================================

# 2. Finding the KS statistics
    max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])

# ===========================================================================

# 3. Hosmer Lemershow Test Goodness of Fit Test
     print("Reject the null hypothesis if p-value is less than the significance level alpha..... ");hoslem.test(data$goodbad, data$score, g = 10)

# ===========================================================================

# 4. Kullback-Leibler Statistic
      x = data$goodbad
      y = data$score
      KL_Stats(x, y)

# ===========================================================================

# 5. Information Statistics
     Inf_Stats(x, y)

# ===========================================================================

# 6. Pietra Index
     print("Pietra Index :");RS(data$score)

# ===========================================================================

# 7. Calculating the top 3 variables
      g<-predict(modelLR_New,
                 type='terms',
                 data)
      # a. Defining a function to find the top 3 variables based on the coefficients
      ftopk <- function(x,top=3){
        res=names(x)[order(x, decreasing = TRUE)][1:top]
        paste(res,collapse=";",sep="")
      }
      # b. Application of the function using the top 3 rows
      topk = apply(g,1,ftopk,top=3)
      #c. Add reason list to scored tets sample
      data1 <- cbind(data, topk)

# ===========================================================================
