#FDA group assign 

#setting directory
#setwd("D:/ernie/waseda/2020秋學期/Topics in Economics (Financial Data Analytics)/Group Assignment")
#importing libraries
library(readxl)
library(haven)
library(DescTools)
library(stargazer)
library(MASS)
library(boot)
#library(ggplot2)
#library(tidyverse)
#Part 01 
df <- read_excel("japan_public_corporations_1119.xlsx")

#Main dependent variables:
#(1) Cash holding is the ratio between cash and total assets; cash/at
df$casholding = df$cash / df$at
#(2) Dividend payment decision: Binary indicator: Yes/No; Yes if totaldividend>0
divdecision <- NA
divdecision[df$totaldividend > 0] <- "Yes"
divdecision[df$totaldividend <= 0] <- "No"
df$divdecision <- divdecision

#Main predictors (independent variables):

#Total assets size = the natural logarithm of total assets, log(at)
df$tlasset = log(df$at)
#Capital expenditure ratio = capital expenditure scaled by total assets, capexp/at
df$cerat = df$capexp / df$at
#Leverage ratio = the ratio between long-term debt and total assets, ltermdebt/at
df$lrat = df$ltermdebt / df$at
#Company age = the natural logarithm of company age, log(year-yearfound+1)
# check_df <- df[is.na(log(df$year - df$yearfound+1)),][,c(1,14)]
df$comage = log(df$year - df$yearfound+1)
#Sales = the natural logarithm of company sales, log(sale)
df$SALES = log(df$sales)
#Ebitda = the ratio between Earnings before interest, taxes, depreciation, and amortization and total assets, ebitda/at.
df$EBITDA = df$ebitda/df$at

####################  Part 1 #######################

#new dataset 
new_df <- df[,c(1,2,3,4,5,20,21,22,23,24,25,26,27)]
new_df <- na.omit(new_df)
write_dta(new_df, "cleaned_dataset.dta")

#winsorizing continuous variables rows
minval.value = quantile(new_df$casholding,0.01)
maxval.value = quantile(new_df$casholding,0.99)
new_df$casholding<- Winsorize(new_df$casholding, probs = c(0.01,0.99),minval = minval.value,maxval = maxval.value)

minval.value = quantile(new_df$tlasset,0.01)
maxval.value = quantile(new_df$tlasset,0.99)
new_df$tlasset<- Winsorize(new_df$tlasset, probs = c(0.01,0.99),minval = minval.value,maxval = maxval.value)

minval.value = quantile(new_df$cerat,0.01)
maxval.value = quantile(new_df$cerat,0.99)
new_df$cerat<- Winsorize(new_df$cerat, probs = c(0.01,0.99),minval = minval.value,maxval = maxval.value)

minval.value = quantile(new_df$lrat,0.01)
maxval.value = quantile(new_df$lrat,0.99)
new_df$lrat<- Winsorize(new_df$lrat, probs = c(0.01,0.99),minval = minval.value,maxval = maxval.value)

minval.value = quantile(new_df$comage,0.01)
maxval.value = quantile(new_df$comage,0.99)
new_df$comage<- Winsorize(new_df$comage, probs = c(0.01,0.99),minval = minval.value,maxval = maxval.value)

minval.value = quantile(new_df$SALES,0.01)
maxval.value = quantile(new_df$SALES,0.99)
new_df$SALES<- Winsorize(new_df$SALES, probs = c(0.01,0.99),minval = minval.value,maxval = maxval.value)

minval.value = quantile(new_df$EBITDA,0.01)
maxval.value = quantile(new_df$EBITDA,0.99)
new_df$EBITDA<- Winsorize(new_df$EBITDA, probs = c(0.01,0.99),minval = minval.value,maxval = maxval.value)

summary(new_df)


######################## Part 3 ##########################
# (a)
# Model 1 - Linear Regression with all predictors
lm_fit = lm(casholding~tlasset+cerat+lrat+comage+SALES+EBITDA,data=new_df)
summary(lm_fit)
stargazer (lm_fit,digits = 4,header = F,type = "text")
#stargazer (lm_fit,digits = 4,header = F,type = "html",out = "lin_reg.html")

# Residuals vs Prediction
df_copy = data.frame(new_df)
df_copy$predict <- predict(lm_fit)
df_copy$residuals <- residuals(lm_fit)

#plot(predict(lm_fit), residuals(lm_fit))
plot(lm_fit)

# MSE
yhat_lm_fit <- predict(lm_fit, new_df) 
lm_mse <- mean((yhat_lm_fit - new_df$casholding)^2)
lm_mse

# Model 2 - Linear Regression adding dividend as one of the predictors
lm_fit2=lm(casholding~divdecision+tlasset+cerat+lrat+comage+SALES+EBITDA,data=new_df)
summary(lm_fit2)


# (b)
# Transform divdecision to factor type variable
new_df$divdecision=as.factor(new_df$divdecision)

## Logistic Regression
glm_fits=glm(divdecision~tlasset+cerat+lrat+comage+SALES+EBITDA,data=new_df,family=binomial)
summary(glm_fits)
stargazer (glm_fits,digits = 4,header = F,type = "text")

# Predicted probability
glm_probs=predict(glm_fits,new_df,type="response")

# Threshold = 0.5
glm_pred=rep("No",24997)
glm_pred[glm_probs>.5]="Yes"
table(glm_pred,new_df$divdecision,dnn = c("Predicted", "Actual"))
# % of dividend decisions that are predicting correctly 
mean(glm_pred==new_df$divdecision)

# Threshold = 0.7
glm_pred_70=rep("No",24997)
glm_pred_70[glm_probs>.7]="Yes"
table(glm_pred_70,new_df$divdecision,dnn = c("Predicted", "Actual"))
# % of dividend decisions that are predicting correctly 
mean(glm_pred_70==new_df$divdecision)

# Threshold = 0.9
glm_pred_90=rep("No",24997)
glm_pred_90[glm_probs>.9]="Yes"
table(glm_pred_90,new_df$divdecision,dnn = c("Predicted", "Actual"))
# % of dividend decisions that are predicting correctly 
mean(glm_pred_90==new_df$divdecision)

# Threshold = 0.4
glm_pred_40=rep("No",24997)
glm_pred_40[glm_probs>.4]="Yes"
table(glm_pred_40,new_df$divdecision,dnn = c("Predicted", "Actual"))
# % of dividend decisions that are predicting correctly 
mean(glm_pred_40==new_df$divdecision)


# Threshold = 0.2
glm_pred_20=rep("No",24997)
glm_pred_20[glm_probs>.2]="Yes"
table(glm_pred_20,new_df$divdecision,dnn = c("Predicted", "Actual"))
# % of dividend decisions that are predicting correctly 
mean(glm_pred_20==new_df$divdecision)

# plot the prediction accuracy vs threshold
threshold = seq(0,1,by = 0.1)
log_df = data.frame(threshold,accuracy=0)
j<- 1
for (i in threshold){
  glm_pred=rep("No",24997)
  glm_pred[glm_probs>i]="Yes"
  table(glm_pred,new_df$divdecision,dnn = c("Predicted", "Actual"))
  # % of dividend decisions that are predicting correctly 
  log_df[j,2]<- mean(glm_pred==new_df$divdecision)
  j = j + 1
}
plot(log_df)

## LDA
lda_fit=lda(divdecision~tlasset+cerat+lrat+comage+SALES+EBITDA,data=new_df)
lda_fit

lda_pred=predict(lda_fit, new_df)

# getting "class"
lda_class=lda_pred$class 
table(lda_class,new_df$divdecision,dnn = c("Predicted", "Actual"))

# % of dividend decisions that are correctly predicted
mean(lda_class==new_df$divdecision)

# The default threshold is 50%
sum(lda_pred$posterior[,1]>=.5)
sum(lda_pred$posterior[,1]<.5) 
# Lowering the threshold to 40%
new.class.40 = rep("Yes", length(new_df$divdecision))
new.class.40[lda_pred$posterior[,1] >= 0.4] = "No"
table(new.class.40, new_df$divdecision,dnn = c("Predicted", "Actual"))
# Lowering the threshold to 30%
new.class.30 = rep("Yes", length(new_df$divdecision))
new.class.30[lda_pred$posterior[,1] >= 0.3] = "No"
table(new.class.30, new_df$divdecision,dnn = c("Predicted", "Actual"))

# (c)
train=new_df[new_df$year<=2016,]
test=new_df[new_df$year>2016,]

## Linear regression
lm_train = lm(casholding~tlasset+cerat+lrat+comage+SALES+EBITDA,data=train)
summary(lm_train)
lm_probs=predict(lm_train,test,type="response")
error <- lm_probs - test$casholding
lm_acc <- 1-sqrt(mean(error^2))

## Logistic regression
glm_train=glm(divdecision~tlasset+cerat+lrat+comage+SALES+EBITDA,data=train,family=binomial)
summary(glm_train)
glm.probs=predict(glm_train,test,type="response")

glm.pred= rep("No",8953)
glm.pred[glm.probs>.5]=" Yes"
table (glm.pred,test$divdecision,dnn = c("Predicted", "Actual"))
log_acc <- mean(glm.pred==test$divdecision)

## LDA
lda_train=lda(divdecision~tlasset+cerat+lrat+comage+SALES+EBITDA,data=train)
pred_test = predict(lda_train, test) 
lda_class_test = pred_test$class
# Confusion table
table(lda_class_test,test$divdecision,dnn = c("Predicted", "Actual"))
# Prediction accuracy
lda_acc <- mean(lda_class_test==test$divdecision)

pred_acc <- data.frame(model = c("linear regression","logistic regression","LDA"), 
                       dependent_variable = c("Cash holding","Dividend payment descision","Dividend payment descision"),
                       accuracy = c(lm_acc,log_acc,lda_acc))
stargazer(pred_acc,digits = 4,summary = F, type = "html", out = "accuracy.html")

####################### Part 4 #########################

### Linear Regression Model
## LOOCV
#error_loocv=rep (0,19)
#for (i in 1:19){
#  lm_fit_lv=glm(casholding~poly(comage,i)+tlasset+cerat+lrat+SALES+EBITDA,data=new_df)
#  print(i)
#  error_loocv[i]= round (cv.glm(new_df,lm_fit_lv)$delta[1],2)
#}
#error_loocv

## The Validation Set Approach
set.seed(1)
train.vsa=sample(c(TRUE,FALSE), nrow(new_df),rep=TRUE)
test.vsa=(!train.vsa)
train_vsa=new_df[train.vsa,]
test_vsa=new_df[test.vsa,]
# Create a table of each power and the corresponding MSE
power = c(1:19)
polytable_vsa = as.data.frame(power)
# Set a vector of 19 MSE
error_vsa=rep(0,19) 

for (i in 1:19) {
  lm.fit=glm(casholding~poly(comage,i)+tlasset+cerat+lrat+SALES+EBITDA,data=train_vsa)
  error_vsa[i] = mean((predict(lm.fit,test_vsa)-test_vsa$casholding)^2)
  polytable_vsa$mse[i] = error_vsa[i]
}
polytable_vsa

## 10-fold Cross-Validation
set.seed (1)
# Create a table of 19 power and the corresponding MSE
power = c(1:19)
polytable_cv = as.data.frame(power)
# Set a vector for 19 MSE
error_cv = rep(0,19)
for (i in 1:19) {
glm.fit=glm(casholding~poly(comage,i)+tlasset+cerat+lrat+SALES+EBITDA,data=new_df)
error_cv[i]=cv.glm(new_df,glm.fit,K=10)$delta [1]
polytable_cv$mse[i]=error_cv[i]
}
polytable_cv

### Logistic Regression Model
## The Validation Set Approach
set.seed(1)
train.vsa=sample(c(TRUE,FALSE), nrow(new_df),rep=TRUE)
test.vsa=(!train.vsa)
train_vsa=new_df[train.vsa,]
test_vsa=new_df[test.vsa,]
# Create a table of each power and the corresponding MSE
power = c(1:19)
logist_polytable_vsa = as.data.frame(power)
# Set a vector of 19 MSE
logist_error_vsa=rep(0,19) 

for (i in 1:19) {
  logist.lm.fit=glm(divdecision~poly(comage,i)+tlasset+cerat+lrat+SALES+EBITDA,data=train_vsa,family=binomial)
  logist_probs=predict(logist.lm.fit,test_vsa,type="response")
  logist_pred= rep("No",nrow(test_vsa))
  logist_pred[logist_probs>.5]=" Yes"
  logist_error_vsa[i] = mean(logist_pred!=test_vsa$divdecision)
  logist_polytable_vsa$mse[i] = logist_error_vsa[i]
}
logist_polytable_vsa

## 10-fold Cross-Validation
set.seed (1)
# Create a table of 19 power and the corresponding MSE
power = c(1:19)
logist_polytable_cv = as.data.frame(power)
# Set a vector for 19 MSE
logist_error_cv = rep(0,19)
for (i in 1:19) {
  logist.glm.fit=glm(divdecision~poly(comage,i)+tlasset+cerat+lrat+SALES+EBITDA,data=new_df,family=binomial)
  logist_error_cv[i]=cv.glm(new_df,logist.glm.fit,K=10)$delta [1]
  logist_polytable_cv$mse[i]=logist_error_cv[i]
}
logist_polytable_cv

### LDA Model
## The Validation Set Approach
set.seed(1)
train.vsa=sample(c(TRUE,FALSE), nrow(new_df),rep=TRUE)
test.vsa=(!train.vsa)
train_vsa=new_df[train.vsa,]
test_vsa=new_df[test.vsa,]
# Create a table of each power and the corresponding MSE
power = c(1:19)
lda_polytable_vsa = as.data.frame(power)
# Set a vector of 19 MSE
lda_error_vsa=rep(0,19) 

for (i in 1:19) {
  lda.lm.fit=lda(divdecision~poly(comage,i)+tlasset+cerat+lrat+SALES+EBITDA,data=train_vsa)
  lda_pred_vsa=predict(lda.lm.fit,test_vsa) 
  lda_class_vsa = lda_pred_vsa$class
  lda_error_vsa[i] = mean(lda_class_vsa!=test_vsa$divdecision)
  lda_polytable_vsa$mse[i] = lda_error_vsa[i]
}
lda_polytable_vsa

## 10-fold Cross-Validation
k=10
set.seed (1)
folds=sample (1:k,nrow(new_df),replace =TRUE)
cv.errors=matrix (NA,k,19,dimname =list(NULL,paste(1:19)))
# Create a table of 19 power and the corresponding MSE
power = c(1:19)
lda_polytable_cv = as.data.frame(power)
# Set a vector for 19 MSE
lda_error_cv = rep(0,19)

for (i in 1:19) {
  for(j in 1:k){
    lda.glm.fit=lda(divdecision~poly(comage,i)+tlasset+cerat+lrat+SALES+EBITDA,data=new_df[folds!=j,])
    pred=predict(lda.glm.fit,new_df[folds==j,],id=i)
    pred.class=pred$class
    cv.errors[j,i]= mean(new_df$divdecision[folds==j]!=pred.class)
  }  
  lda_error_cv[i]=mean(cv.errors[,i])
  lda_polytable_cv$mse[i]=lda_error_cv[i]
}
lda_polytable_cv

comparison <- data.frame(polytable_vsa[1],polytable_vsa[2],polytable_cv[2],logist_polytable_vsa[2],logist_polytable_cv[2],lda_polytable_vsa[2],lda_polytable_cv[2])

