
print("The code section below will first install two R packages: ResourceSelection and pROC.") 
print("Please do not move to the next step until the packages are fully installed.") 
print("This will take some time.") 
print("Once the installation is complete, this step will print first 6 rows of the data set.")


# Loading R packages that are needed for some calculations below
install.packages("ResourceSelection")
install.packages("pROC")

# Loading credit card default data set
credit_default <- read.csv(file='credit_card_default.csv', header=TRUE, sep=",")

# Converting appropriate variables to factors  
credit_default <- within(credit_default, {
   default <- factor(default)
   sex <- factor(sex)
   education <- factor(education)
   marriage <- factor(marriage)
   assets <- factor(assets)
   missed_payment <- factor(missed_payment)
})

print("installation completed!")
print("data set (first 6 observations)")
head(credit_default, 6)

# Create the complete model
logit <- glm(default ~ credit_utilize + education , data = credit_default, family = "binomial")

summary(logit)

conf_int <- confint.default(logit, level=0.95)
round(conf_int,4)

library(ResourceSelection)


print("Hosmer-Lemeshow Goodness of Fit Test")
hl = hoslem.test(logit$y, fitted(logit), g=50)
hl

# Predict default or no_default for the data set using the model
default_model_data <- credit_default[c('education', 'credit_utilize')]
pred <- predict(logit, newdata=default_model_data, type='response')

# If the predicted probability of default is >=0.50 then predict credit default (default='1'), otherwise predict no credit 
# default (default='0') 
depvar_pred = as.factor(ifelse(pred >= 0.5, '1', '0'))

# This creates the confusion matrix
conf.matrix <- table(credit_default$default, depvar_pred)[c('0','1'),c('0','1')]
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ": default=")
colnames(conf.matrix) <- paste("Prediction", colnames(conf.matrix), sep = ": default=")

# Print nicely formatted confusion matrix
print("Confusion Matrix")
format(conf.matrix,justify="centre",digit=2)

library(pROC)

labels <- credit_default$default
predictions <- logit$fitted.values

roc <- roc(labels ~ predictions)

print("Area Under the Curve (AUC)")
round(auc(roc),4)

print("ROC Curve")
# True Positive Rate (Sensitivity) and False Positive Rate (1 - Specificity)
plot(roc, legacy.axes = TRUE)

print("Prediction: education is high school (education='1'), credit utilization is 40% (credit_utilize=0.40)")
newdata1 <- data.frame(education="1", credit_utilize=0.40)
pred1 <- predict(logit, newdata1, type='response')
round(pred1, 4)

print("Prediction: education is postgraduate (education='3'), credit utilization is 35% (credit_utilize=0.35)")
newdata2 <- data.frame(education="3", credit_utilize=0.35)
pred2 <- predict(logit, newdata2, type='response')
round(pred2, 4)


# Create the complete model
logit <- glm(default ~ credit_utilize + education , data = credit_default, family = "binomial")

summary(logit)

# Predict default or no_default for the data set using the model
default_model_data <- credit_default[c('education', 'credit_utilize')]
pred <- predict(logit, newdata=default_model_data, type='response')

# If the predicted probability of default is >=0.50 then predict credit default (default='1'), otherwise predict no credit 
# default (default='0') 
depvar_pred = as.factor(ifelse(pred >= 0.5, '1', '0'))

# This creates the confusion matrix
conf.matrix <- table(credit_default$default, depvar_pred)[c('0','1'),c('0','1')]
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ": default=")
colnames(conf.matrix) <- paste("Prediction", colnames(conf.matrix), sep = ": default=")

# Print nicely formatted confusion matrix
print("Confusion Matrix")
format(conf.matrix,justify="centre",digit=2)

library(ResourceSelection)


print("Hosmer-Lemeshow Goodness of Fit Test")
hl = hoslem.test(logit$y, fitted(logit), g=50)
hl

conf_int <- confint.default(logit, level=0.95)
round(conf_int,4)

library(pROC)

labels <- credit_default$default
predictions <- logit$fitted.values

roc <- roc(labels ~ predictions)

print("Area Under the Curve (AUC)")
round(auc(roc),4)

print("ROC Curve")
# True Positive Rate (Sensitivity) and False Positive Rate (1 - Specificity)
plot(roc, legacy.axes = TRUE)

print("Prediction: education is high school (education='1'), credit utilization is 35% (credit_utilize=0.35)")
newdata1 <- data.frame(education="1", credit_utilize=0.35)
pred1 <- predict(logit, newdata1, type='response')
round(pred1, 4)

print("Prediction: education is postgraduate (education='3'), credit utilization is 35% (credit_utilize=0.35)")
newdata2 <- data.frame(education="3", credit_utilize=0.35)
pred2 <- predict(logit, newdata2, type='response')
round(pred2, 4)

# Create the complete model
logit2 <- glm(default ~ credit_utilize + assets + missed_payment , data = credit_default, family = "binomial")

summary(logit2)

# Predict default or no_default for the data set using the model
default_model_data <- credit_default[c('assets', 'missed_payment', 'credit_utilize')]
pred <- predict(logit2, newdata=default_model_data, type='response')

# If the predicted probability of default is >=0.50 then predict credit default (default='1'), otherwise predict no credit 
# default (default='0') 
depvar_pred = as.factor(ifelse(pred >= 0.5, '1', '0'))

# This creates the confusion matrix
conf.matrix <- table(credit_default$default, depvar_pred)[c('0','1'),c('0','1')]
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ": default=")
colnames(conf.matrix) <- paste("Prediction", colnames(conf.matrix), sep = ": default=")

# Print nicely formatted confusion matrix
print("Confusion Matrix")
format(conf.matrix,justify="centre",digit=2)

conf_int <- confint.default(logit2, level=0.95)
round(conf_int,4)

library(ResourceSelection)


print("Hosmer-Lemeshow Goodness of Fit Test")
hl = hoslem.test(logit$y, fitted(logit2), g=50)
hl

library(pROC)

labels <- credit_default$default
predictions <- logit2$fitted.values

roc <- roc(labels ~ predictions)

print("Area Under the Curve (AUC)")
round(auc(roc),4)

print("ROC Curve")
# True Positive Rate (Sensitivity) and False Positive Rate (1 - Specificity)
plot(roc, legacy.axes = TRUE)

print("Prediction: asset is car only (assets='1'), credit utilization is 35% (credit_utilize=0.35), Has missed a payment (missed_payment='1)")
newdata1 <- data.frame(assets="1", credit_utilize=0.35, missed_payment="1")
pred1 <- predict(logit2, newdata1, type='response')
round(pred1, 4)

print("Prediction: assests are car and house (assets='3'), credit utilization is 35% (credit_utilize=0.35), has not missed paymnet (missed_payment=0)")
newdata2 <- data.frame(assets="3", credit_utilize=0.35, missed_payment="0")
pred2 <- predict(logit2, newdata2, type='response')
round(pred2, 4)

print("The code section below will first install two R packages: ResourceSelection and pROC.") 
print("Please do not move to the next step until the packages are fully installed.") 
print("This will take some time.") 
print("Once the installation is complete, this step will print first 6 rows of the data set.")


# Loading R packages that are needed for some calculations below
install.packages("ResourceSelection")
install.packages("pROC")

# Loading credit card default data set
credit_default <- read.csv(file='credit_card_default.csv', header=TRUE, sep=",")

# Converting appropriate variables to factors  
credit_default <- within(credit_default, {
   default <- factor(default)
   sex <- factor(sex)
   education <- factor(education)
   marriage <- factor(marriage)
   assets <- factor(assets)
   missed_payment <- factor(missed_payment)
})

print("installation completed!")
print("data set (first 6 observations)")
head(credit_default, 601)
