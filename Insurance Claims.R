library(tidyverse)
library(cluster)
library(factoextra)
library(gridExtra)
library(ggplot2)
library(ellipse)
library(ggcorrplot)
library(RColorBrewer)
library(nFactors)
library(psych)
library(lattice)
library(caTools)
library(rpart)
library(rpart.plot)
library(rattle)
library(data.table)
library(ROCR)
library(ineq)
library(StatMeasures)
library(htmlwidgets)
library(DataExplorer)
library(corrplot)
library(partykit)
library(dplyr)
library(purrr)
library(InformationValue)
library(car)
library(ROCR)
library(MASS)
library(e1071)
library(class)
library(caret)
library(readxl)
library(DMwR)
library(ipred)
library(gbm)
library(Matrix)
library(randomForest)
library(mlr)
library(xgboost)

getwd()
setwd("C:/Users/Samrat/Documents/R/Directories/Capstone Project")



data = read_excel("claims data.xlsx")


summary(data)
str(data)
sum(is.na(data))

attach(data)




boxplot(DRV_CLAIM_AMT)

qnt = quantile(DRV_CLAIM_AMT, probs = c(.25, .75),na.rm = T)
caps = quantile(DRV_CLAIM_AMT, probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(DRV_CLAIM_AMT)
DRV_CLAIM_AMT[DRV_CLAIM_AMT < (qnt[1] - H)] = caps[1]
DRV_CLAIM_AMT[DRV_CLAIM_AMT > (qnt[2] + H)] = caps[2]

boxplot(DRV_CLAIM_AMT)


boxplot(Num_Net_OD_Premium)

qnt = quantile(Num_Net_OD_Premium, probs = c(.25, .75),na.rm = T)
caps = quantile(Num_Net_OD_Premium, probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(Num_Net_OD_Premium)
Num_Net_OD_Premium[Num_Net_OD_Premium < (qnt[1] - H)] = caps[1]
Num_Net_OD_Premium[Num_Net_OD_Premium > (qnt[2] + H)] = caps[2]

boxplot(Num_Net_OD_Premium)

boxplot(Num_IDV)

qnt = quantile(Num_IDV, probs = c(.25, .75),na.rm = T)
caps = quantile(Num_IDV, probs = c(.05, .95), na.rm = T)
H = 1.5 * IQR(Num_IDV)
Num_IDV[Num_IDV < (qnt[1] - H)] = caps[1]
Num_IDV[Num_IDV > (qnt[2] + H)] = caps[2]

boxplot(Num_IDV)
 
plot(as.factor(DRV_CLAIM_STATUS))

summary(as.factor(DRV_CLAIM_STATUS))

3876/75200


data2 = data[,-c(1,2,4,10,21,22,23,24,26)]

dendo = hclust( d = dist(data2))
plot(dendo)
rect.hclust(dendo, k=5, border="red")


plot(Num_Net_OD_Premium,DRV_CLAIM_AMT)
lm(DRV_CLAIM_AMT~Num_Net_OD_Premium)
abline(30178.391,1.601)

data2$Boo_Endorsement = as.numeric(data2$Boo_Endorsement)
data2$Txt_Policy_Code = as.numeric(data2$Txt_Policy_Code)
data2$Txt_Class_Code = as.numeric(data2$Txt_Class_Code)
data2$Txt_Zone_Code = as.numeric(data2$Txt_Class_Code)
data2$Num_Vehicle_Age = as.numeric(data2$Num_Vehicle_Age)
data2$Txt_CC_PCC_GVW_Code = as.numeric(data2$Txt_CC_PCC_GVW_Code)
data2$Txt_Permit_Code = as.numeric(data2$Txt_Permit_Code)
data2$Txt_Nature_Goods_Code = as.numeric(data2$Txt_Nature_Goods_Code)
data2$Txt_Road_Type_Code = as.numeric(data2$Txt_Road_Type_Code)
data2$Txt_Vehicle_Driven_By_Code = as.numeric(data2$Txt_Vehicle_Driven_By_Code)
data2$Txt_Driver_Exp_Code = as.numeric(data2$Txt_Driver_Exp_Code)
data2$Txt_Claims_History_Code = as.numeric(data2$Txt_Claims_History_Code)
data2$Txt_Driver_Qualification_Code = as.numeric(data2$Txt_Driver_Qualification_Code)
data2$Txt_Incurred_Claims_Code = as.numeric(data2$Txt_Incurred_Claims_Code)
data2$Boo_TPPD_Statutory_Cover_only = as.numeric(data2$Boo_TPPD_Statutory_Cover_only)
data2$Txt_TAC_NOL_Code = as.numeric(data2$Txt_TAC_NOL_Code)
data2$Boo_AntiTheft = as.numeric(data2$Boo_AntiTheft)
data2$Boo_NCB = as.numeric(data2$Boo_NCB)
data2$Boo_OD_Total_Loss = as.numeric(data2$Boo_OD_Total_Loss)

data2$DRV_CLAIM_STATUS = as.factor(data2$DRV_CLAIM_STATUS)
data2$DRV_CLAIM_STATUS = as.numeric(data2$DRV_CLAIM_STATUS)
data2$DRV_CLAIM_STATUS[data2$DRV_CLAIM_STATUS == 1] = 0
data2$DRV_CLAIM_STATUS[data2$DRV_CLAIM_STATUS == 2] = 1

summary(as.factor(data2$DRV_CLAIM_STATUS))

model1 = lm(DRV_CLAIM_STATUS~., data = data2)
summary(model1)

model2 = lm(DRV_CLAIM_STATUS ~ Num_Vehicle_Age, data = data2)
summary(model2)

data2 = data2[,-c(2,4,5,7,11,13,16,19,22,23)]

str(data2)
summary(data2)

vif(model1)



corr.matrix = round(cor(data2),3) 
corr.matrix

ggcorrplot(corr.matrix, type = "lower", ggtheme = ggplot2::theme_gray,
           show.legend = TRUE, show.diag = TRUE, colors = c("cyan","white","sky blue"),
           lab = TRUE)

my_colors = brewer.pal(7, "Blues")
my_colors = colorRampPalette(my_colors)(100)
plotcorr(corr.matrix , col=my_colors[corr.matrix*50+50] , mar=c(1,1,1,1), )

cortest.bartlett(corr.matrix)

KMO(corr.matrix)

e = eigen(corr.matrix)
ev = e$values
ev

plot(ev, xlab = "Pricipal Components", ylab="Eigen Value", main = "Scree Plot", pch=20, col="blue")
lines(ev, col="red")

PCA = principal(data2[,-12], nfactors = 4, rotate = "none")
PCA
fa.diagram(PCA, simple = FALSE)


PCAdata = data.frame(PCA$scores)

colnames(PCAdata)[1] = "Nature of Goods, Edorsements and Discounts"
colnames(PCAdata)[2] = "Driver Details and Road Type"
colnames(PCAdata)[3] = "Vehicle Details"
colnames(PCAdata)[4] = "Loss and Claim Details"



new.data = cbind(PCAdata,data2$DRV_CLAIM_STATUS)

colnames(new.data)[5] = "Fraudulent Claim"
new.data$`Fraudulent Claim`= as.factor(new.data$`Fraudulent Claim`)


attach(new.data)

set.seed(100)
split = sample.split(new.data$`Fraudulent Claim`, SplitRatio = 0.75)
train.data = subset(new.data, split == TRUE)
test.data = subset(new.data, split == FALSE)



summary(as.factor(train.data$`Fraudulent Claim`))
summary(as.factor(test.data$`Fraudulent Claim`))

model3 = glm(`Fraudulent Claim`~., data = train.data, family = "binomial")
summary(model3)

boxplot(`Fraudulent Claim`,model3$fitted.values)

prediction1 = predict(model3, newdata = test.data, type = "response")
cmLR = table(test.data$`Fraudulent Claim`,prediction1 > 0.1)
cmLR


#Accuracy
sum(diag(cmLR))/sum(cmLR)
#Recall or TPR
recall = 15987/(15987+1844)
print(recall)
#Precision
precision = 15987/(15987+722)
print(precision)
#Specificity
247/(247+722)
#FPR
722/(722+247)
#F1 Score
F1 = (2*precision*recall)/(precision+recall)
print(F1)


ROCRpred = prediction(prediction1, test.data$`Fraudulent Claim`)
as.numeric(ROCR::performance(ROCRpred,"auc")@y.values)
perf = ROCR::performance(ROCRpred, "tpr","fpr")
plot(perf)



set.seed(100)
split = sample.split(data2$DRV_CLAIM_STATUS, SplitRatio = 0.75)
train.data2 = subset(data2, split == TRUE)
test.data2 = subset(data2, split == FALSE)


model4 = glm(DRV_CLAIM_STATUS~., data = train.data2, family = "binomial")
summary(model4)


prediction2 = predict(model4, newdata = test.data2, type = "response")
cmLR2 = table(test.data2$DRV_CLAIM_STATUS,prediction2 > 0.1)
cmLR2


plot(test.data2$DRV_CLAIM_STATUS,prediction2 > 0.1)

#Accuracy
sum(diag(cmLR2))/sum(cmLR2)
#Recall or TPR
recall = 15977/(15977+1854)
print(recall)
#Precision
precision = 15977/(15977+584)
print(precision)
#Specificity
385/(584+385)
#FPR
584/(584+385)
#F1 Score
F1 = (2*precision*recall)/(precision+recall)
print(F1)



ROCRpred1 = prediction(prediction2, test.data2$DRV_CLAIM_STATUS)
as.numeric(ROCR::performance(ROCRpred1, "auc")@y.values)
perf1 = ROCR::performance(ROCRpred1, "tpr","fpr")
plot(perf1)



#SMOTE

train.data$`Fraudulent Claim` = as.factor(train.data$`Fraudulent Claim`)
str(train.data)


set.seed(1000)
balanced.data = SMOTE(`Fraudulent Claim` ~.,perc.over = 500 , data = train.data , k = 5, perc.under = 800)
table(balanced.data$`Fraudulent Claim`)

LR.smote = glm(`Fraudulent Claim`~., data = balanced.data, family = "binomial")
summary(LR.smote)


prediction3 = predict(LR.smote, newdata = test.data, type = "response")
cmLR3 = table(test.data$`Fraudulent Claim`,prediction3 > 0.1)
cmLR3



#Accuracy
sum(diag(cmLR3))/sum(cmLR3)
#Recall or TPR
recall = 8286/(9545+8286)
print(recall)
#Precision
precision = 8286/(8286+187)
print(precision)
#Specificity
782/(187+782)
#FPR
187/(187+782)
#F1 Score
F1 = (2*precision*recall)/(precision+recall)
print(F1)



ROCRpred2 = prediction(prediction3, test.data$`Fraudulent Claim`)
as.numeric(ROCR::performance(ROCRpred2, "auc")@y.values)
perf2 = ROCR::performance(ROCRpred2, "tpr","fpr")
plot(perf2)

#Naive Bayes
NBmodel = naiveBayes(`Fraudulent Claim` ~., data = train.data)
NBpredTest = predict(NBmodel, newdata = test.data, type = "class")
cmNB = table(test.data$`Fraudulent Claim`, NBpredTest)
cmNB

#Accuracy
sum(diag(cmNB))/sum(cmNB)
#Recall or TPR
recall = 17812/(17812+19)
print(recall)
#Precision
precision = 17812/(17812+969)
print(precision)
#Specificity
0/969
#FPR
969/(969+0)
#F1 Score
F1 = (2*precision*recall)/(precision+recall)
print(F1)





NBmodel.bal = naiveBayes(`Fraudulent Claim` ~., data = balanced.data)
NBpredTest.bal = predict(NBmodel.bal, newdata = test.data)
cmNB.bal = table(test.data$`Fraudulent Claim`, NBpredTest.bal)
cmNB.bal

#Accuracy
sum(diag(cmNB.bal))/sum(cmNB.bal)
#Recall or TPR
recall = 17286/(17286+545)
print(recall)
#Precision
precision = 17286/(17286+896)
print(precision)
#Specificity
194/(194+775)
#FPR
775/(194+775)
#F1 Score
F1 = (2*precision*recall)/(precision+recall)
print(F1)


data3 = data2

data3$Boo_Endorsement = as.factor(data3$Boo_Endorsement)
data3$Txt_Class_Code = as.factor(data3$Txt_Class_Code)
data3$Txt_CC_PCC_GVW_Code = as.factor(data3$Txt_CC_PCC_GVW_Code)
data3$Txt_Permit_Code = as.factor(data3$Txt_Permit_Code)
data3$Txt_Nature_Goods_Code = as.factor(data3$Txt_Nature_Goods_Code)
data3$Txt_Road_Type_Code = as.factor(data3$Txt_Road_Type_Code)
data3$Txt_Driver_Exp_Code = as.factor(data3$Txt_Driver_Exp_Code)
data3$Txt_Driver_Qualification_Code = as.factor(data3$Txt_Driver_Qualification_Code)
data3$Txt_Incurred_Claims_Code = as.factor(data3$Txt_Incurred_Claims_Code)
data3$Txt_TAC_NOL_Code = as.factor(data3$Txt_TAC_NOL_Code)
data3$Boo_OD_Total_Loss = as.factor(data3$Boo_OD_Total_Loss)
data3$DRV_CLAIM_STATUS = as.factor(data3$DRV_CLAIM_STATUS)
data3$Boo_AntiTheft = as.factor(data3$Boo_AntiTheft)

str(data3)

#Random Forest

set.seed(100)
split = sample.split(data3$DRV_CLAIM_STATUS, SplitRatio = 0.75)
train.data3 = subset(data3, split == TRUE)
test.data3 = subset(data3, split == FALSE)


RF.model = randomForest(DRV_CLAIM_STATUS~., data = train.data3, ntree = 500, mtry = 5, nodesize = 10, importance = TRUE)
print(RF.model)

plot.tree(RF.model)

RFpredTest = predict(RF.model, newdata = test.data3, type = "class")
cmRF = table(test.data3$DRV_CLAIM_STATUS, RFpredTest)
cmRF

#Accuracy
sum(diag(cmRF))/sum(cmRF)
#Recall or TPR
recall = 17790/(17790+41)
print(recall)
#Precision
precision = 17790/(17790+917)
print(precision)
#Specificity
52/(917+52)
#FPR
917/(917+52)
#F1 Score
F1 = (2*precision*recall)/(precision+recall)
print(F1)





tuned.RFmodel = tuneRF(x=train.data3[,-12], y = train.data3$DRV_CLAIM_STATUS, mtryStart = 3, stepFactor = 1.5, ntreeTry = 501, improve = 0.0001, 
                    trace = TRUE, plot = TRUE, doBest = TRUE, importance = TRUE)

test.data3$Predict.Class = predict(tuned.RFmodel, test.data3, type = "class")
test.data3$Prob = predict(tuned.RFmodel, test.data3, type = "prob")[,"1"]

cmtRF = table(test.data3$DRV_CLAIM_STATUS,test.data3$Predict.Class)
cmtRF


#Accuracy
sum(diag(cmtRF))/sum(cmtRF)
#Recall or TPR
recall = 53491/(53491+2869)
print(recall)
#Precision
precision = 53491/(53491+2)
print(precision)
#Specificity
38/(2869+38)
#FPR
2/(2+38)
#F1 Score
F1 = (2*precision*recall)/(precision+recall)
print(F1)





#Bagging and Boosting

bag.model = bagging(DRV_CLAIM_STATUS ~.,data = train.data2, control=rpart.control(xval = 0, maxdepth = 20, minsplit = 10), coob = TRUE)

BagpredTest = predict(bag.model, newdata = test.data2, type = "class")
cmBag = table(test.data2$DRV_CLAIM_STATUS, BagpredTest)
cmBag

#Accuracy
sum(diag(cmLR3))/sum(cmLR3)

setDT(balanced.data)
setDT(test.data)

features.train = as.matrix(balanced.data[,-5])
label.train = as.matrix(balanced.data$`Fraudulent Claim`)
label.test = test.data$`Fraudulent Claim`
features.test = as.matrix(test.data[,-5])


XGtrain = xgb.DMatrix(data = features.train, label = label.train)




XGBmodel = xgboost(data = features.train , label = label.train,  eta = 0.1,
                   max_depth = 3,
                   nrounds = 10,
                   nfold = 5,
                   objective = "binary:logistic",  
                   verbose = 0,   
                   early_stopping_rounds = 10)


XGBpredTest = predict(XGBmodel, features.test)
cmXGB = table(test.data$`Fraudulent Claim`, XGBpredTest>0.1)
cmXGB
sum(diag(cmXGB))/sum(cmXGB)


