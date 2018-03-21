## Step 1  - Read in Data
data=read.csv("LogisticRegressionData.csv")
names(data)



class(data)
summary(data)
out_sample=which(is.na(data$CKD)==1)
data_out=data[out_sample,]   ## the ones without a disease status
data_in=data[-out_sample,]   ## the ones with a disease status


#predicting missing data #ignore
predict_weight = which(is.na(data$Weight)==1)
predict_height = which(is.na(data$Height)==1)
predict_waist = which(is.na(data$Waist)==1)
predict_SBP = which(is.na(data$SBP)==1)
predict_DBP = which(is.na(data$DBP)==1)
predict_HDL = which(is.na(data$HDL)==1)
predict_LDL = which(is.na(data$LDL)==1)
predict_total = combine(predict_weight,predict_height,predict_waist,predict_SBP,predict_DBP,predict_HDL,predict_LDL)
predict_total = unique(predict_total)
#predict_data = data[-predict_total,]



#remove categorical variables
predict_data = data[,c(2,10,11,14,15,16,17,18)]
names(predict_data)


#####################################
# Predictive mean matching 
#pmm stands for predictive mean matching, default method of mice() for imputation of continous incomplete variables;
#for each missing value, pmm finds a set of observed values with the closest predicted mean as the missing one 
#and imputes the missing values by a random draw from that set. 
#Therefore, pmm is restricted to the observed values, and might do fine even for categorical data (though not recommended).
#install mice
#install.packages("mice")

library(mice)
md.pattern(predict_data)

impute_data <- mice(predict_data,m=5, method = "pmm",maxit = 25,seed = 500)

completedata <- complete(impute_data,5)

data$Age <- completedata$Age
data$Weight <- completedata$Weight
data$Height <- completedata$Height
data$BMI <- data$Weight/(data$Height/100)^2  #BMI formula
data$Obese <- ifelse(data$BMI>29,1,0)
data$Waist <- completedata$Waist
data$SBP <- completedata$SBP
data$DBP <- completedata$DBP
data$HDL <- completedata$HDL
data$LDL <- completedata$LDL
data$Total.Chol <- data$LDL + data$HDL



##########################################
## Support vector machine to determine missing categorical data
#we start from lowest missing data and add on .... we cant use it unless we know what factors contribute to missing data
library(e1071)
completedata$Diabetes <- data$Diabetes
completedata$BMI <- data$BMI
missing_diabetes <- which(is.na(completedata$Diabetes)==1)
names(completedata)
diatrain <- completedata[-missing_diabetes,c("Age","BMI","Diabetes")]
diatest <- completedata[missing_diabetes,c("Age","BMI")]

diasvm <- svm(Diabetes~.,data = diatrain,kernel = "linear",cost = 10,scale = FALSE,type = "C-classification")

#tuned <- tune(svm,Diabetes~.,data = diatrain,kernel = "linear",ranges = list(cost = c(0.001,0.01,.1,1,10,100))) #cost should be 10
summary(tuned)

p <- predict(diasvm,diatest,type = "levels")

p  #shows that id 925 and 8243 both arent diabetic w.r.t age and bmi which are the main factors contributing to diabetes

data[c(925,8243),c("Diabetes")] <- c(0,0) #assigning those values
completedata$Diabetes <- data$Diabetes


####################################################
##KNN

library(caret)
library(class)

diatrain <- completedata[-missing_diabetes,c("Age","BMI","Diabetes")]
diatest <- completedata[missing_diabetes,c("Age","BMI","Diabetes")]

prc_test_pred <- knn(train = diatrain, test = diatest,cl = diatrain$Diabetes, k=5)
prc_test_pred

#Diabetes
completedata$Diabetes <- data$Diabetes
diabetes_missing <- which(is.na(completedata$Diabetes)==1)
diabetes_train <- completedata[-diabetes_missing,]
diabetes_test <- completedata[diabetes_missing,]
diabetes_train$Diabetes <- factor(diabetes_train$Diabetes)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
knn_fit <- train(Diabetes~., data = diabetes_train, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
diabetes_pred <- predict(knn_fit, newdata = diabetes_test)
diabetes_pred #both 0
completedata[diabetes_missing,"Diabetes"] <- 0

#Anemia
completedata$Anemia <- data$Anemia
anemia_missing <- which(is.na(completedata$Anemia)==1)
anemia_train <- completedata[-anemia_missing,]
anemia_test <- completedata[anemia_missing,]
anemia_train$Anemia <- factor(anemia_train$Anemia)
#anemia_test[,"Anemia"] <- 1
#anemia_pred <- knn(train = anemia_train, test = anemia_test,cl = anemia_train$Anemia, k=5)
#anemia_pred #all 0

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
knn_fit <- train(Anemia~., data = anemia_train, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
anemia_pred <- predict(knn_fit, newdata = anemia_test)
anemia_pred #shows values all close to zero
completedata[anemia_missing,"Anemia"] <- 0


#Activity
completedata$Activity <- data$Activity
activity_missing <- which(is.na(completedata$Activity)==1)
activity_train <- completedata[-activity_missing,]
activity_test <- completedata[activity_missing,]
activity_train[["Activity"]] <- factor(activity_train[["Activity"]])


trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
knn_fit <- train(Activity~., data = activity_train, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
activity_pred <- predict(knn_fit, newdata = activity_test)
activity_pred #all are 2 except for 7245 who is a 1

completedata[c(1284,1402,4000,4016,4403,4651,4818,5504,8372),"Activity"] <- 2
completedata[7245,"Activity"]<-1


#Stroke
completedata$Stroke <- data$Stroke
stroke_missing <- which(is.na(completedata$Stroke)==1)
stroke_train <- completedata[-stroke_missing,]
stroke_test <- completedata[stroke_missing,]
stroke_train$Stroke <- factor(stroke_train$Stroke)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
knn_fit <- train(Stroke~., data = stroke_train, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
stroke_pred <- predict(knn_fit, newdata = stroke_test)
stroke_pred #all 0

completedata[stroke_missing,"Stroke"] <- 0

#Education
completedata$Educ <- data$Educ
educ_missing <- which(is.na(completedata$Educ)==1)
educ_train <- completedata[-educ_missing,]
educ_test <- completedata[educ_missing,]
educ_train$Educ <- factor(educ_train$Educ)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
knn_fit <- train(Educ~., data = educ_train, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
educ_pred <- predict(knn_fit, newdata = educ_test)
educ_pred 

educ_test$Educ <- educ_pred
completedata[educ_missing,"Educ"] <- educ_test$Educ


#CVD
completedata$CVD <- data$CVD
cvd_missing <- which(is.na(completedata$CVD)==1)
cvd_train <- completedata[-cvd_missing,]
cvd_test <- completedata[cvd_missing,]
cvd_train$CVD <- factor(cvd_train$CVD)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
knn_fit <- train(CVD~., data = cvd_train, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
cvd_pred <- predict(knn_fit, newdata = cvd_test)
cvd_pred #all 0
completedata[cvd_missing,"CVD"] <- 0


#CHF
completedata$CHF <- data$CHF
chf_missing <- which(is.na(completedata$CHF)==1)
chf_train <- completedata[-chf_missing,]
chf_test <- completedata[chf_missing,]
chf_train$CHF <- factor(chf_train$CHF)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
knn_fit <- train(CHF~., data = chf_train, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
chf_pred <- predict(knn_fit, newdata = chf_test)
chf_pred #all 0
completedata[chf_missing,"CHF"] <- 0


#Hypertension
completedata$Hypertension <- data$Hypertension
hypertension_missing <- which(is.na(completedata$Hypertension)==1)
hypertension_train <- completedata[-hypertension_missing,]
hypertension_test <- completedata[hypertension_missing,]
hypertension_train$Hypertension = factor(hypertension_train$Hypertension)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
knn_fit <- train(Hypertension~., data = hypertension_train, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
hypertension_pred <- predict(knn_fit, newdata = hypertension_test)
hypertension_pred #most 0 few 1
hypertension_test$Hypertension <- hypertension_pred
completedata[hypertension_missing,"Hypertension"] <- hypertension_test$Hypertension

#insured
completedata$Insured <- data$Insured
insured_missing <- which(is.na(completedata$Insured)==1)
insured_train <- completedata[-insured_missing,]
insured_test <- completedata[insured_missing,]
insured_train$Insured <- factor(insured_train$Insured)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
knn_fit <- train(Insured~., data = insured_train, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
insured_pred <- predict(knn_fit, newdata = insured_test)
insured_pred
completedata[insured_missing,"Insured"] <- insured_pred

#fam cvd
completedata$Fam.CVD <- data$Fam.CVD
famcvd_missing <- which(is.na(completedata$Fam.CVD)==1)
famcvd_train <- completedata[-famcvd_missing,]
famcvd_test <- completedata[famcvd_missing,]
famcvd_train$Fam.CVD <- factor(famcvd_train$Fam.CVD)


trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
knn_fit <- train(Fam.CVD~., data = famcvd_train, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
famcvd_pred <- predict(knn_fit, newdata = famcvd_test)
famcvd_pred

completedata[famcvd_missing,"Fam.CVD"] <- famcvd_pred

#unmarried
completedata$Unmarried <- data$Unmarried
unmarried_missing <- which(is.na(completedata$Unmarried)==1)
unmarried_train <- completedata[-unmarried_missing,]
unmarried_test <- completedata[unmarried_missing,]
unmarried_train$Unmarried <- factor(unmarried_train$Unmarried)


trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
knn_fit <- train(Unmarried~., data = unmarried_train, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
unmarried_pred <- predict(knn_fit, newdata = unmarried_test)
unmarried_pred

completedata[unmarried_missing,"Unmarried"] <- unmarried_pred


#poor vision
completedata$PoorVision <- data$PoorVision
poorvision_missing <- which(is.na(completedata$PoorVision)==1)
poorvision_train <- completedata[-poorvision_missing,]
poorvision_test <- completedata[poorvision_missing,]
poorvision_train$PoorVision <- factor(poorvision_train$PoorVision)


trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
knn_fit <- train(PoorVision~., data = poorvision_train, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
poorvision_pred <- predict(knn_fit, newdata = poorvision_test)
poorvision_pred

completedata[poorvision_missing,"PoorVision"] <- poorvision_pred

#income
completedata$Income <- data$Income
income_missing <- which(is.na(completedata$Income)==1)
income_train <- completedata[-income_missing,]
income_test <- completedata[income_missing,]
income_train$Income <- factor(income_train$Income)


trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
knn_fit <- train(Income~., data = income_train, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)
income_pred <- predict(knn_fit, newdata = income_test)
income_pred

completedata[income_missing,"Income"] <- income_pred
###########################################################


###########################################################
#Add on remaining columns to completedata
completedata$Racegrp <- data$Racegrp
completedata$CareSource <- data$CareSource
completedata$Female <- data$Female
completedata$BMI <- data$BMI
completedata$Obese <- data$Obese
completedata$Total.Chol <- data$Total.Chol
completedata$Fam.Diabetes <- data$Fam.Diabetes
data_new=model.matrix(~-1+Racegrp+CareSource,data=completedata)

completedata = completedata[,-22] #removed racegrp
completedata = completedata[,-22] #removed caresource

completedata = cbind(completedata,data_new) #combine race and caresource as binary variables

############################################################
#splitting data into test and train   #already done since we have 2819 NA -> testing set
#6000 rows training set
id <- sample(2,nrow(completedata),prob = c(0.7,0.3), replace = TRUE)
#data_train <- completedata[id==1,]
#data_test <- completedata[id==2,]
#data_train$CKD <- data[id==1,"CKD"]
#data_test$CKD <- data[id==2,"CKD"]
#data_test <- data_test[,-34]

completedata_ckd <- cbind(completedata,data$CKD)
data_train <- completedata_ckd[-which(is.na(completedata_ckd$`data$CKD`)==1),]
data_test <-  completedata_ckd[which(is.na(completedata_ckd$`data$CKD`)==1),]

data_train$`data$CKD` <- factor(data_train$`data$CKD`)
colnames(data_train)[34] <- "CKD"
colnames(data_test)[34] <- "CKD"

#Decision tree
library(rpart)
colnames(completedata)
model <- rpart(CKD~.,data = data_train)
model

summary(model3) #identify relevant variables and try to use it in decision tree


#Decision tree with relevant variables  #can ignore
dtree_mod <- rpart(CKD~Age+BMI+Total.Chol+Diabetes+CVD+Stroke+Female+Anemia+HDL+LDL,data = data_train)
dtree_mod


plot(dtree_mod,margin=0.001)
text(dtree_mod,use.n = TRUE,pretty = TRUE,cex = 0.8)
fancyRpartPlot(dtree_mod)

predictions_dtree <- predict(dtree_mod, newdata = data_train, type="prob")
summary(predictions_dtree)

rocrpred_dtree <- prediction(predictions_dtree[,2],data_train$CKD)
rocrperf_dtree <- performance(rocrpred_dtree,"tpr","fpr") #tpr-true positive rate #fpr - false positive rate

pred.acc_dtree <- performance(rocrpred_dtree,"acc")
pred.acc_dtree

plot(rocrperf_dtree, colorize = TRUE, print.cutoffs.at = seq(0.1,by=0.1))
abline(v=0.10)


dtree_predict <- ifelse(predictions_dtree[,2]>0.10,1,0)

correct <- data_train[which(data_train$CKD!=dtree_predict),]


table(Actualvalue = data_train$CKD, predictedvalue = dtree_predict)




#########

#install.packages("rattle")
library(rattle)

plot(model,margin=0.001)
text(model,use.n = TRUE,pretty = TRUE,cex = 0.8)
fancyRpartPlot(model)

#predict
pred_ckd <- predict(model,newdata = data_train, type = "prob")
summary(pred_ckd)

auc(data_train$CKD, pred_ckd[,2]) #0.77 auc not good enough


library(caret)
table(Predicted = pred_ckd,Actualvalue = data_train$CKD)

sum(data_train$CKD==1)

nrow(data_train)

############################################
#Gradient boosted machine    #Not the best model however we get valuable insight to variables that give us relative importance
#can this be used as screening tool?!

#install.packages("gbm")
library(caret)
library(gbm)
fitcontrol <- trainControl(method = "repeatedcv", number = 10, repeats = 7)
fit <- train(CKD~., data = data_train, method = "gbm",trControl = fitcontrol, verbose = FALSE )
summary(fit)


print(fit)

predictions_train <- predict(fit, newdata = data_train, type="prob")
summary(predictions_train)


predictions_test <- predict(fit, newdata = data_test, type = "prob")
summary(predictions_test)

sum(predictions_test$`1`>0.5)

get_predict <- ifelse(predictions_train$`1`>0.1363801,1,0)


table(Actualvalue = data_train$CKD, predictedvalue = get_predict)


rocrpred_gbm <- prediction(predictions_train$`1`,data_train$CKD)
rocrperf_gbm <- performance(rocrpred_gbm,"tpr","fpr") #tpr-true positive rate #fpr - false positive rate

pred.acc_gbm <- performance(rocrpred_gbm,"acc")
pred.acc_gbm

plot(rocrperf_gbm, colorize = TRUE, print.cutoffs.at = seq(0.1,by=0.1))
plot(rocrperf, add = TRUE)
abline(v=0.115)

max(rocrperf_gbm@y.values[[1]])

cutoff.list.acc <- unlist(pred.acc_gbm @ x.values[[1]])
optimal.cutoff.acc<-cutoff.list.acc[which.max(pred.acc_gbm @ y.values[[1]])]
optimal.cutoff.acc  #gives your optimal cut off value?


cutoffs_gbm <- data.frame(cut=rocrperf_gbm@alpha.values[[1]], fpr=rocrperf_gbm@x.values[[1]], 
                      tpr=rocrperf_gbm@y.values[[1]])

head(cutoffs_gbm)

cutoffs_gbm <- cutoffs_gbm[order(cutoffs_gbm$tpr, decreasing=TRUE),]

head(subset(cutoffs_gbm, fpr < 0.19))

##Assess models   COMPARING GRADIENT BOOSTING vs LOGISTIC REG (0.8985 vs 0.9007) #pretty close 
#I believe gradient boosting provides us with a better insight however, logistic regression is a better predictor numerically

#auc <- performance(rocrpred_gbm, measure = "auc")
#auc <- auc@y.values[[1]]
#auc

library(pROC)

auc(data_train$CKD,predictions_train$`1`)
auc(data_train$CKD, mod_predict_train)


#auc2 <- performance(rocrpred, measure = "auc")
#auc2 <- auc2@y.values[[1]]
#auc2

#trellis.par.set(caretTheme())
#plot(fit, scales = list(x = list(log = 2)))

##########################
# RANDOM STUFF
mod_fit <- gbm(CKD ~. ,  data=data_train,distribution = "gaussian",n.trees = 10000,shrinkage = 0.01,interaction.depth = 4)

summary(mod_fit)

# BAGGING
library(ipred)
library(caret)
library(rpart)
bag_fit <- bagging(CKD~.,data = data_train, control=rpart.control(minsplit=1,xval = 25))

summary(bag_fit)

bag_trainpred <- predict(bag_fit,data_train)
summary(bag_trainpred)

#Random forest
library(randomForest)
id <- sample(2,nrow(data_train),prob = c(0.7,0.3), replace = TRUE)
forest_train <- data_train[id==1,]
forest_test <- data_train[id==2,]


forest_mod <- randomForest(CKD~.,data = forest_train)
summary(forest_mod)

forest_trainpred <- predict(forest_mod,forest_test[,1:33])
summary(forest_trainpred)

#K means on CKD patients
library(flexclust)
ckdppl <- data_train[which(data_train$CKD==1),]
ckdppl <- ckdppl[,c("Age","BMI","LDL","HDL","Diabetes","CVD","Female","SBP","DBP")]
ckdcluster <- kmeans(ckdppl,9)
ckdcluster$centers
ckdcluster

set.seed(123)
tss<-rep(0,10)
for (k in 1:10){tss[k]=kmeans(ckdppl,k)$tot.withinss}
plot(1:10,tss)


##################################################
#Clustering to identify similar people #IGNORE
library(flexclust)

tss<-rep(0,20)
for (k in 1:20){tss[k]=kmeans(completedata,k)$tot.withinss}
plot(1:20,tss)

cl = kmeans(as.matrix(completedata),5)
plot(as.matrix(completedata), col = cl$cluster)
points(cl$centers, col = 1:5, pch = 8)  


text(data,rownames(data),col=cl$cluster)
#################
#point system
pointdata <- data_train
pointdata$agescore <- ifelse(pointdata$Age>70,4,ifelse(pointdata$Age>60,3,ifelse(pointdata$Age>50,2,0)))
pointdata$CVDpoint <- ifelse(pointdata$CVD + pointdata$Fam.CVD==2,1,ifelse(pointdata$CVD+pointdata$Fam.CVD==1,0.5,0))
pointdata$diabetespoint <- ifelse(pointdata$Diabetes + pointdata$Fam.Diabetes ==2,1,ifelse(pointdata$Diabetes+pointdata$Fam.Diabetes==1,0.5,0))

pointdata$total <- rowSums(pointdata[,c("agescore","Anemia","Hypertension","Diabetes","CVDpoint")])

write.csv(pointdata,"pointscore.csv")



#########################################
#log regression  #same as class (we will end up with this model)

#install.packages('ROCR')
library(ROCR)


res_train <- glm(CKD~.,family = "binomial", data=data_train)
summary(res_train)

model3=step(res_train,direction="backward")
summary(model3)

glm.fitted <- fitted(model3)
glm.fitted

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}
logit2prop(coef(model3))


res_test_ckd <- predict(res_train,data_test,type = "response") #with all variables
summary(res_test_ckd)

res_test_ckd <- as.data.frame(res_test_ckd)
res_test_ckd$GBM <- predictions_test$`1`

  
mod_predict_train <- predict(model3,data_train, type = "response") #after elimating using backward
summary(mod_predict_train)

mod_predict_test <- predict(model3, data_test, type = "response") #probabilities of testing set
summary(mod_predict_test)

write.csv(mod_predict_test,file = "Group4.csv")

#res_train_ckd <- predict(res_train,training_data,type = "response")

table(Actualvalue = data_train$CKD, predictedvalue = mod_predict>0.5)

rocrpred <- prediction(mod_predict_train,data_train$CKD)
rocrperf <- performance(rocrpred,"tpr","fpr") #tpr-true positive rate #fpr - false positive rate

pred.acc <- performance(rocrpred,"acc")
pred.acc

max(rocrperf@y.values[[1]])

cutoff.list.acc <- unlist(pred.acc @ x.values[[1]])

optimal.cutoff.acc<-cutoff.list.acc[which.max(pred.acc @ y.values[[1]])]

optimal.cutoff.acc  #gives your optimal cut off value


cutoffs <- data.frame(cut=rocrperf@alpha.values[[1]], fpr=rocrperf@x.values[[1]], 
                      tpr=rocrperf@y.values[[1]])

head(cutoffs)

cutoffs <- cutoffs[order(cutoffs$tpr, decreasing=TRUE),]

head(subset(cutoffs, fpr<0.165))

table(Actualvalue = data_train$CKD, Predictedvalue = mod_predict_train>0.16) 



table(Actualvalue = data_train$CKD, Predictedvalue = mod_predict_train>0.17112)

#recall <- 
#precision <- 
#fmeasure <- 

## PPV curve #check

library(survey)
regTermTest(res_train,"Age")
varImp(res_train)

table(Actualvalue = data_train$CKD, Predictedvalue = mod_predict_train>0.1154) #final answer .1154
##########

plot(rocrpref, colorize = TRUE, print.cutoffs.at = seq(0.1,by=0.1))
abline(v=0.1154)
#spl <- smooth.spline(rocrperf@y.values$num~rocrperf@x.values$num)

#recheck again
table(Actualvalue = data_train$CKD, Predictedvalue = mod_predict>0.5315)



##########################################################################


#### IGNORE BELOW  ######


###################################################################################
install.packages('pROC')
library(pROC)
pROC(data_train$CKD,phat3)
model=roc(data_in$CKD~phat3,percent=TRUE,plot=TRUE)






## Step 2  - Missing Data
summary(data_in)
dim(data_in)
?na.omit
data_in=na.omit(data_in)
dim(data_in)

# Table comparison of data
table1 = c(mean(data$Age,na.rm = TRUE),
            mean(data$Hypertension,na.rm = TRUE),
            mean(data$Diabetes,na.rm = TRUE),
            mean(data$CVD,na.rm = TRUE),
            mean(data$Fam.Hypertension,na.rm = TRUE),
            mean(data$Fam.Diabetes,na.rm = TRUE),
            mean(data$Fam.CVD,na.rm = TRUE))

table2 = c(mean(data_in$Age),
            mean(data_in$Hypertension),
            mean(data_in$Diabetes),
            mean(data_in$CVD),
            mean(data_in$Fam.Hypertension),
            mean(data_in$Fam.Diabetes),
            mean(data_in$Fam.CVD))

table3 = cbind(table1,table2)
rownames(table3) <- c("Age","Hypertension","Diabetes","CVD","Fam.Hypertension","Fam.Diabetes","Fam.CVD")
colnames(table3) <- c("Average including missing data","Average exculding missing data")

## Step 3 and 4 - Correlation
cor(data_in)
summary(data_in)
data_new=model.matrix(~-1+Racegrp+CareSource,data=data_in)
summary(data_new)
data_in=data_in[,-c(4,8)]
data_in=cbind(data_in,data_new)
cor(data_in)
names(data_in)
data_in=data_in[,-33]
cor(data_in)

cor_data = data_in[,c(2,23,24,25,26,28,29)]

cor(data_in$CKD,cor_data)
## any highly correlated - large relation to PCA


## Step 5 - Run a Regression
model=lm(CKD~.,data=data_in)
summary(model)

summary(predict(model))

########################################
## Decision tree  #for later
library(rpart)
colnames(data)
#split in data into two parts  #no need to do this split has been provided based on CKD
#id <- sample(2,nrow(data_in),prob = c(0.7,0.3), replace = TRUE)
#data_in_train <- data_in[id==1,]
#data_in_test <- data_in[id==2,]
#tree_mod <- rpart(CKD~.,data = data_in_train)
#plot(tree_mod, margin = 0.2)
#text(tree_mod,use.n = TRUE,pretty = TRUE,cex = 0.8)


########################
##prediction ignore
library(caret)
#predictckd <- predict(tree_mod,newdata = data_in_test,type = "vector")
#predictckd

#glmpredictckd <- glm(CKD~.,data_in_train,family = "binomial")
#glmres <- predict(glmpredictckd,data_in_test,type = "response")
#table(Actualvalue = data_in_test$CKD, Predictedvalue = glmres>0.6)



