rm(list=ls())
setwd('C:/Users/Samruddhi/Desktop/BigMart')

x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

lapply(x,require,character.only=TRUE)
rm(x)

train_data= read.csv('train.csv',header=TRUE,na.strings=c('',' ','NA'))
test_data= read.csv('test.csv',header=TRUE,na.strings=c('',' ','NA'))


train_data$Item_Type = as.numeric(train_data$Item_Type)
train_data$Outlet_Size = as.numeric(train_data$Outlet_Size)
train_data$Outlet_Location_Type = as.numeric(train_data$Outlet_Location_Type)
train_data$Outlet_Type = as.numeric(train_data$Outlet_Type)

test_data$Item_Type = as.numeric(test_data$Item_Type)
test_data$Outlet_Size = as.numeric(test_data$Outlet_Size)
test_data$Outlet_Location_Type = as.numeric(test_data$Outlet_Location_Type)
test_data$Outlet_Type = as.numeric(test_data$Outlet_Type)

train_data$Item_Fat_Content= gsub('LF','Low Fat',train_data$Item_Fat_Content)
train_data$Item_Fat_Content= gsub('low fat','Low Fat',train_data$Item_Fat_Content)
train_data$Item_Fat_Content= gsub('reg','Regular',train_data$Item_Fat_Content)
test_data$Item_Fat_Content= gsub('LF','Low Fat',test_data$Item_Fat_Content)
test_data$Item_Fat_Content= gsub('low fat','Low Fat',test_data$Item_Fat_Content)
test_data$Item_Fat_Content= gsub('reg','Regular',test_data$Item_Fat_Content)

train_data$Item_Fat_Content= as.factor(train_data$Item_Fat_Content)
train_data$Item_Fat_Content= as.numeric(train_data$Item_Fat_Content)
test_data$Item_Fat_Content= as.factor(test_data$Item_Fat_Content)
test_data$Item_Fat_Content= as.numeric(test_data$Item_Fat_Content)

#Missing Value Analysis
missing_val_tr= data.frame(apply(train_data,2,function(x){(sum(is.na(x)/nrow(train_data))*100)}))
missing_val_te= data.frame(apply(test_data,2,function(x){(sum(is.na(x)/nrow(test_data))*100)}))

mean(train_data$Item_Weight,na.rm = TRUE)
median(train_data$Item_Weight,na.rm = TRUE)
train_data$Item_Weight[69]
train_data = knnImputation(train_data, k = 3,meth = 'median')  #Using KNN Imputation for substituting NAs
sum(is.na(test_data))
test_data = knnImputation(test_data, k = 3,meth = 'median')

# Outlier Analysis
cnames= c('Item_Weight','Item_Visibility','Item_MRP','Outlet_Establishment_Year')
# numeric_index = sapply(train_data,is.numeric)
# numeric_data = train_data[,numeric_index]
# cnames = colnames(numeric_data)
boxplot(train_data$Item_Visibility)
for(i in cnames){
     print(i)
     val = train_data[,i][train_data[,i] %in% boxplot.stats(train_data[,i])$out]
     print(length(val))
     train_data = train_data[which(!train_data[,i] %in% val),]
}

for(i in cnames){
  print(i)
  valu = test_data[,i][test_data[,i] %in% boxplot.stats(test_data[,i])$out]
  print(length(valu))
  test_data = test_data[which(!test_data[,i] %in% valu),]
}

# Feature Selection
corrgram(train_data[,cnames],col.regions = colorRampPalette(c("red", "salmon","white", "royalblue", "navy")))

catnames= c('Item_Fat_Content','Item_Type','Outlet_establishment_Year','Outlet_Size','Outlet_Location_Type','Outlet_Type')
factor_data= train_data[,catnames]
# for (i in 1:10)
# {
#   print(names(factor_data)[i])
#   print(chisq.test(table(factor_data$responded,factor_data[,i])))
# }

# Feature Scaling
#Normalisation
hist(train_data$Item_Weight)
for(i in cnames){
  print(i)
  train_data[,i] = (train_data[,i] - min(train_data[,i]))/
    (max(train_data[,i] - min(train_data[,i])))
}

for(i in cnames){
  print(i)
  test_data[,i] = (test_data[,i] - min(test_data[,i]))/
    (max(test_data[,i] - min(test_data[,i])))
}

train_data$Item_Identifier= sapply(train_data$Item_Identifier,unclass)
train_data$Item_Type= sapply(train_data$Item_Type,unclass)
train_data$Outlet_Identifier =sapply(train_data$Outlet_Identifier,unclass)

test_data$Item_Identifier= sapply(test_data$Item_Identifier,unclass)
test_data$Item_Type= sapply(test_data$Item_Type,unclass)
test_data$Outlet_Identifier =sapply(test_data$Outlet_Identifier,unclass)

write.csv(train_data,'train_data.csv',row.names = FALSE)
write.csv(test_data,'test_data.csv',row.names = FALSE)
############################################## Model Development ##############################################

# Random Forest

rmExcept("train_data")
train_index= createDataPartition(train_data$Item_Outlet_Sales,p=0.8, list = FALSE)
train_1= train_data[train_index,]
test_1 =train_data[-train_index,]

RF_model= randomForest(Item_Outlet_Sales ~ .,data = train_1 ,method='anova',ntree= 500)
RF_Predictions = predict(RF_model, test_1[,-12])

regr.eval(test_1[,12],RF_Predictions,stats=c('mape','mae','rmse','mse'))
#Error rate= 58.097%  #Accuracy= 41.903%

#Linear Regression
library(usdm)
vif(train_data[,-12])
vifcor(train_data[,-12],th=0.9)

lm_model= lm(Item_Outlet_Sales ~ .,data = train_1)
lm_predicions= predict(lm_model,test_1[,1:11])
summary(lm_model)

mape = function(y, yhat){
  mean(abs((y - yhat)/y))
}
mape(test_1[,12],lm_predicions)
#Error Rate= 90.941% Accuracy = 9.06%

#KNN
library(class)
knn_model = knn(train_1[,1:11], test_1[,1:11],train_1$Item_Outlet_Sales, k=1)
knn_prediction= predict(knn_model, test_1[,1:11])
mape(test_1[,12],knn_prediction)