#D. Makowski
#Biomass (g/m2) 

DataSet<-read.table("BiomassMais.txt", sep="\t", header=T)

head(DataSet)
summary(DataSet)


####Regression tree
library(rpart)
library(rpart.plot)

Mod_tree<-rpart(B~T1+T2+T3+RAD1+RAD2+RAD3,data=DataSet)
print(Mod_tree)
dev.new()
par(mfrow=c(1,1))
rpart.plot(Mod_tree)
#text(Mod_tree)

###Random forest
library(randomForest)
Mod_RF<-randomForest(B~T1+T2+T3+RAD1+RAD2+RAD3,data=DataSet,ntree=500, mtry=6)
Mod_RF

plot(Mod_RF)
dev.new()
par(mfrow=c(1,1))
varImpPlot(Mod_RF,type=2)
RMSE_rf<-sqrt(mean((DataSet$B-predict(Mod_RF))^2))
RMSE_rf

#Cross-validation
B_pred_rf<-rep(NA,length(DataSet$B))

List_year<-unique(DataSet$Year)

for (i in 1:length(List_year)) 
{
Training_i<-DataSet[DataSet$Year!=List_year[i],]
Test_i<-DataSet[DataSet$Year==List_year[i],]	
Mod_i<-randomForest(B~T1+T2+T3+RAD1+RAD2+RAD3,data=Training_i, ntree=200)
B_rf_i<-predict(Mod_i, newdata=Test_i)
B_pred_rf[DataSet$Year==List_year[i]]<-B_rf_i	
}

RMSEP_rf<-sqrt(mean((DataSet$B-B_pred_rf)^2))
RMSEP_rf

dev.new()
par(mfrow=c(1,2))
plot(predict(Mod_RF),DataSet$B, xlab="Predicted values", ylab="Data")
abline(0,1)
title("A.                                        ")
text(2500,3100,paste("RMSE= ", round(RMSE_rf, digits=2)))
plot(B_pred_rf,DataSet$B, xlab="Predicted values", ylab="Data")
abline(0,1)
title("B.                                        ")
text(2600,3100,paste("RMSEP= ", round(RMSEP_rf, digits=2)))
