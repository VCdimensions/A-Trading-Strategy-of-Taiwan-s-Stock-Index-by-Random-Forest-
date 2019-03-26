####Custom Function
MonthtoDaily <- function(x,DaysofMonth){
        
        for(i in 1:nrow(DaysofMonth)){
                N = DaysofMonth[i]
                if(i == 1){
                        for(j in 1:N){
                                if(j == 1){
                                        temp = x[i,]
                                }else{
                                        temp2 = x[i,]
                                        temp = rbind.xts(temp,temp2)
                                }
                        }
                        
                        Temp = temp
                }else{
                        for(j in 1:N){
                                if(j == 1){
                                        temp = x[i,]
                                }else{
                                        temp2 = x[i,]
                                        temp = rbind.xts(temp,temp2)
                                }
                        }
                        Temp = rbind.xts(Temp,temp)
                }
        }
        Temp = xts(Temp,order.by = time(Model_Daily))
        return(Temp)
}

####Random Forest####
library(randomForest)
library(caret)
library(lubridate)
TimeGap = 2
AllYear = year(end(TWII)) - year(start(TWII))
S = as.Date('2000-01-01')

####Correlation####
library(mlbench)
library(caret)

# # load the data
# TechnicalIndex_Logical_Cor = cor(TechnicalIndex_Logical[,-1],use = "complete.obs")
# # summarize the correlation matrix
# print(TechnicalIndex_Logical_Cor)
# # 查找強相關屬性(理想是 >0.75)
# highlyCorrelated = findCorrelation(TechnicalIndex_Logical_Cor, cutoff=0.5) #cutoff=0.5找出相關性大於0.5的行
# # print indexes of highly correlated attributes
# print(highlyCorrelated)
# #可以考慮去掉跟其他鄉慣性較高的變數age

####Daily Base####
##Model1 Technical Index (Logical)
Ctrl = trainControl(method = "cv",number = 5, repeats = 2)
Grid_RF = expand.grid(.mtry = seq(5,dim(TechnicalIndex_Logical)[2]-5,1))

for(i in 5:14){
        
        TrainData = TechnicalIndex_Logical[paste0(as.character(S+years(i)),'::',as.character(S+years(i+2)-days(1)))]
        TestData = TechnicalIndex_Logical[as.character(year(S)+i+2)]

        ##RF Model
        # RFmodel = randomForest(as.factor(TWII_UpDown_Daily)~., TrainData , ntree=5000, mtry= 14)
        # RF_Predictions = predict(RFmodel,newdata= TestData[,-1])
        RFmodel = train(as.factor(TWII_UpDown_Daily)~., TrainData , method = "rf",
                           metric = "Accuracy", trControl = Ctrl,
                           tuneGrid = Grid_RF)
        RF_Predictions = predict(RFmodel,newdata= TestData[,-1])
        RF_Predictions = factor(RF_Predictions) ; names(RF_Predictions) = as.character(time(TestData))
        
        TestData[,1][RF_Predictions[RF_Predictions==TRUE] %>% names()] %>% table() %>% print()
        sum(ROC(TWII$Close)[RF_Predictions[RF_Predictions==TRUE] %>% names()]) %>% print()
        
        if(i == 5){Model1 = xts(as.logical(RF_Predictions),order.by = as.Date(names(RF_Predictions)))}else{
                Model1 = rbind.xts(Model1,xts(as.logical(RF_Predictions),order.by = as.Date(names(RF_Predictions))))
        }
        
        if(i == 5){SumRoc = sum(ROC(TWII$Close)[RF_Predictions[RF_Predictions==TRUE] %>% names()],na.rm = T)}else{
                temp = sum(ROC(TWII$Close)[RF_Predictions[RF_Predictions==TRUE] %>% names()],na.rm = T)
                SumRoc = c(SumRoc,temp)
        }
}
names(Model1) = 'Model1'
sum(SumRoc)

##Model2 Technical Index (Numeric)
Ctrl = trainControl(method = "cv",number = 5, repeats = 2)
Grid_RF = expand.grid(.mtry = seq(5,dim(TechnicalIndex_Numeric)[2]-10,1))

for(i in 5:14){
        
        TrainData = TechnicalIndex_Numeric[paste0(as.character(S+years(i)),'::',as.character(S+years(i+2)-days(1)))]
        TestData = TechnicalIndex_Numeric[as.character(year(S)+i+2)]
        
        #Remove Na Inf and standardized
        TrainData = apply(TrainData,2, function(x) ifelse(is.na(x),0,x))
        TrainData = apply(TrainData,2, function(x) ifelse(is.infinite(x),0,x))
        TestData = apply(TestData,2, function(x) ifelse(is.na(x),0,x))
        TestData = apply(TestData,2, function(x) ifelse(is.infinite(x),0,x))

        TrainData[,-1] = apply(TrainData[,-1],2, scale)
        TestData[,-1] = apply(TestData[,-1],2, scale)
        
        TrainData = xts(TrainData, order.by = as.Date(rownames(TrainData)))
        TestData = xts(TestData, order.by = as.Date(rownames(TestData)))
        
        ##RF Model
        RFmodel = train(as.factor(TWII_UpDown_Daily)~., TrainData , method = "rf",
                        metric = "Accuracy", trControl = Ctrl,
                        tuneGrid = Grid_RF)
        RF_Predictions = predict(RFmodel,newdata= TestData[,-1])
        
        temp = sub("0",FALSE,RF_Predictions)
        temp = sub("1",TRUE,temp)
        RF_Predictions = factor(temp) ; names(RF_Predictions) = as.character(time(TestData))
        
        TestData[,1][RF_Predictions[RF_Predictions==TRUE] %>% names()] %>% table() %>% print()
        sum(ROC(TWII$Close)[RF_Predictions[RF_Predictions==TRUE] %>% names()]) %>% print()
        
        if(i == 5){Model2 = xts(as.logical(RF_Predictions),order.by = as.Date(names(RF_Predictions)))}else{
                Model2 = rbind.xts(Model2,xts(as.logical(RF_Predictions),order.by = as.Date(names(RF_Predictions))))
        }
        
        if(i == 5){SumRoc = sum(ROC(TWII$Close)[RF_Predictions[RF_Predictions==TRUE] %>% names()],na.rm = T)}else{
                temp = sum(ROC(TWII$Close)[RF_Predictions[RF_Predictions==TRUE] %>% names()],na.rm = T)
                SumRoc = c(SumRoc,temp)
        }
}
names(Model2) = 'Model2'
sum(SumRoc)

##Model3 Market Data LegalPerson (Logical)
Ctrl = trainControl(method = "cv",number = 5, repeats = 2)
Grid_RF = expand.grid(.mtry = seq(5,dim(LegalPerson_Logical)[2]-5,1))

for(i in 5:14){
        
        TrainData = LegalPerson_Logical[paste0(as.character(S+years(i)),'::',as.character(S+years(i+2)-days(1)))]
        TestData = LegalPerson_Logical[as.character(year(S)+i+2)]
        
        ##RF Model
        RFmodel = train(as.factor(TWII_UpDown_Daily)~., TrainData , method = "rf",
                        metric = "Accuracy", trControl = Ctrl,
                        tuneGrid =         Grid_RF)
        RF_Predictions = predict(RFmodel,newdata= TestData[,-1])
        RF_Predictions = factor(RF_Predictions) ; names(RF_Predictions) = as.character(time(TestData))
        
        TestData[,1][RF_Predictions[RF_Predictions==TRUE] %>% names()] %>% table() %>% print()
        sum(ROC(TWII$Close)[RF_Predictions[RF_Predictions==TRUE] %>% names()]) %>% print()
        
        if(i == 5){Model3 = xts(as.logical(RF_Predictions),order.by = as.Date(names(RF_Predictions)))}else{
                Model3 = rbind.xts(Model3,xts(as.logical(RF_Predictions),order.by = as.Date(names(RF_Predictions))))
        }
        
        if(i == 5){SumRoc = sum(ROC(TWII$Close)[RF_Predictions[RF_Predictions==TRUE] %>% names()],na.rm = T)}else{
                temp = sum(ROC(TWII$Close)[RF_Predictions[RF_Predictions==TRUE] %>% names()],na.rm = T)
                SumRoc = c(SumRoc,temp)
        }
}
names(Model3) = 'Model3'
sum(SumRoc)

##Model4 Market Data LegalPerson (Numeric)
Ctrl = trainControl(method = "cv",number = 5, repeats = 2)
Grid_RF = expand.grid(.mtry = seq(5,dim(DT_LegalPerson)[2]-5,1))

for(i in 5:14){
        
        TrainData = DT_LegalPerson[paste0(as.character(S+years(i)),'::',as.character(S+years(i+2)-days(1)))]
        TestData = DT_LegalPerson[as.character(year(S)+i+2)]
        
        #Remove Na Inf and standardized
        TrainData = apply(TrainData,2, function(x) ifelse(is.na(x),0,x))
        TrainData = apply(TrainData,2, function(x) ifelse(is.infinite(x),0,x))
        TestData = apply(TestData,2, function(x) ifelse(is.na(x),0,x))
        TestData = apply(TestData,2, function(x) ifelse(is.infinite(x),0,x))
        
        TrainData[,-1] = apply(TrainData[,-1],2, scale)
        TestData[,-1] = apply(TestData[,-1],2, scale)
        
        TrainData = xts(TrainData, order.by = as.Date(rownames(TrainData)))
        TestData = xts(TestData, order.by = as.Date(rownames(TestData)))
        
        ##RF Model
        RFmodel = train(as.factor(TWII_UpDown_Daily)~., TrainData , method = "rf",
                        metric = "Accuracy", trControl = Ctrl,
                        tuneGrid = Grid_RF)
        RF_Predictions = predict(RFmodel,newdata= TestData[,-1])
        
        temp = sub("0",FALSE,RF_Predictions)
        temp = sub("1",TRUE,temp)
        RF_Predictions = factor(temp) ; names(RF_Predictions) = as.character(time(TestData))
        
        TestData[,1][RF_Predictions[RF_Predictions==TRUE] %>% names()] %>% table() %>% print()
        sum(ROC(TWII$Close)[RF_Predictions[RF_Predictions==TRUE] %>% names()]) %>% print()
        
        if(i == 5){Model4 = xts(as.logical(RF_Predictions),order.by = as.Date(names(RF_Predictions)))}else{
                Model4 = rbind.xts(Model4,xts(as.logical(RF_Predictions),order.by = as.Date(names(RF_Predictions))))
        }
        
        if(i == 5){SumRoc = sum(ROC(TWII$Close)[RF_Predictions[RF_Predictions==TRUE] %>% names()],na.rm = T)}else{
                temp = sum(ROC(TWII$Close)[RF_Predictions[RF_Predictions==TRUE] %>% names()],na.rm = T)
                SumRoc = c(SumRoc,temp)
        }
}
names(Model4) = 'Model4'
sum(SumRoc)

##Model5 Market Data Futures (Logical)
Ctrl = trainControl(method = "cv",number = 5, repeats = 2)
Grid_RF = expand.grid(.mtry = seq(3,dim(Futures_Logical)[2],1))

for(i in 5:14){
        
        TrainData = Futures_Logical[paste0(as.character(S+years(i)),'::',as.character(S+years(i+2)-days(1)))]
        TestData = Futures_Logical[as.character(year(S)+i+2)]
        
        ##RF Model
        RFmodel = train(as.factor(TWII_UpDown_Daily)~., TrainData , method = "rf",
                        metric = "Accuracy", trControl = Ctrl,
                        tuneGrid =         Grid_RF)
        RF_Predictions = predict(RFmodel,newdata= TestData[,-1])
        RF_Predictions = factor(RF_Predictions) ; names(RF_Predictions) = as.character(time(TestData))
        
        TestData[,1][RF_Predictions[RF_Predictions==TRUE] %>% names()] %>% table() %>% print()
        sum(ROC(TWII$Close)[RF_Predictions[RF_Predictions==TRUE] %>% names()]) %>% print()
        
        if(i == 5){Model5 = xts(as.logical(RF_Predictions),order.by = as.Date(names(RF_Predictions)))}else{
                Model5 = rbind.xts(Model5,xts(as.logical(RF_Predictions),order.by = as.Date(names(RF_Predictions))))
        }
        
        if(i == 5){SumRoc = sum(ROC(TWII$Close)[RF_Predictions[RF_Predictions==TRUE] %>% names()],na.rm = T)}else{
                temp = sum(ROC(TWII$Close)[RF_Predictions[RF_Predictions==TRUE] %>% names()],na.rm = T)
                SumRoc = c(SumRoc,temp)
        }
}
names(Model5) = 'Model5'
sum(SumRoc)

##Model6 Market Data Futures (Numeric)
Ctrl = trainControl(method = "cv",number = 5, repeats = 2)
Grid_RF = expand.grid(.mtry = seq(3,dim(DT_Futures)[2]-1,1))

for(i in 5:14){
        
        TrainData = DT_Futures[paste0(as.character(S+years(i)),'::',as.character(S+years(i+2)-days(1)))]
        TestData = DT_Futures[as.character(year(S)+i+2)]
        
        #Remove Na Inf and standardized
        TrainData = apply(TrainData,2, function(x) ifelse(is.na(x),0,x))
        TrainData = apply(TrainData,2, function(x) ifelse(is.infinite(x),0,x))
        TestData = apply(TestData,2, function(x) ifelse(is.na(x),0,x))
        TestData = apply(TestData,2, function(x) ifelse(is.infinite(x),0,x))
        
        TrainData[,-1] = apply(TrainData[,-1],2, scale)
        TestData[,-1] = apply(TestData[,-1],2, scale)
        
        TrainData = xts(TrainData, order.by = as.Date(rownames(TrainData)))
        TestData = xts(TestData, order.by = as.Date(rownames(TestData)))
        
        ##RF Model
        RFmodel = train(as.factor(TWII_UpDown_Daily)~., TrainData , method = "rf",
                        metric = "Accuracy", trControl = Ctrl,
                        tuneGrid = Grid_RF)
        RF_Predictions = predict(RFmodel,newdata= TestData[,-1])
        
        temp = sub("0",FALSE,RF_Predictions)
        temp = sub("1",TRUE,temp)
        RF_Predictions = factor(temp) ; names(RF_Predictions) = as.character(time(TestData))
        
        TestData[,1][RF_Predictions[RF_Predictions==TRUE] %>% names()] %>% table() %>% print()
        sum(ROC(TWII$Close)[RF_Predictions[RF_Predictions==TRUE] %>% names()]) %>% print()
        
        if(i == 5){Model6 = xts(as.logical(RF_Predictions),order.by = as.Date(names(RF_Predictions)))}else{
                Model6 = rbind.xts(Model6,xts(as.logical(RF_Predictions),order.by = as.Date(names(RF_Predictions))))
        }
        
        if(i == 5){SumRoc = sum(ROC(TWII$Close)[RF_Predictions[RF_Predictions==TRUE] %>% names()],na.rm = T)}else{
                temp = sum(ROC(TWII$Close)[RF_Predictions[RF_Predictions==TRUE] %>% names()],na.rm = T)
                SumRoc = c(SumRoc,temp)
        }
}
names(Model6) = 'Model6'
sum(SumRoc)


##Model7 Market Data TaiwanStock (Logical)
Ctrl = trainControl(method = "cv",number = 5, repeats = 2)
Grid_RF = expand.grid(.mtry = seq(5,dim(TaiwanStock_Logical)[2]-5,1))

for(i in 5:14){
        
        TrainData = TaiwanStock_Logical[paste0(as.character(S+years(i)),'::',as.character(S+years(i+2)-days(1)))]
        TestData = TaiwanStock_Logical[as.character(year(S)+i+2)]
        
        ##RF Model
        RFmodel = train(as.factor(TWII_UpDown_Daily)~., TrainData , method = "rf",
                        metric = "Accuracy", trControl = Ctrl,
                        tuneGrid =         Grid_RF)
        RF_Predictions = predict(RFmodel,newdata= TestData[,-1])
        RF_Predictions = factor(RF_Predictions) ; names(RF_Predictions) = as.character(time(TestData))
        
        TestData[,1][RF_Predictions[RF_Predictions==TRUE] %>% names()] %>% table() %>% print()
        sum(ROC(TWII$Close)[RF_Predictions[RF_Predictions==TRUE] %>% names()]) %>% print()
        
        if(i == 5){Model7 = xts(as.logical(RF_Predictions),order.by = as.Date(names(RF_Predictions)))}else{
                Model7 = rbind.xts(Model7,xts(as.logical(RF_Predictions),order.by = as.Date(names(RF_Predictions))))
        }
        
        if(i == 5){SumRoc = sum(ROC(TWII$Close)[RF_Predictions[RF_Predictions==TRUE] %>% names()],na.rm = T)}else{
                temp = sum(ROC(TWII$Close)[RF_Predictions[RF_Predictions==TRUE] %>% names()],na.rm = T)
                SumRoc = c(SumRoc,temp)
        }
}
names(Model7) = 'Model7'
sum(SumRoc)

##Model8 Market Data TaiwanStock (Numeric)
Ctrl = trainControl(method = "cv",number = 5, repeats = 2)
Grid_RF = expand.grid(.mtry = seq(5,dim(DT_TaiwanStock)[2]-5,1))

for(i in 5:14){
        
        TrainData = DT_TaiwanStock[paste0(as.character(S+years(i)),'::',as.character(S+years(i+2)-days(1)))]
        TestData = DT_TaiwanStock[as.character(year(S)+i+2)]
        
        #Remove Na Inf and standardized
        TrainData = apply(TrainData,2, function(x) ifelse(is.na(x),0,x))
        TrainData = apply(TrainData,2, function(x) ifelse(is.infinite(x),0,x))
        TestData = apply(TestData,2, function(x) ifelse(is.na(x),0,x))
        TestData = apply(TestData,2, function(x) ifelse(is.infinite(x),0,x))
        
        TrainData[,-1] = apply(TrainData[,-1],2, scale)
        TestData[,-1] = apply(TestData[,-1],2, scale)
        
        TrainData = xts(TrainData, order.by = as.Date(rownames(TrainData)))
        TestData = xts(TestData, order.by = as.Date(rownames(TestData)))
        
        ##RF Model
        RFmodel = train(as.factor(TWII_UpDown_Daily)~., TrainData , method = "rf",
                        metric = "Accuracy", trControl = Ctrl,
                        tuneGrid = Grid_RF)
        RF_Predictions = predict(RFmodel,newdata= TestData[,-1])
        
        temp = sub("0",FALSE,RF_Predictions)
        temp = sub("1",TRUE,temp)
        RF_Predictions = factor(temp) ; names(RF_Predictions) = as.character(time(TestData))
        
        TestData[,1][RF_Predictions[RF_Predictions==TRUE] %>% names()] %>% table() %>% print()
        sum(ROC(TWII$Close)[RF_Predictions[RF_Predictions==TRUE] %>% names()]) %>% print()
        
        if(i == 5){Model8 = xts(as.logical(RF_Predictions),order.by = as.Date(names(RF_Predictions)))}else{
                Model8 = rbind.xts(Model8,xts(as.logical(RF_Predictions),order.by = as.Date(names(RF_Predictions))))
        }
        
        if(i == 5){SumRoc = sum(ROC(TWII$Close)[RF_Predictions[RF_Predictions==TRUE] %>% names()],na.rm = T)}else{
                temp = sum(ROC(TWII$Close)[RF_Predictions[RF_Predictions==TRUE] %>% names()],na.rm = T)
                SumRoc = c(SumRoc,temp)
        }
}
names(Model8) = 'Model8'
sum(SumRoc)

##Model9 Market Data StockIndex (Logical)
Ctrl = trainControl(method = "cv",number = 5, repeats = 2)
Grid_RF = expand.grid(.mtry = seq(5,dim(StockIndex_Logical)[2]-5,1))

for(i in 5:14){
        
        TrainData = StockIndex_Logical[paste0(as.character(S+years(i)),'::',as.character(S+years(i+2)-days(1)))]
        TestData = StockIndex_Logical[as.character(year(S)+i+2)]
        
        ##RF Model
        RFmodel = train(as.factor(TWII_UpDown_Daily)~., TrainData , method = "rf",
                        metric = "Accuracy", trControl = Ctrl,
                        tuneGrid =         Grid_RF)
        RF_Predictions = predict(RFmodel,newdata= TestData[,-1])
        RF_Predictions = factor(RF_Predictions) ; names(RF_Predictions) = as.character(time(TestData))
        
        TestData[,1][RF_Predictions[RF_Predictions==TRUE] %>% names()] %>% table() %>% print()
        sum(ROC(TWII$Close)[RF_Predictions[RF_Predictions==TRUE] %>% names()]) %>% print()
        
        if(i == 5){Model9 = xts(as.logical(RF_Predictions),order.by = as.Date(names(RF_Predictions)))}else{
                Model9 = rbind.xts(Model9,xts(as.logical(RF_Predictions),order.by = as.Date(names(RF_Predictions))))
        }
        
        if(i == 5){SumRoc = sum(ROC(TWII$Close)[RF_Predictions[RF_Predictions==TRUE] %>% names()],na.rm = T)}else{
                temp = sum(ROC(TWII$Close)[RF_Predictions[RF_Predictions==TRUE] %>% names()],na.rm = T)
                SumRoc = c(SumRoc,temp)
        }
}
names(Model9) = 'Model9'
sum(SumRoc)


##Model10 Market Data StockIndex (Numeric)
Ctrl = trainControl(method = "cv",number = 5, repeats = 2)
Grid_RF = expand.grid(.mtry = seq(5,dim(DT_StockIndex)[2]-5,1))

for(i in 5:14){
        
        TrainData = DT_StockIndex[paste0(as.character(S+years(i)),'::',as.character(S+years(i+2)-days(1)))]
        TestData = DT_StockIndex[as.character(year(S)+i+2)]
        
        #Remove Na Inf and standardized
        TrainData = apply(TrainData,2, function(x) ifelse(is.na(x),0,x))
        TrainData = apply(TrainData,2, function(x) ifelse(is.infinite(x),0,x))
        TestData = apply(TestData,2, function(x) ifelse(is.na(x),0,x))
        TestData = apply(TestData,2, function(x) ifelse(is.infinite(x),0,x))
        
        TrainData[,-1] = apply(TrainData[,-1],2, scale)
        TestData[,-1] = apply(TestData[,-1],2, scale)
        
        TrainData = xts(TrainData, order.by = as.Date(rownames(TrainData)))
        TestData = xts(TestData, order.by = as.Date(rownames(TestData)))
        
        ##RF Model
        RFmodel = train(as.factor(TWII_UpDown_Daily)~., TrainData , method = "rf",
                        metric = "Accuracy", trControl = Ctrl,
                        tuneGrid = Grid_RF)
        RF_Predictions = predict(RFmodel,newdata= TestData[,-1])
        
        temp = sub("0",FALSE,RF_Predictions)
        temp = sub("1",TRUE,temp)
        RF_Predictions = factor(temp) ; names(RF_Predictions) = as.character(time(TestData))
        
        TestData[,1][RF_Predictions[RF_Predictions==TRUE] %>% names()] %>% table() %>% print()
        sum(ROC(TWII$Close)[RF_Predictions[RF_Predictions==TRUE] %>% names()]) %>% print()
        
        if(i == 5){Model10 = xts(as.logical(RF_Predictions),order.by = as.Date(names(RF_Predictions)))}else{
                Model10 = rbind.xts(Model10,xts(as.logical(RF_Predictions),order.by = as.Date(names(RF_Predictions))))
        }
        
        if(i == 5){SumRoc = sum(ROC(TWII$Close)[RF_Predictions[RF_Predictions==TRUE] %>% names()],na.rm = T)}else{
                temp = sum(ROC(TWII$Close)[RF_Predictions[RF_Predictions==TRUE] %>% names()],na.rm = T)
                SumRoc = c(SumRoc,temp)
        }
}
names(Model10) = 'Model10'
sum(SumRoc)

##Model11 TechnicalIndex DJI
Ctrl = trainControl(method = "cv",number = 5, repeats = 2)
Grid_RF = expand.grid(.mtry = seq(10,dim(TechnicalIndex_DJI)[2]-5,1))

for(i in 5:14){
        
        TrainData = TechnicalIndex_DJI[paste0(as.character(S+years(i)),'::',as.character(S+years(i+2)-days(1)))]
        TestData = TechnicalIndex_DJI[as.character(year(S)+i+2)]
        
        #Remove Na Inf and standardized
        TrainData = apply(TrainData,2, function(x) ifelse(is.na(x),0,x))
        TrainData = apply(TrainData,2, function(x) ifelse(is.infinite(x),0,x))
        TestData = apply(TestData,2, function(x) ifelse(is.na(x),0,x))
        TestData = apply(TestData,2, function(x) ifelse(is.infinite(x),0,x))
        
        TrainData[,-1] = apply(TrainData[,-1],2, scale)
        TestData[,-1] = apply(TestData[,-1],2, scale)
        TrainData = xts(TrainData,order.by = rownames(TrainData) %>% as.Date())
        TestData = xts(TestData,order.by = rownames(TestData) %>% as.Date())
        
        ##RF Model
        RFmodel = train(as.factor(TWII_UpDown_Daily)~., TrainData , method = "rf",
                        metric = "Accuracy", trControl = Ctrl,
                        tuneGrid = Grid_RF)
        RF_Predictions = predict(RFmodel,newdata= TestData[,-1])
        
        temp = sub("0",FALSE,RF_Predictions)
        temp = sub("1",TRUE,temp)
        RF_Predictions = xts(as.logical(temp),order.by = time(TestData))
        
        (TestData[,1])[time(RF_Predictions[RF_Predictions])] %>% table() %>% print()
        sum(ROC(TWII$Close)[time(RF_Predictions[RF_Predictions])] ) %>% print()
        
        if(i == 5){Model11 = xts(as.logical(RF_Predictions),order.by = time(RF_Predictions))}else{
                Model11 = rbind.xts(Model11,xts(as.logical(RF_Predictions),order.by = time(RF_Predictions)))
        }
        
        if(i == 5){SumRoc = sum(ROC(TWII$Close)[time(RF_Predictions[RF_Predictions])], na.rm=T)}else{
                temp = sum(ROC(TWII$Close)[time(RF_Predictions[RF_Predictions])], na.rm=T)
                SumRoc = c(SumRoc,temp)
        }
}
names(Model11) = 'Model11'
sum(SumRoc)

##Model12 TechnicalIndex SP
Ctrl = trainControl(method = "cv",number = 5, repeats = 2)
Grid_RF = expand.grid(.mtry = seq(10,dim(TechnicalIndex_SP)[2]-5,1))

for(i in 5:14){
        
        TrainData = TechnicalIndex_SP[paste0(as.character(S+years(i)),'::',as.character(S+years(i+2)-days(1)))]
        TestData = TechnicalIndex_SP[as.character(year(S)+i+2)]
        
        #Remove Na Inf and standardized
        TrainData = apply(TrainData,2, function(x) ifelse(is.na(x),0,x))
        TrainData = apply(TrainData,2, function(x) ifelse(is.infinite(x),0,x))
        TestData = apply(TestData,2, function(x) ifelse(is.na(x),0,x))
        TestData = apply(TestData,2, function(x) ifelse(is.infinite(x),0,x))
        
        TrainData[,-1] = apply(TrainData[,-1],2, scale)
        TestData[,-1] = apply(TestData[,-1],2, scale)
        TrainData = xts(TrainData,order.by = rownames(TrainData) %>% as.Date())
        TestData = xts(TestData,order.by = rownames(TestData) %>% as.Date())
        
        ##RF Model
        RFmodel = train(as.factor(TWII_UpDown_Daily)~., TrainData , method = "rf",
                        metric = "Accuracy", trControl = Ctrl,
                        tuneGrid = Grid_RF)
        RF_Predictions = predict(RFmodel,newdata= TestData[,-1])
        
        temp = sub("0",FALSE,RF_Predictions)
        temp = sub("1",TRUE,temp)
        RF_Predictions = xts(as.logical(temp),order.by = time(TestData))
        
        (TestData[,1])[time(RF_Predictions[RF_Predictions])] %>% table() %>% print()
        sum(ROC(TWII$Close)[time(RF_Predictions[RF_Predictions])] ) %>% print()
        
        if(i == 5){Model12 = xts(as.logical(RF_Predictions),order.by = time(RF_Predictions))}else{
                Model12 = rbind.xts(Model12,xts(as.logical(RF_Predictions),order.by = time(RF_Predictions)))
        }
        
        if(i == 5){SumRoc = sum(ROC(TWII$Close)[time(RF_Predictions[RF_Predictions])], na.rm=T)}else{
                temp = sum(ROC(TWII$Close)[time(RF_Predictions[RF_Predictions])], na.rm=T)
                SumRoc = c(SumRoc,temp)
        }
}
names(Model12) = 'Model12'
sum(SumRoc)

##Model13 TechnicalIndex NASDAQ
Ctrl = trainControl(method = "cv",number = 5, repeats = 2)
Grid_RF = expand.grid(.mtry = seq(10,dim(TechnicalIndex_NASDAQ)[2]-5,1))

for(i in 5:14){
        
        TrainData = TechnicalIndex_NASDAQ[paste0(as.character(S+years(i)),'::',as.character(S+years(i+2)-days(1)))]
        TestData = TechnicalIndex_NASDAQ[as.character(year(S)+i+2)]
        
        #Remove Na Inf and standardized
        TrainData = apply(TrainData,2, function(x) ifelse(is.na(x),0,x))
        TrainData = apply(TrainData,2, function(x) ifelse(is.infinite(x),0,x))
        TestData = apply(TestData,2, function(x) ifelse(is.na(x),0,x))
        TestData = apply(TestData,2, function(x) ifelse(is.infinite(x),0,x))
        
        TrainData[,-1] = apply(TrainData[,-1],2, scale)
        TestData[,-1] = apply(TestData[,-1],2, scale)
        
        TrainData = xts(TrainData,order.by = rownames(TrainData) %>% as.Date())
        TestData = xts(TestData,order.by = rownames(TestData) %>% as.Date())
        
        ##RF Model
        RFmodel = train(as.factor(TWII_UpDown_Daily)~., TrainData , method = "rf",
                        metric = "Accuracy", trControl = Ctrl,
                        tuneGrid = Grid_RF)
        RF_Predictions = predict(RFmodel,newdata= TestData[,-1])
        
        temp = sub("0",FALSE,RF_Predictions)
        temp = sub("1",TRUE,temp)
        RF_Predictions = xts(as.logical(temp),order.by = time(TestData))
        
        (TestData[,1])[time(RF_Predictions[RF_Predictions])] %>% table() %>% print()
        sum(ROC(TWII$Close)[time(RF_Predictions[RF_Predictions])] ) %>% print()
        
        if(i == 5){Model13 = xts(as.logical(RF_Predictions),order.by = time(RF_Predictions))}else{
                Model13 = rbind.xts(Model13,xts(as.logical(RF_Predictions),order.by = time(RF_Predictions)))
        }
        
        if(i == 5){SumRoc = sum(ROC(TWII$Close)[time(RF_Predictions[RF_Predictions])], na.rm=T)}else{
                temp = sum(ROC(TWII$Close)[time(RF_Predictions[RF_Predictions])], na.rm=T)
                SumRoc = c(SumRoc,temp)
        }
}
names(Model13) = 'Model13'
sum(SumRoc)

Model_Daily = merge.xts(Model1,Model2,Model3,Model4,Model5,Model6,Model7,Model8,Model9,Model10,Model11,Model12,Model13)

####Monthly Base####
TWII_ROC_Monthly =  apply.monthly(na.trim(TWII$Close),function(x) (last(x)-first(x))/first(x))

##Model1 MacroTaiwan (Logical)
Ctrl = trainControl(method = "cv",number = 5, repeats = 2)
Grid_RF = expand.grid(.mtry = seq(5,dim(MacroTaiwan_Logical)[2]-5,1))

for(i in 5:16){
        
        TrainData = MacroTaiwan_Logical[paste0(as.character(S+years(i-5)),'::',as.character(S+years(i)-days(1)))]
        TestData = MacroTaiwan_Logical[as.character(year(S)+i)]
        
        ##RF Model
        RFmodel = train(as.factor(TWII_UpDown_Monthly)~., TrainData , method = "rf",
                        metric = "Accuracy", trControl = Ctrl,
                        tuneGrid =         Grid_RF)
        RF_Predictions = predict(RFmodel,newdata= TestData[,-1])
        RF_Predictions = factor(RF_Predictions) ; names(RF_Predictions) = as.character(time(TestData))
        
        TestData[,1][RF_Predictions[RF_Predictions==TRUE] %>% names()] %>% table() %>% print()
        sum(TWII_ROC_Monthly[RF_Predictions[RF_Predictions==TRUE] %>% names()]) %>% print()

        if(i == 5){Model1_M = xts(as.logical(RF_Predictions),order.by = as.Date(names(RF_Predictions)))}else{
                Model1_M = rbind.xts(Model1_M,xts(as.logical(RF_Predictions),order.by = as.Date(names(RF_Predictions))))
        }
        
        if(i == 5){SumRoc = sum(TWII_ROC_Monthly[RF_Predictions[RF_Predictions==TRUE] %>% names()],na.rm = T)}else{
                temp = sum(TWII_ROC_Monthly[RF_Predictions[RF_Predictions==TRUE] %>% names()],na.rm = T)
                SumRoc = c(SumRoc,temp)
        }
}
names(Model1_M) = 'Model1_M'
sum(SumRoc)


##Model2 MacroTaiwan (Numeric)
Ctrl = trainControl(method = "cv",number = 5, repeats = 2)
Grid_RF = expand.grid(.mtry = seq(20,dim(DT_MacroTaiwan)[2]-15,1))

for(i in 5:16){
        
        TrainData = DT_MacroTaiwan[paste0(as.character(S+years(i-5)),'::',as.character(S+years(i)-days(1)))]
        TestData = DT_MacroTaiwan[as.character(year(S)+i)]
        
        #Remove Na Inf and standardized
        TrainData = apply(TrainData,2, function(x) ifelse(is.na(x),0,x))
        TrainData = apply(TrainData,2, function(x) ifelse(is.infinite(x),0,x))
        TrainData = apply(TrainData,2, function(x) ifelse(is.nan(x),0,x))
        TestData = apply(TestData,2, function(x) ifelse(is.na(x),0,x))
        TestData = apply(TestData,2, function(x) ifelse(is.infinite(x),0,x))
        TestData = apply(TestData,2, function(x) ifelse(is.nan(x),0,x))
        
        TrainData[,-1] = apply(TrainData[,-1],2, scale)
        TestData[,-1] = apply(TestData[,-1],2, scale)
        TestData[,-1] = apply(TestData[,-1],2, function(x) ifelse(is.nan(x),0,x))
        
        TrainData = xts(TrainData,order.by = rownames(TrainData) %>% as.Date())
        TestData = xts(TestData,order.by = rownames(TestData) %>% as.Date())

        ##RF Model
        RFmodel = train(as.factor(TWII_UpDown_Monthly)~., TrainData , method = "rf",
                        metric = "Accuracy", trControl = Ctrl,
                        tuneGrid = Grid_RF)
        RF_Predictions = predict(RFmodel,newdata= TestData[,-1])
        
        temp = sub("0",FALSE,RF_Predictions)
        temp = sub("1",TRUE,temp)
        RF_Predictions = xts(as.logical(temp),order.by = time(TestData))
        
        (TestData[,1])[time(RF_Predictions[RF_Predictions])] %>% table() %>% print()
        sum(TWII_ROC_Monthly[time(RF_Predictions[RF_Predictions])] ) %>% print()
        
        if(i == 5){Model2_M = xts(as.logical(RF_Predictions),order.by = time(RF_Predictions))}else{
                Model2_M = rbind.xts(Model2_M,xts(as.logical(RF_Predictions),order.by = time(RF_Predictions)))
        }
        
        if(i == 5){SumRoc = sum(TWII_ROC_Monthly[time(RF_Predictions[RF_Predictions])], na.rm=T)}else{
                temp = sum(TWII_ROC_Monthly[time(RF_Predictions[RF_Predictions])], na.rm=T)
                SumRoc = c(SumRoc,temp)
        }
}
names(Model2_M) = 'Model2_M'
sum(SumRoc)

##Model3 MacroAmerica (Logical)
Ctrl = trainControl(method = "cv",number = 5, repeats = 2)
Grid_RF = expand.grid(.mtry = seq(5,dim(MacroAmerica_Logical)[2]-5,1))

for(i in 5:16){
        
        TrainData = MacroAmerica_Logical[paste0(as.character(S+years(i-5)),'::',as.character(S+years(i)-days(1)))]
        TestData = MacroAmerica_Logical[as.character(year(S)+i)]
        
        ##RF Model
        RFmodel = train(as.factor(TWII_UpDown_Monthly)~., TrainData , method = "rf",
                        metric = "Accuracy", trControl = Ctrl,
                        tuneGrid =         Grid_RF)
        RF_Predictions = predict(RFmodel,newdata= TestData[,-1])
        RF_Predictions = factor(RF_Predictions) ; names(RF_Predictions) = as.character(time(TestData))
        
        TestData[,1][RF_Predictions[RF_Predictions==TRUE] %>% names()] %>% table() %>% print()
        sum(TWII_ROC_Monthly[RF_Predictions[RF_Predictions==TRUE] %>% names()]) %>% print()
        
        if(i == 5){Model3_M = xts(as.logical(RF_Predictions),order.by = as.Date(names(RF_Predictions)))}else{
                Model3_M = rbind.xts(Model3_M,xts(as.logical(RF_Predictions),order.by = as.Date(names(RF_Predictions))))
        }
        
        if(i == 5){SumRoc = sum(TWII_ROC_Monthly[RF_Predictions[RF_Predictions==TRUE] %>% names()],na.rm = T)}else{
                temp = sum(TWII_ROC_Monthly[RF_Predictions[RF_Predictions==TRUE] %>% names()],na.rm = T)
                SumRoc = c(SumRoc,temp)
        }
}
names(Model3_M) = 'Model3_M'
sum(SumRoc)

##Model4 MacroAmerica (Numeric)
Ctrl = trainControl(method = "cv",number = 5, repeats = 2)
Grid_RF = expand.grid(.mtry = seq(40,dim(DT_MacroAmerica)[2]-30,1))

for(i in 5:16){
        
        TrainData = c[paste0(as.character(S+years(i-5)),'::',as.character(S+years(i)-days(1)))]
        TestData = DT_MacroAmerica[as.character(year(S)+i)]
        
        #Remove Na Inf and standardized
        TrainData = apply(TrainData,2, function(x) ifelse(is.na(x),0,x))
        TrainData = apply(TrainData,2, function(x) ifelse(is.infinite(x),0,x))
        TrainData = apply(TrainData,2, function(x) ifelse(is.nan(x),0,x))
        TestData = apply(TestData,2, function(x) ifelse(is.na(x),0,x))
        TestData = apply(TestData,2, function(x) ifelse(is.infinite(x),0,x))
        TestData = apply(TestData,2, function(x) ifelse(is.nan(x),0,x))
        
        TrainData[,-1] = apply(TrainData[,-1],2, scale)
        TestData[,-1] = apply(TestData[,-1],2, scale)
        TestData[,-1] = apply(TestData[,-1],2, function(x) ifelse(is.nan(x),0,x))
        
        TrainData = xts(TrainData,order.by = rownames(TrainData) %>% as.Date())
        TestData = xts(TestData,order.by = rownames(TestData) %>% as.Date())
        
        ##RF Model
        RFmodel = train(as.factor(TWII_UpDown_Monthly)~., TrainData , method = "rf",
                        metric = "Accuracy", trControl = Ctrl,
                        tuneGrid = Grid_RF)
        RF_Predictions = predict(RFmodel,newdata= TestData[,-1])
        
        temp = sub("0",FALSE,RF_Predictions)
        temp = sub("1",TRUE,temp)
        RF_Predictions = xts(as.logical(temp),order.by = time(TestData))
        
        (TestData[,1])[time(RF_Predictions[RF_Predictions])] %>% table() %>% print()
        sum(TWII_ROC_Monthly[time(RF_Predictions[RF_Predictions])] ) %>% print()
        
        if(i == 5){Model4_M = xts(as.logical(RF_Predictions),order.by = time(RF_Predictions))}else{
                Model4_M = rbind.xts(Model4_M,xts(as.logical(RF_Predictions),order.by = time(RF_Predictions)))
        }
        
        if(i == 5){SumRoc = sum(TWII_ROC_Monthly[time(RF_Predictions[RF_Predictions])], na.rm=T)}else{
                temp = sum(TWII_ROC_Monthly[time(RF_Predictions[RF_Predictions])], na.rm=T)
                SumRoc = c(SumRoc,temp)
        }
}
names(Model4_M) = 'Model4_M'
sum(SumRoc)

##Model5 Commodity (Logical)
Ctrl = trainControl(method = "cv",number = 5, repeats = 2)
Grid_RF = expand.grid(.mtry = seq(3,dim(Commodity_Logical)[2]-1,1))

for(i in 5:16){
        
        TrainData = Commodity_Logical[paste0(as.character(S+years(i-5)),'::',as.character(S+years(i)-days(1)))]
        TestData = Commodity_Logical[as.character(year(S)+i)]
        
        ##RF Model
        RFmodel = train(as.factor(TWII_UpDown_Monthly)~., TrainData , method = "rf",
                        metric = "Accuracy", trControl = Ctrl,
                        tuneGrid =         Grid_RF)
        RF_Predictions = predict(RFmodel,newdata= TestData[,-1])
        RF_Predictions = factor(RF_Predictions) ; names(RF_Predictions) = as.character(time(TestData))
        
        TestData[,1][RF_Predictions[RF_Predictions==TRUE] %>% names()] %>% table() %>% print()
        sum(TWII_ROC_Monthly[RF_Predictions[RF_Predictions==TRUE] %>% names()]) %>% print()
        
        if(i == 5){Model5_M = xts(as.logical(RF_Predictions),order.by = as.Date(names(RF_Predictions)))}else{
                Model5_M = rbind.xts(Model5_M,xts(as.logical(RF_Predictions),order.by = as.Date(names(RF_Predictions))))
        }
        
        if(i == 5){SumRoc = sum(TWII_ROC_Monthly[RF_Predictions[RF_Predictions==TRUE] %>% names()],na.rm = T)}else{
                temp = sum(TWII_ROC_Monthly[RF_Predictions[RF_Predictions==TRUE] %>% names()],na.rm = T)
                SumRoc = c(SumRoc,temp)
        }
}
names(Model5_M) = 'Model5_M'
sum(SumRoc)

##Model6 Commodity (Numeric)
Ctrl = trainControl(method = "cv",number = 5, repeats = 2)
Grid_RF = expand.grid(.mtry = seq(40,dim(DT_Commodity)[2]-30,1))

for(i in 5:16){
        
        TrainData = DT_Commodity[paste0(as.character(S+years(i-5)),'::',as.character(S+years(i)-days(1)))]
        TestData = DT_Commodity[as.character(year(S)+i)]
        
        #Remove Na Inf and standardized
        TrainData = apply(TrainData,2, function(x) ifelse(is.na(x),0,x))
        TrainData = apply(TrainData,2, function(x) ifelse(is.infinite(x),0,x))
        TrainData = apply(TrainData,2, function(x) ifelse(is.nan(x),0,x))
        TestData = apply(TestData,2, function(x) ifelse(is.na(x),0,x))
        TestData = apply(TestData,2, function(x) ifelse(is.infinite(x),0,x))
        TestData = apply(TestData,2, function(x) ifelse(is.nan(x),0,x))
        
        TrainData[,-1] = apply(TrainData[,-1],2, scale)
        TestData[,-1] = apply(TestData[,-1],2, scale)
        TestData[,-1] = apply(TestData[,-1],2, function(x) ifelse(is.nan(x),0,x))
        
        TrainData = xts(TrainData,order.by = rownames(TrainData) %>% as.Date())
        TestData = xts(TestData,order.by = rownames(TestData) %>% as.Date())
        
        ##RF Model
        RFmodel = train(as.factor(TWII_UpDown_Monthly)~., TrainData , method = "rf",
                        metric = "Accuracy", trControl = Ctrl,
                        tuneGrid = Grid_RF)
        RF_Predictions = predict(RFmodel,newdata= TestData[,-1])
        
        temp = sub("0",FALSE,RF_Predictions)
        temp = sub("1",TRUE,temp)
        RF_Predictions = xts(as.logical(temp),order.by = time(TestData))
        
        (TestData[,1])[time(RF_Predictions[RF_Predictions])] %>% table() %>% print()
        sum(TWII_ROC_Monthly[time(RF_Predictions[RF_Predictions])] ) %>% print()
        
        if(i == 5){Model6_M = xts(as.logical(RF_Predictions),order.by = time(RF_Predictions))}else{
                Model6_M = rbind.xts(Model6_M,xts(as.logical(RF_Predictions),order.by = time(RF_Predictions)))
        }
        
        if(i == 5){SumRoc = sum(TWII_ROC_Monthly[time(RF_Predictions[RF_Predictions])], na.rm=T)}else{
                temp = sum(TWII_ROC_Monthly[time(RF_Predictions[RF_Predictions])], na.rm=T)
                SumRoc = c(SumRoc,temp)
        }
}
names(Model6_M) = 'Model6_M'
sum(SumRoc)

#save.image(file = 'FinalModel.RData')

###Combined Model
Model_Monthly = merge.xts(Model1_M,Model2_M,Model3_M,Model4_M,Model5_M,Model6_M)
Model_Monthly = Model_Monthly["2007::2016"]
DaysofMonth = (Model_Daily %>% apply.monthly(nrow) %>% coredata())
Model = merge.xts(TWII_UpDown_Daily["2007::2016"],Model_Daily,MonthtoDaily(Model_Monthly,DaysofMonth))
names(Model)[1] = 'TWII_UpDown_Daily'
Model = Model[!is.na(Model$TWII_UpDown_Daily),]


##Model
Ctrl = trainControl(method = "cv",number = 10, repeats = 4)
Grid_RF = expand.grid(.mtry = seq(3,dim(Model)[2]-1,1))

ReturnDataRF = xts()
Mtry = c(9,9,6,4,3,9,5,3)
for(i in 1:8){
        
        TrainData = Model[paste0(as.character(S+years(i+6)),'::',as.character(S+years(i+8)-days(1)))]
        TestData = Model[as.character(year(S)+i+8)]
        
        ##RF Model
        RFmodel = randomForest(as.factor(TWII_UpDown_Daily)~., TrainData ,Mtry=Mtry[i], ntree=5000,importance=T)
        
        if(i == 1){
                Imp_M = cbind(data.frame(Name = importance(RFmodel)%>%rownames()),data.frame(importance(RFmodel))[,c(3,4)])
        }else{
                Imp_M = merge(Imp_M,cbind(data.frame(Name = importance(RFmodel)%>%rownames()),data.frame(importance(RFmodel))[,c(3,4)]),by=c("Name"))
        }       
        
        plot(RFmodel,main= paste0(as.character(S+years(i+6)),'~',as.character(S+years(i+8)-days(1))),mfrow=c(4,2))
        randomForest::varImpPlot(RFmodel,main = paste0(as.character(S+years(i+6)),'~',as.character(S+years(i+8)-days(1))),mfrow=c(4,2))

        RF_Predictions = predict(RFmodel,newdata= TestData[,-1])
        RF_Predictions = factor(RF_Predictions) ; names(RF_Predictions) = as.character(time(TestData))
        
        if( i == 1){
                Model_Outcome = xts(as.logical(RF_Predictions),order.by = time(TestData))
        }else{
                Model_Outcome = rbind.xts(Model_Outcome,xts(as.logical(RF_Predictions),order.by = time(TestData)))
        }
        
        TestData[,1][RF_Predictions[RF_Predictions==TRUE] %>% names()] %>% table() %>% print()
        sum(ROC(TWII$Close)[RF_Predictions[RF_Predictions==TRUE] %>% names()]) %>% print()
        
        if(i == 1){
                ReturnDataRF = ROC(Futures$收盤價)[RF_Predictions[RF_Predictions==T] %>% names()]
        }else{
                ReturnDataRF = rbind.xts(ReturnDataRF, ROC(Futures$收盤價)[RF_Predictions[RF_Predictions==T] %>% names()])
        }
}

#write.csv(x = ReturnDataRF,file = 'tempRF.csv')
#write.csv(x = temp , file = 'Mtry.csv')
write.csv(x = Imp_M , file = 'Imp_M.csv')
names(ReturnDataRF) = 'Profit'
Model_Outcome = merge.xts(as.logical(Model_Outcome),ROC(Futures$收盤價,type = 'discrete')['2009::2016'])
names(Model_Outcome) = c('M_Predict','Rate of Trading Return')
#write.csv(Model_Outcome,'Outcome_M.csv')
png(filename = "PerformanceSummary.png",
    width = 150, height = 200, units = "mm",res =500)
library(PerformanceAnalytics)
myReturn = Model_Outcome[,1]*Model_Outcome[,2]
charts.PerformanceSummary(cbind(Model_Outcome[,2],ReturnDataRF))
dev.off()
#yearly outcome
for(i in 1:8){
        
        temp = myReturn[as.character(year(S)+i+8)]
        temp = Performance(temp)
        if(i == 1){
                Model_Yearly_Outcome = temp
        }else{
                Model_Yearly_Outcome = rbind(Model_Yearly_Outcome,temp)
        }
}
write.csv(Model_Yearly_Outcome,'Model_Yearly_Outcome.csv')
#all outcome        
names(myReturn) = 'Profit'
myReturn
Performance(myReturn)






#setwd('D:\\中山大學財管所\\畢業論文\\R Code') 
#load('FinalFinal.RData')
#save.image(file = 'FinalFinal.RData')


