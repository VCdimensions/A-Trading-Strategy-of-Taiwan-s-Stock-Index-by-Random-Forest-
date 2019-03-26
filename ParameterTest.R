###Custom Function
##每五年調整一次參數
AdjPar <- function(x){
        for(i in 2:5){
                x[[i]] = x[[1]]
        }
        for(i in 7:10){
                x[[i]] = x[[6]]
        }
        for(i in 12:13){
                x[[i]] = x[[11]]
        }
        return(x)
}

###用Rolling Window的方式，每兩年找出一個"報酬"最大的最佳化參數
library(lubridate)
TimeGap = 2
AllYear = year(end(TWII)) - year(start(TWII))

S = as.Date('2000-01-01')

setwd('D:\\中山大學財管所\\畢業論文\\R Code') 
##OBV:OBV連續Cont天變動率大於零
Cont = seq(0,10,1)
Parameter_OBV_t1 = list()
Parameter_OBV_t2 = list()
for(i in 1:(AllYear - 3)){
        t1_max = double(1)
        t2_max = double(1)
        for(j in Cont){
                TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)),'::',as.character(S+years(TimeGap+i-1+1+2)-days(1)))]
                OBV <- TTR::OBV(price = TWII_Period$Close, volume = TWII_Period$Volume)
                ROC_OBV <- AdjROC(OBV)
                ROC_OBV_Signal <-  ROC_OBV > 0
                ROC_OBV_Signal[is.na(ROC_OBV_Signal)] <-  F
                ROC_OBV_Signal_Cont <- ContinuingPeriod(ROC_OBV_Signal, j)
                #漲跌次數最多當最佳參數
                if(all(ROC_OBV_Signal_Cont$Signal == FALSE) ){
                        temp = temp
                }else{
                        temp = table(ROC(TWII_Period$Close)[ROC_OBV_Signal_Cont$Signal] > 0)        
                }
                t1 = temp[2]/(temp[1]+temp[2]) #漲跌預測準確率
                
                if(is.na(t1)){
                        t1_max = t1_max
                }else if(t1 > t1_max){
                        t1_max = t1
                        Parameter_OBV_t1[i] = j
                }
                
                #累積報酬最多當作最佳參數

                t2 = sum(ROC(TWII_Period$Close)[ROC_OBV_Signal_Cont$Signal])
                if(t2 > t2_max){
                        t2_max = t2
                        Parameter_OBV_t2[i] = j
                }
                print(c(i,j,t1,t2))
                
        }
}

print('OBV DONE!!!!')
save.image(file = 'OptPar.RData')

##MACD:DIF與MACD的差的變動率連續四天大於Cont
Fast = seq(1,8,1)
Slow = seq(10,30,5)
Cont = 0
Sig = seq(2,5,1)

#MACD:DIF與MACD的差的變動率連續四天大於Cont在隔天買進
Parameter_MACD_t1 = list()
Parameter_MACD_t2 = list()

for(i in 1:(AllYear - 3)){
        t1_max = double(1)
        t2_max = double(1)
        for(j in Fast){
                for(k in Slow){
                        for(l in Sig){
                                TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)),'::',as.character(S+years(TimeGap+i-1+1+2)-days(1)))]
                                        
                                MACD <- TTR::MACD(TWII_Period, nFast = j, nSlow = k, nSig = l)
                                ROC_MACD <- AdjROC(MACD$macd-MACD$signal)
                                Cond1_MACD <- (MACD$macd > 0) & (MACD$signal > 0)
                                ROC_MACD_Signal <- (ROC_MACD > 0) & (Cond1_MACD == T)
                                ROC_MACD_Signal <- ROC_MACD > 0
                                ROC_MACD_Signal[is.na(ROC_MACD_Signal)] <- F
                                ROC_MACD_Signal_Cont <- ContinuingPeriod(ROC_MACD_Signal,Cont)
                                
                                temp = table(ROC(TWII_Period$Close)[ROC_MACD_Signal_Cont$Signal] > 0)
                                t1 = temp[2]/(temp[1]+temp[2])
                                
                                if(t1 > t1_max){
                                        t1_max = t1
                                        temp = list(c(j,k,l))
                                        Parameter_MACD_t1[i] = temp
                                }
                                
                                t2 = sum(ROC(TWII_Period$Close)[ROC_MACD_Signal_Cont$Signal])
                                
                                if(t2 > t2_max){
                                        t2_max = t2
                                        temp = list(c(j,k,l))
                                        Parameter_MACD_t2[i] = temp
                                }
                                print(c(i,j,k,l,t2))
                                
                        }

                }
        }
}

print('MACD DONE!!!!')
save.image(file = 'OptPar.RData')

##SMA快線向上穿越持續Cont天
Fast = seq(1,8,1)
Slow = seq(10,40,5)
Cont = seq(0,5,1)

Parameter_SMA_t1 = list()
Parameter_SMA_t2 = list()

for(i in 1:(AllYear - 3)){
        t1_max = double(1)
        t2_max = double(1)
        for(j in Fast){
                for(k in Slow){
                        for(l in Cont){
                                
                                TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)),'::',as.character(S+years(TimeGap+i-1+1+2)-days(1)))]
                                
                                SMA_Fast <- TTR::SMA(TWII_Period$Close, j)
                                SMA_Slow <- TTR::SMA(TWII_Period$Close, k)
                                #快線向上穿越持續Cont天，則在隔天買進
                                SMA_UPThru <- SMA_Fast - SMA_Slow
                                SMA_UPThru[SMA_UPThru <= 0] <- 0
                                ROC_SMA_UPThru <- ROC(SMA_UPThru,type = 'discrete')
                                ROC_SMA_UPThru_Signal <- ROC_SMA_UPThru > 0
                                ROC_SMA_UPThru_Signal[is.na(ROC_SMA_UPThru_Signal)] <-  F
                                ROC_SMA_UPThru_Signal_Cont <- ContinuingPeriod(ROC_SMA_UPThru_Signal, l)
                                
                                if(all(ROC_SMA_UPThru_Signal_Cont$Signal == FALSE) ){
                                        temp = temp
                                }else{
                                        temp = table(ROC(TWII_Period$Close)[ROC_SMA_UPThru_Signal_Cont$Signal] > 0 )
                                        t1 = temp[2]/(temp[1]+temp[2])
                                }
                                
                                if(is.na(t1)){
                                        t1_max = t1_max
                                }else if(t1 > t1_max){
                                        t1_max = t1 
                                        temp = list(c(j,k,l))
                                        Parameter_SMA_t1[i] = temp
                                }
                                
                                t2 = sum(ROC(TWII_Period$Close)[ROC_SMA_UPThru_Signal_Cont$Signal])
                                
                                if(t2 > t2_max){
                                        t2_max = t2 
                                        temp = list(c(j,k,l))
                                        Parameter_SMA_t2[i] = temp
                                }
                                
                                print(c(i,j,k,l,t2))                                
                        }
                }
        }
}

print('SMA DONE!!!!')
save.image(file = 'OptPar.RData')

##VWMA:(上漲通常量大所以會變動較快，下跌通常量小所以變動較慢)
Fast = seq(1,8,1)
Slow = seq(10,40,5)
Cont = seq(0,5,1)

Parameter_VWMA_t1 = list()
Parameter_VWMA_t2 = list()
for(i in 1:(AllYear - 3)){
        t1_max = double(1)
        t2_max = double(1)
        for(j in Fast){
                for(k in Slow){
                        for(l in Cont){
                                
                                TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)),'::',as.character(S+years(TimeGap+i-1+1+2)-days(1)))]
                                
                                VWMA_Fast <- TTR::VWMA(TWII_Period$Close, TWII_Period$Volume, j)
                                VWMA_Slow <- TTR::VWMA(TWII_Period$Close, TWII_Period$Volume, k)
                                #快線向上穿越持續Cont天，則在隔天買進
                                VWMA_UPThru <- VWMA_Fast - VWMA_Slow
                                VWMA_UPThru[VWMA_UPThru <= 0] <- 0
                                ROC_VWMA_UPThru <- ROC(VWMA_UPThru,type = 'discrete')
                                ROC_VWMA_UPThru_Signal <- ROC_VWMA_UPThru > 0
                                ROC_VWMA_UPThru_Signal[is.na(ROC_VWMA_UPThru_Signal)] <-  F
                                ROC_VWMA_UPThru_Signal_Cont <- ContinuingPeriod(ROC_VWMA_UPThru_Signal, l)
                                
                                if(all(ROC_VWMA_UPThru_Signal_Cont$Signal == FALSE)){
                                        next
                                }                                
                                temp = table(ROC(TWII_Period$Close)[ROC_VWMA_UPThru_Signal_Cont$Signal] > 0 )
                                
                                t1 = temp[2]/(temp[1]+temp[2])
                                if(is.na(t1)){
                                        next
                                }
                                if(t1 > t1_max){
                                        t1_max = t1 
                                        temp = list(c(j,k,l))
                                        Parameter_VWMA_t1[i] = temp
                                }
                                
                                t2 = sum(ROC(TWII_Period$Close)[ROC_VWMA_UPThru_Signal_Cont$Signal])
                                if(t2 > t2_max){
                                        t2_max = t2
                                        temp = list(c(j,k,l))
                                        Parameter_VWMA_t2[i] = temp
                                }
                                
                                print(c(i,j,k,l,t2))
                        }
                }
        }
}
 
print('VWMA DONE!!!!')
save.image(file = 'OptPar.RData')

##EMA
Fast = seq(1,8,1)
Slow = seq(10,40,5)
Cont = seq(0,4,1)

Parameter_EMA_t1 = list()
Parameter_EMA_t2 = list()
for(i in 1:(AllYear - 3)){
        t1_max = double(1)
        t2_max = double(1)
        for(j in Fast){
                for(k in Slow){
                        for(l in Cont){
                                
                                TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)),'::',as.character(S+years(TimeGap+i-1+1+2)-days(1)))]
                                
                                EMA_Fast <- TTR::EMA(TWII_Period$Close, j)
                                EMA_Slow <- TTR::EMA(TWII_Period$Close, k)
                                #快線向上穿越持續Cont天，則在隔天買進
                                EMA_UPThru <- EMA_Fast - EMA_Slow
                                EMA_UPThru[EMA_UPThru <= 0] <- 0
                                ROC_EMA_UPThru <- ROC(EMA_UPThru,type = 'discrete')
                                ROC_EMA_UPThru_Signal <- ROC_EMA_UPThru > 0
                                ROC_EMA_UPThru_Signal[is.na(ROC_EMA_UPThru_Signal)] <-  F
                                ROC_EMA_UPThru_Signal_Cont <- ContinuingPeriod(ROC_EMA_UPThru_Signal, l)
                                
                                if(all(ROC_EMA_UPThru_Signal_Cont$Signal == FALSE)){
                                        next
                                }                                
                                
                                temp = table(ROC(TWII_Period$Close)[ROC_EMA_UPThru_Signal_Cont$Signal] > 0 )
                                
                                t1 = temp[2]/(temp[1]+temp[2])
                                if(is.na(t1)){
                                        next
                                }
                                if(t1 > t1_max){
                                        t1_max = t1 
                                        temp = list(c(j,k,l))
                                        Parameter_EMA_t1[i] = temp
                                }
                                
                                t2 = sum(ROC(TWII_Period$Close)[ROC_EMA_UPThru_Signal_Cont$Signal])
                                if(t2 > t2_max){
                                        t2_max = t2
                                        temp = list(c(j,k,l))
                                        Parameter_EMA_t2[i] = temp
                                }
                                
                                print(c(i,j,k,l,t2))
                        }
                }
        }
}

print('EMA DONE!!!!')
save.image(file = 'OptPar.RData')

##BBand
N = seq(1,25,3)
DownB = seq(0.01,0.29,0.02)
Lag = seq(1,10,1)

Parameter_BBand_t1 = list()
Parameter_BBand_t2 = list()
for(i in 1:(AllYear - 3)){
        t1_max = double(1)
        t2_max = double(1)
        for(j in N){
                for(k in DownB){
                        for(l in Lag){
                                
                                TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)),'::',as.character(S+years(TimeGap+i-1+1+2)-days(1)))]
                                
                                BBand <- TTR::BBands(cbind.xts(TWII_Period$High,TWII_Period$Low,TWII_Period$Close), n = j)
                                BBand_Signal_Down <- BBand$pctB < k
                                BBand_Signal_Down_Lag <- lag(BBand_Signal_Down,l)
                                BBand_Signal_Down_Lag[is.na(BBand_Signal_Down_Lag)] <-  F
                                
                                if(all(BBand_Signal_Down_Lag$pctB == FALSE)){
                                        next
                                }                                
                                
                                temp = table(ROC(TWII_Period$Close)[BBand_Signal_Down_Lag$pctB] > 0 )
                                
                                t1 = temp[2]/(temp[1]+temp[2])
                                if(is.na(t1)){
                                        next
                                }
                                if(t1 > t1_max){
                                        t1_max = t1 
                                        temp = list(c(j,k,l))
                                        Parameter_BBand_t1[i] = temp
                                }
                                
                                t2 = sum(ROC(TWII_Period$Close)[BBand_Signal_Down_Lag$pctB])
                                if(t2 > t2_max){
                                        t2_max = t2
                                        temp = list(c(j,k,l))
                                        Parameter_BBand_t2[i] = temp
                                }
                                
                                print(c(i,j,k,l,t2))
                                        
                        }
                }
        }
}

print('BBand DONE!!!!')
save.image(file = 'OptPar.RData')

##KD
kd = seq(10,16,1) #(相當於nFastK)
K = seq(3,10,1) #(相當於nFastD)
D = seq(3,10,1) #(相當於nSlowD)
Cont = 0

Parameter_KD_t1 = list()
Parameter_KD_t2 = list()
for(i in 1:(AllYear - 3)){
        t1_max = double(1)
        t2_max = double(1)
        for(j in kd){
                for(k in K){
                        for(l in D){
                                
                                TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)),'::',as.character(S+years(TimeGap+i-1+1+2)-days(1)))]
                                
                                KD <- TTR::stoch(HLC(TWII_Period), nFastK = j, nFastD = k, nSlowD = l)
                                #K線大於D線且兩個兩個值都大於0.2則做多
                                KD_K <- KD$fastD
                                KD_D <- KD$slowD
                                KD_UPThru <- KD_K - KD_D
                                KD_UPThru[KD_UPThru<=0] <- 0
                                ROC_KD_UPThru <- ROC(KD_UPThru, type = 'discrete')
                                ROC_KD_UPThru_Signal <- ROC_KD_UPThru > 0
                                Cond1_KD <- ContSmallThanN(KD$fastD,N = 3,.2) & ContSmallThanN(KD$slowD,N = 3,.2)
                                Cond1_KD <- xts(Cond1_KD,order.by = time(TWII_Period))
                                ROC_KD_UPThru_Signal <- (ROC_KD_UPThru_Signal > 0) & (Cond1_KD == T) & (TWII$Close  > AvgP_TWII_20 ) & (TWII$Volume > AvgV_TWII_5)
                                ROC_KD_UPThru_Signal[is.na(ROC_KD_UPThru_Signal)] <-  F
                                ROC_KD_UPThru_Signal_Cont <- ContinuingPeriod(ROC_KD_UPThru_Signal, Cont)
                                
                                if(all(ROC_KD_UPThru_Signal_Cont$Signal == FALSE)){
                                        next
                                }                                
                                
                                temp = table(ROC(TWII_Period$Close)[ROC_KD_UPThru_Signal_Cont$Signal] > 0 )
                                
                                t1 = temp[2]/(temp[1]+temp[2])
                                if(is.na(t1)){
                                        next
                                }
                                if(t1 > t1_max){
                                        t1_max = t1 
                                        temp = list(c(j,k,l))
                                        Parameter_KD_t1[i] = temp
                                }
                                
                                t2 = sum(ROC(TWII_Period$Close)[ROC_KD_UPThru_Signal_Cont$Signal])
                                if(t2 > t2_max){
                                        t2_max = t2
                                        temp = list(c(j,k,l))
                                        Parameter_KD_t2[i] = temp
                                }
                                
                                print(c(i,j,k,l,t2))
                                
                        }
                }
        }
}

print('KD DONE!!!!')
save.image(file = 'OptPar.RData')

##RSI
Fast = seq(1,8,1)
Slow = seq(9,18,1)
# UpRsi = 80
# DownRsi = 50
Cont = 0

Parameter_RSI_t1 = list()
Parameter_RSI_t2 = list()
for(i in 1:(AllYear - 3)){
        t1_max = double(1)
        t2_max = double(1)
        for(j in Fast){
                for(k in Slow){
                                
                        TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)),'::',as.character(S+years(TimeGap+i-1+1+2)-days(1)))]
                        
                        #N天的RSI小於DownRsi時，在Lag天後做多
                        RSI_Fast <- TTR::RSI(TWII_Period$Close,n = j)
                        RSI_Slow <- TTR::RSI(TWII_Period$Close,n = k)
                        
                        RSI_Signal <- RSI_Fast - RSI_Slow
                        RSI_Signal[RSI_Signal<=0] <- 0
                        ROC_RSI_Signal <- ROC(RSI_Signal, type = 'discrete')
                        ROC_RSI_Signal <- ROC_RSI_Signal > 0
                        RSI_Signal_Cond <- (ROC_RSI_Signal == T) 
                        RSI_Signal_Cond[is.na(RSI_Signal_Cond)] <-  F
                        RSI_Signal_Cont <- ContinuingPeriod(RSI_Signal_Cond,Cont)
                        
                        if(all(RSI_Signal_Cont$Signal == FALSE)){
                                next
                        }                                
                        
                        temp = table(ROC(TWII_Period$Close)[RSI_Signal_Cont$Signal] > 0 )
                        
                        t1 = temp[2]/(temp[1]+temp[2])
                        if(is.na(t1)){
                                next
                        }
                        if(t1 > t1_max){
                                t1_max = t1 
                                temp = list(c(j,k))
                                Parameter_RSI_t1[i] = temp
                        }
                        
                        t2 = sum(ROC(TWII_Period$Close)[RSI_Signal_Cont$Signal])
                        if(t2 > t2_max){
                                t2_max = t2
                                temp = list(c(j,k))
                                Parameter_RSI_t2[i] = temp
                        }
                        
                        print(c(i,j,k,t2))
                }
        }
}

print('RSI DONE!!!!')
save.image(file = 'OptPar.RData')

##ADX
N = seq(2,20,1)
Cont = 0

Parameter_ADX_t1 = list()
Parameter_ADX_t2 = list()
for(i in 1:(AllYear - 3)){
        t1_max = double(1)
        t2_max = double(1)
        for(j in N){
                TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)),'::',as.character(S+years(TimeGap+i-1+1+2)-days(1)))]
                ADX <- TTR::ADX(HLC = cbind.xts(TWII_Period$High,TWII_Period$Low,TWII_Period$Close) ,n = j)
                
                ADX$Criteria1 <- (ADX$DIp > ADX$ADX) & (ADX$DIn > ADX$ADX)
                ADX$Criteria2 <- ADX$DIp > ADX$DIn
                # ADX$Criteria2[ADX$Criteria2<=0] <- 0
                # ADX$Criteria2 <- ROC(ADX$Criteria2, type = 'discrete')
                # ADX$Criteria2 <- ADX$Criteria2 > 0
                ADX$Criteria1[is.na(ADX$Criteria1)] = F
                ADX$Criteria1 = xts(ContinuingPeriod(ADX$Criteria1,Cont),order.by = time(ADX$Criteria1))
                ADX$Criteria2[is.na(ADX$Criteria2)] = F
                ADX$Criteria2 = xts(ContinuingPeriod(ADX$Criteria2,Cont),order.by = time(ADX$Criteria2))
                ADX_Signal_UpTru <- (ADX$Criteria1 == T) & (ADX$Criteria2 == T)
                
                table(ROC(TWII_Period$Close)[ADX_Signal_UpTru==T] > 0)
                
                if(all(ADX_Signal_UpTru$Criteria1 == FALSE) ){
                        temp = temp
                }else{
                        temp = table(ROC(TWII_Period$Close)[ADX_Signal_UpTru$Criteria1] > 0)        
                }
                t1 = temp[2]/(temp[1]+temp[2]) #漲跌預測準確率
                
                if(is.na(t1)){
                        t1_max = t1_max
                }else if(t1 > t1_max){
                        t1_max = t1
                        Parameter_ADX_t1[i] = j
                }
                
                #累積報酬最多當作最佳參數
                
                t2 = sum(ROC(TWII_Period$Close)[ADX_Signal_UpTru$Criteria1==T])
                if(t2 > t2_max){
                        t2_max = t2
                        Parameter_ADX_t2[i] = j
                }
                print(c(i,j,t1,t2))
                
        }
}

print('ADX DONE!!!!')
save.image(file = 'OptPar.RData')

##CCI
UpCCI = seq(80,140,5)
# DownCCI = -100
N = seq(3,15,1)
Cont = 0

Parameter_CCI_t1 = list()
Parameter_CCI_t2 = list()
for(i in 1:(AllYear - 3)){
        t1_max = double(1)
        t2_max = double(1)
        for(j in UpCCI){
                for(k in N){
                        
                        TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)),'::',as.character(S+years(TimeGap+i-1+1+2)-days(1)))]
                        
                        #CCI大於Up的天數為Cont天，在隔天買進
                        CCI <- CCI(HLC = cbind.xts(TWII_Period$High,TWII_Period$Low,TWII_Period$Close),n =  k)
                        CCI_Signal_Up <- CCI$cci - j
                        CCI_Signal_Up[CCI_Signal_Up<=0] <- 0
                        ROC_CCI_Signal <- ROC(CCI_Signal_Up, type = 'discrete')
                        ROC_CCI_Signal <- ROC_CCI_Signal > 0
                        ROC_CCI_Signal_Cond <- (ROC_CCI_Signal == T) 
                        ROC_CCI_Signal_Cond[is.na(ROC_CCI_Signal_Cond$cci)] = F
                        ROC_CCI_Signal_Cont <- ContinuingPeriod(ROC_CCI_Signal_Cond,Cont)
                        
                        if(all(ROC_CCI_Signal_Cont$Signal == FALSE)){
                                next
                        }                                
                        
                        temp = table(ROC(TWII_Period$Close)[ROC_CCI_Signal_Cont$Signal] > 0)
                        
                        t1 = temp[2]/(temp[1]+temp[2])
                        if(is.na(t1)){
                                next
                        }
                        if(t1 > t1_max){
                                t1_max = t1 
                                temp = list(c(j,k))
                                Parameter_CCI_t1[i] = temp
                        }
                        
                        t2 = sum(ROC(TWII_Period$Close)[ROC_CCI_Signal_Cont$Signal])
                        if(t2 > t2_max){
                                t2_max = t2
                                temp = list(c(j,k))
                                Parameter_CCI_t2[i] = temp
                        }
                        
                        print(c(i,j,k,t2))
                }
        }
}

print('CCI DONE!!!!')
save.image(file = 'OptPar.RData')

##Aroon
N = seq(2,10,1)
aroon = seq(50,90,5)
Cont = 0

Parameter_Aroon_t1 = list()
Parameter_Aroon_t2 = list()
for(i in 1:(AllYear - 3)){
        t1_max = double(1)
        t2_max = double(1)
        for(j in N){
                for(k in aroon){
                        
                        TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)),'::',as.character(S+years(TimeGap+i-1+1+2)-days(1)))]

                        Aroon <- TTR::aroon(cbind.xts(TWII_Period$High,TWII_Period$Low),j)
                        #aroonUp大於aroonDn且aroonUp大於aroon時在隔天買進
                        Aroon_UpTru <- Aroon$aroonUp - Aroon$aroonDn
                        Cond1_Aroon <- Aroon$aroonUp > k
                        ROC_Aroon_Signal_Cond <- Aroon_UpTru>0 & Cond1_Aroon
                        ROC_Aroon_Signal_Cond[is.na(ROC_Aroon_Signal_Cond$aroonUp)] = F
                        Aroon_Signal_Up_Cont <- ContinuingPeriod(ROC_Aroon_Signal_Cond,Cont)
                        
                        if(all(Aroon_Signal_Up_Cont$Signal == FALSE)){
                                next
                        }                                
                        
                        temp = table(ROC(TWII_Period$Close)[Aroon_Signal_Up_Cont$Signal] > 0)
                        
                        t1 = temp[2]/(temp[1]+temp[2])
                        if(is.na(t1)){
                                next
                        }
                        if(t1 > t1_max){
                                t1_max = t1 
                                temp = list(c(j,k))
                                Parameter_Aroon_t1[i] = temp
                        }
                        
                        t2 = sum(ROC(TWII_Period$Close)[Aroon_Signal_Up_Cont$Signal])
                        if(t2 > t2_max){
                                t2_max = t2
                                temp = list(c(j,k))
                                Parameter_Aroon_t2[i] = temp
                        }
                        
                        print(c(i,j,k,t2))
                }
        }
}

print('Aroon DONE!!!!')
save.image(file = 'OptPar.RData')


##EMV
N = seq(3,30,3)
t = seq(-.1,.3 ,0.05)
Cont = 1

Parameter_EMV_t1 = list()
Parameter_EMV_t2 = list()
for(i in 1:(AllYear - 3)){
        t1_max = double(1)
        t2_max = double(1)
        for(j in N){
                for(k in t){
                        
                        TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)),'::',as.character(S+years(TimeGap+i-1+1+2)-days(1)))]
                        
                        EMV <- EMV(cbind.xts(TWII_Period$High,TWII_Period$Low),TWII_Period$Volume,j)
                        #EMV大於0時隔天買進
                        EMV_UpThru <- EMV$emv - k
                        ROC_EMV_Signal_Cond <- EMV_UpThru > 0
                        ROC_EMV_Signal_Cond[is.na(ROC_EMV_Signal_Cond)] = F
                        EMV_Signal_Up_Cont <- ContinuingPeriod(ROC_EMV_Signal_Cond,Cont)
                        
                        if(all(EMV_Signal_Up_Cont$Signal == FALSE)){
                                next
                        }                                
                        
                        temp = table(ROC(TWII_Period$Close)[EMV_Signal_Up_Cont$Signal] > 0 )
                        
                        t1 = temp[2]/(temp[1]+temp[2])
                        if(is.na(t1)){
                                next
                        }
                        if(t1 > t1_max){
                                t1_max = t1 
                                temp = list(c(j,k))
                                Parameter_EMV_t1[i] = temp
                        }
                        
                        t2 = sum(ROC(TWII_Period$Close)[EMV_Signal_Up_Cont$Signal])
                        if(t2 > t2_max){
                                t2_max = t2
                                temp = list(c(j,k))
                                Parameter_EMV_t2[i] = temp
                        }
                        
                        print(c(i,j,k,t2))
                }
        }
}

print('EMV DONE!!!!')
save.image(file = 'OptPar.RData')

##ChaikinVolatility
N = seq(3,20)
T_Down = seq(-0.3,0.2,0.05)
Cont = 0 

Parameter_CV_t1 = list()
Parameter_CV_t2 = list()
for(i in 1:(AllYear - 3)){
        t1_max = double(1)
        t2_max = -1
        for(j in N){
                for(k in T_Down){
                        
                        TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)),'::',as.character(S+years(TimeGap+i-1+1+2)-days(1)))]
                        
                        ChaikinVolatility <- chaikinVolatility(cbind.xts(TWII_Period$High,TWII_Period$Low),j)
                        ChaikinVolatility_Signal <- ChaikinVolatility < k
                        ChaikinVolatility_Signal[is.na(ChaikinVolatility_Signal)] = F
                        ChaikinVolatility_Signal <- ContinuingPeriod(ChaikinVolatility_Signal,Cont)
                        
                        if(all(ChaikinVolatility_Signal$Signal == FALSE)){
                                next
                        }                                
                        
                        temp = table(ROC(TWII_Period$Close)[ChaikinVolatility_Signal$Signal] > 0 )
                        
                        t1 = temp[2]/(temp[1]+temp[2])
                        if(is.na(t1)){
                                next
                        }
                        if(t1 > t1_max){
                                t1_max = t1 
                                temp = list(c(j,k))
                                Parameter_CV_t1[i] = temp
                        }
                        
                        t2 = sum(ROC(TWII_Period$Close)[ChaikinVolatility_Signal$Signal])
                        if(t2 > t2_max){
                                t2_max = t2
                                temp = list(c(j,k))
                                Parameter_CV_t2[i] = temp
                        }
                        
                        print(c(i,j,k,t2))
                }
        }
}

print('CV DONE!!!!')
save.image(file = 'OptPar.RData')

##MFI
N = seq(3,20)
T_Down = seq(30,80,5)
Cont = 0 

Parameter_MFI_t1 = list()
Parameter_MFI_t2 = list()
for(i in 1:(AllYear - 3)){
        t1_max = double(1)
        t2_max = -1
        for(j in N){
                for(k in T_Down){
                        
                        TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)),'::',as.character(S+years(TimeGap+i-1+1+2)-days(1)))]
                        
                        MFI <- MFI(cbind.xts(TWII_Period$High,TWII_Period$Low,TWII_Period$Close),TWII_Period$Volume,j)
                        MFI_Signal <- MFI > k
                        MFI_Signal[is.na(MFI_Signal)] = F
                        MFI_Signal <- ContinuingPeriod(MFI_Signal,Cont)
                        
                        if(all(MFI_Signal$Signal == FALSE)){
                                next
                        }                                
                        
                        temp = table(ROC(TWII_Period$Close)[MFI_Signal$Signal] > 0 )
                        
                        t1 = temp[2]/(temp[1]+temp[2])
                        if(is.na(t1)){
                                next
                        }
                        if(t1 > t1_max){
                                t1_max = t1 
                                temp = list(c(j,k))
                                Parameter_MFI_t1[i] = temp
                        }
                        
                        t2 = sum(ROC(TWII_Period$Close)[MFI_Signal$Signal])
                        if(t2 > t2_max){
                                t2_max = t2
                                temp = list(c(j,k))
                                Parameter_MFI_t2[i] = temp
                        }
                        
                        print(c(i,j,k,t2))
                }
        }
}

print('MFI DONE!!!!')
save.image(file = 'OptPar.RData')

##CMO
N = seq(3,20)
T_Down = seq(-20.3045,-20.3043,0.00001)
Cont = 0 

Parameter_CMO_t1 = list()
Parameter_CMO_t2 = list()
for(i in 1:(AllYear - 3)){
        t1_max = double(1)
        t2_max = -1
        for(j in N){
                for(k in T_Down){
                        
                        TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)),'::',as.character(S+years(TimeGap+i-1+1+2)-days(1)))]
                        
                        CMO <- CMO(cbind.xts(TWII_Period$Close,TWII_Period$Volume),j)
                        CMO_Signal <- CMO > k
                        CMO_Signal[is.na(CMO_Signal)] = F
                        CMO_Signal <- ContinuingPeriod(CMO_Signal,Cont)
                        
                        if(all(CMO_Signal$Signal == FALSE)){
                                next
                        }                                
                        
                        temp = table(ROC(TWII_Period$Close)[CMO_Signal$Signal] > 0 )
                        
                        t1 = temp[2]/(temp[1]+temp[2])
                        if(is.na(t1)){
                                next
                        }
                        if(t1 > t1_max){
                                t1_max = t1 
                                temp = list(c(j,k))
                                Parameter_CMO_t1[i] = temp
                        }
                        
                        t2 = sum(ROC(TWII_Period$Close)[CMO_Signal$Signal])
                        if(t2 > t2_max){
                                t2_max = t2
                                temp = list(c(j,k))
                                Parameter_CMO_t2[i] = temp
                        }
                        
                        print(c(i,j,k,t2))
                }
        }
}

print('CMO DONE!!!!')
save.image(file = 'OptPar.RData')


##Load Environment
#load('OptPar.RData')

