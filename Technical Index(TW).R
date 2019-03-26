library(TTR)
library(quantmod)
library(data.table)
library(dplyr)
setwd('D:\\中山大學財管所\\畢業論文\\Data\\美國台灣指數Daily Data')

####Custom function####
ContinuingPeriod <- function(Signal,N){
        n = N
        temp = data.frame(Signal=rep(F,length(Signal)))
        temp <- tryCatch({for(i in seq_along(Signal)){
                if(eval(parse(text = paste('Signal[','i+',0:n,']','==T',collapse = '&&')))){
                        if((i+n+1)<=length(Signal)){
                                temp[i+n+1,1] = T
                        }else{break}
                        
                }
                i = i + 1
                
        }
                return(temp)}
        ,error = function(err){
                return(temp)
        }
        )
        return(temp)
}

AdjROC <- function(x){
        tt = rep(NA,length(x))
        for(i in seq_along(x)){
                if((i <= length(x)-1) && !is.na(x[i])){
                        if((sign(x[i]) == 1 && sign(x[i+1]) == 1)){ 
                                tt[i+1] = (coredata(x[i+1])-coredata(x[i]))/coredata(x[i]) }
                        else if((sign(x[i]) == 1 && sign(x[i+1]) == -1)){
                                tt[i+1] = (coredata(x[i+1])-coredata(x[i]))/coredata(x[i]) }
                        else if((sign(x[i]) == -1 && sign(x[i+1]) == -1)){
                                tt[i+1] = (coredata(x[i])-coredata(x[i+1]))/coredata(x[i]) }
                        else if((sign(x[i]) == -1 && sign(x[i+1]) == 1)){
                                tt[i+1] = (coredata(x[i])-coredata(x[i+1]))/coredata(x[i])
                        }
                }
        }
        return(tt)
}

SpreadTime <- function(x,N){
        temp = x+1
        for(i in 1:(N-1)){
                temp = c(temp,x+i+1)
        }
        return(temp)
}

#數列x連續N天小於S，則為鈍化訊號(KD Index)
ContSmallThanN <- function(Index,N,S){
        n = N
        Index = Index > S 
        Index[which(is.na(Index))] = T
        temp = data.frame(Index=rep(T,length(Index)))
        temp <- tryCatch({for(i in seq_along(Index)){
                if(eval(parse(text = paste('Index[','i+',0:n,']','==F',collapse = '&&')))){
                        if((i+n+1)<=length(Index)){
                                temp[i+n,1] = F
                        }else{break}
                        
                }
        }
                return(temp)}
        ,error = function(err){
                return(temp)
        }
        )
        return(temp)
}

####Import Data####
##Taiwan 2000~2016
TWII <- fread('TWII.csv')
TWII <- xts(x = TWII %>% select(-Date) , order.by = as.Date(TWII$Date))

TWII_UpDown_Daily <- ROC(TWII$Close) > 0
TWII_UpDown_Daily <- TWII_UpDown_Daily['2000::2016']

##一週出現的所有技術分析訊號次數加總與週的漲跌關西
#TWII_UpDown_weekly <- apply.weekly(na.trim(ROC(TWII$Close)+1),function(x) prod(x)-1) > 0
TWII_UpDown_weekly <- apply.weekly(na.trim(TWII$Close),function(x) last(x)-first(x)) > 0

#TWII_UpDown_Monthly <- apply.monthly(na.trim(ROC(TWII$Close)+1),function(x) prod(x)-1) > 0
TWII_UpDown_Monthly <- apply.monthly(na.trim(TWII$Close),function(x) last(x)-first(x)) > 0
TWII_UpDown_Monthly <- TWII_UpDown_Monthly["2000::2016"]
Futures_ROC_Monthly <- apply.monthly(na.trim(Futures$收盤價),function(x) (last(x)-first(x))/first(x)) 
Futures_ROC_Monthly <- Futures_ROC_Monthly["2000::2016"]

TWII <- TWII['2000::2016']
TechnicalIndex <- data.frame(TWII_UpDown_Daily = (data.frame(TWII_UpDown_Daily))$Close)
TechnicalIndex_Numeric <- data.frame(TWII_UpDown_Daily = (data.frame(TWII_UpDown_Daily))$Close)

# a = lapply( TechnicalIndex , function(x) apply.weekly(x,sum))
# a = as.xts(data.frame(a))
# a = a %>% data.frame() %>% select(contains('Buy')) %>% as.xts(order.by = time(TWII_UpDown_weekly))
# aa = merge(TWII_UpDown_weekly,a)
# table(data.frame(merge(TWII_UpDown_weekly,rowSums(aa[,2:ncol(aa)]))))
# 
# b = lapply(TechnicalIndex , function(x) apply.weekly(x,sum))
# b = as.xts(data.frame(b))
# b = b %>% data.frame() %>% select(contains('Sell')) %>% as.xts(order.by = time(TWII_UpDown_weekly))
# bb = merge(TWII_UpDown_weekly,b)
# table(data.frame(merge(TWII_UpDown_weekly,rowSums(bb[,2:ncol(bb)]))))

##平均台股價格
AvgP_TWII_5 <- EMA(TWII$Close,5) %>% lag.xts(1)
AvgP_TWII_20 <- EMA(TWII$Close,20) %>% lag.xts(1)
AvgP_TWII_60 <- EMA(TWII$Close,60) %>% lag.xts(1)
##平均台股交易量
AvgV_TWII_5 <- EMA(TWII$Volume,5) %>% lag.xts(1)
AvgV_TWII_20 <- EMA(TWII$Volume,20) %>% lag.xts(1)
AvgV_TWII_60 <- EMA(TWII$Volume,60) %>% lag.xts(1)

##America
#Dow Jones
DJI <- fread('DJI.csv')
DJI <- xts(x = DJI %>% select(-Date) , order.by = as.Date(DJI$Date))
#S&P
SP500 <- fread('SP500.csv')
SP500 <- xts(x = SP500 %>% select(-Date) , order.by = as.Date(SP500$Date))
#NASDAQ
NASDAQ <- fread('NASDAQ.csv')
NASDAQ <- xts(x = NASDAQ %>% select(-Date) , order.by = as.Date(NASDAQ$Date))

####Techical Analysis####
##OBV:OBV連續Cont天變動率大於零
Cont = 0
#OBV連續Cont天變動率大於零在隔天買進
OBV <- TTR::OBV(price = TWII$Close, volume = TWII$Volume)
ROC_OBV <- AdjROC(OBV)
ROC_OBV_Signal <- ( ROC_OBV > 0) & (TWII$Close > AvgP_TWII_5) & (TWII$Volume > AvgV_TWII_5)
ROC_OBV_Signal[is.na(ROC_OBV_Signal)] <-  F
ROC_OBV_Signal_Cont <- ContinuingPeriod(ROC_OBV_Signal, Cont)
table(ROC(TWII$Close)[ROC_OBV_Signal_Cont$Signal] > 0)
sum(ROC(TWII$Close)[ROC_OBV_Signal_Cont$Signal])

TechnicalIndex$OBV_Buy <- ROC_OBV_Signal_Cont$Signal

#OBV連續Cont天變動率大於零在隔天放空
OBV <- TTR::OBV(price = TWII$Close, volume = TWII$Volume)
ROC_OBV <- ROC(OBV,type = 'discrete')
ROC_OBV_Signal <-  ROC_OBV < 0
ROC_OBV_Signal[is.na(ROC_OBV_Signal)] <-  F
ROC_OBV_Signal_Cont <- ContinuingPeriod(ROC_OBV_Signal,Cont)
table(ROC(TWII$Close)[ROC_OBV_Signal_Cont$Signal] < 0)
sum(-ROC(TWII$Close)[ROC_OBV_Signal_Cont$Signal])

# TechnicalIndex$OBV_Sell <- ROC_OBV_Signal_Cont$Signal  

##MACD:DIF與MACD的差的變動率連續四天大於Cont
Cont = 0
Fast = 2
Slow = 35
Sig = 4
#MACD:DIF與MACD的差的變動率連續四天大於Cont在隔天買進
MACD <- TTR::MACD(TWII, nFast = Fast, nSlow = Slow, nSig = Sig,percent = F)
ROC_MACD <- AdjROC(MACD$macd-MACD$signal)
Cond1_MACD <- (MACD$macd > 0) & (MACD$signal > 0)
ROC_MACD_Signal <- (ROC_MACD > 0) & (Cond1_MACD == T) & (TWII$Close  > AvgP_TWII_20 ) & (TWII$Volume > AvgV_TWII_20)
ROC_MACD_Signal[is.na(ROC_MACD_Signal)] <- F
ROC_MACD_Signal_Cont <- ContinuingPeriod(ROC_MACD_Signal,Cont)
table(ROC(TWII$Close)[ROC_MACD_Signal_Cont$Signal] > 0)
sum(ROC(TWII$Close)[ROC_MACD_Signal_Cont$Signal])

TechnicalIndex$MACD_Buy <- ROC_MACD_Signal_Cont$Signal

#MACD:DIF與MACD的差的變動率連續四天大於Cont在隔天放空
MACD <- TTR::MACD(TWII, nFast = Fast, nSlow = Slow, nSig = Sig)
ROC_MACD <- AdjROC((MACD$macd-MACD$signal))
ROC_MACD_Signal <- ROC_MACD < 0
ROC_MACD_Signal[is.na(ROC_MACD_Signal)] <- F
ROC_MACD_Signal_Cont <- ContinuingPeriod(ROC_MACD_Signal,Cont)
table(ROC(TWII$Close)[ROC_MACD_Signal_Cont$Signal] < 0)
sum(-ROC(TWII$Close)[ROC_MACD_Signal_Cont$Signal])

# TechnicalIndex$MACD_Sell <- ROC_MACD_Signal_Cont$Signal

##SMA快線向上穿越持續Cont天
Fast = 8
Slow = 40
Cont = 0

SMA_Fast <- TTR::SMA(TWII$Close, Fast)
SMA_Slow <- TTR::SMA(TWII$Close, Slow)
#快線向上穿越持續Cont天，則在隔天買進
SMA_UPThru <- SMA_Fast - SMA_Slow
Cond1_SMA <- (SMA_Fast > 0) & (SMA_Slow > 0)
SMA_UPThru_Signal <- (SMA_UPThru > 0) & (Cond1_SMA == T) & (TWII$Close  > AvgP_TWII_60 ) & (TWII$Volume > AvgV_TWII_60)
# SMA_UPThru[SMA_UPThru <= 0] <- 0
# ROC_SMA_UPThru <- ROC(SMA_UPThru,type = 'discrete')
# ROC_SMA_UPThru_Signal <- ROC_SMA_UPThru > 0
SMA_UPThru_Signal[is.na(SMA_UPThru_Signal)] <-  F
SMA_UPThru_Signal_Cont <- ContinuingPeriod(SMA_UPThru_Signal, Cont)
table(ROC(TWII$Close)[SMA_UPThru_Signal_Cont$Signal] > 0 )
sum(ROC(TWII$Close)[SMA_UPThru_Signal_Cont$Signal])

TechnicalIndex$SMA_Buy <- SMA_UPThru_Signal_Cont$Signal

#快線向下穿越持續Cont天，則在隔天放空
SMA_DOWNThru <- SMA_Fast - SMA_Slow
SMA_DOWNThru[SMA_DOWNThru >= 0] <- 0
ROC_SMA_DOWNThru <- ROC(SMA_DOWNThru,type = 'discrete')
ROC_SMA_DOWNThru_Signal <- ROC_SMA_DOWNThru > 0
ROC_SMA_DOWNThru_Signal[is.na(ROC_SMA_DOWNThru_Signal)] <-  F
ROC_SMA_DOWNThru_Signal_Cont <- ContinuingPeriod(ROC_SMA_DOWNThru_Signal, Cont)
table(ROC(TWII$Close)[ROC_SMA_DOWNThru_Signal_Cont$Signal] < 0)
sum(-ROC(TWII$Close)[ROC_SMA_DOWNThru_Signal_Cont$Signal])

# TechnicalIndex$SMA_Sell <- ROC_SMA_DOWNThru_Signal_Cont$Signal

##VWMA:(上漲通常量大所以會變動較快，下跌通常量小所以變動較慢)
Fast = 2
Slow = 25
Cont = 0

VWMA_Fast <- TTR::VWMA(TWII$Close, TWII$Volume, Fast)
VWMA_Slow <- TTR::VWMA(TWII$Close, TWII$Volume, Slow)
#快線向上穿越持續Cont天，則在隔天買進
VWMA_UPThru <- VWMA_Fast - VWMA_Slow
# VWMA_UPThru[VWMA_UPThru <= 0] <- 0
# ROC_VWMA_UPThru <- ROC(VWMA_UPThru,type = 'discrete')
ROC_VWMA_UPThru_Signal <- VWMA_UPThru > 0
Cond1_VWMA <- (VWMA_Fast > 0) & (VWMA_Slow > 0)
ROC_VWMA_UPThru_Signal <- (ROC_VWMA_UPThru_Signal > 0) & (Cond1_VWMA == T) & (TWII$Close  > AvgP_TWII_20 ) & (TWII$Volume > AvgV_TWII_5)
ROC_VWMA_UPThru_Signal[is.na(ROC_VWMA_UPThru_Signal)] <-  F
ROC_VWMA_UPThru_Signal_Cont <- ContinuingPeriod(ROC_VWMA_UPThru_Signal, Cont)
table(ROC(TWII$Close)[ROC_VWMA_UPThru_Signal_Cont$Signal] > 0 )
sum(ROC(TWII$Close)[ROC_VWMA_UPThru_Signal_Cont$Signal])

TechnicalIndex$VWMA_Buy <-  ROC_VWMA_UPThru_Signal_Cont$Signal

#快線向下穿越持續Cont天，則在隔天放空
VWMA_DOWNThru <- VWMA_Fast - VWMA_Slow
VWMA_DOWNThru[VWMA_DOWNThru >= 0] <- 0
ROC_VWMA_DOWNThru <- ROC(VWMA_DOWNThru,type = 'discrete')
ROC_VWMA_DOWNThru_Signal <- ROC_VWMA_DOWNThru > 0
ROC_VWMA_DOWNThru_Signal[is.na(ROC_VWMA_DOWNThru_Signal)] <-  F
ROC_VWMA_DOWNThru_Signal_Cont <- ContinuingPeriod(ROC_VWMA_DOWNThru_Signal, Cont)
table(ROC(TWII$Close)[ROC_VWMA_DOWNThru_Signal_Cont$Signal] < 0)
sum(-ROC(TWII$Close)[ROC_VWMA_DOWNThru_Signal_Cont$Signal])

# TechnicalIndex$VWMA_Sell <- ROC_VWMA_DOWNThru_Signal_Cont$Signal

##EMA
Fast = 5
Slow = 40
Cont = 0

EMA_Fast <- TTR::EMA(TWII$Close, Fast)
EMA_Slow <- TTR::EMA(TWII$Close, Slow)
#快線向上穿越持續Cont天，則在隔天買進
EMA_UPThru <- EMA_Fast - EMA_Slow
EMA_UPThru[EMA_UPThru <= 0] <- 0
ROC_EMA_UPThru <- ROC(EMA_UPThru,type = 'discrete')
ROC_EMA_UPThru_Signal <- ROC_EMA_UPThru > 0
Cond1_EMA <- (EMA_Fast > 0) & (EMA_Slow > 0)
ROC_EMA_UPThru_Signal <- (ROC_EMA_UPThru_Signal > 0) & (Cond1_EMA == T) & (TWII$Close  > AvgP_TWII_20 ) & (TWII$Volume > AvgV_TWII_5)
ROC_EMA_UPThru_Signal[is.na(ROC_EMA_UPThru_Signal)] <-  F
ROC_EMA_UPThru_Signal_Cont <- ContinuingPeriod(ROC_EMA_UPThru_Signal, Cont)
table(ROC(TWII$Close)[ROC_EMA_UPThru_Signal_Cont$Signal] > 0 )
sum(ROC(TWII$Close)[ROC_EMA_UPThru_Signal_Cont$Signal])

TechnicalIndex$EMA_Buy <- ROC_EMA_UPThru_Signal_Cont$Signal

#快線向下穿越持續Cont天，則在隔天放空
EMA_DOWNThru <- EMA_Fast - EMA_Slow
EMA_DOWNThru[EMA_DOWNThru >= 0] <- 0
ROC_EMA_DOWNThru <- ROC(EMA_DOWNThru,type = 'discrete')
ROC_EMA_DOWNThru_Signal <- ROC_EMA_DOWNThru > 0
ROC_EMA_DOWNThru_Signal[is.na(ROC_EMA_DOWNThru_Signal)] <-  F
ROC_EMA_DOWNThru_Signal_Cont <- ContinuingPeriod(ROC_EMA_DOWNThru_Signal, Cont)
table(ROC(TWII$Close)[ROC_EMA_DOWNThru_Signal_Cont$Signal] < 0)
sum(-ROC(TWII$Close)[ROC_EMA_DOWNThru_Signal_Cont$Signal])

# TechnicalIndex$EMA_Sell <- ROC_EMA_DOWNThru_Signal_Cont$Signal

##BBand
N = 9
UpB = .85
DownB = .2
Lag = 3

#當移動平均低於兩配標準差(DownBand)Lag天後，在隔天做多
BBand <- TTR::BBands(cbind.xts(TWII$High,TWII$Low,TWII$Close), n = N)
BBand_Signal_Down <- BBand$pctB < DownB
BBand_Signal_Down_Lag <- lag(BBand_Signal_Down,Lag)
BBand_Signal_Down_Lag[is.na(BBand_Signal_Down_Lag)] <-  F
BBand_Signal_Down_Lag <- (BBand_Signal_Down_Lag == T) & (TWII$Close  > AvgP_TWII_5 ) & (TWII$Volume > AvgV_TWII_5)
table(ROC(TWII$Close)[BBand_Signal_Down_Lag] > 0)
sum(ROC(TWII$Close)[BBand_Signal_Down_Lag])

TechnicalIndex$BBand_Buy <- BBand_Signal_Down_Lag

#當移動平均超過兩倍標準差(UpBand)Lag天後，在隔天放空
BBand <- TTR::BBands(cbind.xts(TWII$High,TWII$Low,TWII$Close), n = N)
BBand_Signal_Up <- BBand$pctB > UpB
BBand_Signal_Up_Lag <- lag(BBand_Signal_Up,Lag)
BBand_Signal_Up_Lag[is.na(BBand_Signal_Up_Lag)] <-  F
table(ROC(TWII$Close)[BBand_Signal_Up_Lag] < 0)
sum(-ROC(TWII$Close)[BBand_Signal_Up_Lag])

# TechnicalIndex$BBand_Sell <- BBand_Signal_Up_Lag

##KD
kd = 9 #(相當於nFastK)
K = 3 #(相當於nFastD)
D = 3 #(相當於nSlowD)
Cont = 0

KD <- TTR::stoch(HLC(TWII), nFastK = kd, nFastD = K, nSlowD = D)
#K線大於D線且兩個兩個值都大於0.2則做多
KD_K <- KD$fastD
KD_D <- KD$slowD
KD_UPThru <- KD_K - KD_D
KD_UPThru[KD_UPThru<=0] <- 0
ROC_KD_UPThru <- ROC(KD_UPThru, type = 'discrete')
ROC_KD_UPThru_Signal <- ROC_KD_UPThru > 0
Cond1_KD <- ContSmallThanN(KD$fastD,N = 3,.2) & ContSmallThanN(KD$slowD,N = 3,.2)
Cond1_KD <- xts(Cond1_KD,order.by = time(TWII))
ROC_KD_UPThru_Signal <- (ROC_KD_UPThru_Signal > 0) & (Cond1_KD == T) & (TWII$Close  > AvgP_TWII_20 ) & (TWII$Volume > AvgV_TWII_5)
ROC_KD_UPThru_Signal[is.na(ROC_KD_UPThru_Signal)] <-  F
ROC_KD_UPThru_Signal_Cont <- ContinuingPeriod(ROC_KD_UPThru_Signal, Cont)
table(ROC(TWII$Close)[ROC_KD_UPThru_Signal_Cont$Signal] > 0 )
sum(ROC(TWII$Close)[ROC_KD_UPThru_Signal_Cont$Signal])

TechnicalIndex$KD_Buy <- ROC_KD_UPThru_Signal_Cont$Signal


##RSI
Fast = 5
Slow = 14
# UpRsi = 80
# DownRsi = 50
Cont = 0

#N天的RSI小於DownRsi時，在Lag天後做多
RSI_Fast <- TTR::RSI(TWII$Close,n = Fast)
RSI_Slow <- TTR::RSI(TWII$Close,n = Slow)

RSI_Signal <- RSI_Fast - RSI_Slow
RSI_Signal[RSI_Signal<=0] <- 0
ROC_RSI_Signal <- ROC(RSI_Signal, type = 'discrete')
ROC_RSI_Signal <- ROC_RSI_Signal > 0
RSI_Signal_Cond <- (ROC_RSI_Signal == T) 
RSI_Signal_Cond[is.na(RSI_Signal_Cond)] <-  F
RSI_Signal_Cont <- ContinuingPeriod(RSI_Signal_Cond,Cont)
table(ROC(TWII$Close)[RSI_Signal_Cont$Signal] > 0)
sum(ROC(TWII$Close)[RSI_Signal_Cont$Signal])

TechnicalIndex$RSI_Buy <- RSI_Signal_Cont$Signal

#N天的RSI大於UpRsi時，在Lag天後做多
RSI <- TTR::RSI(TWII$Close,n = N)
RSI_Signal_Up <- RSI$EMA > UpRsi
RSI_Signal_Up_Lag <- lag(RSI_Signal_Up,Lag)
RSI_Signal_Up_Lag[is.na(RSI_Signal_Up_Lag)] <-  F
table(ROC(TWII$Close)[RSI_Signal_Up_Lag] > 0)
sum(-ROC(TWII$Close)[RSI_Signal_Up_Lag])

# TechnicalIndex$RSI_Sell <- RSI_Signal_Up_Lag

##ADX
N = 14
Cont = 0

#當ADX大於Adx，且DI+向上穿越DI-持續Cont天時在隔天買進
ADX <- TTR::ADX(HLC = cbind.xts(TWII_Period$High,TWII_Period$Low,TWII_Period$Close) ,n = N)
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
table(ROC(TWII$Close)[ADX_Signal_UpTru==1] > 0)
sum(ROC(TWII$Close)[ADX_Signal_UpTru==1])

TechnicalIndex$ADX_Buy <- ADX_Signal_UpTru==1

#當ADX大於Adx，且DI+向下穿越DI-持續Cont天時在隔天賣進
ADX <- TTR::ADX(HLC = cbind.xts(TWII$High,TWII$Low,TWII$Close) ,n = N)
ADX$Criteria <- ADX$ADX>25 & (ADX$DIp < ADX$DIn)
ADX$Criteria = lag(ADX$Criteria)
ADX$Criteria[is.na(ADX$Criteria)] = F
ADX_Signal_DownTru <- ADX$Criteria
table(ROC(TWII$Close)[ADX_Signal_DownTru==1] < 0)
sum(-ROC(TWII$Close)[ADX_Signal_DownTru==1])

# TechnicalIndex$ADX_Sell <- ADX_Signal_DownTru

##CCI
UpCCI = 100
# DownCCI = -100
N = 11
Cont = 0

#CCI大於Up的天數為Cont天，在隔天買進
CCI <- TTR::CCI(HLC = cbind.xts(TWII$High,TWII$Low,TWII$Close),n =  N)
CCI_Signal_Up <- CCI$cci - UpCCI
CCI_Signal_Up[CCI_Signal_Up<=0] <- 0
ROC_CCI_Signal <- ROC(CCI_Signal_Up, type = 'discrete')
ROC_CCI_Signal <- ROC_CCI_Signal > 0
ROC_CCI_Signal_Cond <- (ROC_CCI_Signal == T) 
ROC_CCI_Signal_Cond[is.na(ROC_CCI_Signal_Cond$cci)] = F
CCI_Signal_Up_Cont <- ContinuingPeriod(ROC_CCI_Signal_Cond,Cont)
table(ROC(TWII$Close)[CCI_Signal_Up_Cont$Signal] > 0)
sum(ROC(TWII$Close)[CCI_Signal_Up_Cont$Signal])

TechnicalIndex$CCI_Buy <- CCI_Signal_Up_Cont$Signal

#CCI小於Down的天數為Cont天，在隔天賣出
CCI <- CCI(HLC = cbind.xts(TWII$High,TWII$Low,TWII$Close),n =  N)
CCI_Signal_Down <- CCI$cci < DownCCI
CCI_Signal_Down_Lag <- lag(CCI_Signal_Down,Lag)
CCI_Signal_Down_Lag[is.na(CCI_Signal_Down_Lag)] <- F
table(ROC(TWII$Close)[CCI_Signal_Down_Lag] < 0)
sum(-ROC(TWII$Close)[CCI_Signal_Down_Lag])

# TechnicalIndex$CCI_Sell <- CCI_Signal_Down_Lag


##aroon
N = 3
aroon = 70
Cont = 0

Aroon <- TTR::aroon(cbind.xts(TWII$High,TWII$Low),N)
#aroonUp大於aroonDn且aroonUp大於aroon時在隔天買進

Aroon_UpTru <- Aroon$aroonUp - Aroon$aroonDn
Cond1_Aroon <- Aroon$aroonUp > aroon
# Aroon_UpTru[Aroon_UpTru<=0] <- 0
# ROC_Aroon_Signal <- ROC(Aroon_UpTru, type = 'discrete')
# ROC_Aroon_Signal <- ROC_Aroon_Signal > 0
ROC_Aroon_Signal_Cond <- Aroon_UpTru>0 & Cond1_Aroon
ROC_Aroon_Signal_Cond[is.na(ROC_Aroon_Signal_Cond$aroonUp)] = F
Aroon_Signal_Up_Cont <- ContinuingPeriod(ROC_Aroon_Signal_Cond,Cont)
table(ROC(TWII$Close)[Aroon_Signal_Up_Cont$Signal] > 0)
sum(ROC(TWII$Close)[Aroon_Signal_Up_Cont$Signal])

TechnicalIndex$Aroon_Buy <- Aroon_Signal_Up_Cont$Signal

#aroonDn大於aroonUp且aroonDn大於aroon時在隔天賣出
Aroon_DownTru <- (Aroon$aroonDn > Aroon$aroonUp) & Aroon$aroonDn > aroon
Aroon_DownTru_Lag <- lag(Aroon_DownTru)
Aroon_DownTru_Lag[is.na(Aroon_DownTru_Lag)] <- F
table(ROC(TWII$Close)[Aroon_DownTru_Lag] < 0)
sum(-ROC(TWII$Close)[Aroon_DownTru_Lag])

# TechnicalIndex$Aroon_Sell <- Aroon_DownTru_Lag


##EMV價量合成指標
N = 15
t = 0
Cont = 1

EMV <- TTR::EMV(cbind.xts(TWII$High,TWII$Low),TWII$Volume,N)
#EMV大於0時隔天買進
EMV_UpThru <- EMV$emv - t
# EMV_UpThru[EMV_UpThru<=0] <- 0
# ROC_EMV_Signal <- ROC(EMV_UpThru, type = 'discrete')
# ROC_EMV_Signal <- ROC_EMV_Signal > 0
ROC_EMV_Signal_Cond <- EMV_UpThru > 0
ROC_EMV_Signal_Cond[is.na(ROC_EMV_Signal_Cond)] = F
EMV_Signal_Up_Cont <- ContinuingPeriod(ROC_EMV_Signal_Cond,Cont)
table(ROC(TWII$Close)[EMV_Signal_Up_Cont$Signal] > 0 )
sum(ROC(TWII$Close)[EMV_Signal_Up_Cont$Signal])

TechnicalIndex$EMV_Buy <- EMV_Signal_Up_Cont$Signal

#EMV小於0時隔天賣出
EMV_DownThru <- EMV$emv < 0
EMV_DownThru_Lag <- lag(EMV_DownThru)
EMV_DownThru_Lag[is.na(EMV_DownThru_Lag)] <-  F
table(ROC(TWII$Close)[EMV_DownThru_Lag] < 0 )
sum(-ROC(TWII$Close)[EMV_DownThru_Lag])

# TechnicalIndex$EMV_Sell <- EMV_DownThru_Lag


##ChaikinVolatility
N = 3
T_Down = -.2
Cont = 0

ChaikinVolatility <- TTR::chaikinVolatility(cbind.xts(TWII$High,TWII$Low),N)
ChaikinVolatility_Signal <- ChaikinVolatility < T_Down
ChaikinVolatility_Signal[is.na(ChaikinVolatility_Signal)] = F
ChaikinVolatility_Signal <- ContinuingPeriod(ChaikinVolatility_Signal$EMA,Cont)
table(ROC(TWII$Close)[ChaikinVolatility_Signal$Signal] > 0 )
sum(ROC(TWII$Close)[ChaikinVolatility_Signal$Signal])

# cutN = 10
# temp1 = cut(ChaikinVolatility,cutN)
# for(i in 1:cutN){
#         print(table(ROC(TWII$Close)[time(ChaikinVolatility[temp1==levels(temp1)[i]])] > 0))
#         print(ROC(TWII$Close)[time(ChaikinVolatility[temp1==levels(temp1)[i]])+1] %>% sum())
# }

TechnicalIndex$ChaikinVolatility = ChaikinVolatility_Signal$Signal

##MFI
N = 10
T_Down = 50
Cont = 0

MFI <- TTR::MFI(cbind.xts(TWII$High,TWII$Low,TWII$Close),TWII$Volume,N)
MFI_Signal <- MFI > T_Down
MFI_Signal[is.na(MFI_Signal)] = F
MFI_Signal <- ContinuingPeriod(MFI_Signal$mfi,Cont)
table(ROC(TWII$Close)[MFI_Signal$Signal] > 0 )
sum(ROC(TWII$Close)[MFI_Signal$Signal])

TechnicalIndex$MFI = MFI_Signal$Signal

##CMO 
N = 10
T_Down = -20.3045
Cont = 0

CMO <- TTR::CMO(cbind.xts(TWII$Close,TWII$Volume),N)
CMO_Signal <- CMO > T_Down
CMO_Signal[is.na(CMO_Signal)] = F
CMO_Signal <- ContinuingPeriod(CMO_Signal[],Cont)
table(ROC(TWII$Close)[CMO_Signal$Signal] > 0 )
sum(ROC(TWII$Close)[CMO_Signal$Signal])

TechnicalIndex$CMO = CMO_Signal$Signal

##TechnicalIndex
TechnicalIndex <- xts(x = TechnicalIndex, order.by = index(TWII))

####K線型態####

##吞噬型態
N = 10 #價量為近N期
Nday = 10

Engulfing = TWII
Engulfing$Cond1 = F
Engulfing$Cond2 = F
Engulfing$Cond3 = F
#出現吞噬在隔天買進，並持有Nday
for(i in 1:nrow(Engulfing)){
        if(i == nrow(Engulfing)-N){break}
        #Cond1今天上漲 昨天下跌 今天收盤大於昨天開盤和收盤(吞噬)
        if(Engulfing$Close[i+N-1] > Engulfing$Open[i+N-1] &&  Engulfing$Close[i+N-2] < Engulfing$Open[i+N-2] &&
           Engulfing$Close[i+N-1] >= max(Engulfing$Close[i+N-2],Engulfing$Open[i+N-2])){
                Engulfing$Cond1[i+N-1] = T
        }
        #Cond2近期低點
        if(min(Engulfing$Low[(i+N-3):(i+N-2)]) == min(Engulfing$Low[i:(i+N-1)]) &&
           max(Engulfing$Volume[(i+N-3):(i+N-2)]) == max(Engulfing$Volume[i:(i+N-1)])){
                Engulfing$Cond2[i+N-1] = T
        }
        #Cond3上關價
        if(coredata(Engulfing$High[i+N-1]) < coredata(Engulfing$High[i+N-3]) &&
           Engulfing$Close[i+N-1] >= (Engulfing$Low[i+N-1] + ((Engulfing$High[i+N-1]-Engulfing$Low[i+N-1])/3)*2)){
                Engulfing$Cond3[i+N-1] = T
        }
}

Engulfing$Engulfing = Engulfing$Cond1 & Engulfing$Cond2 & Engulfing$Cond3
Time <- index(subset(Engulfing,Engulfing==1))
Time <- sort(SpreadTime(Time,Nday))
table(ROC(TWII$Close)[Time] > 0 )
sum(ROC(TWII$Close)[Time])












