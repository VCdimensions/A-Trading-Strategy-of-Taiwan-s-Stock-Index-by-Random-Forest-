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
TWII <- fread('TWII.csv')
TWII <- xts(x = TWII %>% select(-Date) , order.by = as.Date(TWII$Date))

TWII_UpDown_Daily <- ROC(TWII$Close) > 0
TWII_UpDown_Daily <- TWII_UpDown_Daily['2000::2016']
##America
#Dow Jones
DJI <- fread('DJI.csv')
DJI <- xts(x = DJI %>% select(-Date) , order.by = as.Date(DJI$Date))
#將Volume為0的用前一天取代
coredata(DJI$Volume[which(coredata(DJI$Volume) == 0)]) = coredata(DJI$Volume[which(coredata(DJI$Volume) == 0)-1])
coredata(DJI$Volume[which(coredata(DJI$Volume) == 0)]) = coredata(DJI$Volume[which(coredata(DJI$Volume) == 0)-1])
DJI <- DJI['2000::2016']
TechnicalIndex_DJI <- TWII_UpDown_Daily ; names(TechnicalIndex_DJI) = 'TWII_UpDown_Daily'
TechnicalIndex_DJI_Numeric <- TWII_UpDown_Daily ; names(TechnicalIndex_DJI) = 'TWII_UpDown_Daily'
#S&P
SP <- fread('SP500.csv')
SP <- xts(x = SP %>% select(-Date) , order.by = as.Date(SP$Date))
SP <- SP['2000::2016']
TechnicalIndex_SP <- TWII_UpDown_Daily ; names(TechnicalIndex_SP) = 'TWII_UpDown_Daily'
TechnicalIndex_SP_Numeric <- TWII_UpDown_Daily ; names(TechnicalIndex_SP) = 'TWII_UpDown_Daily'
#NASDAQ
NASDAQ <- fread('NASDAQ.csv')
NASDAQ <- xts(x = NASDAQ %>% select(-Date) , order.by = as.Date(NASDAQ$Date))
#將Volume為0的用前一天取代
coredata(NASDAQ$Volume[which(coredata(NASDAQ$Volume) == 0)]) = coredata(NASDAQ$Volume[which(coredata(NASDAQ$Volume) == 0)-1])
NASDAQ <- NASDAQ['2000::2016']
TechnicalIndex_NASDAQ <- TWII_UpDown_Daily ; names(TechnicalIndex_NASDAQ) = 'TWII_UpDown_Daily'
TechnicalIndex_NASDAQ_Numeric <- TWII_UpDown_Daily ; names(TechnicalIndex_NASDAQ) = 'TWII_UpDown_Daily'


####Techical Analysis####
####OBV:OBV連續Cont天變動率大於零####
#DJI
#OBV連續Cont天變動率大於零在隔天買進
OBV <- TTR::OBV(price = DJI$Close, volume = DJI$Volume) 
ROC_OBV <- AdjROC(OBV)
ROC_OBV <- xts(ROC_OBV,order.by = time(OBV)) %>% lag.xts(1)
OBV = OBV %>% lag.xts(1)

temp = merge.xts(TWII_UpDown_Daily, OBV) 
OBV = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, ROC_OBV)
ROC_OBV = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])

TechnicalIndex_DJI <- merge.xts(TechnicalIndex_DJI, OBV[,2],ROC_OBV[,2])
#SP
OBV <- TTR::OBV(price = SP$Close, volume = SP$Volume) 
ROC_OBV <- AdjROC(OBV)
ROC_OBV <- xts(ROC_OBV,order.by = time(OBV)) %>% lag.xts(1)
OBV = OBV %>% lag.xts(1)

temp = merge.xts(TWII_UpDown_Daily, OBV) 
OBV = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, ROC_OBV)
ROC_OBV = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])

TechnicalIndex_SP <- merge.xts(TechnicalIndex_SP, OBV[,2],ROC_OBV[,2])
#NASDAQ
OBV <- TTR::OBV(price = NASDAQ$Close, volume = NASDAQ$Volume) 
ROC_OBV <- AdjROC(OBV)
ROC_OBV <- xts(ROC_OBV,order.by = time(OBV)) %>% lag.xts(1)
OBV = OBV %>% lag.xts(1)

temp = merge.xts(TWII_UpDown_Daily, OBV) 
OBV = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, ROC_OBV)
ROC_OBV = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])

TechnicalIndex_NASDAQ <- merge.xts(TechnicalIndex_NASDAQ, OBV[,2],ROC_OBV[,2])

####MACD:DIF與MACD的差的變動率連續四天大於Cont####
#DJI
Fast = 13
Slow = 26
Sig = 9
#MACD:DIF與MACD的差的變動率連續四天大於Cont在隔天買進
MACD <- TTR::MACD(DJI, nFast = Fast, nSlow = Slow, nSig = Sig,percent = F)
ROC_MACD <- AdjROC(MACD$macd-MACD$signal)
ROC_MACD <- xts(ROC_MACD,order.by = time(MACD)) %>% lag.xts(1)
MACD = (MACD$macd-MACD$signal) %>% lag.xts(1)

temp = merge.xts(TWII_UpDown_Daily, MACD) 
MACD = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, ROC_MACD)
ROC_MACD = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])

TechnicalIndex_DJI <- merge.xts(TechnicalIndex_DJI, MACD[,2],ROC_MACD[,2])

#SP
Fast = 13
Slow = 26
Sig = 9
#MACD:DIF與MACD的差的變動率連續四天大於Cont在隔天買進
MACD <- TTR::MACD(SP, nFast = Fast, nSlow = Slow, nSig = Sig,percent = F)
ROC_MACD <- AdjROC(MACD$macd-MACD$signal)
ROC_MACD <- xts(ROC_MACD,order.by = time(MACD)) %>% lag.xts(1)
MACD = (MACD$macd-MACD$signal) %>% lag.xts(1)

temp = merge.xts(TWII_UpDown_Daily, MACD) 
MACD = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, ROC_MACD)
ROC_MACD = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])

TechnicalIndex_SP <- merge.xts(TechnicalIndex_SP, MACD[,2],ROC_MACD[,2])
#NASDAQ
Fast = 13
Slow = 26
Sig = 9
#MACD:DIF與MACD的差的變動率連續四天大於Cont在隔天買進
MACD <- TTR::MACD(NASDAQ, nFast = Fast, nSlow = Slow, nSig = Sig,percent = F)
ROC_MACD <- AdjROC(MACD$macd-MACD$signal)
ROC_MACD <- xts(ROC_MACD,order.by = time(MACD)) %>% lag.xts(1)
MACD = (MACD$macd-MACD$signal) %>% lag.xts(1)

temp = merge.xts(TWII_UpDown_Daily, MACD) 
MACD = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, ROC_MACD)
ROC_MACD = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])

TechnicalIndex_NASDAQ <- merge.xts(TechnicalIndex_NASDAQ, MACD[,2],ROC_MACD[,2])

##SMA快線向上穿越持續Cont天
#DJI
Fast = 8
Slow = 40
Cont = 0

SMA_Fast <- TTR::SMA(DJI$Close, Fast)
SMA_Slow <- TTR::SMA(DJI$Close, Slow)
#快線向上穿越持續Cont天，則在隔天買進
ROC_SMA <- AdjROC(SMA_Fast - SMA_Slow)
ROC_SMA <- xts(ROC_SMA,order.by = time(SMA_Fast)) %>% lag.xts(1)
SMA <- SMA_Fast - SMA_Slow %>% lag.xts(1)

temp = merge.xts(TWII_UpDown_Daily, SMA) 
SMA = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, ROC_SMA)
ROC_SMA = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])

TechnicalIndex_DJI <- merge.xts(TechnicalIndex_DJI, SMA[,2],ROC_SMA[,2])
#SP
Fast = 8
Slow = 40
Cont = 0

SMA_Fast <- TTR::SMA(SP$Close, Fast)
SMA_Slow <- TTR::SMA(SP$Close, Slow)
#快線向上穿越持續Cont天，則在隔天買進
ROC_SMA <- AdjROC(SMA_Fast - SMA_Slow)
ROC_SMA <- xts(ROC_SMA,order.by = time(SMA_Fast)) %>% lag.xts(1)
SMA <- SMA_Fast - SMA_Slow %>% lag.xts(1)

temp = merge.xts(TWII_UpDown_Daily, SMA) 
SMA = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, ROC_SMA)
ROC_SMA = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])

TechnicalIndex_SP <- merge.xts(TechnicalIndex_SP, SMA[,2],ROC_SMA[,2])
#NASDAQ
Fast = 8
Slow = 40
Cont = 0

SMA_Fast <- TTR::SMA(NASDAQ$Close, Fast)
SMA_Slow <- TTR::SMA(NASDAQ$Close, Slow)
#快線向上穿越持續Cont天，則在隔天買進
ROC_SMA <- AdjROC(SMA_Fast - SMA_Slow)
ROC_SMA <- xts(ROC_SMA,order.by = time(SMA_Fast)) %>% lag.xts(1)
SMA <- SMA_Fast - SMA_Slow %>% lag.xts(1)

temp = merge.xts(TWII_UpDown_Daily, SMA) 
SMA = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, ROC_SMA)
ROC_SMA = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])

TechnicalIndex_NASDAQ <- merge.xts(TechnicalIndex_NASDAQ, SMA[,2],ROC_SMA[,2])

##VWMA:(上漲通常量大所以會變動較快，下跌通常量小所以變動較慢)
#DJI
Fast = 2
Slow = 25
Cont = 0

VWMA_Fast <- TTR::VWMA(DJI$Close, DJI$Volume, Fast)
VWMA_Fast[is.nan(VWMA_Fast)] = VWMA_Fast[time(VWMA_Fast[is.nan(VWMA_Fast)])-1]
VWMA_Slow <- TTR::VWMA(DJI$Close, DJI$Volume, Slow)
VWMA_Slow[is.nan(VWMA_Slow)] = VWMA_Slow[time(VWMA_Slow[is.nan(VWMA_Slow)])-1]
#快線向上穿越持續Cont天，則在隔天買進
ROC_VWMA <- AdjROC(VWMA_Fast - VWMA_Slow)
ROC_VWMA <- xts(ROC_VWMA,order.by = time(VWMA_Fast)) %>% lag.xts(1)
VWMA <- (VWMA_Fast - VWMA_Slow) %>% lag.xts(1)
names(VWMA) = 'VWMA'

temp = merge.xts(TWII_UpDown_Daily, VWMA) 
VWMA = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, ROC_VWMA)
ROC_VWMA = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])

TechnicalIndex_DJI <- merge.xts(TechnicalIndex_DJI, VWMA[,2],ROC_VWMA[,2])
#SP
Fast = 2
Slow = 25
Cont = 0

VWMA_Fast <- TTR::VWMA(SP$Close, SP$Volume, Fast)
VWMA_Fast[is.nan(VWMA_Fast)] = VWMA_Fast[time(VWMA_Fast[is.nan(VWMA_Fast)])-1]
VWMA_Slow <- TTR::VWMA(SP$Close, SP$Volume, Slow)
VWMA_Slow[is.nan(VWMA_Slow)] = VWMA_Slow[time(VWMA_Slow[is.nan(VWMA_Slow)])-1]
#快線向上穿越持續Cont天，則在隔天買進
ROC_VWMA <- AdjROC(VWMA_Fast - VWMA_Slow)
ROC_VWMA <- xts(ROC_VWMA,order.by = time(VWMA_Fast)) %>% lag.xts(1)
VWMA <- VWMA_Fast - VWMA_Slow %>% lag.xts(1)
names(VWMA) = 'VWMA'

temp = merge.xts(TWII_UpDown_Daily, VWMA) 
VWMA = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, ROC_VWMA)
ROC_VWMA = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])

TechnicalIndex_SP <- merge.xts(TechnicalIndex_SP, VWMA[,2],ROC_VWMA[,2])

#NASDAQ
Fast = 2
Slow = 25
Cont = 0

VWMA_Fast <- TTR::VWMA(NASDAQ$Close, NASDAQ$Volume, Fast)
VWMA_Fast[is.nan(VWMA_Fast)] = VWMA_Fast[time(VWMA_Fast[is.nan(VWMA_Fast)])-1]
VWMA_Slow <- TTR::VWMA(NASDAQ$Close, NASDAQ$Volume, Slow)
VWMA_Slow[is.nan(VWMA_Slow)] = VWMA_Slow[time(VWMA_Slow[is.nan(VWMA_Slow)])-1]
#快線向上穿越持續Cont天，則在隔天買進
ROC_VWMA <- AdjROC(VWMA_Fast - VWMA_Slow)
ROC_VWMA <- xts(ROC_VWMA,order.by = time(VWMA_Fast)) %>% lag.xts(1)
VWMA <- VWMA_Fast - VWMA_Slow %>% lag.xts(1)
names(VWMA) = 'VWMA'

temp = merge.xts(TWII_UpDown_Daily, VWMA) 
VWMA = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, ROC_VWMA)
ROC_VWMA = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])

TechnicalIndex_NASDAQ <- merge.xts(TechnicalIndex_NASDAQ, VWMA[,2],ROC_VWMA[,2])

##EMA
#DJI
Fast = 5
Slow = 40
Cont = 0

EMA_Fast <- TTR::EMA(DJI$Close, Fast)
EMA_Slow <- TTR::EMA(DJI$Close, Slow)
#快線向上穿越持續Cont天，則在隔天買進
ROC_EMA <- AdjROC(EMA_Fast - EMA_Slow)
ROC_EMA <- xts(ROC_EMA,order.by = time(EMA_Fast)) %>% lag.xts(1)
EMA <- EMA_Fast - EMA_Slow %>% lag.xts(1)

temp = merge.xts(TWII_UpDown_Daily, EMA) 
EMA = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, ROC_EMA)
ROC_EMA = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])

TechnicalIndex_DJI <- merge.xts(TechnicalIndex_NASDAQ, EMA[,2],ROC_EMA[,2])
#SP
Fast = 5
Slow = 40
Cont = 0

EMA_Fast <- TTR::EMA(SP$Close, Fast)
EMA_Slow <- TTR::EMA(SP$Close, Slow)
#快線向上穿越持續Cont天，則在隔天買進
ROC_EMA <- AdjROC(EMA_Fast - EMA_Slow)
ROC_EMA <- xts(ROC_EMA,order.by = time(EMA_Fast)) %>% lag.xts(1)
EMA <- EMA_Fast - EMA_Slow %>% lag.xts(1)

temp = merge.xts(TWII_UpDown_Daily, EMA) 
EMA = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, ROC_EMA)
ROC_EMA = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])

TechnicalIndex_SP <- merge.xts(TechnicalIndex_SP, EMA[,2],ROC_EMA[,2])

#NASDAQ
Fast = 5
Slow = 40
Cont = 0

EMA_Fast <- TTR::EMA(NASDAQ$Close, Fast)
EMA_Slow <- TTR::EMA(NASDAQ$Close, Slow)
#快線向上穿越持續Cont天，則在隔天買進
ROC_EMA <- AdjROC(EMA_Fast - EMA_Slow)
ROC_EMA <- xts(ROC_EMA,order.by = time(EMA_Fast)) %>% lag.xts(1)
EMA <- EMA_Fast - EMA_Slow %>% lag.xts(1)

temp = merge.xts(TWII_UpDown_Daily, EMA) 
EMA = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, ROC_EMA)
ROC_EMA = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])

TechnicalIndex_NASDAQ <- merge.xts(TechnicalIndex_NASDAQ, EMA[,2],ROC_EMA[,2])

##BBand
#DJI
N = 9
UpB = .85
DownB = .2
Lag = 3

#當移動平均低於兩配標準差(DownBand)Lag天後，在隔天做多
BBand <- TTR::BBands(cbind.xts(DJI$High,DJI$Low,DJI$Close), n = N)
ROC_BBand <- AdjROC((DJI$Close - BBand$dn)) 
ROC_BBand <- xts(ROC_BBand,order.by = time(BBand)) %>% lag.xts(1)
BBand <- (DJI$Close - BBand$dn) %>% lag.xts(1)

temp = merge.xts(TWII_UpDown_Daily, BBand) 
names(temp)[2] = 'BBand'
BBand = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, ROC_BBand)
ROC_BBand = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])

TechnicalIndex_DJI <- merge.xts(TechnicalIndex_DJI, BBand[,2],ROC_BBand[,2])

#SP
N = 9
UpB = .85
DownB = .2
Lag = 3

#當移動平均低於兩配標準差(DownBand)Lag天後，在隔天做多
BBand <- TTR::BBands(cbind.xts(SP$High,SP$Low,SP$Close), n = N)
ROC_BBand <- AdjROC((SP$Close - BBand$dn)) 
ROC_BBand <- xts(ROC_BBand,order.by = time(BBand)) %>% lag.xts(1)
BBand <- (SP$Close - BBand$dn) %>% lag.xts(1)

temp = merge.xts(TWII_UpDown_Daily, BBand) 
names(temp)[2] = 'BBand'
BBand = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, ROC_BBand)
ROC_BBand = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])

TechnicalIndex_SP <- merge.xts(TechnicalIndex_SP, BBand[,2],ROC_BBand[,2])

#NASDAQ
N = 9
UpB = .85
DownB = .2
Lag = 3

#當移動平均低於兩配標準差(DownBand)Lag天後，在隔天做多
BBand <- TTR::BBands(cbind.xts(NASDAQ$High,NASDAQ$Low,NASDAQ$Close), n = N)
ROC_BBand <- AdjROC((NASDAQ$Close - BBand$dn))
ROC_BBand <- xts(ROC_BBand,order.by = time(BBand)) %>% lag.xts(1)
BBand <- (NASDAQ$Close - BBand$dn) %>% lag.xts(1)

temp = merge.xts(TWII_UpDown_Daily, BBand) 
names(temp)[2] = 'BBand'
BBand = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, ROC_BBand)
ROC_BBand = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])

TechnicalIndex_NASDAQ <- merge.xts(TechnicalIndex_NASDAQ, BBand[,2],ROC_BBand[,2])

##KD
#DJI
kd = 9 #(相當於nFastK)
K = 3 #(相當於nFastD)
D = 3 #(相當於nSlowD)
Cont = 0

KD <- TTR::stoch(HLC(DJI), nFastK = kd, nFastD = K, nSlowD = D)
#K線大於D線且兩個兩個值都大於0.2則做多
KD_K <- KD$fastD
KD_D <- KD$slowD

ROC_KD <- AdjROC(KD_K - KD_D)
ROC_KD <- xts(ROC_KD,order.by = time(KD_K)) %>% lag.xts(1)
KD <- (KD_K - KD_D) %>% lag.xts(1)
names(KD) = 'KD'

temp = merge.xts(TWII_UpDown_Daily, KD) 
KD = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, ROC_KD)
ROC_KD = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])

TechnicalIndex_DJI <- merge.xts(TechnicalIndex_DJI, KD[,2],ROC_KD[,2])

#SP
kd = 9 #(相當於nFastK)
K = 3 #(相當於nFastD)
D = 3 #(相當於nSlowD)
Cont = 0

KD <- TTR::stoch(HLC(SP), nFastK = kd, nFastD = K, nSlowD = D)
#K線大於D線且兩個兩個值都大於0.2則做多
KD_K <- KD$fastD
KD_D <- KD$slowD

ROC_KD <- AdjROC(KD_K - KD_D)
ROC_KD <- xts(ROC_KD,order.by = time(KD_K)) %>% lag.xts(1)
KD <- (KD_K - KD_D) %>% lag.xts(1)
names(KD) = 'KD'

temp = merge.xts(TWII_UpDown_Daily, KD) 
KD = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, ROC_KD)
ROC_KD = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])

TechnicalIndex_SP <- merge.xts(TechnicalIndex_SP, KD[,2],ROC_KD[,2])
#NASDAQ
kd = 9 #(相當於nFastK)
K = 3 #(相當於nFastD)
D = 3 #(相當於nSlowD)
Cont = 0

KD <- TTR::stoch(HLC(NASDAQ), nFastK = kd, nFastD = K, nSlowD = D)
#K線大於D線且兩個兩個值都大於0.2則做多
KD_K <- KD$fastD
KD_D <- KD$slowD

ROC_KD <- AdjROC(KD_K - KD_D)
ROC_KD <- xts(ROC_KD,order.by = time(KD_K)) %>% lag.xts(1)
KD <- (KD_K - KD_D) %>% lag.xts(1)
names(KD) = 'KD'

temp = merge.xts(TWII_UpDown_Daily, KD) 
KD = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, ROC_KD)
ROC_KD = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])

TechnicalIndex_NASDAQ <- merge.xts(TechnicalIndex_NASDAQ, KD[,2],ROC_KD[,2])

##RSI
#DJI
Fast = 5
Slow = 14
# UpRsi = 80
# DownRsi = 50
Cont = 0

#N天的RSI小於DownRsi時，在Lag天後做多
RSI_Fast <- TTR::RSI(DJI$Close,n = Fast)
RSI_Slow <- TTR::RSI(DJI$Close,n = Slow)

ROC_RSI <- AdjROC(RSI_Fast - RSI_Slow)
ROC_RSI <- xts(ROC_RSI,order.by = time(RSI_Fast)) %>% lag.xts(1)
RSI <- (RSI_Fast - RSI_Slow) %>% lag.xts(1)
names(RSI) = 'RSI'

temp = merge.xts(TWII_UpDown_Daily, RSI) 
RSI = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, ROC_RSI)
ROC_RSI = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])

TechnicalIndex_DJI <- merge.xts(TechnicalIndex_DJI, RSI[,2],ROC_RSI[,2])

#SP
Fast = 5
Slow = 14
# UpRsi = 80
# DownRsi = 50
Cont = 0

#N天的RSI小於DownRsi時，在Lag天後做多
RSI_Fast <- TTR::RSI(SP$Close,n = Fast)
RSI_Slow <- TTR::RSI(SP$Close,n = Slow)

ROC_RSI <- AdjROC(RSI_Fast - RSI_Slow)
ROC_RSI <- xts(ROC_RSI,order.by = time(RSI_Fast)) %>% lag.xts(1)
RSI <- (RSI_Fast - RSI_Slow) %>% lag.xts(1)
names(RSI) = 'RSI'

temp = merge.xts(TWII_UpDown_Daily, RSI) 
RSI = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, ROC_RSI)
ROC_RSI = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])

TechnicalIndex_SP <- merge.xts(TechnicalIndex_SP, RSI[,2],ROC_RSI[,2])

#NASDAQ
Fast = 5
Slow = 14
# UpRsi = 80
# DownRsi = 50
Cont = 0

#N天的RSI小於DownRsi時，在Lag天後做多
RSI_Fast <- TTR::RSI(NASDAQ$Close,n = Fast)
RSI_Slow <- TTR::RSI(NASDAQ$Close,n = Slow)

ROC_RSI <- AdjROC(RSI_Fast - RSI_Slow)
ROC_RSI <- xts(ROC_RSI,order.by = time(RSI_Fast)) %>% lag.xts(1)
RSI <- (RSI_Fast - RSI_Slow) %>% lag.xts(1)
names(RSI) = 'RSI'

temp = merge.xts(TWII_UpDown_Daily, RSI) 
RSI = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, ROC_RSI)
ROC_RSI = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])

TechnicalIndex_NASDAQ <- merge.xts(TechnicalIndex_NASDAQ, RSI[,2],ROC_RSI[,2])

##ADX
#DJI
N = 14
Cont = 0

#當ADX大於Adx，且DI+向上穿越DI-持續Cont天時在隔天買進
ADX <- TTR::ADX(HLC = cbind.xts(DJI$High,DJI$Low,DJI$Close) ,n = N)

Diff_ADX1 = (ADX$DIp - ADX$ADX) %>% lag.xts(1)
names(Diff_ADX1) = "Diff_ADX1"
Diff_ADX2 = (ADX$DIn - ADX$ADX) %>% lag.xts(1)
names(Diff_ADX2) = "Diff_ADX2"
Diff_ADX3 = (ADX$DIp - ADX$DIn) %>% lag.xts(1)
names(Diff_ADX3) = "Diff_ADX3"

temp = merge.xts(TWII_UpDown_Daily, Diff_ADX1) 
Diff_ADX1 = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, Diff_ADX2)
Diff_ADX2 = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, Diff_ADX3)
Diff_ADX3 = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])

TechnicalIndex_DJI <- merge.xts(TechnicalIndex_DJI, Diff_ADX1[,2],Diff_ADX2[,2],Diff_ADX3[,2])

#SP
N = 14
Cont = 0

#當ADX大於Adx，且DI+向上穿越DI-持續Cont天時在隔天買進
ADX <- TTR::ADX(HLC = cbind.xts(SP$High,SP$Low,SP$Close) ,n = N)

Diff_ADX1 = (ADX$DIp - ADX$ADX) %>% lag.xts(1)
names(Diff_ADX1) = "Diff_ADX1"
Diff_ADX2 = (ADX$DIn - ADX$ADX) %>% lag.xts(1)
names(Diff_ADX2) = "Diff_ADX2"
Diff_ADX3 = (ADX$DIp - ADX$DIn) %>% lag.xts(1)
names(Diff_ADX3) = "Diff_ADX3"

temp = merge.xts(TWII_UpDown_Daily, Diff_ADX1) 
Diff_ADX1 = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, Diff_ADX2)
Diff_ADX2 = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, Diff_ADX3)
Diff_ADX3 = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])

TechnicalIndex_SP <- merge.xts(TechnicalIndex_SP, Diff_ADX1[,2],Diff_ADX2[,2],Diff_ADX3[,2])

#NASDAQ
N = 14
Cont = 0

#當ADX大於Adx，且DI+向上穿越DI-持續Cont天時在隔天買進
ADX <- TTR::ADX(HLC = cbind.xts(NASDAQ$High,NASDAQ$Low,NASDAQ$Close) ,n = N)

Diff_ADX1 = (ADX$DIp - ADX$ADX) %>% lag.xts(1)
names(Diff_ADX1) = "Diff_ADX1"
Diff_ADX2 = (ADX$DIn - ADX$ADX) %>% lag.xts(1)
names(Diff_ADX2) = "Diff_ADX2"
Diff_ADX3 = (ADX$DIp - ADX$DIn) %>% lag.xts(1)
names(Diff_ADX3) = "Diff_ADX3"

temp = merge.xts(TWII_UpDown_Daily, Diff_ADX1) 
Diff_ADX1 = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, Diff_ADX2)
Diff_ADX2 = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, Diff_ADX3)
Diff_ADX3 = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])

TechnicalIndex_NASDAQ <- merge.xts(TechnicalIndex_NASDAQ, Diff_ADX1[,2],Diff_ADX2[,2],Diff_ADX3[,2])

##CCI
#DJI
UpCCI = 100
# DownCCI = -100
N = 11
Cont = 0

#CCI大於Up的天數為Cont天，在隔天買進
CCI <- TTR::CCI(HLC = cbind.xts(DJI$High,DJI$Low,DJI$Close),n =  N)
ROC_CCI <- AdjROC(CCI$cci - UpCCI)
ROC_CCI <- xts(ROC_CCI,order.by = time(CCI)) %>% lag.xts(1)
CCI <- (CCI$cci - UpCCI) %>% lag.xts(1)

temp = merge.xts(TWII_UpDown_Daily, CCI) 
CCI = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, ROC_CCI)
ROC_CCI = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])

TechnicalIndex_DJI <- merge.xts(TechnicalIndex_DJI, CCI[,2],ROC_CCI[,2])

#SP
UpCCI = 100
# DownCCI = -100
N = 11
Cont = 0

#CCI大於Up的天數為Cont天，在隔天買進
CCI <- TTR::CCI(HLC = cbind.xts(SP$High,SP$Low,SP$Close),n =  N)
ROC_CCI <- AdjROC(CCI$cci - UpCCI)
ROC_CCI <- xts(ROC_CCI,order.by = time(CCI)) %>% lag.xts(1)
CCI <- (CCI$cci - UpCCI) %>% lag.xts(1)

temp = merge.xts(TWII_UpDown_Daily, CCI) 
CCI = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, ROC_CCI)
ROC_CCI = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])

TechnicalIndex_SP <- merge.xts(TechnicalIndex_SP, CCI[,2],ROC_CCI[,2])

#NASDAQ
UpCCI = 100
# DownCCI = -100
N = 11
Cont = 0

#CCI大於Up的天數為Cont天，在隔天買進
CCI <- TTR::CCI(HLC = cbind.xts(NASDAQ$High,NASDAQ$Low,NASDAQ$Close),n =  N)
ROC_CCI <- AdjROC(CCI$cci - UpCCI)
ROC_CCI <- xts(ROC_CCI,order.by = time(CCI)) %>% lag.xts(1)
CCI <- (CCI$cci - UpCCI) %>% lag.xts(1)

temp = merge.xts(TWII_UpDown_Daily, CCI) 
CCI = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, ROC_CCI)
ROC_CCI = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])

TechnicalIndex_NASDAQ <- merge.xts(TechnicalIndex_NASDAQ, CCI[,2],ROC_CCI[,2])

##aroon
#DJI
N = 3
aroon = 70
Cont = 0

Aroon <- TTR::aroon(cbind.xts(DJI$High,DJI$Low),N)
Diff_Aroon1 = (Aroon$aroonUp - Aroon$aroonDn) %>% lag.xts(1)
names(Diff_Aroon1) = "Diff_Aroon1"
Diff_Aroon2 = (Aroon$aroonUp - aroon) %>% lag.xts(1)
names(Diff_Aroon2) = "Diff_Aroon2"

temp = merge.xts(TWII_UpDown_Daily, Diff_Aroon1) 
Diff_Aroon1 = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, Diff_Aroon2)
Diff_Aroon2 = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])

TechnicalIndex_DJI <- merge.xts(TechnicalIndex_DJI, Diff_Aroon1[,2],Diff_Aroon2[,2])

#SP
N = 3
aroon = 70
Cont = 0

Aroon <- TTR::aroon(cbind.xts(SP$High,SP$Low),N)
Diff_Aroon1 = (Aroon$aroonUp - Aroon$aroonDn) %>% lag.xts(1)
names(Diff_Aroon1) = "Diff_Aroon1"
Diff_Aroon2 = (Aroon$aroonUp - aroon) %>% lag.xts(1)
names(Diff_Aroon2) = "Diff_Aroon2"

temp = merge.xts(TWII_UpDown_Daily, Diff_Aroon1) 
Diff_Aroon1 = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, Diff_Aroon2)
Diff_Aroon2 = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])

TechnicalIndex_SP <- merge.xts(TechnicalIndex_SP, Diff_Aroon1[,2],Diff_Aroon2[,2])

#NASDAQ
N = 3
aroon = 70
Cont = 0

Aroon <- TTR::aroon(cbind.xts(NASDAQ$High,NASDAQ$Low),N)
Diff_Aroon1 = (Aroon$aroonUp - Aroon$aroonDn) %>% lag.xts(1)
names(Diff_Aroon1) = "Diff_Aroon1"
Diff_Aroon2 = (Aroon$aroonUp - aroon) %>% lag.xts(1)
names(Diff_Aroon2) = "Diff_Aroon2"

temp = merge.xts(TWII_UpDown_Daily, Diff_Aroon1) 
Diff_Aroon1 = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, Diff_Aroon2)
Diff_Aroon2 = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])

TechnicalIndex_NASDAQ <- merge.xts(TechnicalIndex_NASDAQ, Diff_Aroon1[,2],Diff_Aroon2[,2])


##EMV價量合成指標
#DJI
N = 15
t = 0
Cont = 1

EMV <- TTR::EMV(cbind.xts(DJI$High,DJI$Low),DJI$Volume,N)
ROC_EMV <- AdjROC((EMV$emv - t)) 
ROC_EMV<- xts(ROC_EMV,order.by = time(EMV)) %>% lag.xts(1)
EMV <- (EMV$emv - t) %>% lag.xts(1)

temp = merge.xts(TWII_UpDown_Daily, EMV) 
EMV = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, ROC_EMV)
ROC_EMV = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])

TechnicalIndex_DJI <- merge.xts(TechnicalIndex_DJI, EMV[,2],ROC_EMV[,2])

#SP
N = 15
t = 0
Cont = 1

EMV <- TTR::EMV(cbind.xts(SP$High,SP$Low),SP$Volume,N)
ROC_EMV <- AdjROC((EMV$emv - t)) 
ROC_EMV<- xts(ROC_EMV,order.by = time(EMV)) %>% lag.xts(1)
EMV <- (EMV$emv - t) %>% lag.xts(1)

temp = merge.xts(TWII_UpDown_Daily, EMV) 
EMV = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, ROC_EMV)
ROC_EMV = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])

TechnicalIndex_SP <- merge.xts(TechnicalIndex_SP, EMV[,2],ROC_EMV[,2])

#NASDAQ
N = 15
t = 0
Cont = 1

EMV <- TTR::EMV(cbind.xts(NASDAQ$High,NASDAQ$Low),NASDAQ$Volume,N)
ROC_EMV <- AdjROC((EMV$emv - t)) 
ROC_EMV<- xts(ROC_EMV,order.by = time(EMV)) %>% lag.xts(1)
EMV <- (EMV$emv - t) %>% lag.xts(1)

temp = merge.xts(TWII_UpDown_Daily, EMV) 
EMV = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, ROC_EMV)
ROC_EMV = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])

TechnicalIndex_NASDAQ <- merge.xts(TechnicalIndex_NASDAQ, EMV[,2],ROC_EMV[,2])

##ChaikinVolatility
#DJI
N = 3
T_Down = -.2
Cont = 0

ChaikinVolatility <- TTR::chaikinVolatility(cbind.xts(DJI$High,DJI$Low),N)
ROC_CV <- AdjROC((T_Down - ChaikinVolatility )) 
ROC_CV<- xts(ROC_CV,order.by = time(ChaikinVolatility)) %>% lag.xts(1)
CV <- (T_Down - ChaikinVolatility) %>% lag.xts(1)
names(CV) = 'CV'

temp = merge.xts(TWII_UpDown_Daily, CV) 
CV = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, ROC_CV)
ROC_CV = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])

TechnicalIndex_DJI <- merge.xts(TechnicalIndex_DJI, CV[,2],ROC_CV[,2])

#SP
N = 3
T_Down = -.2
Cont = 0

ChaikinVolatility <- TTR::chaikinVolatility(cbind.xts(SP$High,SP$Low),N)
ROC_CV <- AdjROC((T_Down - ChaikinVolatility )) 
ROC_CV<- xts(ROC_CV,order.by = time(ChaikinVolatility)) %>% lag.xts(1)
CV <- (T_Down - ChaikinVolatility) %>% lag.xts(1)
names(CV) = 'CV'

temp = merge.xts(TWII_UpDown_Daily, CV) 
CV = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, ROC_CV)
ROC_CV = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])

TechnicalIndex_SP <- merge.xts(TechnicalIndex_SP, CV[,2],ROC_CV[,2])

#NASDAQ
N = 3
T_Down = -.2
Cont = 0

ChaikinVolatility <- TTR::chaikinVolatility(cbind.xts(NASDAQ$High,NASDAQ$Low),N)
ROC_CV <- AdjROC((T_Down - ChaikinVolatility )) 
ROC_CV<- xts(ROC_CV,order.by = time(ChaikinVolatility)) %>% lag.xts(1)
CV <- (T_Down - ChaikinVolatility) %>% lag.xts(1)
names(CV) = 'CV'

temp = merge.xts(TWII_UpDown_Daily, CV) 
CV = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, ROC_CV)
ROC_CV = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])

TechnicalIndex_NASDAQ <- merge.xts(TechnicalIndex_NASDAQ, CV[,2],ROC_CV[,2])


##MFI
#DJI
N = 10
T_Down = 50
Cont = 0

MFI <- MFI(cbind.xts(DJI$High,DJI$Low,DJI$Close),DJI$Volume,N)
ROC_MFI <- AdjROC((MFI - T_Down)) 
ROC_MFI<- xts(ROC_MFI,order.by = time(MFI)) %>% lag.xts(1)
MFI <- (MFI - T_Down) %>% lag.xts(1)

temp = merge.xts(TWII_UpDown_Daily, MFI) 
MFI = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, ROC_MFI)
ROC_MFI = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])

TechnicalIndex_DJI <- merge.xts(TechnicalIndex_DJI, MFI[,2],ROC_MFI[,2])

#SP
N = 10
T_Down = 50
Cont = 0

MFI <- MFI(cbind.xts(SP$High,SP$Low,SP$Close),SP$Volume,N)
ROC_MFI <- AdjROC((MFI - T_Down)) 
ROC_MFI<- xts(ROC_MFI,order.by = time(MFI)) %>% lag.xts(1)
MFI <- (MFI - T_Down) %>% lag.xts(1)

temp = merge.xts(TWII_UpDown_Daily, MFI) 
MFI = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, ROC_MFI)
ROC_MFI = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])

TechnicalIndex_SP <- merge.xts(TechnicalIndex_SP, MFI[,2],ROC_MFI[,2])

#NASDAQ
N = 10
T_Down = 50
Cont = 0

MFI <- MFI(cbind.xts(NASDAQ$High,NASDAQ$Low,NASDAQ$Close),NASDAQ$Volume,N)
ROC_MFI <- AdjROC((MFI - T_Down)) 
ROC_MFI<- xts(ROC_MFI,order.by = time(MFI)) %>% lag.xts(1)
MFI <- (MFI - T_Down) %>% lag.xts(1)

temp = merge.xts(TWII_UpDown_Daily, MFI) 
MFI = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])
temp = merge.xts(TWII_UpDown_Daily, ROC_MFI)
ROC_MFI = xts(temp %>% data.frame() %>% filter(!is.na(Close)), time(temp)[which(!is.na(temp$Close))])

TechnicalIndex_NASDAQ <- merge.xts(TechnicalIndex_NASDAQ, MFI[,2],ROC_MFI[,2])


####----####
##平均DJI股價格
AvgP_DJI_5 <- EMA(DJI$Close,5) %>% lag.xts(1) ; names(AvgP_DJI_5) = 'AvgP_DJI_5'
AvgP_DJI_20 <- EMA(DJI$Close,20) %>% lag.xts(1) ; names(AvgP_DJI_20) = 'AvgP_DJI_20'
AvgP_DJI_60 <- EMA(DJI$Close,60) %>% lag.xts(1) ; names(AvgP_DJI_60) = 'AvgP_DJI_60'
##平均DJI股交易量
AvgV_DJI_5 <- EMA(DJI$Volume,5) %>% lag.xts(1) ; names(AvgV_DJI_5) = 'AvgV_DJI_5'
AvgV_DJI_20 <- EMA(DJI$Volume,20) %>% lag.xts(1) ; names(AvgV_DJI_20) = 'AvgV_DJI_20'
AvgV_DJI_60 <- EMA(DJI$Volume,60) %>% lag.xts(1) ; names(AvgV_DJI_60) = 'AvgV_DJI_60'

DJI_Lag = DJI %>% lag.xts(1)
TechnicalIndex_DJI = merge.xts(TechnicalIndex_DJI,AvgP_DJI_5,AvgP_DJI_20,AvgP_DJI_60,
                                   AvgV_DJI_5,AvgV_DJI_20,AvgV_DJI_60,DJI_Lag)

##平均SP股價格
AvgP_SP_5 <- EMA(SP$Close,5) %>% lag.xts(1) ; names(AvgP_SP_5) = 'AvgP_SP_5'
AvgP_SP_20 <- EMA(SP$Close,20) %>% lag.xts(1) ; names(AvgP_SP_20) = 'AvgP_SP_20'
AvgP_SP_60 <- EMA(SP$Close,60) %>% lag.xts(1) ; names(AvgP_SP_60) = 'AvgP_SP_60'
##平均SP股交易量
AvgV_SP_5 <- EMA(SP$Volume,5) %>% lag.xts(1) ; names(AvgV_SP_5) = 'AvgV_SP_5'
AvgV_SP_20 <- EMA(SP$Volume,20) %>% lag.xts(1) ; names(AvgV_SP_20) = 'AvgV_SP_20'
AvgV_SP_60 <- EMA(SP$Volume,60) %>% lag.xts(1) ; names(AvgV_SP_60) = 'AvgV_SP_60'

SP_Lag = SP %>% lag.xts(1)
TechnicalIndex_SP = merge.xts(TechnicalIndex_SP,AvgP_SP_5,AvgP_SP_20,AvgP_SP_60,
                               AvgV_SP_5,AvgV_SP_20,AvgV_SP_60,SP_Lag)

##平均NASDAQ股價格
AvgP_NASDAQ_5 <- EMA(NASDAQ$Close,5) %>% lag.xts(1) ; names(AvgP_NASDAQ_5) = 'AvgP_NASDAQ_5'
AvgP_NASDAQ_20 <- EMA(NASDAQ$Close,20) %>% lag.xts(1) ; names(AvgP_NASDAQ_20) = 'AvgP_NASDAQ_20'
AvgP_NASDAQ_60 <- EMA(NASDAQ$Close,60) %>% lag.xts(1) ; names(AvgP_NASDAQ_60) = 'AvgP_NASDAQ_60'
##平均NASDAQ股交易量
AvgV_NASDAQ_5 <- EMA(NASDAQ$Volume,5) %>% lag.xts(1) ; names(AvgV_NASDAQ_5) = 'AvgV_NASDAQ_5'
AvgV_NASDAQ_20 <- EMA(NASDAQ$Volume,20) %>% lag.xts(1) ; names(AvgV_NASDAQ_20) = 'AvgV_NASDAQ_20'
AvgV_NASDAQ_60 <- EMA(NASDAQ$Volume,60) %>% lag.xts(1) ; names(AvgV_NASDAQ_60) = 'AvgV_NASDAQ_60'

NASDAQ_Lag = NASDAQ %>% lag.xts(1)
TechnicalIndex_NASDAQ = merge.xts(TechnicalIndex_NASDAQ,AvgP_NASDAQ_5,AvgP_NASDAQ_20,AvgP_NASDAQ_60,
                              AvgV_NASDAQ_5,AvgV_NASDAQ_20,AvgV_NASDAQ_60,NASDAQ_Lag)

####----####


