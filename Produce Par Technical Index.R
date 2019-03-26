###
library(lubridate)
TimeGap = 2
AllYear = year(end(TWII)) - year(start(TWII))
S = as.Date('2000-01-01')

TechnicalIndex <- data.frame(TWII_UpDown_Daily = (data.frame(TWII_UpDown_Daily))$Close) %>% xts(order.by = time(TWII_UpDown_Daily))
TechnicalIndex_Numeric <- data.frame(TWII_UpDown_Daily = (data.frame(TWII_UpDown_Daily))$Close) %>% xts(order.by = time(TWII_UpDown_Daily))

##OBV
Parameter_OBV_t2 = AdjPar(Parameter_OBV_t2)
for(i in 6:(length(Parameter_OBV_t2)+4)){
        
        TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)-months(20)),'::',as.character(S+years(i)-days(1)))]
        
        Cont = Parameter_OBV_t2[[i-4]]
        
        #OBV連續Cont天變動率大於零在隔天買進
        OBV <- TTR::OBV(price = TWII_Period$Close, volume = TWII_Period$Volume)
        ROC_OBV <- AdjROC(OBV) %>% xts(order.by = time(OBV)) %>% lag.xts(1)
        Diff_OBV <- diff.xts(OBV) %>% lag.xts(1)
        
        ROC_OBV <- ROC_OBV[as.character(year(S)+i-1)]
        OBV <- OBV[as.character(year(S)+i-1)]
        Diff_OBV = Diff_OBV[as.character(year(S)+i-1)]
        TWII_Period = TWII_Period[as.character(year(S)+i-1)]
        
        ROC_OBV_Signal <- ( ROC_OBV > 0) 
        ROC_OBV_Signal[is.na(ROC_OBV_Signal)] <-  F
        ROC_OBV_Signal_Cont <- ContinuingPeriod(ROC_OBV_Signal, Cont)        
        ROC_OBV_Signal_Cont <- xts(ROC_OBV_Signal_Cont,order.by = time(OBV))
        
        if(i == 6){
                OBV_Logical = ROC_OBV_Signal_Cont
                OBV_DT = merge.xts(ROC_OBV = ROC_OBV,Diff_OBV = Diff_OBV)
        }else{
                OBV_Logical = rbind.xts(OBV_Logical,ROC_OBV_Signal_Cont)
                OBV_DT = rbind.xts(OBV_DT,merge.xts(ROC_OBV=ROC_OBV,Diff_OBV=Diff_OBV))
        }
}

names(OBV_Logical) = "OBV"
names(OBV_DT) = c("ROC_OBV","Diff_OBV")
        
TechnicalIndex_Logical = merge.xts(TechnicalIndex, OBV_Logical)
TechnicalIndex_Numeric = merge.xts(TechnicalIndex_Numeric, OBV_DT)

ROC(TWII$Close)[time(OBV_Logical[OBV_Logical == T])] %>% sum()
(ROC(TWII$Close)[time(OBV_Logical[OBV_Logical == T])] > 0) %>% table()



##MACD
Parameter_MACD_t2 = AdjPar(Parameter_MACD_t2)
for(i in 6:(length(Parameter_MACD_t2)+4)){
        
        TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)-months(20)),'::',as.character(S+years(i)-days(1)))]
        
        Fast = Parameter_MACD_t2[[i-5]][1]
        Slow = Parameter_MACD_t2[[i-5]][2]
        Cont = 0
        Sig = Parameter_MACD_t2[[i-5]][3]
        
        MACD <- TTR::MACD(TWII_Period, nFast = Fast, nSlow = Slow, nSig = Sig, percent = F)
        Diff_MACD <- (MACD$macd-MACD$signal) %>% lag.xts(1)
        ROC_MACD <- AdjROC(MACD$macd-MACD$signal)
        ROC_MACD <- xts(ROC_MACD,order.by = time(MACD)) %>% lag.xts(1)
        
        ROC_MACD <- ROC_MACD[as.character(year(S)+i-1)]
        MACD <- MACD[as.character(year(S)+i-1)]
        Diff_MACD = Diff_MACD[as.character(year(S)+i-1)]
        TWII_Period = TWII_Period[as.character(year(S)+i-1)]
        
        Cond1_MACD <- (MACD$macd > 0) & (MACD$signal > 0)
        ROC_MACD_Signal <- (ROC_MACD > 0) & (Cond1_MACD == T) #& (TWII_Period$Close  > AvgP_TWII_20 ) & (TWII_Period$Volume > AvgV_TWII_20)
        ROC_MACD_Signal[is.na(ROC_MACD_Signal)] <- F
        ROC_MACD_Signal_Cont <- ContinuingPeriod(ROC_MACD_Signal,Cont)
        ROC_MACD_Signal_Cont <- xts(ROC_MACD_Signal_Cont,order.by = time(MACD))
        
        if(i == 6){
                MACD_Logical = ROC_MACD_Signal_Cont
                MACD_DT = merge.xts(ROC_MACD,Diff_MACD)
        }else{
                MACD_Logical = rbind.xts(MACD_Logical,ROC_MACD_Signal_Cont)
                MACD_DT = rbind.xts(MACD_DT,merge.xts(ROC_MACD=ROC_MACD,Diff_MACD=Diff_MACD))
        }
}

names(MACD_Logical) = "MACD"
names(MACD_DT) = c("ROC_MACD","Diff_MACD")

TechnicalIndex_Logical = merge.xts(TechnicalIndex_Logical, MACD = MACD_Logical)
TechnicalIndex_Numeric = merge.xts(TechnicalIndex_Numeric, MACD = MACD_DT)

ROC(TWII$Close)[time(MACD_Logical[MACD_Logical == T])] %>% sum()
(ROC(TWII$Close)[time(MACD_Logical[MACD_Logical == T])] > 0) %>% table()

##SMA
Parameter_SMA_t2 = AdjPar(Parameter_SMA_t2)
for(i in 6:(length(Parameter_SMA_t2)+4)){
        TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)-months(20)),'::',as.character(S+years(i)-days(1)))]        
        
        Fast = Parameter_SMA_t2[[i-5]][1]
        Slow = Parameter_SMA_t2[[i-5]][2]
        Cont = Parameter_SMA_t2[[i-5]][3]
        
        SMA_Fast <- TTR::SMA(TWII_Period$Close, Fast)
        SMA_Slow <- TTR::SMA(TWII_Period$Close, Slow)
        ROC_SMA <- AdjROC(SMA_Fast-SMA_Slow) %>% xts(order.by = time(SMA_Fast)) %>% lag.xts(1)
        Diff_SMA <- (SMA_Fast - SMA_Slow) %>% lag.xts(1)
        
        SMA_Slow <- SMA_Slow[as.character(year(S)+i-1)]
        SMA_Fast <- SMA_Fast[as.character(year(S)+i-1)]
        ROC_SMA <- ROC_SMA[as.character(year(S)+i-1)]
        Diff_SMA <- Diff_SMA[as.character(year(S)+i-1)]
        TWII_Period = TWII_Period[as.character(year(S)+i-1)]
        #快線向上穿越持續Cont天，則在隔天買進
        SMA_UPThru <- SMA_Fast - SMA_Slow
        SMA_UPThru[SMA_UPThru <= 0] <- 0
        ROC_SMA_UPThru <- AdjROC(SMA_UPThru)
        ROC_SMA_UPThru_Signal <- ROC_SMA_UPThru > 0
        Cond1_SMA <- (SMA_Fast > 0) & (SMA_Slow > 0)
        ROC_SMA_UPThru_Signal <- (ROC_SMA_UPThru_Signal > 0) & (Cond1_SMA == T) # & (TWII$Close  > AvgP_TWII_60 ) & (TWII$Volume > AvgV_TWII_60)
        ROC_SMA_UPThru_Signal[is.na(ROC_SMA_UPThru_Signal)] <-  F
        ROC_SMA_UPThru_Signal_Cont <- ContinuingPeriod(ROC_SMA_UPThru_Signal, Cont)
        ROC_SMA_UPThru_Signal_Cont <- xts(ROC_SMA_UPThru_Signal_Cont,order.by = time(TWII_Period))
        
        if(i == 6){
                SMA_Logical = ROC_SMA_UPThru_Signal_Cont
                SMA_DT = merge.xts(ROC_SMA,Diff_SMA)
        }else{
                SMA_Logical = rbind.xts(SMA_Logical,ROC_SMA_UPThru_Signal_Cont)
                SMA_DT = rbind.xts(SMA_DT,merge.xts(ROC_SMA, Diff_SMA))
        }
        
        
}

names(SMA_Logical) = "SMA"
names(SMA_DT) = c("ROC_SMA","Diff_SMA")

TechnicalIndex_Logical = merge.xts(TechnicalIndex_Logical, SMA_Logical)
TechnicalIndex_Numeric = merge.xts(TechnicalIndex_Numeric, SMA_DT)

ROC(TWII$Close)[time(SMA_Logical[SMA_Logical == T])] %>% sum()
(ROC(TWII$Close)[time(SMA_Logical[SMA_Logical == T])] > 0) %>% table()

##VWMA
Parameter_VWMA_t2 = AdjPar(Parameter_VWMA_t2)
for(i in 6:(length(Parameter_VWMA_t2)+4)){
        TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)-months(20)),'::',as.character(S+years(i)-days(1)))]        
        
        Fast = Parameter_VWMA_t2[[i-5]][1]
        Slow = Parameter_VWMA_t2[[i-5]][2]
        Cont = Parameter_VWMA_t2[[i-5]][3]
        
        VWMA_Fast <- TTR::VWMA(TWII_Period$Close, TWII_Period$Volume, Fast)
        VWMA_Slow <- TTR::VWMA(TWII_Period$Close, TWII_Period$Volume, Slow)
        ROC_VWMA <- AdjROC(VWMA_Fast-VWMA_Slow) 
        ROC_VWMA <- xts(ROC_VWMA,order.by = time(VWMA_Fast)) %>% lag.xts(1)
        Diff_VWMA <- (VWMA_Fast - VWMA_Slow) %>% lag.xts(1)
        
        VWMA_Slow <- VWMA_Slow[as.character(year(S)+i-1)]
        VWMA_Fast <- VWMA_Fast[as.character(year(S)+i-1)]
        ROC_VWMA <- ROC_VWMA[as.character(year(S)+i-1)]
        Diff_VWMA <- Diff_VWMA[as.character(year(S)+i-1)]
        TWII_Period = TWII_Period[as.character(year(S)+i-1)]        
        
        VWMA_UPThru <- VWMA_Fast - VWMA_Slow
        VWMA_UPThru[VWMA_UPThru <= 0] <- 0
        ROC_VWMA_UPThru <- AdjROC(VWMA_UPThru)
        ROC_VWMA_UPThru_Signal <- ROC_VWMA_UPThru > 0
        Cond1_VWMA <- (VWMA_Fast > 0) & (VWMA_Slow > 0)
        ROC_VWMA_UPThru_Signal <- (ROC_VWMA_UPThru_Signal > 0) & (Cond1_VWMA == T) # & (TWII$Close  > AvgP_TWII_20 ) & (TWII$Volume > AvgV_TWII_5)
        ROC_VWMA_UPThru_Signal[is.na(ROC_VWMA_UPThru_Signal)] <-  F
        ROC_VWMA_UPThru_Signal_Cont <- ContinuingPeriod(ROC_VWMA_UPThru_Signal, Cont)
        ROC_VWMA_UPThru_Signal_Cont <- xts(ROC_VWMA_UPThru_Signal_Cont,order.by = time(TWII_Period))
        
        if(i == 6){
                VWMA_Logical = ROC_VWMA_UPThru_Signal_Cont
                VWMA_DT = merge.xts(ROC_VWMA,Diff_VWMA)
        }else{
                VWMA_Logical = rbind.xts(VWMA_Logical,ROC_VWMA_UPThru_Signal_Cont)
                VWMA_DT = rbind.xts(VWMA_DT,merge.xts(ROC_VWMA, Diff_VWMA))
        }  
}

names(VWMA_Logical) = "VWMA"
names(VWMA_DT) = c("ROC_VWMA","Diff_VWMA")

TechnicalIndex_Logical = merge.xts(TechnicalIndex_Logical, VWMA_Logical)
TechnicalIndex_Numeric = merge.xts(TechnicalIndex_Numeric, VWMA_DT)

ROC(TWII$Close)[time(VWMA_Logical[VWMA_Logical == T])] %>% sum()
(ROC(TWII$Close)[time(VWMA_Logical[VWMA_Logical == T])] > 0) %>% table()


##EMA
Parameter_EMA_t2 = AdjPar(Parameter_EMA_t2)
for(i in 6:(length(Parameter_EMA_t2)+4)){
        TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)-months(20)),'::',as.character(S+years(i)-days(1)))]        
        
        Fast = Parameter_EMA_t2[[i-5]][1]
        Slow = Parameter_EMA_t2[[i-5]][2]
        Cont = Parameter_EMA_t2[[i-5]][3]
        
        EMA_Fast <- TTR::EMA(TWII_Period$Close, Fast)
        EMA_Slow <- TTR::EMA(TWII_Period$Close, Slow)
        ROC_EMA <- AdjROC(EMA_Fast-EMA_Slow) %>% xts(order.by = time(EMA_Fast)) %>% lag.xts(1)
        Diff_EMA <- (EMA_Fast - EMA_Slow) %>% lag.xts(1)
        
        EMA_Slow <- EMA_Slow[as.character(year(S)+i-1)]
        EMA_Fast <- EMA_Fast[as.character(year(S)+i-1)]
        ROC_EMA <- ROC_EMA[as.character(year(S)+i-1)]
        Diff_EMA <- Diff_EMA[as.character(year(S)+i-1)]
        TWII_Period = TWII_Period[as.character(year(S)+i-1)]        
        #快線向上穿越持續Cont天，則在隔天買進
        EMA_UPThru <- EMA_Fast - EMA_Slow
        EMA_UPThru[EMA_UPThru <= 0] <- 0
        ROC_EMA_UPThru <- AdjROC(EMA_UPThru)
        ROC_EMA_UPThru_Signal <- ROC_EMA_UPThru > 0
        ROC_EMA_UPThru_Signal[is.na(ROC_EMA_UPThru_Signal)] <-  F
        ROC_EMA_UPThru_Signal_Cont <- ContinuingPeriod(ROC_EMA_UPThru_Signal, Cont)
        ROC_EMA_UPThru_Signal_Cont <- xts(ROC_EMA_UPThru_Signal_Cont, order.by = time(TWII_Period))
        
        if(i == 6){
                EMA_Logical = ROC_EMA_UPThru_Signal_Cont
                EMA_DT = merge.xts(ROC_EMA,Diff_EMA)
        }else{
                EMA_Logical = rbind.xts(EMA_Logical,ROC_EMA_UPThru_Signal_Cont)
                EMA_DT = rbind.xts(EMA_DT,merge.xts(ROC_EMA, Diff_EMA))
        }  
}

names(EMA_Logical) = "EMA"
names(EMA_DT) = c("ROC_EMA","Diff_EMA")

TechnicalIndex_Logical = merge.xts(TechnicalIndex_Logical, EMA_Logical)
TechnicalIndex_Numeric = merge.xts(TechnicalIndex_Numeric, EMA_DT)

ROC(TWII$Close)[time(EMA_Logical[EMA_Logical == T])] %>% sum()
(ROC(TWII$Close)[time(EMA_Logical[EMA_Logical == T])] > 0) %>% table()


##BBand                
Parameter_BBand_t2 = AdjPar(Parameter_BBand_t2)
for(i in 6:(length(Parameter_BBand_t2)+4)){
        TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)-months(20)),'::',as.character(S+years(i)-days(1)))]        
        
        N = Parameter_BBand_t2[[i-5]][1]
        DownB = Parameter_BBand_t2[[i-5]][2]
        Lag = Parameter_BBand_t2[[i-5]][3]
        
        #當移動平均低於兩配標準差(DownBand)Lag天後，在隔天做多
        BBand <- TTR::BBands(cbind.xts(TWII_Period$High,TWII_Period$Low,TWII_Period$Close), n = N)
        ROC_BBand <- AdjROC((TWII_Period$Close - BBand$dn)) %>% xts(order.by = time(BBand)) %>% lag.xts(1)
        Diff_BBand <- (TWII_Period$Close - BBand$dn) %>% lag.xts(1)
        
        BBand <- BBand[as.character(year(S)+i-1)]
        ROC_BBand <- ROC_BBand[as.character(year(S)+i-1)]
        Diff_BBand <- Diff_BBand[as.character(year(S)+i-1)]
        TWII_Period = TWII_Period[as.character(year(S)+i-1)]
        
        BBand_Signal_Down <- BBand$pctB < DownB
        BBand_Signal_Down_Lag <- lag(BBand_Signal_Down,Lag)
        BBand_Signal_Down_Lag[is.na(BBand_Signal_Down_Lag)] <-  F
        BBand_Signal_Down_Lag <- (BBand_Signal_Down_Lag == T) # & (TWII$Close  > AvgP_TWII_5 ) & (TWII$Volume > AvgV_TWII_5)
        BBand_Signal_Down_Lag <- xts(BBand_Signal_Down_Lag, order.by = time(TWII_Period))
        
        if(i == 6){
                BBand_Logical = BBand_Signal_Down_Lag
                BBand_DT = merge.xts(ROC_BBand,Diff_BBand)
        }else{
                BBand_Logical = rbind.xts(BBand_Logical,BBand_Signal_Down_Lag)
                BBand_DT = rbind.xts(BBand_DT,merge.xts(ROC_BBand, Diff_BBand))
        }  
}

names(BBand_Logical) = "BBand"
names(BBand_DT) = c("ROC_BBand","Diff_BBand")

TechnicalIndex_Logical = merge.xts(TechnicalIndex_Logical, BBand_Logical)
TechnicalIndex_Numeric = merge.xts(TechnicalIndex_Numeric, BBand_DT)

ROC(TWII$Close)[time(BBand_Logical[BBand_Logical == T])] %>% sum()
(ROC(TWII$Close)[time(BBand_Logical[BBand_Logical == T])] > 0) %>% table()


##KD
Parameter_KD_t2 = AdjPar(Parameter_KD_t2)
for(i in 6:(length(Parameter_KD_t2)+4)){
        TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)-months(20)),'::',as.character(S+years(i)-days(1)))]        
        
        kd = Parameter_KD_t2[[i-5]][1]
        K = Parameter_KD_t2[[i-5]][2]
        D = Parameter_KD_t2[[i-5]][3]
        Cont = 0

        KD <- TTR::stoch(HLC(TWII_Period), nFastK = kd, nFastD = K, nSlowD = D)
        KD_K <- KD$fastD
        KD_D <- KD$slowD
        ROC_KD <- AdjROC(KD_K - KD_D) %>% xts(order.by = time(KD)) %>% lag.xts(1)
        Diff_KD <- (KD_K - KD_D) %>% lag.xts(1)
        
        KD_K <- KD_K[as.character(year(S)+i-1)]
        KD_D <- KD_D[as.character(year(S)+i-1)]
        ROC_KD <- ROC_KD[as.character(year(S)+i-1)]
        Diff_KD <- Diff_KD[as.character(year(S)+i-1)]
        TWII_Period = TWII_Period[as.character(year(S)+i-1)]        
        KD <- KD[as.character(year(S)+i-1)]
        
        #K線大於D線且兩個兩個值都大於0.2則做多
        
        KD_UPThru <- KD_K - KD_D
        KD_UPThru[KD_UPThru<=0] <- 0
        ROC_KD_UPThru <- AdjROC(KD_UPThru)
        ROC_KD_UPThru_Signal <- ROC_KD_UPThru > 0
        Cond1_KD <- ContSmallThanN(KD$fastD,N = 3,.2) & ContSmallThanN(KD$slowD,N = 3,.2)
        Cond1_KD <- xts(Cond1_KD,order.by = time(TWII_Period))
        ROC_KD_UPThru_Signal <- (ROC_KD_UPThru_Signal > 0) & (Cond1_KD == T) # & (TWII$Close  > AvgP_TWII_20 ) & (TWII$Volume > AvgV_TWII_5)
        ROC_KD_UPThru_Signal[is.na(ROC_KD_UPThru_Signal)] <-  F
        ROC_KD_UPThru_Signal_Cont <- ContinuingPeriod(ROC_KD_UPThru_Signal, Cont)
        ROC_KD_UPThru_Signal_Cont <- xts(ROC_KD_UPThru_Signal_Cont,order.by = time(TWII_Period))
        
        if(i == 6){
                KD_Logical = ROC_KD_UPThru_Signal_Cont
                KD_DT = merge.xts(ROC_KD,Diff_KD)
        }else{
                KD_Logical = rbind.xts(KD_Logical,ROC_KD_UPThru_Signal_Cont)
                KD_DT = rbind.xts(KD_DT,merge.xts(ROC_KD, Diff_KD))
        }  
}

names(KD_Logical) = "KD"
names(KD_DT) = c("ROC_KD","Diff_KD")

TechnicalIndex_Logical = merge.xts(TechnicalIndex_Logical, KD_Logical)
TechnicalIndex_Numeric = merge.xts(TechnicalIndex_Numeric, KD_DT)

ROC(TWII$Close)[time(KD_Logical[KD_Logical == T])] %>% sum()
(ROC(TWII$Close)[time(KD_Logical[KD_Logical == T])] > 0) %>% table()


####RSI
Parameter_RSI_t2 = AdjPar(Parameter_RSI_t2)
for(i in 6:(length(Parameter_RSI_t2)+4)){
        TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)-months(20)),'::',as.character(S+years(i)-days(1)))]        
        
        Fast = Parameter_RSI_t2[[i-5]][1]
        if(Fast == 1){Fast = 2}
        Slow = Parameter_RSI_t2[[i-5]][2]
        Cont = 0
        
        RSI_Fast <- TTR::RSI(TWII_Period$Close,n = Fast)
        RSI_Slow <- TTR::RSI(TWII_Period$Close,n = Slow)
        ROC_RSI <- AdjROC(RSI_Fast-RSI_Slow) %>% xts(order.by = time(RSI_Fast)) %>% lag.xts(1)
        Diff_RSI <- (RSI_Fast - RSI_Slow) %>% lag.xts(1)
        
        RSI_Slow <- RSI_Slow[as.character(year(S)+i-1)]
        RSI_Fast <- RSI_Fast[as.character(year(S)+i-1)]
        ROC_RSI <- ROC_RSI[as.character(year(S)+i-1)]
        Diff_RSI <- Diff_RSI[as.character(year(S)+i-1)]
        TWII_Period = TWII_Period[as.character(year(S)+i-1)]        
        
        RSI_Signal <- RSI_Fast - RSI_Slow
        RSI_Signal[RSI_Signal<=0] <- 0
        ROC_RSI_Signal <- AdjROC(RSI_Signal)
        ROC_RSI_Signal <- ROC_RSI_Signal > 0
        RSI_Signal_Cond <- (ROC_RSI_Signal == T) 
        RSI_Signal_Cond[is.na(RSI_Signal_Cond)] <-  F
        RSI_Signal_Cont <- ContinuingPeriod(RSI_Signal_Cond,Cont)
        RSI_Signal_Cont <- xts(RSI_Signal_Cont,order.by = time(TWII_Period))
        
        if(i == 6){
                RSI_Logical = RSI_Signal_Cont
                RSI_DT = merge.xts(ROC_RSI,Diff_RSI)
        }else{
                RSI_Logical = rbind.xts(RSI_Logical,RSI_Signal_Cont)
                RSI_DT = rbind.xts(RSI_DT,merge.xts(ROC_RSI, Diff_RSI))
        }  
}

names(RSI_Logical) = "RSI"
names(RSI_DT) = c("ROC_RSI","Diff_RSI")

TechnicalIndex_Logical = merge.xts(TechnicalIndex_Logical, RSI_Logical)
TechnicalIndex_Numeric = merge.xts(TechnicalIndex_Numeric, RSI_DT)

ROC(TWII$Close)[time(RSI_Logical[RSI_Logical == T])] %>% sum()
(ROC(TWII$Close)[time(RSI_Logical[RSI_Logical == T])] > 0) %>% table()


##ADX
Parameter_ADX_t2 = AdjPar(Parameter_ADX_t2)
for(i in 6:(length(Parameter_ADX_t2)+4)){
        TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)-months(20)),'::',as.character(S+years(i)-days(1)))]        
        
        N = Parameter_ADX_t2[[i-5]]
        Cont = 0
        
        ADX <- TTR::ADX(HLC = cbind.xts(TWII_Period$High,TWII_Period$Low,TWII_Period$Close) ,n = N)
        
        Diff_ADX1 = (ADX$DIp - ADX$ADX) %>% lag.xts(1)
        Diff_ADX2 = (ADX$DIn - ADX$ADX) %>% lag.xts(1)
        Diff_ADX3 = (ADX$DIp - ADX$DIn) %>% lag.xts(1)
        
        ADX <- ADX[as.character(year(S)+i-1)]
        Diff_ADX1 = Diff_ADX1[as.character(year(S)+i-1)]
        Diff_ADX2 = Diff_ADX2[as.character(year(S)+i-1)]
        Diff_ADX3 = Diff_ADX3[as.character(year(S)+i-1)]
        TWII_Period = TWII_Period[as.character(year(S)+i-1)]
        
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
        
        if(i == 6){
                ADX_Logical = ADX_Signal_UpTru
                ADX_DT = merge.xts(Diff_ADX1, Diff_ADX2, Diff_ADX3)
        }else{
                ADX_Logical = rbind.xts(ADX_Logical,ADX_Signal_UpTru)
                ADX_DT = rbind.xts(ADX_DT,merge.xts(Diff_ADX1, Diff_ADX2, Diff_ADX3))
        }  
}

names(ADX_Logical) = "ADX"
names(ADX_DT) = c("Diff_ADX1","Diff_ADX2","Diff_ADX3")

TechnicalIndex_Logical = merge.xts(TechnicalIndex_Logical, ADX_Logical)
TechnicalIndex_Numeric = merge.xts(TechnicalIndex_Numeric, ADX_DT)

ROC(TWII$Close)[time(ADX_Logical[ADX_Logical == T])] %>% sum()
(ROC(TWII$Close)[time(ADX_Logical[ADX_Logical == T])] > 0) %>% table()


##CCI
Parameter_CCI_t2 = AdjPar(Parameter_CCI_t2)
for(i in 6:(length(Parameter_CCI_t2)+4)){
        TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)-months(20)),'::',as.character(S+years(i)-days(1)))]        
        
        UpCCI = Parameter_CCI_t2[[i-5]][1]
        N = Parameter_CCI_t2[[i-5]][2]
        Cont = 0
        
        #CCI大於Up的天數為Cont天，在隔天買進
        CCI <- TTR::CCI(HLC = cbind.xts(TWII_Period$High,TWII_Period$Low,TWII_Period$Close),n =  N)
        ROC_CCI <- AdjROC((CCI$cci - UpCCI)) %>% xts(order.by = time(CCI)) %>% lag.xts(1)
        Diff_CCI <- (CCI$cci - UpCCI) %>% lag.xts(1)
        
        CCI <- CCI[as.character(year(S)+i-1)]
        ROC_CCI <- ROC_CCI[as.character(year(S)+i-1)]
        Diff_CCI <- Diff_CCI[as.character(year(S)+i-1)]
        TWII_Period = TWII_Period[as.character(year(S)+i-1)]
        
        CCI_Signal_Up <- CCI$cci - UpCCI
        ROC_CCI_Signal <- AdjROC(CCI_Signal_Up)
        ROC_CCI_Signal <- ROC_CCI_Signal > 0
        ROC_CCI_Signal_Cond <- (ROC_CCI_Signal == T) 
        ROC_CCI_Signal_Cond[is.na(ROC_CCI_Signal_Cond)] = F
        CCI_Signal_Up_Cont <- ContinuingPeriod(ROC_CCI_Signal_Cond,Cont)
        CCI_Signal_Up_Cont <- xts(CCI_Signal_Up_Cont,order.by = time(TWII_Period))
        
        if(i == 6){
                CCI_Logical = CCI_Signal_Up_Cont
                CCI_DT = merge.xts(ROC_CCI,Diff_CCI)
        }else{
                CCI_Logical = rbind.xts(CCI_Logical,CCI_Signal_Up_Cont)
                CCI_DT = rbind.xts(CCI_DT,merge.xts(ROC_CCI, Diff_CCI))
        }  
}

names(CCI_Logical) = "CCI"
names(CCI_DT) = c("ROC_CCI","Diff_CCI")

TechnicalIndex_Logical = merge.xts(TechnicalIndex_Logical, CCI_Logical)
TechnicalIndex_Numeric = merge.xts(TechnicalIndex_Numeric, CCI_DT)

ROC(TWII$Close)[time(CCI_Logical[CCI_Logical == T])] %>% sum()
(ROC(TWII$Close)[time(CCI_Logical[CCI_Logical == T])] > 0) %>% table()

##Aroon
Parameter_Aroon_t2 = AdjPar(Parameter_Aroon_t2)
for(i in 6:(length(Parameter_Aroon_t2)+4)){
        TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)-months(20)),'::',as.character(S+years(i)-days(1)))]        
        
        N = Parameter_Aroon_t2[[i-5]][1]
        aroon = Parameter_Aroon_t2[[i-5]][2]
        Cont = 0
        
        Aroon <- TTR::aroon(cbind.xts(TWII_Period$High,TWII_Period$Low),N)
        Diff_Aroon1 = (Aroon$aroonUp - Aroon$aroonDn) %>% lag.xts(1)
        Diff_Aroon2 = (Aroon$aroonUp - aroon) %>% lag.xts(1)
        
        Aroon <- Aroon[as.character(year(S)+i-1)]
        Diff_Aroon1 <- Diff_Aroon1[as.character(year(S)+i-1)]
        Diff_Aroon2 <- Diff_Aroon2[as.character(year(S)+i-1)]
        TWII_Period = TWII_Period[as.character(year(S)+i-1)]
        
        #aroonUp大於aroonDn且aroonUp大於aroon時在隔天買進
        Aroon_UpTru <- Aroon$aroonUp - Aroon$aroonDn
        Cond1_Aroon <- Aroon$aroonUp > aroon
        ROC_Aroon_Signal_Cond <- Aroon_UpTru>0 & Cond1_Aroon
        ROC_Aroon_Signal_Cond[is.na(ROC_Aroon_Signal_Cond$aroonUp)] = F
        Aroon_Signal_Up_Cont <- ContinuingPeriod(ROC_Aroon_Signal_Cond,Cont)
        Aroon_Signal_Up_Cont <- xts(Aroon_Signal_Up_Cont,order.by = time(TWII_Period))
        
        if(i == 6){
                Aroon_Logical = Aroon_Signal_Up_Cont
                Aroon_DT = merge.xts(Diff_Aroon1,Diff_Aroon2)
        }else{
                Aroon_Logical = rbind.xts(Aroon_Logical,Aroon_Signal_Up_Cont)
                Aroon_DT = rbind.xts(Aroon_DT,merge.xts(Diff_Aroon1,Diff_Aroon2))
        }  
}

names(Aroon_Logical) = "Aroon"
names(Aroon_DT) = c("Diff_Aroon1","Diff_Aroon2")

TechnicalIndex_Logical = merge.xts(TechnicalIndex_Logical, Aroon_Logical)
TechnicalIndex_Numeric = merge.xts(TechnicalIndex_Numeric, Aroon_DT)

ROC(TWII$Close)[time(Aroon_Logical[Aroon_Logical == T])] %>% sum()
(ROC(TWII$Close)[time(Aroon_Logical[Aroon_Logical == T])] > 0) %>% table()

##EMV
Parameter_EMV_t2 = AdjPar(Parameter_EMV_t2)
for(i in 6:(length(Parameter_EMV_t2)+4)){
        TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)-months(20)),'::',as.character(S+years(i)-days(1)))]        
        
        N = Parameter_EMV_t2[[i-5]][1]
        t = Parameter_EMV_t2[[i-5]][2]
        Cont = 1
        
        EMV <- TTR::EMV(cbind.xts(TWII_Period$High,TWII_Period$Low),TWII_Period$Volume,N)
        ROC_EMV <- AdjROC((EMV$emv - t)) %>% xts(order.by = time(EMV)) %>% lag.xts(1)
        Diff_EMV <- (EMV$emv - t) %>% lag.xts(1)
        
        EMV <- EMV[as.character(year(S)+i-1)]
        ROC_EMV <- ROC_EMV[as.character(year(S)+i-1)]
        Diff_EMV <- Diff_EMV[as.character(year(S)+i-1)]
        TWII_Period = TWII_Period[as.character(year(S)+i-1)]
        
        #EMV大於0時隔天買進
        EMV_UpThru <- EMV$emv - t
        ROC_EMV_Signal_Cond <- EMV_UpThru > 0
        ROC_EMV_Signal_Cond[is.na(ROC_EMV_Signal_Cond)] = F
        EMV_Signal_Up_Cont <- ContinuingPeriod(ROC_EMV_Signal_Cond,Cont)
        EMV_Signal_Up_Cont <- xts(EMV_Signal_Up_Cont,order.by = time(TWII_Period))
        
        if(i == 6){
                EMV_Logical = EMV_Signal_Up_Cont
                EMV_DT = merge.xts(ROC_EMV,Diff_EMV)
        }else{
                EMV_Logical = rbind.xts(EMV_Logical,EMV_Signal_Up_Cont)
                EMV_DT = rbind.xts(EMV_DT,merge.xts(ROC_EMV, Diff_EMV))
        }  
}

names(EMV_Logical) = "EMV"
names(EMV_DT) = c("ROC_EMV","Diff_EMV")

TechnicalIndex_Logical = merge.xts(TechnicalIndex_Logical, EMV_Logical)
TechnicalIndex_Numeric = merge.xts(TechnicalIndex_Numeric, EMV_DT)

ROC(TWII$Close)[time(EMV_Logical[EMV_Logical == T])] %>% sum()
(ROC(TWII$Close)[time(EMV_Logical[EMV_Logical == T])] > 0) %>% table()

##ChaikinVolatility
Parameter_CV_t2 = AdjPar(Parameter_CV_t2)
for(i in 6:(length(Parameter_CV_t2)+4)){
        TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)-months(20)),'::',as.character(S+years(i)-days(1)))]        
        
        N = Parameter_CV_t2[[i-5]][1]
        T_Down = Parameter_CV_t2[[i-5]][2]
        Cont = 0
        
        ChaikinVolatility <- TTR::chaikinVolatility(cbind.xts(TWII_Period$High,TWII_Period$Low),N)
        ROC_CV <- AdjROC((T_Down - ChaikinVolatility )) %>% xts(order.by = time(ChaikinVolatility)) %>% lag.xts(1)
        Diff_CV <- (T_Down - ChaikinVolatility) %>% lag.xts(1)
        
        ChaikinVolatility <- ChaikinVolatility[as.character(year(S)+i-1)]
        ROC_CV <- ROC_CV[as.character(year(S)+i-1)]
        Diff_CV <- Diff_CV[as.character(year(S)+i-1)]
        TWII_Period = TWII_Period[as.character(year(S)+i-1)]
        
        ChaikinVolatility_Signal <- ChaikinVolatility < T_Down
        ChaikinVolatility_Signal[is.na(ChaikinVolatility_Signal)] = F
        ChaikinVolatility_Signal <- ContinuingPeriod(ChaikinVolatility_Signal,Cont)
        ChaikinVolatility_Signal <- xts(ChaikinVolatility_Signal,order.by = time(TWII_Period))
        
        if(i == 6){
                CV_Logical = ChaikinVolatility_Signal
                CV_DT = merge.xts(ROC_CV,Diff_CV)
        }else{
                CV_Logical = rbind.xts(CV_Logical,ChaikinVolatility_Signal)
                CV_DT = rbind.xts(CV_DT,merge.xts(ROC_CV, Diff_CV))
        }  
}

names(CV_Logical) = "CV"
names(CV_DT) = c("ROC_CV","Diff_CV")

TechnicalIndex_Logical = merge.xts(TechnicalIndex_Logical, CV_Logical)
TechnicalIndex_Numeric = merge.xts(TechnicalIndex_Numeric, CV_DT)

ROC(TWII$Close)[time(CV_Logical[CV_Logical == T])] %>% sum()
(ROC(TWII$Close)[time(CV_Logical[CV_Logical == T])] > 0) %>% table()


##MFI
Parameter_MFI_t2 = AdjPar(Parameter_MFI_t2)
for(i in 6:(length(Parameter_MFI_t2)+4)){
        TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)-months(20)),'::',as.character(S+years(i)-days(1)))]        
        
        N = Parameter_MFI_t2[[i-5]][1]
        T_Down = Parameter_MFI_t2[[i-5]][2]
        Cont = 0
        
        MFI <- TTR::MFI(cbind.xts(TWII_Period$High,TWII_Period$Low,TWII_Period$Close),TWII_Period$Volume,N)
        ROC_MFI <- AdjROC((MFI - T_Down)) %>% xts(order.by = time(MFI)) %>% lag.xts(1)
        Diff_MFI <- (MFI - T_Down) %>% lag.xts(1)
        
        MFI <- MFI[as.character(year(S)+i-1)]
        ROC_MFI <- ROC_MFI[as.character(year(S)+i-1)]
        Diff_MFI <- Diff_MFI[as.character(year(S)+i-1)]
        TWII_Period = TWII_Period[as.character(year(S)+i-1)]
        
        MFI_Signal <- MFI > T_Down
        MFI_Signal[is.na(MFI_Signal)] = F
        MFI_Signal <- ContinuingPeriod(MFI_Signal,Cont)
        MFI_Signal <- xts(MFI_Signal,order.by = time(TWII_Period))
        
        if(i == 6){
                MFI_Logical = MFI_Signal
                MFI_DT = merge.xts(ROC_MFI,Diff_MFI)
        }else{
                MFI_Logical = rbind.xts(MFI_Logical,MFI_Signal)
                MFI_DT = rbind.xts(MFI_DT,merge.xts(ROC_MFI, Diff_MFI))
        }  
}

names(MFI_Logical) = "MFI"
names(MFI_DT) = c("ROC_MFI","Diff_MFI")

TechnicalIndex_Logical = merge.xts(TechnicalIndex_Logical, MFI_Logical)
TechnicalIndex_Numeric = merge.xts(TechnicalIndex_Numeric, MFI_DT)

ROC(TWII$Close)[time(MFI_Logical[MFI_Logical == T])] %>% sum()
(ROC(TWII$Close)[time(MFI_Logical[MFI_Logical == T])] > 0) %>% table()


####----####
##Logical
##平均台股價格
TWIIPrice = TWII$Close
AvgP_TWII_5 <- EMA(TWII$Close,5) 
AvgP_TWII_5 = (TWIIPrice > AvgP_TWII_5) %>% lag.xts(1) ; names(AvgP_TWII_5) = 'AvgP_TWII_5'
AvgP_TWII_20 <- EMA(TWII$Close,20) 
AvgP_TWII_20 = (TWIIPrice > AvgP_TWII_20) %>% lag.xts(1) ; names(AvgP_TWII_20) = 'AvgP_TWII_20'
AvgP_TWII_60 <- EMA(TWII$Close,60) 
AvgP_TWII_60 = (TWIIPrice > AvgP_TWII_60) %>% lag.xts(1) ; names(AvgP_TWII_60) = 'AvgP_TWII_60'
##平均台股交易量
TWIIVolume = TWII$Volume
AvgV_TWII_5 <- EMA(TWII$Volume,5) 
AvgV_TWII_5 = (TWIIVolume > AvgV_TWII_5) %>% lag.xts(1) ; names(AvgV_TWII_5) = 'AvgV_TWII_5'
AvgV_TWII_20 <- EMA(TWII$Volume,20) 
AvgV_TWII_20 = (TWIIVolume > AvgV_TWII_20) %>% lag.xts(1) ; names(AvgV_TWII_20) = 'AvgV_TWII_20'
AvgV_TWII_60 <- EMA(TWII$Volume,60) 
AvgV_TWII_60 = (TWIIVolume > AvgV_TWII_60) %>% lag.xts(1) ; names(AvgV_TWII_60) = 'AvgV_TWII_60'

TechnicalIndex_Logical = merge.xts(TechnicalIndex_Logical,AvgP_TWII_5,AvgP_TWII_20,AvgP_TWII_60,
                                   AvgV_TWII_5,AvgV_TWII_20,AvgV_TWII_60)

##Numeric
##平均台股價格
TWIIPrice = TWII$Close
AvgP_TWII_5 <- EMA(TWII$Close,5) ; names(AvgP_TWII_5) = 'AvgP_TWII_5'
# AvgP_TWII_5 = (TWIIPrice - AvgP_TWII_5) %>% lag.xts(1) 
AvgP_TWII_20 <- EMA(TWII$Close,20) ; names(AvgP_TWII_20) = 'AvgP_TWII_20'
# AvgP_TWII_20 = (TWIIPrice - AvgP_TWII_20) %>% lag.xts(1) 
AvgP_TWII_60 <- EMA(TWII$Close,60) ; names(AvgP_TWII_60) = 'AvgP_TWII_60'
# AvgP_TWII_60 = (TWIIPrice - AvgP_TWII_60) %>% lag.xts(1) 
##平均台股交易量
TWIIVolume = TWII$Volume
AvgV_TWII_5 <- EMA(TWII$Volume,5) ; names(AvgV_TWII_5) = 'AvgV_TWII_5'
# AvgV_TWII_5 = (TWIIVolume - AvgV_TWII_5) %>% lag.xts(1) 
AvgV_TWII_20 <- EMA(TWII$Volume,20) ; names(AvgV_TWII_20) = 'AvgV_TWII_20'
# AvgV_TWII_20 = (TWIIVolume - AvgV_TWII_20) %>% lag.xts(1) 
AvgV_TWII_60 <- EMA(TWII$Volume,60) ; names(AvgV_TWII_60) = 'AvgV_TWII_60'
# AvgV_TWII_60 = (TWIIVolume - AvgV_TWII_60) %>% lag.xts(1) 

TWII_Lag = TWII %>% lag.xts(1)
TechnicalIndex_Numeric = merge.xts(TechnicalIndex_Numeric,AvgP_TWII_5,AvgP_TWII_20,AvgP_TWII_60,
                                   AvgV_TWII_5,AvgV_TWII_20,AvgV_TWII_60,TWII_Lag)


####----####










##CMO
# Parameter_CMO_t2 = AdjPar(Parameter_CMO_t2)
# for(i in 6:(length(Parameter_CMO_t2)+4)){
#         TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)-months(20)),'::',as.character(S+years(i)-days(1)))]        
#         
#         # N = Parameter_CMO_t2[[i-5]][1]
#         # T_Down = Parameter_CMO_t2[[i-5]][1]
#         # Cont = 0
#         
#         
#         CMO <- CMO(cbind.xts(TWII_Period$Close,TWII_Period$Volume),N)
#         ROC_CMO <- ROC((CMO - T_Down),type = 'discrete') %>% lag.xts(1)
#         Diff_CMO <- (CMO - T_Down) %>% lag.xts(1)
#         
#         CMO <- CMO[as.character(year(S)+i-1)]
#         ROC_CMO <- ROC_CMO[as.character(year(S)+i-1)]
#         Diff_CMO <- Diff_CMO[as.character(year(S)+i-1)]
#         TWII_Period = TWII_Period[as.character(year(S)+i-1)]
#         
#         CMO_Signal <- CMO > T_Down
#         CMO_Signal[is.na(CMO_Signal)] = F
#         CMO_Signal <- ContinuingPeriod(CMO_Signal,Cont)
#         CMO_Signal <- xts(CMO_Signal,order.by = time(TWII_Period))
#         
#         if(i == 6){
#                 CMO_Logical = CMO_Signal
#                 CMO_DT = merge.xts(ROC_CMO,Diff_CMO)
#         }else{
#                 CMO_Logical = rbind.xts(CMO_Logical,CMO_Signal)
#                 CMO_DT = rbind.xts(CMO_DT,merge.xts(ROC_CMO, Diff_CMO))
#         }  
# }
# 
# names(CMO_Logical) = "CMO"
# names(CMO_DT) = c("ROC_CMO","Diff_CMO")
# 
# TechnicalIndex_Logical = merge.xts(TechnicalIndex_Logical, CMO_Logical)
# TechnicalIndex_Numeric = merge.xts(TechnicalIndex_Numeric, CMO_DT)
# 
# ROC(TWII$Close)[time(CMO_Logical[CMO_Logical == T])] %>% sum()
# (ROC(TWII$Close)[time(CMO_Logical[CMO_Logical == T])] > 0) %>% table()
# 
