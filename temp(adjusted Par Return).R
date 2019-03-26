####Custom function####
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

####OBV
temp1 = c()
temp2 = c()

Parameter_OBV_t2 = AdjPar(Parameter_OBV_t2)
for(i in 5:(length(Parameter_OBV_t2)+3)){
        
        #if(i == length(Parameter_SMA_t2)){break}
        #TWII_Period <- TWII[paste0(as.character(S+years(i + 1)-years(1)),'::',as.character(S+years(TimeGap+i+1-1)-days(1)))]                
        TWII_Period <- TWII[as.character(year(S)+i)]
        
        Cont = Parameter_OBV_t1[[i-4]]
        #OBV連續Cont天變動率大於零在隔天買進
        OBV <- TTR::OBV(price = TWII_Period$Close, volume = TWII_Period$Volume)
        ROC_OBV <- AdjROC(OBV)
        ROC_OBV_Signal <- ( ROC_OBV > 0) 
        ROC_OBV_Signal[is.na(ROC_OBV_Signal)] <-  F
        ROC_OBV_Signal_Cont <- ContinuingPeriod(ROC_OBV_Signal, Cont)
        if(all(ROC_OBV_Signal_Cont == F)){t1 = 0
        }else{
        t1 = table(ROC(TWII_Period$Close)[ROC_OBV_Signal_Cont$Signal] > 0)
        }
        temp1 = c(temp1, t1)
        t2 = sum(ROC(TWII_Period$Close)[ROC_OBV_Signal_Cont$Signal])
        temp2 = c(temp2, t2)
        
}

####MACD
temp1 = c()
temp2 = c()

Parameter_MACD_t2 = AdjPar(Parameter_MACD_t2)
for(i in 6:(length(Parameter_MACD_t2)+4)){
        
        #if(i == length(Parameter_SMA_t2)){break}
        #TWII_Period <- TWII[paste0(as.character(S+years(i + 1)-years(1)),'::',as.character(S+years(TimeGap+i+1-1)-days(1)))]
        TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)-months(20)),'::',as.character(S+years(i)-days(1)))]
        
        Fast = Parameter_MACD_t2[[i-5]][1]
        Slow = Parameter_MACD_t2[[i-5]][2]
        Cont = 0
        Sig = Parameter_MACD_t2[[i-5]][3]
        # Cont = 0
        # Fast = 2
        # Slow = 35
        # Sig = 4
        
        MACD <- TTR::MACD(TWII_Period, nFast = Fast, nSlow = Slow, nSig = Sig, percent = F)
        MACD <- MACD[as.character(year(S)+i-1)]
        TWII_Period = TWII_Period[as.character(year(S)+i-1)]
        ROC_MACD <- AdjROC(MACD$macd-MACD$signal)
        Cond1_MACD <- (MACD$macd > 0) & (MACD$signal > 0)
        ROC_MACD_Signal <- (ROC_MACD > 0) & (Cond1_MACD == T) & (TWII_Period$Close  > AvgP_TWII_20 ) & (TWII_Period$Volume > AvgV_TWII_20)
        ROC_MACD_Signal[is.na(ROC_MACD_Signal)] <- F
        ROC_MACD_Signal_Cont <- ContinuingPeriod(ROC_MACD_Signal,Cont)
        t1 = table(ROC(TWII_Period$Close)[ROC_MACD_Signal_Cont$Signal] > 0)
        temp1 = c(temp1,t1)

        t2 = sum(ROC(TWII_Period$Close)[ROC_MACD_Signal_Cont$Signal])
        temp2 = c(temp2, t2)
        
}


####SMA
temp1 = c()
temp2 = c()

Parameter_SMA_t2 = AdjPar(Parameter_SMA_t2)
for(i in 6:(length(Parameter_SMA_t2)+4)){
        TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)-months(20)),'::',as.character(S+years(i)-days(1)))]        

        Fast = Parameter_SMA_t2[[i-5]][1]
        Slow = Parameter_SMA_t2[[i-5]][2]
        Cont = Parameter_SMA_t2[[i-5]][3]
        
        SMA_Fast <- TTR::SMA(TWII_Period$Close, Fast)
        SMA_Fast <- SMA_Fast[as.character(year(S)+i-1)]
        SMA_Slow <- TTR::SMA(TWII_Period$Close, Slow)
        SMA_Slow <- SMA_Slow[as.character(year(S)+i-1)]
        TWII_Period = TWII_Period[as.character(year(S)+i-1)]
        #快線向上穿越持續Cont天，則在隔天買進
        SMA_UPThru <- SMA_Fast - SMA_Slow
        SMA_UPThru[SMA_UPThru <= 0] <- 0
        ROC_SMA_UPThru <- ROC(SMA_UPThru,type = 'discrete')
        ROC_SMA_UPThru_Signal <- ROC_SMA_UPThru > 0
        Cond1_SMA <- (SMA_Fast > 0) & (SMA_Slow > 0)
        ROC_SMA_UPThru_Signal <- (ROC_SMA_UPThru_Signal > 0) & (Cond1_SMA == T) & (TWII$Close  > AvgP_TWII_60 ) & (TWII$Volume > AvgV_TWII_60)
        ROC_SMA_UPThru_Signal[is.na(ROC_SMA_UPThru_Signal)] <-  F
        ROC_SMA_UPThru_Signal_Cont <- ContinuingPeriod(ROC_SMA_UPThru_Signal, Cont)
        t1 = table(ROC(TWII_Period$Close)[ROC_SMA_UPThru_Signal_Cont$Signal] > 0 )
        temp1 = c(temp1,t1)
        t2 = sum(ROC(TWII_Period$Close)[ROC_SMA_UPThru_Signal_Cont$Signal])
        temp2 = c(temp2,t2)
}

####VWMA
temp1 = c()
temp2 = c()
Parameter_VWMA_t2 = AdjPar(Parameter_VWMA_t2)
for(i in 6:(length(Parameter_VWMA_t2)+4)){
        TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)-months(20)),'::',as.character(S+years(i)-days(1)))]        
        
        Fast = Parameter_VWMA_t2[[i-5]][1]
        Slow = Parameter_VWMA_t2[[i-5]][2]
        Cont = Parameter_VWMA_t2[[i-5]][3]
        
        VWMA_Fast <- TTR::VWMA(TWII_Period$Close, TWII_Period$Volume, Fast)
        VWMA_Fast <- VWMA_Fast[as.character(year(S)+i-1)]
        VWMA_Slow <- TTR::VWMA(TWII_Period$Close, TWII_Period$Volume, Slow)
        VWMA_Fast <- VWMA_Fast[as.character(year(S)+i-1)]
        TWII_Period = TWII_Period[as.character(year(S)+i-1)]
        #快線向上穿越持續Cont天，則在隔天買進
        VWMA_UPThru <- VWMA_Fast - VWMA_Slow
        VWMA_UPThru[VWMA_UPThru <= 0] <- 0
        ROC_VWMA_UPThru <- ROC(VWMA_UPThru,type = 'discrete')
        ROC_VWMA_UPThru_Signal <- ROC_VWMA_UPThru > 0
        Cond1_VWMA <- (VWMA_Fast > 0) & (VWMA_Slow > 0)
        ROC_VWMA_UPThru_Signal <- (ROC_VWMA_UPThru_Signal > 0) & (Cond1_VWMA == T) & (TWII$Close  > AvgP_TWII_20 ) & (TWII$Volume > AvgV_TWII_5)
        ROC_VWMA_UPThru_Signal[is.na(ROC_VWMA_UPThru_Signal)] <-  F
        ROC_VWMA_UPThru_Signal_Cont <- ContinuingPeriod(ROC_VWMA_UPThru_Signal, Cont)
        t1 = table(ROC(TWII_Period$Close)[ROC_VWMA_UPThru_Signal_Cont$Signal] > 0 )
        temp1 = c(temp1,t1)
        t2 = sum(ROC(TWII_Period$Close)[ROC_VWMA_UPThru_Signal_Cont$Signal])
        temp2 = c(temp2,t2)
        
}

####EMA                 
temp1 = c()
temp2 = c()
Parameter_EMA_t2 = AdjPar(Parameter_EMA_t2)
for(i in 6:(length(Parameter_EMA_t2)+4)){
        TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)-months(20)),'::',as.character(S+years(i)-days(1)))]        
        
        Fast = Parameter_EMA_t2[[i-5]][1]
        Slow = Parameter_EMA_t2[[i-5]][2]
        Cont = Parameter_EMA_t2[[i-5]][3]
        
        EMA_Fast <- TTR::EMA(TWII_Period$Close, Fast)
        EMA_Fast <- EMA_Fast[as.character(year(S)+i-1)]
        EMA_Slow <- TTR::EMA(TWII_Period$Close, Slow)
        EMA_Fast <- EMA_Fast[as.character(year(S)+i-1)]
        TWII_Period = TWII_Period[as.character(year(S)+i-1)]
        #快線向上穿越持續Cont天，則在隔天買進
        EMA_UPThru <- EMA_Fast - EMA_Slow
        EMA_UPThru[EMA_UPThru <= 0] <- 0
        ROC_EMA_UPThru <- ROC(EMA_UPThru,type = 'discrete')
        ROC_EMA_UPThru_Signal <- ROC_EMA_UPThru > 0
        ROC_EMA_UPThru_Signal[is.na(ROC_EMA_UPThru_Signal)] <-  F
        ROC_EMA_UPThru_Signal_Cont <- ContinuingPeriod(ROC_EMA_UPThru_Signal, Cont)
        t1 = table(ROC(TWII_Period$Close)[ROC_EMA_UPThru_Signal_Cont$Signal] > 0 )
        temp1 = c(temp1, t1)
        t2 = sum(ROC(TWII_Period$Close)[ROC_EMA_UPThru_Signal_Cont$Signal])
        temp2 = c(temp2, t2)
        
}


####BBand                
temp1 = c()
temp2 = c()
Parameter_BBand_t2 = AdjPar(Parameter_BBand_t2)
for(i in 6:(length(Parameter_BBand_t2)+4)){
        TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)-months(20)),'::',as.character(S+years(i)-days(1)))]        
        
        N = Parameter_BBand_t2[[i-5]][1]
        DownB = Parameter_BBand_t2[[i-5]][2]
        Lag = Parameter_BBand_t2[[i-5]][3]
        
        #當移動平均低於兩配標準差(DownBand)Lag天後，在隔天做多
        BBand <- TTR::BBands(HLC(TWII_Period), n = N)
        ROC_B
        
        BBand <- BBand[as.character(year(S)+i-1)]
        TWII_Period = TWII_Period[as.character(year(S)+i-1)]
        
        BBand_Signal_Down <- BBand$pctB < DownB
        BBand_Signal_Down_Lag <- lag(BBand_Signal_Down,Lag)
        BBand_Signal_Down_Lag[is.na(BBand_Signal_Down_Lag)] <-  F
        BBand_Signal_Down_Lag <- (BBand_Signal_Down_Lag == T) & (TWII$Close  > AvgP_TWII_5 ) & (TWII$Volume > AvgV_TWII_5)
        t1 = table(ROC(TWII_Period$Close)[BBand_Signal_Down_Lag] > 0)
        temp1 = c(temp1, t1)
        t2 = sum(ROC(TWII_Period$Close)[BBand_Signal_Down_Lag])
        temp2 = c(temp2, t2)
        
}


####KD
temp1 = c()
temp2 = c()
Parameter_KD_t2 = AdjPar(Parameter_KD_t2)
for(i in 6:(length(Parameter_KD_t2)+4)){
        TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)-months(20)),'::',as.character(S+years(i)-days(1)))]        

        kd = Parameter_KD_t2[[i-5]][1]
        K = Parameter_KD_t2[[i-5]][2]
        D = Parameter_KD_t2[[i-5]][3]
        Cont = 0
        
        KD <- TTR::stoch(HLC(TWII_Period), nFastK = kd, nFastD = K, nSlowD = D)
        KD <- KD[as.character(year(S)+i-1)]
        TWII_Period = TWII_Period[as.character(year(S)+i-1)]
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
        t1 = table(ROC(TWII_Period$Close)[ROC_KD_UPThru_Signal_Cont$Signal] > 0 )
        temp1 = c(temp1, t1)
        t2 = sum(ROC(TWII_Period$Close)[ROC_KD_UPThru_Signal_Cont$Signal])
        temp2 = c(temp2, t2)
        
}


####RSI
temp1 = c()
temp2 = c()
Parameter_RSI_t2 = AdjPar(Parameter_RSI_t2)
for(i in 6:(length(Parameter_RSI_t2)+4)){
        TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)-months(20)),'::',as.character(S+years(i)-days(1)))]        

        Fast = Parameter_RSI_t2[[i-5]][1]
        Slow = Parameter_RSI_t2[[i-5]][2]
        Cont = 0

        RSI_Fast <- TTR::RSI(TWII_Period$Close,n = Fast)
        RSI_Slow <- TTR::RSI(TWII_Period$Close,n = Slow)
        RSI_Fast <- RSI_Fast[as.character(year(S)+i-1)]
        RSI_Slow <- RSI_Slow[as.character(year(S)+i-1)]
        TWII_Period = TWII_Period[as.character(year(S)+i-1)]
        
        RSI_Signal <- RSI_Fast - RSI_Slow
        RSI_Signal[RSI_Signal<=0] <- 0
        ROC_RSI_Signal <- ROC(RSI_Signal, type = 'discrete')
        ROC_RSI_Signal <- ROC_RSI_Signal > 0
        RSI_Signal_Cond <- (ROC_RSI_Signal == T) 
        RSI_Signal_Cond[is.na(RSI_Signal_Cond)] <-  F
        RSI_Signal_Cont <- ContinuingPeriod(RSI_Signal_Cond,Cont)
        t1 = table(ROC(TWII_Period$Close)[RSI_Signal_Cont$Signal] > 0)
        temp1 = c(temp1, t1)
        t2 = sum(ROC(TWII_Period$Close)[RSI_Signal_Cont$Signal])
        temp2 = c(temp2, t2)
}


####ADX
temp1 = c()
temp2 = c()

Parameter_ADX_t2 = AdjPar(Parameter_ADX_t2)
for(i in 6:(length(Parameter_ADX_t2)+4)){
        TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)-months(20)),'::',as.character(S+years(i)-days(1)))]        
        
        N = Parameter_ADX_t2[[i-5]]
        Cont = 0
        
        ADX <- TTR::ADX(HLC = cbind.xts(TWII_Period$High,TWII_Period$Low,TWII_Period$Close) ,n = N)
        ADX <- ADX[as.character(year(S)+i-1)]
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
        
        t1 = table(ROC(TWII_Period$Close)[ADX_Signal_UpTru==T] > 0)
        temp1 = c(temp1, t1)
        t2 = sum(ROC(TWII_Period$Close)[ADX_Signal_UpTru==T])
        temp2 = c(temp2, t2)                 
}

####CCI
temp1 = c()
temp2 = c()
Parameter_CCI_t2 = AdjPar(Parameter_CCI_t2)
for(i in 6:(length(Parameter_CCI_t2)+4)){
        TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)-months(20)),'::',as.character(S+years(i)-days(1)))]        
        
        UpCCI = Parameter_CCI_t2[[i-5]][1]
        N = Parameter_CCI_t2[[i-5]][2]
        Cont = 0

        #CCI大於Up的天數為Cont天，在隔天買進
        CCI <- CCI(HLC = cbind.xts(TWII_Period$High,TWII_Period$Low,TWII_Period$Close),n =  N)
        CCI <- CCI[as.character(year(S)+i-1)]
        TWII_Period = TWII_Period[as.character(year(S)+i-1)]
        CCI_Signal_Up <- CCI$cci - UpCCI
        CCI_Signal_Up[CCI_Signal_Up<=0] <- 0
        ROC_CCI_Signal <- ROC(CCI_Signal_Up, type = 'discrete')
        ROC_CCI_Signal <- ROC_CCI_Signal > 0
        ROC_CCI_Signal_Cond <- (ROC_CCI_Signal == T) 
        ROC_CCI_Signal_Cond[is.na(ROC_CCI_Signal_Cond$cci)] = F
        CCI_Signal_Up_Cont <- ContinuingPeriod(ROC_CCI_Signal_Cond,Cont)
        t1 = table(ROC(TWII_Period$Close)[CCI_Signal_Up_Cont$Signal] > 0)
        temp1 = c(temp1, t1)
        t2 = sum(ROC(TWII_Period$Close)[CCI_Signal_Up_Cont$Signal])
        temp2 = c(temp2, t2)
}


####Aroon
temp1 = c()
temp2 = c()
Parameter_Aroon_t2 = AdjPar(Parameter_Aroon_t2)
for(i in 6:(length(Parameter_Aroon_t2)+4)){
        TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)-months(20)),'::',as.character(S+years(i)-days(1)))]        
        
        N = Parameter_Aroon_t2[[i-5]][1]
        aroon = Parameter_Aroon_t2[[i-5]][2]
        Cont = 0

        Aroon <- aroon(cbind.xts(TWII_Period$High,TWII_Period$Low),N)
        Aroon <- Aroon[as.character(year(S)+i-1)]
        TWII_Period = TWII_Period[as.character(year(S)+i-1)]
        #aroonUp大於aroonDn且aroonUp大於aroon時在隔天買進
        Aroon_UpTru <- Aroon$aroonUp - Aroon$aroonDn
        Cond1_Aroon <- Aroon$aroonUp > aroon
        ROC_Aroon_Signal_Cond <- Aroon_UpTru>0 & Cond1_Aroon
        ROC_Aroon_Signal_Cond[is.na(ROC_Aroon_Signal_Cond$aroonUp)] = F
        Aroon_Signal_Up_Cont <- ContinuingPeriod(ROC_Aroon_Signal_Cond,Cont)
        
        t1 = table(ROC(TWII_Period$Close)[Aroon_Signal_Up_Cont$Signal] > 0)
        temp1 = c(temp1, t1)
        t2 = sum(ROC(TWII_Period$Close)[Aroon_Signal_Up_Cont$Signal])
        temp2 = c(temp2, t2)
}


        
####EMV
temp1 = c()
temp2 = c()
# Parameter_EMV_t2 = AdjPar(Parameter_EMV_t2)
Parameter_EMV_t1 = AdjPar(Parameter_EMV_t1)
for(i in 6:(length(Parameter_EMV_t2)+4)){
        TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)-months(20)),'::',as.character(S+years(i)-days(1)))]        
        
        N = Parameter_EMV_t1[[i-5]][1]
        t = Parameter_EMV_t1[[i-5]][2]
        Cont = 1
        
        EMV <- EMV(cbind.xts(TWII_Period$High,TWII_Period$Low),TWII_Period$Volume,N)
        EMV <- EMV[as.character(year(S)+i-1)]
        TWII_Period = TWII_Period[as.character(year(S)+i-1)]
        #EMV大於0時隔天買進
        EMV_UpThru <- EMV$emv - t
        ROC_EMV_Signal_Cond <- EMV_UpThru > 0
        ROC_EMV_Signal_Cond[is.na(ROC_EMV_Signal_Cond)] = F
        EMV_Signal_Up_Cont <- ContinuingPeriod(ROC_EMV_Signal_Cond,Cont)
        
        t1 = table(ROC(TWII_Period$Close)[EMV_Signal_Up_Cont$Signal] > 0 )
        temp1 = c(temp1, t1)
        t2 = sum(ROC(TWII_Period$Close)[EMV_Signal_Up_Cont$Signal])
        temp2 = c(temp2, t2)
}

####ChaikinVolatility
temp1 = c()
temp2 = c()
Parameter_CV_t2 = AdjPar(Parameter_CV_t2)
for(i in 6:(length(Parameter_CV_t2)+4)){
        TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)-months(20)),'::',as.character(S+years(i)-days(1)))]        

        N = Parameter_CV_t2[[i-5]][1]
        T_Down = Parameter_CV_t2[[i-5]][2]
        Cont = 0 
        
        ChaikinVolatility <- chaikinVolatility(cbind.xts(TWII_Period$High,TWII_Period$Low),N)
        ChaikinVolatility <- ChaikinVolatility[as.character(year(S)+i-1)]
        TWII_Period = TWII_Period[as.character(year(S)+i-1)]
        
        ChaikinVolatility_Signal <- ChaikinVolatility < T_Down
        ChaikinVolatility_Signal[is.na(ChaikinVolatility_Signal)] = F
        ChaikinVolatility_Signal <- ContinuingPeriod(ChaikinVolatility_Signal,Cont)

        
        t1 = table(ROC(TWII_Period$Close)[ChaikinVolatility_Signal$Signal] > 0 )
        temp1 = c(temp1, t1)
        t2 = sum(ROC(TWII_Period$Close)[ChaikinVolatility_Signal$Signal])
        temp2 = c(temp2, t2)
}


####MFI
temp1 = c()
temp2 = c()
Parameter_MFI_t2 = AdjPar(Parameter_MFI_t2)
for(i in 6:(length(Parameter_MFI_t2)+4)){
        TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)-months(20)),'::',as.character(S+years(i)-days(1)))]        
        
        N = Parameter_MFI_t2[[i-5]][1]
        T_Down = Parameter_MFI_t2[[i-5]][2]
        Cont = 0
        
        MFI <- MFI(cbind.xts(TWII_Period$High,TWII_Period$Low,TWII_Period$Close),TWII_Period$Volume,N)
        MFI <- MFI[as.character(year(S)+i-1)]
        TWII_Period = TWII_Period[as.character(year(S)+i-1)]
        
        MFI_Signal <- MFI > T_Down
        MFI_Signal[is.na(MFI_Signal)] = F
        MFI_Signal <- ContinuingPeriod(MFI_Signal,Cont)
        
        t1 = table(ROC(TWII_Period$Close)[MFI_Signal$Signal] > 0 )
        temp1 = c(temp1, t1)
        t2 = sum(ROC(TWII_Period$Close)[MFI_Signal$Signal])
        temp2 = c(temp2, t2)
}


####CMO
temp1 = c()
temp2 = c()
# Parameter_EMV_t2 = AdjPar(Parameter_EMV_t2)
for(i in 6:(length(Parameter_CMO_t2)+4)){
        TWII_Period <- TWII[paste0(as.character(S+years(i)-years(1)-months(20)),'::',as.character(S+years(i)-days(1)))]        
        
        N = Parameter_CMO_t2[[i-5]][1]
        T_Down = Parameter_CMO_t2[[i-5]][1]
        Cont = 0
        
        
        CMO <- CMO(cbind.xts(TWII_Period$Close,TWII_Period$Volume),N)
        CMO <- CMO[as.character(year(S)+i-1)]
        TWII_Period = TWII_Period[as.character(year(S)+i-1)]
        CMO_Signal <- CMO > T_Down
        CMO_Signal[is.na(CMO_Signal)] = F
        CMO_Signal <- ContinuingPeriod(CMO_Signal,Cont)
        
        t1 = table(ROC(TWII_Period$Close)[CMO_Signal$Signal] > 0 )
        temp1 = c(temp1, t1)
        t2 = sum(ROC(TWII_Period$Close)[CMO_Signal$Signal])
        temp2 = c(temp2, t2)
}

