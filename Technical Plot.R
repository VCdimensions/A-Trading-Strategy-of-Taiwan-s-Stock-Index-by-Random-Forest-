library(TTR)
library(quantmod)
library(data.table)
library(dplyr)
library(plotly)

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

####Import Data####
##Taiwan
TWII <- fread('TWII.csv')
TWII <- xts(x = TWII %>% select(-Date) , order.by = as.Date(TWII$Date))
# TWII <- TWII['2016']
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
Cont = seq(0,8,1)

temp_Up = matrix(0,ncol= len, nrow = 1,
                  dimnames = list(as.character('SumROC'),as.character(Cont)))
temp_Down = matrix(0,ncol= len, nrow = 1,
                 dimnames = list(as.character('SumROC'),as.character(Cont)))
len = length(Cont)

for(i in Cont){
        
        
        #OBV連續Cont天變動率大於零在隔天買進
        OBV <- TTR::OBV(price = TWII$Close, volume = TWII$Volume)
        ROC_OBV <- AdjROC(OBV)
        ROC_OBV_Signal <-  ROC_OBV > 0
        ROC_OBV_Signal[is.na(ROC_OBV_Signal)] <-  F
        ROC_OBV_Signal_Cont <- ContinuingPeriod(ROC_OBV_Signal,i)
        table(ROC(TWII$Close)[ROC_OBV_Signal_Cont$Signal] > 0)
        a = sum(ROC(TWII$Close)[ROC_OBV_Signal_Cont$Signal])
        
        temp_Up[,i+1] = a
        
        #OBV連續Cont天變動率大於零在隔天放空
        OBV <- TTR::OBV(price = TWII$Close, volume = TWII$Volume)
        ROC_OBV <- ROC(OBV,type = 'discrete')
        ROC_OBV_Signal <-  ROC_OBV < 0
        ROC_OBV_Signal[is.na(ROC_OBV_Signal)] <-  F
        ROC_OBV_Signal_Cont <- ContinuingPeriod(ROC_OBV_Signal,i)
        table(ROC(TWII$Close)[ROC_OBV_Signal_Cont$Signal] < 0)
        b = sum(-ROC(TWII$Close)[ROC_OBV_Signal_Cont$Signal])
        
        temp_Down[,i+1] = b

}
plot(temp_Up[,])
plot(temp_Down[,])

##MACD:DIF與MACD的差的變動率連續四天大於Cont
Fast = seq(1,8,1)
lenF = length(Fast)
Slow = seq(10,30,5)
lenS = length(Slow)
Cont = 0
Sig = 5

temp_Up = matrix(0, nrow = lenF, ncol= lenS,
                 dimnames = list(as.character(Fast),as.character(Slow)))
temp_Down = matrix(0, nrow = lenF, ncol= lenS,
                   dimnames = list(as.character(Fast),as.character(Slow)))
c1 = 0
for(i in Fast){
        c2 = 0
        c1 = c1 + 1
        for(j in Slow){
                c2 = c2 + 1
                #MACD:DIF與MACD的差的變動率連續四天大於Cont在隔天買進
                MACD <- TTR::MACD(TWII, nFast = i, nSlow = j, nSig = Sig)
                ROC_MACD <- AdjROC(MACD$macd-MACD$signal)
                Cond1_MACD <- (MACD$macd > 0) & (MACD$signal > 0)
                ROC_MACD_Signal <- (ROC_MACD > 0) & (Cond1_MACD == T)
                ROC_MACD_Signal[is.na(ROC_MACD_Signal)] <- F
                ROC_MACD_Signal_Cont <- ContinuingPeriod(ROC_MACD_Signal,Cont)
                table(ROC(TWII$Close)[ROC_MACD_Signal_Cont$Signal] > 0)
                a = sum(ROC(TWII$Close)[ROC_MACD_Signal_Cont$Signal])
                
                temp_Up[c1,c2] = a
                #MACD:DIF與MACD的差的變動率連續四天大於Cont在隔天放空
                # MACD <- TTR::MACD(TWII, nFast = i, nSlow = j, nSig = Sig)
                # ROC_MACD <- AdjROC((MACD$macd-MACD$signal))
                # Cond1_MACD <- (MACD$macd < 0) & (MACD$signal < 0)
                # ROC_MACD_Signal <- (ROC_MACD < 0) & (Cond1_MACD == T)
                # ROC_MACD_Signal[is.na(ROC_MACD_Signal)] <- F
                # ROC_MACD_Signal_Cont <- ContinuingPeriod(ROC_MACD_Signal,Cont)
                # table(ROC(TWII$Close)[ROC_MACD_Signal_Cont$Signal] < 0)
                # b = sum(-ROC(TWII$Close)[ROC_MACD_Signal_Cont$Signal])
                #  
                # temp_Down[c1,c2] = b 
                
        }
}


Sys.setenv("plotly_username"="a77689466")
Sys.setenv("plotly_api_key"="n4Ee3sjxB9Dg78T8o3Vh")
# volcano is a numeric matrix that ships with R
aa = rownames(temp_Up)
bb = colnames(temp_Up)
tt = list(a = as.numeric(aa),b = as.numeric(bb),c = temp_Up)
a <- plot_ly(x=~tt$b, y =~tt$a , z = ~tt$c) %>% add_surface() %>%  layout(
        scene = list(
                xaxis = list(title = "Slow"),
                yaxis = list(title = "Fast"),
                zaxis = list(title = "累積報酬"))
        ) %>%
        layout(showlegend = FALSE)
#plotly_IMAGE(a, format = "png",width = 2000,height = 900,dpi = 500,units = 'cm', out_file = "MACD_Sig2.png")

api_create(a, filename = "技術分析參數最佳化(MACD_Sig5)")

# aa = rownames(temp_Down)
# bb = colnames(temp_Down)
# tt = list(a = aa, b = bb, c = temp_Down)
# b <- plot_ly(x=tt$a, y = tt$b , z = tt$c) %>% add_surface()
# 
# # a <- plot_ly(z = ~ temp_Up) %>% add_surface()
# # b <- plot_ly(z = ~ temp_Down) %>% add_surface()


##SMA快線向上穿越持續Cont天
Fast = seq(1,9,1)
lenF = length(Fast)
Slow = seq(10,40,5)
lenS = length(Slow)
Cont = 1

temp_Up = matrix(0, nrow = lenF, ncol= lenS,
                 dimnames = list(as.character(Fast),as.character(Slow)))
temp_Down = matrix(0, nrow = lenF, ncol= lenS,
                   dimnames = list(as.character(Fast),as.character(Slow)))

c1 = 0
for(i in Fast){
        c2 = 0
        c1 = c1 + 1
        for(j in Slow){
                c2 = c2 + 1
                SMA_Fast <- TTR::SMA(TWII$Close, i)
                SMA_Slow <- TTR::SMA(TWII$Close, j)
                #快線向上穿越持續Cont天，則在隔天買進
                SMA_UPThru <- SMA_Fast - SMA_Slow
                SMA_UPThru[SMA_UPThru <= 0] <- 0
                ROC_SMA_UPThru <- ROC(SMA_UPThru,type = 'discrete')
                ROC_SMA_UPThru_Signal <- ROC_SMA_UPThru > 0
                ROC_SMA_UPThru_Signal[is.na(ROC_SMA_UPThru_Signal)] <-  F
                ROC_SMA_UPThru_Signal_Cont <- ContinuingPeriod(ROC_SMA_UPThru_Signal, Cont)
                a = sum(ROC(TWII$Close)[ROC_SMA_UPThru_Signal_Cont$Signal])
                
                temp_Up[c1,c2] = a
                #快線向下穿越持續Cont天，則在隔天放空
                SMA_DOWNThru <- SMA_Fast - SMA_Slow
                SMA_DOWNThru[SMA_DOWNThru >= 0] <- 0
                ROC_SMA_DOWNThru <- ROC(SMA_DOWNThru,type = 'discrete')
                ROC_SMA_DOWNThru_Signal <- ROC_SMA_DOWNThru > 0
                ROC_SMA_DOWNThru_Signal[is.na(ROC_SMA_DOWNThru_Signal)] <-  F
                ROC_SMA_DOWNThru_Signal_Cont <- ContinuingPeriod(ROC_SMA_DOWNThru_Signal, Cont)
                table(ROC(TWII$Close)[ROC_SMA_DOWNThru_Signal_Cont$Signal] < 0)
                b = sum(-ROC(TWII$Close)[ROC_SMA_DOWNThru_Signal_Cont$Signal])
                
                temp_Down[c1,c2] = b 
                
        }
}

# volcano is a numeric matrix that ships with R
aa = rownames(temp_Up)
bb = colnames(temp_Up)
tt = list(a = aa,b = bb,c = temp_Up)
a <- plot_ly(x=tt$a, y = tt$b , z = tt$c) %>% add_surface()

aa = rownames(temp_Down)
bb = colnames(temp_Down)
tt = list(a = aa,b = bb,c = temp_Down)
b <- plot_ly(x=tt$a, y = tt$b , z = tt$c) %>% add_surface()



##VWMA:(上漲通常量大所以會變動較快，下跌通常量小所以變動較慢)
Fast = seq(1,9,1)
lenF = length(Fast)
Slow = seq(10,40,5)
lenS = length(Slow)
Cont = 0

temp_Up = matrix(0, nrow = lenF, ncol= lenS,
                 dimnames = list(as.character(Fast),as.character(Slow)))
temp_Down = matrix(0, nrow = lenF, ncol= lenS,
                   dimnames = list(as.character(Fast),as.character(Slow)))

c1 = 0
for(i in Fast){
        c2 = 0
        c1 = c1 + 1
        for(j in Slow){
                c2 = c2 + 1
                
                VWMA_Fast <- TTR::VWMA(TWII$Close, TWII$Volume, i)
                VWMA_Slow <- TTR::VWMA(TWII$Close, TWII$Volume, j)
                #快線向上穿越持續Cont天，則在隔天買進
                VWMA_UPThru <- VWMA_Fast - VWMA_Slow
                VWMA_UPThru[VWMA_UPThru <= 0] <- 0
                ROC_VWMA_UPThru <- ROC(VWMA_UPThru,type = 'discrete')
                ROC_VWMA_UPThru_Signal <- ROC_VWMA_UPThru > 0
                ROC_VWMA_UPThru_Signal[is.na(ROC_VWMA_UPThru_Signal)] <-  F
                ROC_VWMA_UPThru_Signal_Cont <- ContinuingPeriod(ROC_VWMA_UPThru_Signal, Cont)
                a = sum(ROC(TWII$Close)[ROC_VWMA_UPThru_Signal_Cont$Signal])

                temp_Up[c1,c2] = a
                #快線向下穿越持續Cont天，則在隔天放空
                VWMA_DOWNThru <- VWMA_Fast - VWMA_Slow
                VWMA_DOWNThru[VWMA_DOWNThru >= 0] <- 0
                ROC_VWMA_DOWNThru <- ROC(VWMA_DOWNThru,type = 'discrete')
                ROC_VWMA_DOWNThru_Signal <- ROC_VWMA_DOWNThru > 0
                ROC_VWMA_DOWNThru_Signal[is.na(ROC_VWMA_DOWNThru_Signal)] <-  F
                ROC_VWMA_DOWNThru_Signal_Cont <- ContinuingPeriod(ROC_VWMA_DOWNThru_Signal, Cont)
                b = sum(-ROC(TWII$Close)[ROC_VWMA_DOWNThru_Signal_Cont$Signal])
        
                temp_Down[c1,c2] = b 
                
        }
}

# volcano is a numeric matrix that ships with R
aa = rownames(temp_Up)
bb = colnames(temp_Up)
tt = list(a = aa,b = bb,c = temp_Up)
a <- plot_ly(x=tt$a, y = tt$b , z = tt$c) %>% add_surface()

aa = rownames(temp_Down)
bb = colnames(temp_Down)
tt = list(a = aa,b = bb,c = temp_Down)
b <- plot_ly(x=tt$a, y = tt$b , z = tt$c) %>% add_surface()



##EMA
Fast = seq(1,9,1)
lenF = length(Fast)
Slow = seq(10,40,5)
lenS = length(Slow)
Cont = 0

temp_Up = matrix(0, nrow = lenF, ncol= lenS,
                 dimnames = list(as.character(Fast),as.character(Slow)))
temp_Down = matrix(0, nrow = lenF, ncol= lenS,
                   dimnames = list(as.character(Fast),as.character(Slow)))

c1 = 0
for(i in Fast){
        c2 = 0
        c1 = c1 + 1
        for(j in Slow){
                c2 = c2 + 1
                
                EMA_Fast <- TTR::EMA(TWII$Close, i)
                EMA_Slow <- TTR::EMA(TWII$Close, j)
                #快線向上穿越持續Cont天，則在隔天買進
                EMA_UPThru <- EMA_Fast - EMA_Slow
                EMA_UPThru[EMA_UPThru <= 0] <- 0
                ROC_EMA_UPThru <- ROC(EMA_UPThru,type = 'discrete')
                ROC_EMA_UPThru_Signal <- ROC_EMA_UPThru > 0
                ROC_EMA_UPThru_Signal[is.na(ROC_EMA_UPThru_Signal)] <-  F
                ROC_EMA_UPThru_Signal_Cont <- ContinuingPeriod(ROC_EMA_UPThru_Signal, Cont)
                a = sum(ROC(TWII$Close)[ROC_EMA_UPThru_Signal_Cont$Signal])
                
                temp_Up[c1,c2] = a

                #快線向下穿越持續Cont天，則在隔天放空
                EMA_DOWNThru <- EMA_Fast - EMA_Slow
                EMA_DOWNThru[EMA_DOWNThru >= 0] <- 0
                ROC_EMA_DOWNThru <- ROC(EMA_DOWNThru,type = 'discrete')
                ROC_EMA_DOWNThru_Signal <- ROC_EMA_DOWNThru > 0
                ROC_EMA_DOWNThru_Signal[is.na(ROC_EMA_DOWNThru_Signal)] <-  F
                ROC_EMA_DOWNThru_Signal_Cont <- ContinuingPeriod(ROC_EMA_DOWNThru_Signal, Cont)
                b = sum(-ROC(TWII$Close)[ROC_EMA_DOWNThru_Signal_Cont$Signal])
                
                temp_Down[c1,c2] = b 
                
        }
}

# volcano is a numeric matrix that ships with R
aa = rownames(temp_Up)
bb = colnames(temp_Up)
tt = list(a = aa,b = bb,c = temp_Up)
a <- plot_ly(x=tt$a, y = tt$b , z = tt$c) %>% add_surface()

aa = rownames(temp_Down)
bb = colnames(temp_Down)
tt = list(a = aa,b = bb,c = temp_Down)
b <- plot_ly(x=tt$a, y = tt$b , z = tt$c) %>% add_surface()



##BBand
N = seq(5,40,5)
lenN = length(N)
UpB = .9
DownB = .1
Lag = seq(1,9,1)
lenLag = length(Lag)

temp_Up = matrix(0, nrow = lenN, ncol= lenLag,
                 dimnames = list(as.character(N),as.character(Lag)))
temp_Down = matrix(0, nrow = lenN, ncol= lenLag,
                   dimnames = list(as.character(N),as.character(Lag)))

c1 = 0
for(i in N){
        c2 = 0
        c1 = c1 + 1
        for(j in Lag){
                c2 = c2 + 1
                
                #當移動平均低於兩配標準差(DownBand)Lag天後，在隔天做多
                BBand <- TTR::BBands(cbind.xts(TWII$High,TWII$Low,TWII$Close), n = i)
                BBand_Signal_Down <- BBand$pctB < DownB
                BBand_Signal_Down_Lag <- lag(BBand_Signal_Down,j)
                BBand_Signal_Down_Lag[is.na(BBand_Signal_Down_Lag)] <-  F
                a = sum(ROC(TWII$Close)[BBand_Signal_Down_Lag])
                
                temp_Up[c1,c2] = a
                
                #當移動平均超過兩倍標準差(UpBand)Lag天後，在隔天放空
                BBand <- TTR::BBands(cbind.xts(TWII$High,TWII$Low,TWII$Close), n = i)
                BBand_Signal_Up <- BBand$pctB > UpB
                BBand_Signal_Up_Lag <- lag(BBand_Signal_Up,j)
                BBand_Signal_Up_Lag[is.na(BBand_Signal_Up_Lag)] <-  F
                b = sum(-ROC(TWII$Close)[BBand_Signal_Up_Lag])
                
                temp_Down[c1,c2] = b 
                
        }
}

# volcano is a numeric matrix that ships with R
aa = rownames(temp_Up)
bb = colnames(temp_Up)
tt = list(a = aa,b = bb,c = temp_Up)
a <- plot_ly(x=tt$a, y = tt$b , z = tt$c) %>% add_surface()

aa = rownames(temp_Down)
bb = colnames(temp_Down)
tt = list(a = aa,b = bb,c = temp_Down)
b <- plot_ly(x=tt$a, y = tt$b , z = tt$c) %>% add_surface()


##RSI
UpRsi = 80
DownRsi = 35
N = 11
Lag = 3

#N天的RSI小於DownRsi時，在Lag天後做多
RSI <- TTR::RSI(TWII$Close,n = N)
RSI_Signal_Down <- RSI$EMA < DownRsi
RSI_Signal_Down_Lag <- lag(RSI_Signal_Down,Lag)
RSI_Signal_Down_Lag[is.na(RSI_Signal_Down_Lag)] <-  F
table(ROC(TWII$Close)[RSI_Signal_Down_Lag] > 0)
sum(ROC(TWII$Close)[RSI_Signal_Down_Lag])

#N天的RSI大於UpRsi時，在Lag天後做多
RSI <- TTR::RSI(TWII$Close,n = N)
RSI_Signal_Up <- RSI$EMA > UpRsi
RSI_Signal_Up_Lag <- lag(RSI_Signal_Up,Lag)
RSI_Signal_Up_Lag[is.na(RSI_Signal_Up_Lag)] <-  F
table(ROC(TWII$Close)[RSI_Signal_Up_Lag] > 0)
sum(-ROC(TWII$Close)[RSI_Signal_Up_Lag])

##ADX
N = 5
Adx = 30

#當ADX大於Adx，且DI+向上穿越DI-持續Cont天時在隔天買進
ADX <- TTR::ADX(HLC = cbind.xts(TWII$High,TWII$Low,TWII$Close) ,n = N)
ADX$Criteria <- (ADX$ADX > Adx) & (ADX$DIp > ADX$DIn)
ADX$Criteria = lag(ADX$Criteria)
ADX$Criteria[is.na(ADX$Criteria)] = F
ADX_Signal_UpTru <- ADX$Criteria
table(ROC(TWII$Close)[ADX_Signal_UpTru==1] > 0)
sum(ROC(TWII$Close)[ADX_Signal_UpTru==1])


#當ADX大於Adx，且DI+向下穿越DI-持續Cont天時在隔天賣進
ADX <- TTR::ADX(HLC = cbind.xts(TWII$High,TWII$Low,TWII$Close) ,n = N)
ADX$Criteria <- ADX$ADX>25 & (ADX$DIp < ADX$DIn)
ADX$Criteria = lag(ADX$Criteria)
ADX$Criteria[is.na(ADX$Criteria)] = F
ADX_Signal_DownTru <- ADX$Criteria
table(ROC(TWII$Close)[ADX_Signal_DownTru==1] < 0)
sum(-ROC(TWII$Close)[ADX_Signal_DownTru==1])

##CCI
UpCCI = 100 
DownCCI = -100
N = 11
Lag = 12

#CCI大於Up的天數為Cont天，在隔天買進
CCI <- CCI(HLC = cbind.xts(TWII$High,TWII$Low,TWII$Close),n =  N)
CCI_Signal_Up <- CCI$cci > UpCCI
CCI_Signal_Up_Lag <- lag(CCI_Signal_Up,Lag)
CCI_Signal_Up_Lag[is.na(CCI_Signal_Up_Lag)] <- F
table(ROC(TWII$Close)[CCI_Signal_Up_Lag] > 0)
sum(ROC(TWII$Close)[CCI_Signal_Up_Lag])

#CCI小於Down的天數為Cont天，在隔天賣出
CCI <- CCI(HLC = cbind.xts(TWII$High,TWII$Low,TWII$Close),n =  N)
CCI_Signal_Down <- CCI$cci < DownCCI
CCI_Signal_Down_Lag <- lag(CCI_Signal_Down,Lag)
CCI_Signal_Down_Lag[is.na(CCI_Signal_Down_Lag)] <- F
table(ROC(TWII$Close)[CCI_Signal_Down_Lag] < 0)
sum(-ROC(TWII$Close)[CCI_Signal_Down_Lag])

##aroon
N = 3
aroon <- 70
Aroon <- aroon(cbind.xts(TWII$High,TWII$Low),N)

#aroonUp大於aroonDn且aroonUp大於aroon時在隔天買進
Aroon_UpTru <- (Aroon$aroonUp > Aroon$aroonDn) & Aroon$aroonUp > aroon
Aroon_UpTru_Lag <- lag(Aroon_UpTru)
Aroon_UpTru_Lag[is.na(Aroon_UpTru_Lag)] <- F
table(ROC(TWII$Close)[Aroon_UpTru_Lag] > 0)
sum(ROC(TWII$Close)[Aroon_UpTru_Lag])

#aroonDn大於aroonUp且aroonDn大於aroon時在隔天賣出
Aroon_DownTru <- (Aroon$aroonDn > Aroon$aroonUp) & Aroon$aroonDn > aroon
Aroon_DownTru_Lag <- lag(Aroon_DownTru)
Aroon_DownTru_Lag[is.na(Aroon_DownTru_Lag)] <- F
table(ROC(TWII$Close)[Aroon_DownTru_Lag] < 0)
sum(-ROC(TWII$Close)[Aroon_DownTru_Lag])

##EMV價量合成指標
EMV <- EMV(cbind.xts(TWII$High,TWII$Low),TWII$Volume,N)
#EMV大於0時隔天買進
EMV_UpThru <- EMV$emv > 0
EMV_UpThru_Lag <- lag(EMV_UpThru)
EMV_UpThru_Lag[is.na(EMV_UpThru_Lag)] <-  F
table(ROC(TWII$Close)[EMV_UpThru_Lag] > 0 )
sum(ROC(TWII$Close)[EMV_UpThru_Lag])

#EMV小於0時隔天賣出
EMV_DownThru <- EMV$emv < 0
EMV_DownThru_Lag <- lag(EMV_DownThru)
EMV_DownThru_Lag[is.na(EMV_DownThru_Lag)] <-  F
table(ROC(TWII$Close)[EMV_DownThru_Lag] < 0 )
sum(-ROC(TWII$Close)[EMV_DownThru_Lag])

##ChaikinVolatility
N = 10
ChaikinVolatility <- chaikinVolatility(cbind.xts(TWII$High,TWII$Low),N)

##MFI
N = 14
MFI <- MFI(cbind.xts(TWII$High,TWII$Low,TWII$Close),TWII$Volume,N)

##CMO 
N = 14
CMO(cbind.xts(TWII$Close,TWII$Volume),N)


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












