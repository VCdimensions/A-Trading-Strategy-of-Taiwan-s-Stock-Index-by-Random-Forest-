library(TTR)
library(quantmod)
library(data.table)
library(dplyr)
library(plotly)

setwd('D:\\���s�j�ǰ]�ީ�\\���~�פ�\\Data\\����x�W����Daily Data')

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
##OBV:OBV�s��Cont���ܰʲv�j��s
Cont = seq(0,8,1)

temp_Up = matrix(0,ncol= len, nrow = 1,
                  dimnames = list(as.character('SumROC'),as.character(Cont)))
temp_Down = matrix(0,ncol= len, nrow = 1,
                 dimnames = list(as.character('SumROC'),as.character(Cont)))
len = length(Cont)

for(i in Cont){
        
        
        #OBV�s��Cont���ܰʲv�j��s�b�j�ѶR�i
        OBV <- TTR::OBV(price = TWII$Close, volume = TWII$Volume)
        ROC_OBV <- AdjROC(OBV)
        ROC_OBV_Signal <-  ROC_OBV > 0
        ROC_OBV_Signal[is.na(ROC_OBV_Signal)] <-  F
        ROC_OBV_Signal_Cont <- ContinuingPeriod(ROC_OBV_Signal,i)
        table(ROC(TWII$Close)[ROC_OBV_Signal_Cont$Signal] > 0)
        a = sum(ROC(TWII$Close)[ROC_OBV_Signal_Cont$Signal])
        
        temp_Up[,i+1] = a
        
        #OBV�s��Cont���ܰʲv�j��s�b�j�ѩ��
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

##MACD:DIF�PMACD���t���ܰʲv�s��|�Ѥj��Cont
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
                #MACD:DIF�PMACD���t���ܰʲv�s��|�Ѥj��Cont�b�j�ѶR�i
                MACD <- TTR::MACD(TWII, nFast = i, nSlow = j, nSig = Sig)
                ROC_MACD <- AdjROC(MACD$macd-MACD$signal)
                Cond1_MACD <- (MACD$macd > 0) & (MACD$signal > 0)
                ROC_MACD_Signal <- (ROC_MACD > 0) & (Cond1_MACD == T)
                ROC_MACD_Signal[is.na(ROC_MACD_Signal)] <- F
                ROC_MACD_Signal_Cont <- ContinuingPeriod(ROC_MACD_Signal,Cont)
                table(ROC(TWII$Close)[ROC_MACD_Signal_Cont$Signal] > 0)
                a = sum(ROC(TWII$Close)[ROC_MACD_Signal_Cont$Signal])
                
                temp_Up[c1,c2] = a
                #MACD:DIF�PMACD���t���ܰʲv�s��|�Ѥj��Cont�b�j�ѩ��
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
                zaxis = list(title = "�ֿn���S"))
        ) %>%
        layout(showlegend = FALSE)
#plotly_IMAGE(a, format = "png",width = 2000,height = 900,dpi = 500,units = 'cm', out_file = "MACD_Sig2.png")

api_create(a, filename = "�޳N���R�ѼƳ̨Τ�(MACD_Sig5)")

# aa = rownames(temp_Down)
# bb = colnames(temp_Down)
# tt = list(a = aa, b = bb, c = temp_Down)
# b <- plot_ly(x=tt$a, y = tt$b , z = tt$c) %>% add_surface()
# 
# # a <- plot_ly(z = ~ temp_Up) %>% add_surface()
# # b <- plot_ly(z = ~ temp_Down) %>% add_surface()


##SMA�ֽu�V�W��V����Cont��
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
                #�ֽu�V�W��V����Cont�ѡA�h�b�j�ѶR�i
                SMA_UPThru <- SMA_Fast - SMA_Slow
                SMA_UPThru[SMA_UPThru <= 0] <- 0
                ROC_SMA_UPThru <- ROC(SMA_UPThru,type = 'discrete')
                ROC_SMA_UPThru_Signal <- ROC_SMA_UPThru > 0
                ROC_SMA_UPThru_Signal[is.na(ROC_SMA_UPThru_Signal)] <-  F
                ROC_SMA_UPThru_Signal_Cont <- ContinuingPeriod(ROC_SMA_UPThru_Signal, Cont)
                a = sum(ROC(TWII$Close)[ROC_SMA_UPThru_Signal_Cont$Signal])
                
                temp_Up[c1,c2] = a
                #�ֽu�V�U��V����Cont�ѡA�h�b�j�ѩ��
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



##VWMA:(�W���q�`�q�j�ҥH�|�ܰʸ��֡A�U�^�q�`�q�p�ҥH�ܰʸ��C)
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
                #�ֽu�V�W��V����Cont�ѡA�h�b�j�ѶR�i
                VWMA_UPThru <- VWMA_Fast - VWMA_Slow
                VWMA_UPThru[VWMA_UPThru <= 0] <- 0
                ROC_VWMA_UPThru <- ROC(VWMA_UPThru,type = 'discrete')
                ROC_VWMA_UPThru_Signal <- ROC_VWMA_UPThru > 0
                ROC_VWMA_UPThru_Signal[is.na(ROC_VWMA_UPThru_Signal)] <-  F
                ROC_VWMA_UPThru_Signal_Cont <- ContinuingPeriod(ROC_VWMA_UPThru_Signal, Cont)
                a = sum(ROC(TWII$Close)[ROC_VWMA_UPThru_Signal_Cont$Signal])

                temp_Up[c1,c2] = a
                #�ֽu�V�U��V����Cont�ѡA�h�b�j�ѩ��
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
                #�ֽu�V�W��V����Cont�ѡA�h�b�j�ѶR�i
                EMA_UPThru <- EMA_Fast - EMA_Slow
                EMA_UPThru[EMA_UPThru <= 0] <- 0
                ROC_EMA_UPThru <- ROC(EMA_UPThru,type = 'discrete')
                ROC_EMA_UPThru_Signal <- ROC_EMA_UPThru > 0
                ROC_EMA_UPThru_Signal[is.na(ROC_EMA_UPThru_Signal)] <-  F
                ROC_EMA_UPThru_Signal_Cont <- ContinuingPeriod(ROC_EMA_UPThru_Signal, Cont)
                a = sum(ROC(TWII$Close)[ROC_EMA_UPThru_Signal_Cont$Signal])
                
                temp_Up[c1,c2] = a

                #�ֽu�V�U��V����Cont�ѡA�h�b�j�ѩ��
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
                
                #�����ʥ����C���t�зǮt(DownBand)Lag�ѫ�A�b�j�Ѱ��h
                BBand <- TTR::BBands(cbind.xts(TWII$High,TWII$Low,TWII$Close), n = i)
                BBand_Signal_Down <- BBand$pctB < DownB
                BBand_Signal_Down_Lag <- lag(BBand_Signal_Down,j)
                BBand_Signal_Down_Lag[is.na(BBand_Signal_Down_Lag)] <-  F
                a = sum(ROC(TWII$Close)[BBand_Signal_Down_Lag])
                
                temp_Up[c1,c2] = a
                
                #�����ʥ����W�L�⭿�зǮt(UpBand)Lag�ѫ�A�b�j�ѩ��
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

#N�Ѫ�RSI�p��DownRsi�ɡA�bLag�ѫᰵ�h
RSI <- TTR::RSI(TWII$Close,n = N)
RSI_Signal_Down <- RSI$EMA < DownRsi
RSI_Signal_Down_Lag <- lag(RSI_Signal_Down,Lag)
RSI_Signal_Down_Lag[is.na(RSI_Signal_Down_Lag)] <-  F
table(ROC(TWII$Close)[RSI_Signal_Down_Lag] > 0)
sum(ROC(TWII$Close)[RSI_Signal_Down_Lag])

#N�Ѫ�RSI�j��UpRsi�ɡA�bLag�ѫᰵ�h
RSI <- TTR::RSI(TWII$Close,n = N)
RSI_Signal_Up <- RSI$EMA > UpRsi
RSI_Signal_Up_Lag <- lag(RSI_Signal_Up,Lag)
RSI_Signal_Up_Lag[is.na(RSI_Signal_Up_Lag)] <-  F
table(ROC(TWII$Close)[RSI_Signal_Up_Lag] > 0)
sum(-ROC(TWII$Close)[RSI_Signal_Up_Lag])

##ADX
N = 5
Adx = 30

#��ADX�j��Adx�A�BDI+�V�W��VDI-����Cont�Ѯɦb�j�ѶR�i
ADX <- TTR::ADX(HLC = cbind.xts(TWII$High,TWII$Low,TWII$Close) ,n = N)
ADX$Criteria <- (ADX$ADX > Adx) & (ADX$DIp > ADX$DIn)
ADX$Criteria = lag(ADX$Criteria)
ADX$Criteria[is.na(ADX$Criteria)] = F
ADX_Signal_UpTru <- ADX$Criteria
table(ROC(TWII$Close)[ADX_Signal_UpTru==1] > 0)
sum(ROC(TWII$Close)[ADX_Signal_UpTru==1])


#��ADX�j��Adx�A�BDI+�V�U��VDI-����Cont�Ѯɦb�j�ѽ�i
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

#CCI�j��Up���ѼƬ�Cont�ѡA�b�j�ѶR�i
CCI <- CCI(HLC = cbind.xts(TWII$High,TWII$Low,TWII$Close),n =  N)
CCI_Signal_Up <- CCI$cci > UpCCI
CCI_Signal_Up_Lag <- lag(CCI_Signal_Up,Lag)
CCI_Signal_Up_Lag[is.na(CCI_Signal_Up_Lag)] <- F
table(ROC(TWII$Close)[CCI_Signal_Up_Lag] > 0)
sum(ROC(TWII$Close)[CCI_Signal_Up_Lag])

#CCI�p��Down���ѼƬ�Cont�ѡA�b�j�ѽ�X
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

#aroonUp�j��aroonDn�BaroonUp�j��aroon�ɦb�j�ѶR�i
Aroon_UpTru <- (Aroon$aroonUp > Aroon$aroonDn) & Aroon$aroonUp > aroon
Aroon_UpTru_Lag <- lag(Aroon_UpTru)
Aroon_UpTru_Lag[is.na(Aroon_UpTru_Lag)] <- F
table(ROC(TWII$Close)[Aroon_UpTru_Lag] > 0)
sum(ROC(TWII$Close)[Aroon_UpTru_Lag])

#aroonDn�j��aroonUp�BaroonDn�j��aroon�ɦb�j�ѽ�X
Aroon_DownTru <- (Aroon$aroonDn > Aroon$aroonUp) & Aroon$aroonDn > aroon
Aroon_DownTru_Lag <- lag(Aroon_DownTru)
Aroon_DownTru_Lag[is.na(Aroon_DownTru_Lag)] <- F
table(ROC(TWII$Close)[Aroon_DownTru_Lag] < 0)
sum(-ROC(TWII$Close)[Aroon_DownTru_Lag])

##EMV���q�X������
EMV <- EMV(cbind.xts(TWII$High,TWII$Low),TWII$Volume,N)
#EMV�j��0�ɹj�ѶR�i
EMV_UpThru <- EMV$emv > 0
EMV_UpThru_Lag <- lag(EMV_UpThru)
EMV_UpThru_Lag[is.na(EMV_UpThru_Lag)] <-  F
table(ROC(TWII$Close)[EMV_UpThru_Lag] > 0 )
sum(ROC(TWII$Close)[EMV_UpThru_Lag])

#EMV�p��0�ɹj�ѽ�X
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


####K�u���A####

##�]�����A
N = 10 #���q����N��
Nday = 10

Engulfing = TWII
Engulfing$Cond1 = F
Engulfing$Cond2 = F
Engulfing$Cond3 = F
#�X�{�]���b�j�ѶR�i�A�ë���Nday
for(i in 1:nrow(Engulfing)){
        if(i == nrow(Engulfing)-N){break}
        #Cond1���ѤW�� �Q�ѤU�^ ���Ѧ��L�j��Q�Ѷ}�L�M���L(�]��)
        if(Engulfing$Close[i+N-1] > Engulfing$Open[i+N-1] &&  Engulfing$Close[i+N-2] < Engulfing$Open[i+N-2] &&
           Engulfing$Close[i+N-1] >= max(Engulfing$Close[i+N-2],Engulfing$Open[i+N-2])){
                Engulfing$Cond1[i+N-1] = T
        }
        #Cond2����C�I
        if(min(Engulfing$Low[(i+N-3):(i+N-2)]) == min(Engulfing$Low[i:(i+N-1)]) &&
           max(Engulfing$Volume[(i+N-3):(i+N-2)]) == max(Engulfing$Volume[i:(i+N-1)])){
                Engulfing$Cond2[i+N-1] = T
        }
        #Cond3�W����
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











