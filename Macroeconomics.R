library(data.table)
library(TTR)
library(quantmod)
library(data.table)
library(dplyr)

####Self-Function####
Fixed_ROC <- function(x){
        temp = ROC(x[!is.na(x)], type = 'discrete')
        x[!is.na(x)] = temp
        return(x)
}

YearOverYear<-function (x,periodsPerYear){
        if(NROW(x)<=periodsPerYear){
                stop("too few rows")
        }
        else{
                indexes<-1:(NROW(x)-periodsPerYear)
                return(c(rep(NA,periodsPerYear),(x[indexes+periodsPerYear]-x[indexes])/x[indexes]))
        }
}
####Import Data####
###日頻資料###
DT_MarketData <- data.frame(TWII_UpDown_Daily = (data.frame(TWII_UpDown_Daily))$Close)
DT_MarketData <- xts(x = DT_MarketData, order.by = index(TWII_UpDown_Daily))
##三大法人 
setwd('D:\\中山大學財管所\\畢業論文\\Data\\三大法人')
LegalPerson <- fread('三大法人買賣.csv')
Date = LegalPerson$Date
LegalPerson_G <- replace(LegalPerson %>% select(-Date) , colnames(sapply(LegalPerson %>% select(-Date),Fixed_ROC)),
        sapply(LegalPerson %>% select(-Date),Fixed_ROC))
LegalPerson_G <- sapply(LegalPerson_G, function(x) as.double(gsub(Inf,0,x)))
LegalPerson_G <- xts(x = LegalPerson_G  , order.by = as.Date(Date))
LegalPerson <- xts(LegalPerson %>% select(-1),order.by = as.Date(Date))
LegalPerson_Logical <- merge.xts(LegalPerson_G > 0)
LegalPerson_Logical <- lag.xts(LegalPerson_Logical)
LegalPerson_Logical = merge.xts(TWII_UpDown_Daily,LegalPerson_Logical)
names(LegalPerson_Logical)[1] = 'TWII_UpDown_Daily'

DT_LegalPerson <- merge.xts(LegalPerson, LegalPerson_G[-1,])
DT_LegalPerson <- lag.xts(DT_LegalPerson)
DT_LegalPerson = merge.xts(TWII_UpDown_Daily,DT_LegalPerson)
names(DT_LegalPerson)[1] = 'TWII_UpDown_Daily'

LegalPerson_Logical <- LegalPerson_Logical['2000::2016']
DT_LegalPerson <- DT_LegalPerson['2000::2016']

TechnicalIndex <- merge.xts(TechnicalIndex,LegalPerson_Logical)
DT_MarketData <- merge.xts(DT_MarketData, DT_LegalPerson)

##台指期(近月期貨)
setwd('D:\\中山大學財管所\\畢業論文\\Data\\期貨')
Futures <- fread('台指期.csv')
Futures <- Futures 
Date = Futures$Date
Futures_G <- replace(Futures %>% select(-Date) , colnames(sapply(Futures %>% select(-Date),ROC,type = 'discrete')),
                       sapply(Futures %>% select(-Date),ROC,type='discrete'))
Futures_G <- xts(x = Futures_G  , order.by = as.Date(Date))
Futures <- xts(Futures %>% select(-1),order.by = as.Date(Date))
Futures_Logical <- merge.xts(Futures_G > 0)
Futures_Logical <- lag.xts(Futures_Logical)
Futures_Logical = na.fill(Futures_Logical,FALSE)

Futures_Logical = merge.xts(TWII_UpDown_Daily,Futures_Logical)
names(Futures_Logical)[1] = 'TWII_UpDown_Daily'

DT_Futures <- merge.xts(Futures, Futures_G)
DT_Futures <- lag.xts(DT_Futures)
DT_Futures = merge.xts(TWII_UpDown_Daily,DT_Futures)
names(DT_Futures)[1] = 'TWII_UpDown_Daily'

Futures_Logical <- Futures_Logical['2000::2016']
DT_Futures <- DT_Futures['2000::2016']

TechnicalIndex <- merge.xts(TechnicalIndex,Futures_Logical)
DT_MarketData <- merge.xts(DT_MarketData, DT_Futures)

##市場資料
setwd('D:\\中山大學財管所\\畢業論文\\Data\\台灣股市資料')
TaiwanStock <- fread('台灣加權指數.csv')
TaiwanStock <- TaiwanStock %>% select(-`報酬率％`)
Date = TaiwanStock$Date
TaiwanStock_G <- replace(TaiwanStock %>% select(-Date) , colnames(sapply(TaiwanStock %>% select(-Date),ROC,type = 'discrete')),
                   sapply(TaiwanStock %>% select(-Date),ROC,type='discrete'))
TaiwanStock_G <- xts(x = TaiwanStock_G  , order.by = as.Date(Date))
TaiwanStock <- xts(TaiwanStock %>% select(-1),order.by = as.Date(Date))
TaiwanStock_Logical <- merge.xts(TaiwanStock_G > 0)
TaiwanStock_Logical <- lag.xts(TaiwanStock_Logical)
TaiwanStock_Logical = merge.xts(TWII_UpDown_Daily,TaiwanStock_Logical)
names(TaiwanStock_Logical)[1] = 'TWII_UpDown_Daily'

DT_TaiwanStock <- merge.xts(TaiwanStock, TaiwanStock_G)
DT_TaiwanStock <- lag.xts(DT_TaiwanStock)
DT_TaiwanStock = merge.xts(TWII_UpDown_Daily,DT_TaiwanStock)
names(DT_TaiwanStock)[1] = 'TWII_UpDown_Daily'

TaiwanStock_Logical <- TaiwanStock_Logical['2000::2016']
DT_TaiwanStock <- DT_TaiwanStock['2000::2016']

TechnicalIndex <- merge.xts(TechnicalIndex,TaiwanStock_Logical)
DT_MarketData <- merge.xts(DT_MarketData, DT_TaiwanStock)
               
##各國指數
setwd('D:\\中山大學財管所\\畢業論文\\Data\\各國股價指數')
StockIndex <- fread('各國股價指數.csv')
Date = StockIndex$Date
StockIndex_G <- replace(StockIndex %>% select(-Date) , colnames(sapply(StockIndex %>% select(-Date),Fixed_ROC)),
                       sapply(StockIndex %>% select(-Date),Fixed_ROC))
StockIndex_G <- xts(x = StockIndex_G  , order.by = as.Date(Date))
StockIndex <- xts(StockIndex %>% select(-1),order.by = as.Date(Date))
StockIndex_Logical <- merge.xts(StockIndex_G > 0)
StockIndex_Logical <- lag.xts(StockIndex_Logical)
StockIndex_Logical = merge.xts(TWII_UpDown_Daily,StockIndex_Logical)
names(StockIndex_Logical)[1] = 'TWII_UpDown_Daily'

DT_StockIndex <- merge.xts(StockIndex, StockIndex_G)
DT_StockIndex <- lag.xts(DT_StockIndex)
DT_StockIndex = merge.xts(TWII_UpDown_Daily,DT_StockIndex)
names(DT_StockIndex)[1] = 'TWII_UpDown_Daily'

StockIndex_Logical <- StockIndex_Logical['2000::2016']
DT_StockIndex <- DT_StockIndex['2000::2016']

StockIndex_Logical <- na.fill(StockIndex_Logical,FALSE)

TechnicalIndex <- merge.xts(TechnicalIndex,StockIndex_Logical)
DT_MarketData <- merge.xts(DT_MarketData, DT_StockIndex)

        
###月頻資料###
DT_Macro <- data.frame(TWII_UpDown_Monthly = (data.frame(TWII_UpDown_Monthly))$Close)
DT_Macro <- xts(x = DT_Macro, order.by = index(TWII_UpDown_Monthly))

##台灣總體經濟
setwd('D:\\中山大學財管所\\畢業論文\\Data\\台灣總體經濟資料')
MacroTaiwan <- fread('MacroTaiwan.csv')
Date = MacroTaiwan$Date
MacroTaiwan_G <- replace(MacroTaiwan %>% select(-Date) , colnames(sapply(MacroTaiwan %>% select(-Date),ROC,type = 'discrete')),
                         sapply(MacroTaiwan %>% select(-Date),ROC,type='discrete'))
MacroTaiwan_G <- xts(x = MacroTaiwan_G , order.by = as.Date(Date))
MacroTaiwan_G <- na.fill(MacroTaiwan_G,0)

MacroTaiwan_YoY <- replace(MacroTaiwan %>% select(-Date) , colnames(sapply(MacroTaiwan %>% select(-Date),YearOverYear,12)),
                                            sapply(MacroTaiwan %>% select(-Date),YearOverYear,12))
MacroTaiwan_YoY <- xts(x = MacroTaiwan_YoY , order.by = as.Date(Date))
MacroTaiwan_YoY <- na.fill(MacroTaiwan_YoY,0)

MacroTaiwan <- xts(MacroTaiwan %>% select(-1),order.by = as.Date(Date))

MacroTaiwan_Logical <- merge.xts(MacroTaiwan_G > 0, MacroTaiwan_YoY > 0)
MacroTaiwan_Logical <- lag.xts(MacroTaiwan_Logical,2) #延遲兩個月，以保證所有的總經資料已經公布
MacroTaiwan_Logical = merge.xts(TWII_UpDown_Monthly,MacroTaiwan_Logical)
names(MacroTaiwan_Logical)[1] = 'TWII_UpDown_Monthly'

DT_MacroTaiwan <- merge.xts(MacroTaiwan, MacroTaiwan_G, MacroTaiwan_YoY)
DT_MacroTaiwan <- lag.xts(DT_MacroTaiwan,2) #延遲兩個月，以保證所有的總經資料已經公布
DT_MacroTaiwan = merge.xts(TWII_UpDown_Monthly,DT_MacroTaiwan)
names(DT_MacroTaiwan)[1] = 'TWII_UpDown_Monthly'

MacroTaiwan_Logical <- MacroTaiwan_Logical['2000::2016']
DT_MacroTaiwan <- DT_MacroTaiwan['2000::2016']

MacroTaiwan_Logical <- na.fill(MacroTaiwan_Logical,FALSE)
DT_MacroTaiwan <- na.fill(DT_MacroTaiwan,FALSE)

TechnicalIndex <- merge.xts(TechnicalIndex,MacroTaiwan_Logical)
DT_Macro <- merge.xts(DT_Macro,DT_MacroTaiwan)

##美國總體經濟
setwd('D:\\中山大學財管所\\畢業論文\\Data\\美國總體經濟資料')
MacroAmerica <- fread("MacroAmerica.csv")
Date = MacroAmerica$Date
MacroAmerica_G <- replace(MacroAmerica %>% select(-Date) , colnames(sapply(MacroAmerica %>% select(-Date),ROC,type = 'discrete')),
                         sapply(MacroAmerica %>% select(-Date),ROC,type='discrete'))
MacroAmerica_G <- xts(x = MacroAmerica_G , order.by = as.Date(Date))
MacroAmerica_G <- na.fill(MacroAmerica_G,0)

MacroAmerica_YoY <- replace(MacroAmerica %>% select(-Date) , colnames(sapply(MacroAmerica %>% select(-Date),YearOverYear,12)),
                           sapply(MacroAmerica %>% select(-Date),YearOverYear,12))
MacroAmerica_YoY <- xts(x = MacroAmerica_YoY , order.by = as.Date(Date))
MacroAmerica_YoY <- na.fill(MacroAmerica_YoY,0)

MacroAmerica <- xts(MacroAmerica %>% select(-1),order.by = as.Date(Date))

MacroAmerica_Logical <- merge.xts(MacroAmerica_G > 0, MacroAmerica_YoY > 0)
MacroAmerica_Logical <- lag.xts(MacroAmerica_Logical,2) #延遲兩個月，以保證所有的總經資料已經公布
MacroAmerica_Logical = merge.xts(TWII_UpDown_Monthly,MacroAmerica_Logical)
names(MacroAmerica_Logical)[1] = 'TWII_UpDown_Monthly'

DT_MacroAmerica <- merge.xts(MacroAmerica, MacroAmerica_G, MacroAmerica_YoY)
DT_MacroAmerica <- lag.xts(DT_MacroAmerica,2) #延遲兩個月，以保證所有的總經資料已經公布
DT_MacroAmerica = merge.xts(TWII_UpDown_Monthly,DT_MacroAmerica)
names(DT_MacroAmerica)[1] = 'TWII_UpDown_Monthly'

MacroAmerica_Logical <- MacroAmerica_Logical['2000::2016']
DT_MacroAmerica <- DT_MacroAmerica['2000::2016']

MacroAmerica_Logical <- na.fill(MacroAmerica_Logical,FALSE)
DT_MacroAmerica <- na.fill(DT_MacroAmerica,FALSE)

TechnicalIndex <- merge.xts(TechnicalIndex,MacroAmerica_Logical)
DT_Macro <- merge.xts(DT_Macro,DT_MacroAmerica)

##商品原物料
setwd('D:\\中山大學財管所\\畢業論文\\Data\\商品原物料')
Commodity <- fread("Commodity.csv")

Date = Commodity$Date
Commodity_G <- replace(Commodity %>% select(-Date) , colnames(sapply(Commodity %>% select(-Date),ROC,type = 'discrete')),
                          sapply(Commodity %>% select(-Date),ROC,type='discrete'))
Commodity_G <- xts(x = Commodity_G , order.by = as.Date(Date))
Commodity_G <- na.fill(Commodity_G,0)

Commodity_YoY <- replace(Commodity %>% select(-Date) , colnames(sapply(Commodity %>% select(-Date),YearOverYear,12)),
                            sapply(Commodity %>% select(-Date),YearOverYear,12))
Commodity_YoY <- xts(x = Commodity_YoY , order.by = as.Date(Date))
Commodity_YoY <- na.fill(Commodity_YoY,0)

Commodity <- xts(Commodity %>% select(-1),order.by = as.Date(Date))

Commodity_Logical <- merge.xts(Commodity_G > 0, Commodity_YoY > 0)
Commodity_Logical <- lag.xts(Commodity_Logical,2) #延遲兩個月，以保證所有的總經資料已經公布
Commodity_Logical = merge.xts(TWII_UpDown_Monthly,Commodity_Logical)
names(Commodity_Logical)[1] = 'TWII_UpDown_Monthly'

DT_Commodity <- merge.xts(Commodity, Commodity_G, Commodity_YoY)
DT_Commodity <- lag.xts(DT_Commodity,2) #延遲兩個月，以保證所有的總經資料已經公布
DT_Commodity = merge.xts(TWII_UpDown_Monthly,DT_Commodity)
names(DT_Commodity)[1] = 'TWII_UpDown_Monthly'

Commodity_Logical <- Commodity_Logical['2000::2016']
DT_Commodity <- DT_Commodity['2000::2016']

Commodity_Logical <- na.fill(Commodity_Logical,FALSE)
DT_Commodity <- na.fill(DT_Commodity,FALSE)

TechnicalIndex <- merge.xts(TechnicalIndex,Commodity_Logical)
DT_Macro <- merge.xts(DT_Macro,DT_Commodity)

