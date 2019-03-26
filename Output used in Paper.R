library(stargazer)

####Self Function####
Calc_Cont_Period <- function(Data){
        days = nrow(Data)
        Data$IfUp[1] = FALSE
        n1 = 0
        n2 = 0
        n3 = 0
        n4 = 0
        n5 = 0
        n6 = 0
        n7 = 0
        n8 = 0
        n9 = 0
        n10 = 0
        n11 = 0
        n12 = 0
        for(i in 2:(days-1)){
                if((Data$IfUp[i] == TRUE & Data$IfUp[i+1] == FALSE & Data$IfUp[i-1] == FALSE)){
                        n1 = n1 + 1
                }
        }        
        for(i in 2:(days-2)){
                if(Data$IfUp[i] ==TRUE & Data$IfUp[i-1] ==FALSE & Data$IfUp[i+1] ==TRUE & Data$IfUp[i+2] == FALSE){
                        n2 = n2 + 1
                }
        }        
        
        for(i in 2:(days-3)){
                if(Data$IfUp[i] ==TRUE & Data$IfUp[i-1] ==FALSE & all(Data$IfUp[(i+1):(i+2)] == TRUE) & Data$IfUp[i+3] == FALSE){
                        n3 = n3 + 1
                }
        }
        
        for(i in 2:(days-4)){
                if(Data$IfUp[i] ==TRUE & Data$IfUp[i-1] ==FALSE & all(Data$IfUp[(i+1):(i+3)] == TRUE) & Data$IfUp[i+4] == FALSE){
                        n4 = n4 + 1
                }
        }
        
        for(i in 2:(days-5)){
                if(Data$IfUp[i] ==TRUE & Data$IfUp[i-1] ==FALSE & all(Data$IfUp[(i+1):(i+4)] == TRUE) & Data$IfUp[i+5] == FALSE){
                        n5 = n5 + 1
                }
        }
        
        for(i in 2:(days-6)){
                if(Data$IfUp[i] ==TRUE & Data$IfUp[i-1] ==FALSE & all(Data$IfUp[(i+1):(i+5)] == TRUE) & Data$IfUp[i+6] == FALSE){
                        n6 = n6 + 1
                }
        }
        
        for(i in 2:(days-7)){
                if(Data$IfUp[i] ==TRUE & Data$IfUp[i-1] ==FALSE & all(Data$IfUp[(i+1):(i+6)] == TRUE) & Data$IfUp[i+7] == FALSE){
                        n7 = n7 + 1
                }
        }
        
        for(i in 2:(days-8)){
                if(Data$IfUp[i] ==TRUE & Data$IfUp[i-1] ==FALSE & all(Data$IfUp[(i+1):(i+7)] == TRUE) & Data$IfUp[i+8] == FALSE){
                        n8 = n8 + 1
                }
        }
        
        for(i in 2:(days-9)){
                if(Data$IfUp[i] ==TRUE & Data$IfUp[i-1] ==FALSE & all(Data$IfUp[(i+1):(i+8)] == TRUE) & Data$IfUp[i+9] == FALSE){
                         n9 = n9 + 1
                }
        }
        
        for(i in 2:(days-10)){
                if(Data$IfUp[i] ==TRUE & Data$IfUp[i-1] ==FALSE & all(Data$IfUp[(i+1):(i+9)] == TRUE) & Data$IfUp[i+10] == FALSE){
                        n10 = n10 + 1
                }
        }
        
        for(i in 2:(days-11)){
                if(Data$IfUp[i] ==TRUE & Data$IfUp[i-1] ==FALSE & all(Data$IfUp[(i+1):(i+10)] == TRUE) & Data$IfUp[i+11] == FALSE){
                        n11 = n11 + 1
                }
        }
        
        for(i in 2:(days-12)){
                if(Data$IfUp[i] ==TRUE & Data$IfUp[i-1] ==FALSE & all(Data$IfUp[(i+1):(i+11)] == TRUE) & Data$IfUp[i+12] == FALSE){
                        n12 = n12 + 1
                }
        }
        return(data.frame(n1 = n1, n2 = n2, n3=n3,n4=n4,n5=n5,n6=n6,n7=n7,n8=n8,n9=n9,n10=n10,n11=n11,n12=n12))
}

####Statistics####
##TWII Statistics
setwd('D:\\中山大學財管所\\畢業論文\\Data\\美國台灣指數Daily Data')
TWII <- fread('TWII.csv')
TWII <- xts(x = TWII %>% select(-Date), order.by = as.Date(TWII$Date))
TWII = TWII['1999-12-31::2016-12-31'] 

setwd("D:\\中山大學財管所\\畢業論文\\輸出圖檔")
apply.yearly(((TWII$Close %>% ROC(type = 'discrete'))*100) ,FUN = function(x) stargazer(x %>% data.frame(),type='html')) %>% write('TWII_Stat(4.1).html')
stargazer((((TWII["1999-12-31::2016-12-31"])$Close %>% ROC(type = 'discrete'))) %>% data.frame() , type = 'html') %>% write('TWII_Stat(4.1).html')

##TWII boxplot
setwd('D:\\中山大學財管所\\畢業論文\\Data\\美國台灣指數Daily Data')
TWII <- fread('TWII.csv')
TWII <- xts(x = TWII %>% select(-Date), order.by = as.Date(TWII$Date))
TWII = TWII["2000::2016"] %>% data.frame()
Date = substr(rownames(TWII),1,4)
TWII_Boxplot <- TWII %>% select(Close) %>% ROC(type='discrete') %>% mutate(Date = Date) %>% 
        ggplot(aes(x=factor(Date),y=Close)) + geom_boxplot() +
        xlab('') + ylab('Rate of Return') +
        theme(
                panel.background=element_rect(fill='#FFFFFF'),
                axis.title = element_text(size=rel(2),color="black"),
                axis.text = element_text(size = rel(2),color="black")
        )
setwd("D:\\中山大學財管所\\畢業論文\\輸出圖檔")
ggsave(paste0('TWII_boxplot','.png'),width = 40,height = 18,dpi = 500,units = 'cm',plot = TWII_Boxplot)

##TWII 漲跌持續時間
setwd('D:\\中山大學財管所\\畢業論文\\Data\\美國台灣指數Daily Data')
TWII <- fread('TWII.csv')
TWII <- xts(x = TWII %>% select(-Date), order.by = as.Date(TWII$Date))
TWII = TWII["2000::2016"] %>% data.frame()
Date = substr(rownames(TWII),1,4)
TWII_Cont_Period <- TWII %>% select(Close) %>% ROC(type='discrete') %>% mutate(Date = Date, IfUp = Close>=0)

setwd("D:\\中山大學財管所\\畢業論文\\輸出圖檔")
sink('Cont_Period.txt')
for(i in (2000:2016)){
        TWII_Cont_Period %>% filter(Date == i) %>% Calc_Cont_Period() %>% print()
}                            

Calc_Cont_Period(TWII_Cont_Period)
sink()

##TWII 上漲報酬分配
library(ggridges)
setwd('D:\\中山大學財管所\\畢業論文\\Data\\美國台灣指數Daily Data')
TWII <- fread('TWII.csv')
TWII <- xts(x = TWII %>% select(-Date), order.by = as.Date(TWII$Date))
TWII = TWII["2000::2016"] %>% data.frame()
Date = substr(rownames(TWII),1,4)
TWII_density <- TWII %>% select(Close) %>% ROC(type='discrete') %>% mutate(Date = Date) %>% 
        ggplot(aes(x=Close,y=factor(Date))) + 
        geom_density_ridges_gradient(
                aes(fill = ..x..), scale = 3, size = 0.3
        )+
        scale_fill_gradientn(
                colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF")
        )+
        xlab('') + ylab('') +
        theme(
                panel.background=element_rect(fill='#FFFFFF'),
                axis.title = element_text(size=rel(2),color="black"),
                axis.text = element_text(size = rel(2),color="black"),
                legend.position = ' '
        )
setwd("D:\\中山大學財管所\\畢業論文\\輸出圖檔")
ggsave(paste0('TWII_density','.png'),width = 40,height = 18,dpi = 500,units = 'cm',plot = TWII_density)


##TWII 偏態峰態係數
library(psych)  
setwd('D:\\中山大學財管所\\畢業論文\\Data\\美國台灣指數Daily Data')
TWII <- fread('TWII.csv')
TWII <- xts(x = TWII %>% select(-Date), order.by = as.Date(TWII$Date))
TWII = TWII["2000::2016"] %>% data.frame()
Date = substr(rownames(TWII),1,4)
TWII <- TWII %>% select(Close) %>% ROC(type='discrete') %>% mutate(Date = Date)

setwd("D:\\中山大學財管所\\畢業論文\\輸出圖檔")
sink('Skewness&kurtosis.txt')
for(i in (2000:2016)){
        TWII %>% filter(Date == i) %>% select(Close) %>% describe() %>% print()
}                            
sink()











