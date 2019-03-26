##OBV
temp = xts(x = ROC_OBV, order.by = time(TWII)) %>% lag.xts() %>% merge.xts(TWII_UpDown_Daily)
#隔天股票上漲或下跌，則今天的指標平均為多少
temp %>% data.frame() %>% group_by(Close) %>% summarise(mean(.,na.rm =T))
#成長率超過一個門檻的指標，隔天股市上漲還下跌
temp %>% data.frame() %>% na.trim() %>% group_by(.>0.05,Close) %>% summarise(n = n())
#成長率超過一個門檻的指標，隔天上漲的報酬總和
temp1 = xts(x = ROC_OBV, order.by = time(TWII)) %>% lag.xts() %>% merge.xts(ROC(TWII$Close))
temp1 %>% data.frame() %>% na.trim() %>% group_by(.>.03) %>% summarise(n = n(),mean = mean(Close))

t1 = double(1)
for(i in seq(.001,.1,.001)){
        print(i)
        a = temp1 %>% data.frame() %>% na.trim() %>% group_by(.>i) %>% summarise(n = n(),mean = mean(Close)) %>% print()
        if((a$n[2] >=300) && ( a$mean[2] > t1)){
                t1 = a$mean[2]
                T_OBV = i
        }
}


##MACD
temp = xts(x = ROC_MACD, order.by = time(TWII)) %>% lag.xts() %>% merge.xts(TWII_UpDown_Daily)
#隔天股票上漲或下跌，則今天的指標平均為多少
temp %>% data.frame() %>% group_by(Close) %>% summarise(mean(.,na.rm =T))
#成長率超過一個門檻的指標，隔天股市上漲還下跌
temp %>% data.frame() %>% na.trim() %>% group_by(.>0.05,Close) %>% summarise(n = n())
#成長率超過一個門檻的指標，隔天上漲的報酬總和
temp1 = xts(x = ROC_MACD, order.by = time(TWII)) %>% lag.xts() %>% merge.xts(ROC(TWII$Close))
temp1 %>% data.frame() %>% na.trim() %>% group_by(.>.05) %>% summarise(n = n(),mean = mean(Close))


t1 = double(1)
for(i in seq(.001,.1,.001)){
        print(i)
        a = temp1 %>% data.frame() %>% na.trim() %>% group_by(.>i) %>% summarise(n = n(),mean = mean(Close)) %>% print()
        if((a$n[2] >=300) && ( a$mean[2] > t1)){
                t1 = a$mean[2]
                T_MACD = i
        }
}



