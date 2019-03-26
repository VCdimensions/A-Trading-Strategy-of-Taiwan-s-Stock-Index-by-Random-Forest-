Performance=function (tx){
        tx = data.frame(tx)
        profit=setNames(tx$Profit,rownames(tx) )	## profit vector
        
        DD=rep(0,length(profit))	## Draw Down
        topprofit=rep(profit[1],length(profit))	## temp maximum profit
        
        for (m in 2:length(profit)){
                if (sum(profit[1:m])>topprofit[m-1]){
                        topprofit[m:length(profit)]=sum(profit[1:m])} ## setting top profit
                
                DD[m]=sum(profit[1:m])-topprofit[m]	## current draw down
        }
        
        
        par(cex.lab=1.8)
        plot(DD,type="h",col="darkgreen",lwd=2,ylab="cumulative profit&loss",xlab="per trade",
             ylim=c(min(DD),max(cumsum(profit))))
        par(new=T,cex.lab=1.8)
        plot(cumsum(profit),type="h",col="Tomato",lwd=2,ylab="cumulative profit&loss",xlab="per trade",font=4,
             ylim=c(min(DD),max(cumsum(profit))))
        
        TPT=rep(1,1);i=1
        
        for (m in 2:length(profit)){
                if (topprofit[m]>topprofit[m-1]){points(m,topprofit[m],pch=4,col="purple")
                        TPT[i]=m
                        i=i+1}
        }
        
        DDT=rep(TPT[1],1)
        
        if (length(TPT)>=2){for (i in 2:length(TPT)){DDT[i]=TPT[i]-TPT[i-1]}}else{
                DDT=length(profit)}
        
        Cl=setNames((TWII$Close['2009::2016'] %>% data.frame())$Close,index(TWII$Close['2009::2016']))
        par(new=T)
        plot(y=a$Close,x=1:1981,type="l",col="black",lwd=3,xaxt = "n",yaxt = "n",xlab = '',ylab = '')
        par(new=T)
        plot(y=a$Close[which(myReturn!=0)]-1000,x = which(myReturn!=0),pch=17,cex=.5,col="red",xaxt = "n",yaxt = "n",xlab = '',ylab = '')

        axis(4,seq(min(Cl),max(Cl),by=100),col="black",lwd=2,font=2)
        cat("Total Profit:",sum(profit),"\n"
            ,"Trading Days:",length(profit),"\n"
            ,"Profit Per Trade:",sum(profit)/length(profit[profit!=0]),"\n"
            ,"# of Win:",length(profit[profit>0]),"\n"
            ,"Win Rate:",length(profit[profit>0])*100/length(profit[profit!=0]),"%","\n"
            ,"Winning Average:",mean(profit[profit>0]),"\n"
            ,"Lossing Average:",mean(profit[profit<0]),"\n"
            ,"Maximum Draw Down:",abs(min(DD)),"\n"
            ,"The Periods of MDD:",tail(sort(DDT),5),"\n"
            ,"Profit Factor:",sum(profit[profit>0])/-sum(profit[profit<0]),"\n"
            ,"Total Profit/MDD:",sum(profit)/abs(min(DD)),"\n")

        
}
