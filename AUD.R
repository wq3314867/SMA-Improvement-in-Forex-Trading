setwd("H:/fin 600/final/forex data")
data<-read.csv("AUD_USD.csv",header=T)

#compute the weighted power ma
mapa <- function(arr, n,a){
  res = arr
  total<-double(n)
  for(i in n:length(arr)){
  pa <- res^a
  y<-1:n
    total[i] = sum(y^2*pa[(i-n+1):i])
    res[i] = (total[i]/sum(y^2))^(1/a)     
  }
  res
}

#compute the normal MA:
ma <- function(arr, n){
  res = arr
  for(i in n:length(arr)){
    res[i] = mean(arr[(i-n+1):i])
  }
  res
}

#Compute the Logrithm MA:
mala<-function(arr,n,a){
    res = arr
    total = double(length(arr))
    for(i in n:length(arr)){
        La <- log(res,a)
        y<-1:n
        total[i] = sum(y^2*La[(i-n+1):i])
        res[i] = a^(total[i]/sum(y^2))
	}
	res
}

#Compute the powered MA and T matrix
arr<-data[,2]
ret<-diff(log(data[,2])) #compute geometric returns
viol<-ret^2
library(forecast)
tsdisplay(ret)
qqnorm(ret)
qqline(ret,col=2)

MAP10<-mapa(arr,10,3)
MAP5<-mapa(arr,5,3)
plot(data[,2],type="l",lwd=1.5,main="Power3 MA system",ylab="price")
 lines(MAP5,col="red")
 lines(MAP10,col="green")
X1<-log(MAP5[-(1:10)])-log(MAP10[-(1:10)])

#AUD is always decreasing during the past 5years, so we assume that custmers will use sell strategy:
T1<-ifelse(X1>=0.001,1,-1)
write.table(T1,"AUDP3.txt",row.names=FALSE)

#Compute the normal MA and T matrix
MA10<-ma(arr,10)
MA5<-ma(arr,5)
plot(data[,2],type="l",lwd=1.5, main="Normal MA system",ylab="price")
 lines(MA5,col="red")
 lines(MA10,col="green")
X2<-log(MA5[-(1:10)])-log(MA10[-(1:10)])
T2<-ifelse(X2>=0.001,1,-1)
write.table(T2,"AUDMA.txt",row.names=FALSE)

#Compute the logrithm MA and T matrix:
MAL10<-mala(arr,10,3)
MAL5<-mala(arr,5,3)
plot(data[,2],type="l",lwd=3, main="Logrithm 3 MA system",ylab="price")
 lines(MAL5,col="red",lwd=2)
 lines(MAL10,col="green",lwd=2)
X3<-log(MAL5[-(1:10)])-log(MAL10[-(1:10)])
T3<-ifelse(X3>=0.001,1,-1)
write.table(T3,"AUDL3.txt",row.names=FALSE)

#iteration by python:

#by using python to iterate the data, we can get the fllowing results:

indexL3<-read.table("AUDL3pair.txt")
IMVL3<-as.vector(as.matrix(indexL3,nrow=1,ncol=length(indexL3)))
IMVL3<-IMVL3[order(IMVL3)]
i=1:length(IMVL3)
z1L3<-IMVL3[i%%2==0]
z2L3<-IMVL3[i%%2!=0]
z3L3<-sum(z1L3-z2L3)
trperiodL3<-double(z3L3)
trperiodL3[1:(IMVL3[2]-IMVL3[1]+1)]=ret[IMVL3[1]:IMVL3[2]]
p<-rep(0,length(IMVL3))
i=3
h=1
while (i <length(IMVL3)){
  for(h in 1:(length(IMVL3)/2+1)){
  p[h]=length(IMVL3[i-2]:IMVL3[i-1])
  p[h+1]=length(IMVL3[i]:IMVL3[i+1])
  trperiodL3[(sum(p[1:h])+1):sum(p[1:(h+1)])]=ret[IMVL3[i]:IMVL3[i+1]]
  i=i+2
  h=h+1
  }
 trperiodL3
}

cumretL3<--cumsum(trperiodL3)


indexP3<-read.table("AUDP3pair.txt")
IMVP3<-as.vector(as.matrix(indexP3+1,nrow=1,ncol=length(indexP3)))
IMVP3<-IMVP3[order(IMVP3)]
i=1:length(IMVP3)
z1P3<-IMVP3[i%%2==0]
z2P3<-IMVP3[i%%2!=0]
z3P3<-sum(z1P3-z2P3)
trperiodP3<-double(z3P3)
trperiodP3[1:(IMVP3[2]-IMVP3[1]+1)]=ret[IMVP3[1]:IMVP3[2]]
p<-rep(0,length(IMVP3))
i=3
h=1
while (i<length(IMVP3)){
  for(h in 1:(length(IMVP3)/2+1)){
  p[h]=length(IMVP3[i-2]:IMVP3[i-1])
  p[h+1]=length(IMVP3[i]:IMVP3[i+1])

  trperiodP3[(sum(p[1:h])+1):sum(p[1:(h+1)])]=ret[IMVP3[i]:IMVP3[i+1]]
  i=i+2
  h=h+1
  }
 trperiodP3
}
cumretP3<--cumsum(trperiodP3)

indexMA<-read.table("AUDMApair.txt")
IMVMA<-as.vector(as.matrix(indexMA+1,nrow=1,ncol=length(indexMA)))
IMVMA<-IMVMA[order(IMVMA)]
i=1:length(IMVMA)
z1MA<-IMVMA[i%%2==0]
z2MA<-IMVMA[i%%2!=0]
z3MA<-sum(z1MA-z2MA)
trperiodMA<-double(z3MA)
trperiodMA[1:(IMVMA[2]-IMVMA[1]+1)]=ret[IMVMA[1]:IMVMA[2]]
p<-rep(0,length(IMVMA))
i=3
h=1
while (i <length(IMVMA)){
  for(h in 1:(length(IMVMA)/2+1)){
  p[h]=length(IMVMA[i-2]:IMVMA[i-1])
  p[h+1]=length(IMVMA[i]:IMVMA[i+1])
  trperiodMA[(sum(p[1:h])+1):sum(p[1:(h+1)])]=ret[IMVMA[i]:IMVMA[i+1]]
  i=i+2
  h=h+1
  }
 trperiodMA
}
trperiodMA<-trperiodMA[-trperiodMA[1:length(trperiodMA)]!=0]
cumretMA<--cumsum(trperiodMA)


#plot all the returns:
par(mfrow=c(2,2))
plot(cumretL3,xlab="AUD Cummulative Return By Logrithm",col="red",type="l",lwd=3)
plot(cumretMA,xlab="AUD Cummulative Return SMA",col="green",type="l",lwd=3)
plot(cumretP3,xlab="AUD Cummulative Return Powered MA",col="blue",type="l",lwd=3)


plot(cumretP3,col="blue",ylab="cummulative return", main="3 Systems MA compare",type="l",lwd=3)
lines(cumretMA,col="green",lwd=3)
lines(cumretL3,col="red",lwd=3)

list("max(cumretP3)/max(cumretL3)=",cumretR[1],"max(cumretP3)/max(cumretMA)=",cumretR[2],"max(cumretL3)/max(cumretMA)=",cumretR[3])
cat("Trading times based on Log3=",length(IMVL3)/2,"\n","Trading times based on x^3=",length(IMVP3)/2,"\n","Trading times based on SMA=",length(IMVMA)/2)
cat("Total days of holding the stock based on Log3=",length(cumretL3),"\n", "Total days of holding the stock based on x^3=",length(cumretP3),"\n","Total days of holding the stock based on SMA=",length(cumretMA))