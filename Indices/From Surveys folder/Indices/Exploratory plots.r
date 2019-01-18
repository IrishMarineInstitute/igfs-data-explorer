# Using R Version 2.4.1
library(FLCore)         #   Version: 1.4-3
library(FLEDA)          #   Version: 1.4-2
library(FLAssess)       #   Version: 1.4-1
library(FLXSA)          #   Version: 1.4-2

library(RODBC)
library(nnet)

installed.packages()[substr(rownames(installed.packages()),1,2)=='FL',3]

setwd('//GalwayFS01/Fishdata/Surveys/IGFS/Indices')

#read the data from the spreadsheet
xlsFile=odbcConnectExcel('IBTS2010_MEG_Indices_VIIb.xls')
tempTun=sqlFetch(xlsFile,'Index')
LF=sqlFetch(xlsFile,'LF')
Age=sqlFetch(xlsFile,'Age')
close(xlsFile)

#create a temporary text file that the function 'read.FLIndices()' can read, this cannot read directly from excel
write.table(rbind(NA,tempTun),'tempTun.txt',row.names=FALSE,col.names=F,na='')
tun=read.FLIndices('tempTun.txt')
names(tun)=names(tempTun)[1]
unlink('tempTun.txt')


#standardised indices, first define a function to standardise them:
standardise=function(x){
  arr <- apply(x@.Data, c(1,3,4,5), function(x) scale(x,center=F))   #SURBA does not center the data
  arr <- aperm(arr, c(2,1,3,4,5))
  dimnames(arr) <- dimnames(x)
  out <- FLQuant(arr)
  out
}

#by year
tun.stan=data.list(lapply(tun,function(x) standardise(index(x))))
ages=sort(unique(as.numeric(tun.stan$age)))
key=simpleKey(space='right',text=as.character(ages),lines=F,columns=1,points=T)
key$points$pch=1:9
xyplot(log(data)~year|qname,data=tun.stan,type='b',groups=age,key=key,ylab='log standardised indices',pch=1:9,cex=0.5)

#by yearclass
tun.stan$yearclass=as.numeric(tun.stan$year)-as.numeric(tun.stan$age)
xyplot(log(data)~yearclass|qname,data=tun.stan,type='b',groups=age,key=key,xlab='Yearclass',ylab='log standardised indices',pch=1:9,cex=0.5)

#by year and yearclass
tun.ind=data.list(lapply(tun,index))
tun.ind$yearclass=as.numeric(tun.ind$year)-as.numeric(tun.ind$age)
xyplot(log(data)~year|qname,groups=yearclass,data=tun.ind,type='l',ylab='log indices',col=1)

#scatter: if you have more ages than years, you will need to trim the age range
plot(trim(tun[[1]],age=1:6))
plot(trim(tun[[1]],age=5:10))

#bubble plots
bubbles(age~year,data=spay(index(tun[[1]])),bub.scale=8,main=paste('Standardised proportions at age\n',tun[[1]]@name))

#ALK plot
lengths=c(min(LF$Length,na.rm=T):max(LF$Length,na.rm=T))
model=multinom(Age~fldFishLength,data=Age)
pred=predict(model,data.frame(fldFishLength=lengths),type='probs')
alk=table(Age$fldFishLength,Age$Age)/rowSums(table(Age$fldFishLength,Age$Age))
ages=colnames(alk)
plot(NA,type='n',xlim=range(lengths),ylim=c(0,1),xlab='length',ylab='proportion-at-age')
points(LF$Length,LF$Frequency/max(LF$Frequency,na.rm=T),type='h',col='lightgrey',lwd=10,lend=2)
for(a in ages){
  lines(lengths,pred[,a],col=as.numeric(a))
  text(as.numeric(rownames(alk)),alk[,a],a,col=as.numeric(a))
  }
  
