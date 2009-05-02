Plot.PI <-
function(x,fore,Interval,start,end,frequency)
{
library(zoo)
h<-nrow(fore)
y1 <- zooreg(x,start,end,frequency)
if( frequency == 12) m1 <- yearmon(index(y1))
if( frequency == 4) m1 <- yearqtr(index(y1))
if( frequency == 1) m1 <- index(y1)

fstart <- m1[length(m1)]+1/frequency
fend <- m1[length(m1)]+h/frequency
y2 <- zooreg(fore,fstart,fend,frequency)
y3 <- zooreg(Interval,fstart,fend,frequency)
ts.plot(ts(y1,start,end,frequency),ts(y2,fstart,fend,frequency),ts(y3,fstart,fend,frequency),lwd=c(1,rep(2,1+ncol(Interval))),col=c(1,4,rep(2,ncol(Interval))))
title(main="Time Plot and Prediction Intervals",sub="red = prediction quantiles; blue = point forecasts", col.sub=2)
abline(v=fstart,col=3,lwd=2)
}

