Plot.PI <- 
function(x,fore,Interval,start,end,frequency){
x=ts(x,start,end,frequency)
if(frequency==1) d2=end+1
if (frequency>1){
d2=end;d2[2]=d2[2]+1;
if(d2[2] > frequency) { d2[1]=d2[1]+1; d2[2]=d2[2]-frequency}
}
y1=ts(fore,start=d2,frequency=frequency)
y2=ts(Interval,start=d2,frequency=frequency)
ts.plot(x,y1,y2,lwd=c(1,rep(2,1+ncol(Interval))),col=c(1,4,rep(2,ncol(Interval))))
title(main="Time Plot and Prediction Intervals",sub="red = prediction quantiles; blue = point forecasts", col.sub=2)
}
