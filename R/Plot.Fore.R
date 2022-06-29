Plot.Fore <-
function(x,fore,start,end,frequency)
{
x=ts(x,start,end,frequency)
if(frequency==1) d2=end+1
if (frequency>1){
d2=end;d2[2]=d2[2]+1;
if(d2[2] > frequency) { d2[1]=d2[1]+1; d2[2]=d2[2]-frequency}
}
f=ts(fore,start=d2,frequency=frequency)
ts.plot(x,f,lwd=c(1,2),col=c(1,4))
title(main="Time Plot and Point Forecasts",sub="blue = point forecasts", col.sub=4)
}