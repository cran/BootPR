`BootPI` <-
function(x,p,h,nboot,prob,type)
{x<-as.matrix(x)
if (type=="const")
M <- Boot.PI(x,p,h,nboot,prob) 
if (type=="const+trend")
M <- BootT.PI(x,p,h,nboot,prob) 
return(list(PI=M$PI,forecast=M$forecast))
}

