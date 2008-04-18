`BootAfterBootPI` <-
function(x,p,h,nboot,prob,type,correct)
{x<-as.matrix(x)
if (type=="const")
M <- BootAfterBoot.PI(x,p,h,nboot,prob,correct) 
if (type=="const+trend")
M <- BootAfterBootT.PI(x,p,h,nboot,prob,correct) 
return(list(PI=M$PI,forecast=M$forecast))
}

