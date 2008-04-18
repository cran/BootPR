`BootBC` <-
function(x,p,h,nboot,type,correct)
{x<-as.matrix(x)
if (type=="const")
M <- Bootstrap(x,p,h,nboot,correct) 
if (type=="const+trend")
M <- BootstrapT(x,p,h,nboot,correct) 
return(list(coef=M$coef,resid=M$resid,forecast=M$forecast))
}

