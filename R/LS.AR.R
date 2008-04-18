`LS.AR` <-
function(x,p,h,type)
{
x<-as.matrix(x)
if (type=="const")
M <- OLS.AR(x,p,h)
if (type=="const+trend")
M <- OLS.ART(x,p,h)
return(list(coef=M$coef,resid=M$resid,forecast=M$forecast))
}

