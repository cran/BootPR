`LS.AR` <-
function(x,p,h,type,prob)
{
x<-as.matrix(x)
if (type=="const")
M <- OLS.AR(x,p,h,prob)
if (type=="const+trend")
M <- OLS.ART(x,p,h,prob)
return(list(coef=M$coef,resid=M$resid,forecast=M$forecast,PI=M$PI))
}
