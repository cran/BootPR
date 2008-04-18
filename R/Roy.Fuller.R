`Roy.Fuller` <-
function(x,p,h,type)
{
x<-as.matrix(x)
if (type=="const")
M <- Fuller(x,p,h)
if (type=="const+trend")
M <- FullerT(x,p,h)
return(list(coef=M$coef,resid=M$resid,forecast=M$forecast))
}

