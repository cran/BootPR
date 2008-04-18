`Stine.Shaman` <-
function(x,p,h,type,correct)
{
x<-as.matrix(x)
if (type=="const")
M <- Shaman.Stine(x,p,h,correct)
if (type=="const+trend")
M <- Shaman.StineT(x,p,h,correct)
return(list(coef=M$coef,resid=M$resid,forecast=M$forecast))
}

