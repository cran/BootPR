Stine.Shaman <-
function(x,p,h,type,correct)
{
x<-as.matrix(x)
if (type=="const")
M <- Shaman.Stine(x,p,h,correct)
if (type=="const+trend")
M <- Shaman.StineT(x,p,h,correct)
rownames(M$coef) <- ARnames(p,type); colnames(M$coef) <- "coefficients"
colnames(M$forecast) <- "forecasts"; rownames(M$forecast) <- paste("h",1:h,sep="")
return(list(coef=M$coef,resid=M$resid,forecast=M$forecast))
}

