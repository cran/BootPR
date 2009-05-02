BootBC <-
function(x,p,h,nboot,type,correct)
{x<-as.matrix(x)
if (type=="const")
M <- Bootstrap(x,p,h,nboot,correct) 
if (type=="const+trend")
M <- BootstrapT(x,p,h,nboot,correct) 
rownames(M$coef) <- ARnames(p,type); colnames(M$coef) <- "coefficients"
colnames(M$forecast) <- "forecasts"; rownames(M$forecast) <- paste("h",1:h,sep="")
return(list(coef=M$coef,resid=M$resid,forecast=M$forecast))
}

