`ShamanStine.PI` <-
function(x,p,h,nboot,prob,type,correct,pmax)
{
x<-as.matrix(x)
if (type=="const" & pmax==0)
M <- ShamanStine1.PI(x,p,h,nboot,prob,correct)
if (type=="const+trend" & pmax==0)
M <- ShamanStine1T.PI(x,p,h,nboot,prob,correct)
if (type=="const" & pmax > 0)
M <- ShamanStine2.PI(x,p,h,nboot,prob,correct,pmax)
if (type=="const+trend" & pmax > 0)
M <- ShamanStine2T.PI(x,p,h,nboot,prob,correct,pmax)
return(list(PI=M$PI,forecast=M$forecast))
}

