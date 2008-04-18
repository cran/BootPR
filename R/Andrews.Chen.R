`Andrews.Chen` <-
function(x,p,h,type)
{
x<-as.matrix(x)
if (type=="const")
M <- Andrews.Chen1(x,p,h)
if (type=="const+trend")
M <- Andrews.Chen2(x,p,h)
return(list(coef=M$coef,ecm.coef=M$ecmcoef,resid=M$resid,forecast=M$forecast))
}

