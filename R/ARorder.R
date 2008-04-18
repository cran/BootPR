`ARorder` <-
function(x,pmax,type)
{
x<-as.matrix(x)
if (type=="const")
M <- AR.order(x,pmax)
if (type=="const+trend")
M <- ART.order(x,pmax)
return(list(ARorder=M$ARorder,Criteria=M$Criteria))
}

