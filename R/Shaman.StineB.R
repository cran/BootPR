Shaman.StineB <-
function(x,p,h,correct)
{
x <- as.matrix(x)
n <- nrow(x)
b <- LSMB(x,p)$coef
bc <- c(Stine(b,n,p),b[p+1])

    if( correct == "kilian")
    bc <- adjust(b,bc,p)
    if( correct == "ssf")
    bc <- adjust2(bc,p)

bc[p+1] <- mean(x)*(1-sum(bc[1:p]))
e <- RESID(x,bc)
f <- {}
if(h > 0)
f <- AR.Fore(x,bc,h)
return(list(coef=bc,resid=e,forecast=f))
}

