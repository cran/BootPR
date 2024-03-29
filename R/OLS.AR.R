OLS.AR <-
function(x,p,h,prob)
{
    x <- as.matrix(x)
    n <- nrow(x)
    B <- LSM(x,p)
    b <- B$coef
    e <- B$resid
    f <- {}; PImat <- {}
    if(h > 0)
    {
    f <- AR.Fore(x,b,h)
    s2 <- sum(e^2)/(nrow(x)-length(b))
    mf <- mainf(b,h-1,p)
    se <- as.matrix(sqrt(s2*cumsum(mf^2)))

    PImat <- matrix(NA,ncol=length(prob),nrow=h)
    for( i in 1:length(prob))
    {PImat[,i] <- f + qnorm(prob[i])*se }
    }
return(list(coef=b,resid=e,forecast=f,PI=PImat))
}
