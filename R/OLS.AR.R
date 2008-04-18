`OLS.AR` <-
function(x,p,h)
{
    x <- as.matrix(x)
    n <- nrow(x)
    B <- LSM(x,p)
    b <- B$coef
    e <- B$resid
    f <- {}
    if(h > 0)
    f <- AR.Fore(x,b,h)
return(list(coef=b,resid=e,forecast=f))
}

