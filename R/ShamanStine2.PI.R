ShamanStine2.PI <-
function(x,p,h,nboot,prob,correct,pmax)
{
set.seed(12345)
n <- nrow(x)

BC <- Shaman.Stine(x,p,h,correct)
BCB <- Shaman.StineB(x,p,h,correct)

bb <- BCB$coef
eb <- sqrt( (n-p) / ( (n-p)-p-1))*BCB$resid
ef <- sqrt( (n-p) / ( (n-p)-p-1))*BC$resid

fore <- matrix(NA,nrow=nboot,ncol=h)
for(i in 1:nboot)
    {    
        index <- as.integer(runif(n-p, min=1, max=nrow(eb)))
        es <- eb[index,1]
        xs <- ysb(x, bb, es)
        ps <- AR.order(xs,pmax)$ARorder[1]
        bs <- Shaman.Stine(xs,ps,h,correct)$coef
        fore[i,] <- AR.ForeB(xs,bs,h,ef,length(bb)-1)
    }

Interval <- matrix(NA,nrow=h,ncol=length(prob),dimnames=list(1:h,prob))
for( i in 1:h)
Interval[i,] <- quantile(fore[,i],probs=prob)
return(list(PI=Interval,forecast=BC$forecast))
}

