`adjust2` <-
function(b,p)
{
library(combinat)
options(warn=-1)
s <- polyroot(c(1,-b[1:p]))
if( min(Mod(s)) > 1) return(as.matrix(b,nrow=length(b)))


cvec <- 1/rev(s)

for(i in 1:length(cvec))
{if(Mod(cvec[i]) > 1)
cvec[i] <- 1/cvec[i]}

p <- length(cvec)
mat <- b
for( i in 1:p)
{index <- as.matrix(combn(p,i))
sum <- 0
    for(j in 1:ncol(index))
    sum <- sum + prod( cvec[index[,j]] )
mat[i] <- -(-1)^i*sum
}   
mat <- as.real(mat)
mat <- as.matrix(mat,nrow=nrow(b))
return(mat)
}

