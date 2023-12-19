permut <-
function(n,r)
{nfact<-factorial(n)
nrfact <- factorial(n-r)
permt <- nfact / nrfact
return(permt)
}
