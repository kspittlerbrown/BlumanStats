combin <-
function(n,r)
{nfact <- factorial(n)
rfact <- factorial(r)
nrfact <- factorial(n-r)
comb <- nfact / (rfact*nrfact)
return(comb)
}
