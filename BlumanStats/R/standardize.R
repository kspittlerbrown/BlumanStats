standardize <-
function(x,mean, std_dev)
{z <- (x-mean)/std_dev
return(z)
}
