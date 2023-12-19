\name{Two_Samp_ZTest}
\alias{Two_Samp_ZTest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Two_Samp_ZTest(xbar1, sigma1, n1, xbar2, sigma2, n2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xbar1}{
%%     ~~Describe \code{xbar1} here~~
}
  \item{sigma1}{
%%     ~~Describe \code{sigma1} here~~
}
  \item{n1}{
%%     ~~Describe \code{n1} here~~
}
  \item{xbar2}{
%%     ~~Describe \code{xbar2} here~~
}
  \item{sigma2}{
%%     ~~Describe \code{sigma2} here~~
}
  \item{n2}{
%%     ~~Describe \code{n2} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (xbar1, sigma1, n1, xbar2, sigma2, n2) 
{
    var1 <- sigma1^2
    var2 <- sigma2^2
    SE <- sqrt(var1/n1 + var2/n2)
    z_stat <- (xbar1 - xbar2)/SE
    p_value_2Tail_Test <- 2 * (1 - pnorm(abs(z_stat)))
    p_value_1Tail_test <- (1 - pnorm(abs(z_stat)))
    result <- c(z_stat, p_value_1Tail_test, p_value_2Tail_Test, 
        SE)
    names(result) <- c("Test Statistic", "   One Tailed P-Value", 
        "   Two Tailed P-Value", "   Standard Error")
    return(result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
