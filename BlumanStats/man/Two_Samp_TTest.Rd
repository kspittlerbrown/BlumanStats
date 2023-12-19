\name{Two_Samp_TTest}
\alias{Two_Samp_TTest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Two_Samp_TTest(xbar1, sd1, n1, xbar2, sd2, n2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xbar1}{
%%     ~~Describe \code{xbar1} here~~
}
  \item{sd1}{
%%     ~~Describe \code{sd1} here~~
}
  \item{n1}{
%%     ~~Describe \code{n1} here~~
}
  \item{xbar2}{
%%     ~~Describe \code{xbar2} here~~
}
  \item{sd2}{
%%     ~~Describe \code{sd2} here~~
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
function (xbar1, sd1, n1, xbar2, sd2, n2) 
{
    var1 <- sd1^2
    var2 <- sd2^2
    SE <- sqrt(var1/n1 + var2/n2)
    t_stat <- (xbar1 - xbar2)/SE
    df <- min(n1, n2) - 1
    p_value1 <- pt(abs(t_stat), df, lower.tail = FALSE)
    p_value2 <- 2 * pt(abs(t_stat), df, lower.tail = FALSE)
    result <- c(t_stat, p_value1, p_value2, SE)
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
