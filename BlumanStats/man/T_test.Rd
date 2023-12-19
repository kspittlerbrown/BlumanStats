\name{T_test}
\alias{T_test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ T_test
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
T_test(n, xbar, stdev, mu)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{xbar}{
%%     ~~Describe \code{xbar} here~~
}
  \item{stdev}{
%%     ~~Describe \code{stdev} here~~
}
  \item{mu}{
%%     ~~Describe \code{mu} here~~
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
function (n, xbar, stdev, mu) 
{
    t <- (xbar - mu)/(stdev/sqrt(n))
    p_value_2Tail_Test <- 2 * pt(abs(t), df = n - 1, lower.tail = FALSE)
    p_value_1Tail_test <- pt(abs(t), df = n - 1, lower.tail = FALSE)
    result <- c(t, p_value_1Tail_test, p_value_2Tail_Test)
    names(result) <- c("Test Statistic", "P-Value for One Tailed Test", 
        "     P-Value for Two Tailed Test")
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
