\name{one_pop_prop_test}
\alias{one_pop_prop_test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{one_pop_prop_test
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
one_pop_prop_test(x, n, po)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{po}{
%%     ~~Describe \code{po} here~~
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
function (x, n, po) 
{
    phat <- x/n
    numerator <- phat - po
    q <- 1 - po
    denom <- sqrt((po * q)/n)
    z <- numerator/denom
    p_value_2Tail_Test <- 2 * (1 - pnorm(abs(z)))
    p_value_1Tail_test <- (1 - pnorm(abs(z)))
    cat("Sample Proportion:  ", phat, "\n")
    cat("Test Statistic:   z = ", z, "\n")
    cat("p-value for One Tail Test:  ", p_value_1Tail_test, "\n")
    cat("p-value for Two Tail Test:  ", p_value_2Tail_Test, "\n")
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
