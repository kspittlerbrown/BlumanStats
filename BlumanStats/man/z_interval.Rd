\name{z_interval}
\alias{z_interval}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Z_interval
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
z_interval(n, xbar, sigma, alpha)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{xbar}{
%%     ~~Describe \code{xbar} here~~
}
  \item{sigma}{
%%     ~~Describe \code{sigma} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
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
function (n, xbar, sigma, alpha) 
{
    confid_level <- (1 - alpha) * 100
    margin_error_z <- qnorm(1 - alpha/2) * (sigma/sqrt(n))
    lower_bound_z <- xbar - margin_error_z
    upper_bound_z <- xbar + margin_error_z
    result <- c(confid_level, lower_bound_z, upper_bound_z, margin_error_z, 
        xbar)
    names(result) <- c("\% Confidence Level", "Lower Bound", "Upper Bound", 
        "Margin of Error", "Sample Mean")
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
