\name{Two_Samp_Z_Interval}
\alias{Two_Samp_Z_Interval}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Two_Samp_Z_Interval(xbar1, sigma1, n1, xbar2, sigmas2, n2, alpha)
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
  \item{sigmas2}{
%%     ~~Describe \code{sigmas2} here~~
}
  \item{n2}{
%%     ~~Describe \code{n2} here~~
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
function (xbar1, sigma1, n1, xbar2, sigmas2, n2, alpha) 
{
    critical_value <- round(qnorm(1 - alpha/2), 4)
    Conf_Level <- round((1 - alpha) * 100, 2)
    point_est <- xbar1 - xbar2
    Margin_Error <- round(critical_value * sqrt((sigma1^2/n1) + 
        (sigma2^2/n2)), 4)
    lower_bound <- round(point_est - Margin_Error, 4)
    upper_bound <- round(point_est + Margin_Error, 4)
    result <- c(Conf_Level, critical_value, Margin_Error, point_est, 
        lower_bound, upper_bound)
    names(result) <- c("Confidence Level \%", "Critical Value", 
        "Margin or Error", "Point Estimate", "Lower Bound", "Upper Bound")
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
