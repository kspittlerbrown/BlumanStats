\name{Two_Samp_T_Interval}
\alias{Two_Samp_T_Interval}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Two_Samp_T_Interval(xbar1, sd1, n1, xbar2, sd2, n2, alpha)
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
function (xbar1, sd1, n1, xbar2, sd2, n2, alpha) 
{
    df <- round(((sd1^2/n1) + (sd2^2/n2))^2/((sd1^4/(n1^2 * (n1 - 
        1)) + (sd2^4/(n2^2 * (n2 - 1))))), 4)
    critical_value <- round(qt(1 - alpha/2, df), 5)
    Conf_Level <- round((1 - alpha) * 100, 2)
    point_est <- xbar1 - xbar2
    Margin_Error <- round(critical_value * sqrt((sd1^2/n1) + 
        (sd2^2/n2)), 5)
    lower_bound <- round(point_est - Margin_Error, 5)
    upper_bound <- round(point_est + Margin_Error, 5)
    result <- c(Conf_Level, critical_value, Margin_Error, point_est, 
        df, lower_bound, upper_bound)
    names(result) <- c("Confidence Level \%", "T Critical Value", 
        "Margin or Error", "Point Estimate", "Degrees of Freedom", 
        "Lower Bound", "Upper Bound")
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
