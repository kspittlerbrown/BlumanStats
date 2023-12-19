\name{Two_Samp_Prop_Interval}
\alias{Two_Samp_Prop_Interval}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Two_Samp_Prop_I
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Two_Samp_Prop_Interval(x1, n1, x2, n2, alpha = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x1}{
%%     ~~Describe \code{x1} here~~
}
  \item{n1}{
%%     ~~Describe \code{n1} here~~
}
  \item{x2}{
%%     ~~Describe \code{x2} here~~
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
function (x1, n1, x2, n2, alpha = 0.05) 
{
    p1 <- x1/n1
    p2 <- x2/n2
    q1 <- 1 - p1
    q2 <- 1 - p2
    se <- sqrt((p1 * q1/n1) + (p2 * q2/n2))
    point_estimate <- p1 - p2
    z <- qnorm(1 - alpha/2)
    margin_error <- z * se
    Conf_Level <- (1 - alpha) * 100
    lower_bound <- (p1 - p2) - margin_error
    upper_bound <- (p1 - p2) + margin_error
    cat("Confidence Level:", Conf_Level, "\%   p-hat1=", p1, "  p-hat2=", 
        p2, "\n")
    cat("Point Estimate:", point_estimate, "\n")
    cat("Margin of Error:", margin_error, "\n")
    cat("Critical Value:", z, "\n", "\n")
    cat("Lower Bound:", lower_bound, "     Upper Bound:", upper_bound, 
        "\n")
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
