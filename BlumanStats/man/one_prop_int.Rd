\name{one_prop_int}
\alias{one_prop_int}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{one_prop_int
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
one_prop_int(num_successes, n, alpha)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{num_successes}{
%%     ~~Describe \code{num_successes} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
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
function (num_successes, n, alpha) 
{
    confid_level <- (1 - alpha) * 100
    p_hat <- num_successes/n
    z_critical_value <- qnorm(1 - alpha/2)
    standard_error_prop <- sqrt((p_hat * (1 - p_hat)/n))
    margin_error_prop <- standard_error_prop * z_critical_value
    lower_bound_prop <- p_hat - margin_error_prop
    upper_bound_prop <- p_hat + margin_error_prop
    result <- c(confid_level, lower_bound_prop, upper_bound_prop, 
        margin_error_prop, p_hat)
    names(result) <- c("\% Confidence Level", "Lower Bound", "Upper Bound", 
        "Margin of Error", "Sample Proportion")
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
