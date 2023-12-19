\name{critical_z}
\alias{critical_z}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
critical_z(alpha)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
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
function (alpha) 
{
    if (alpha <= 0 | alpha >= 1) {
        stop("Alpha must be between 0 and 1 exclusive.")
    }
    z_critical <- round(qnorm(1 - alpha), 4)
    z_crit_left <- round(-1 * qnorm(1 - alpha), 4)
    z_critical2 <- round(qnorm(1 - alpha/2), 4)
    neg_z_crit2 <- round(-1 * z_critical2, 4)
    result <- c(z_critical, z_crit_left, z_critical2, paste(neg_z_crit2, 
        z_critical2, sep = " and "))
    names(result) <- c("Right Tail Test", "Left Tail Test", "   Confid Interval / Sample Size", 
        "Two Tail Test")
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
