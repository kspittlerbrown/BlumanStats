\name{outlier_Q1_Q3}
\alias{outlier_Q1_Q3}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
outlier_Q1_Q3(Q1, Q3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Q1}{
%%     ~~Describe \code{Q1} here~~
}
  \item{Q3}{
%%     ~~Describe \code{Q3} here~~
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
function (Q1, Q3) 
{
    iqr <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * iqr
    upper_bound <- Q3 + 1.5 * iqr
    outlier_range <- c(lower_bound, upper_bound, iqr)
    names(outlier_range) <- c("Lower Bound", "     Upper Bound", 
        "IQR")
    return(outlier_range)
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
