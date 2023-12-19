\name{sign_test}
\alias{sign_test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sign_test(data, hypothesized_median = 0, alpha = 0.05, alternative = c("two.sided", "less", "greater"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{hypothesized_median}{
%%     ~~Describe \code{hypothesized_median} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
}
  \item{alternative}{
%%     ~~Describe \code{alternative} here~~
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
function (data, hypothesized_median = 0, alpha = 0.05, alternative = c("two.sided", 
    "less", "greater")) 
{
    n <- length(data)
    signs <- sign(data - hypothesized_median)
    n_plus <- sum(signs == 1)
    n_minus <- sum(signs == -1)
    n_min <- min(n_plus, n_minus)
    n_use = n_plus + n_minus
    if (n <= 25) {
        if (alternative == "two.sided") {
            colname <- paste0("a", alpha)
        }
        else {
            colname <- paste0("a", (alpha * 2))
        }
        ntable <- c(8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 
            19, 20, 21, 22, 23, 24, 25)
        a0.01 <- c(0, 0, 0, 0, 1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 
            4, 4, 5, 5)
        a0.02 <- c(0, 0, 0, 1, 1, 1, 2, 2, 2, 3, 3, 4, 4, 4, 
            5, 5, 5, 6)
        a0.05 <- c(0, 1, 1, 1, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 
            5, 6, 6, 6)
        a0.1 <- c(1, 1, 1, 2, 2, 3, 3, 3, 4, 4, 5, 5, 5, 6, 6, 
            7, 7, 7)
        tablej <- data.frame(ntable, a0.01, a0.02, a0.05, a0.1)
        critical_value = tablej[(tablej$ntable == n_use), colname]
        if (alternative == "two.sided") {
            test_statistic <- n_min
        }
        else if (alternative == "less") {
            test_statistic <- n_plus
        }
        else {
            test_statistic <- n_minus
        }
        cat("Sample Size:", n, "\n")
        cat("Critical Value (", alternative, "):", critical_value, 
            "\n")
        cat("Test Statistic:", test_statistic, "\n")
        decision <- ifelse(test_statistic < critical_value, "Reject Ho", 
            "Fail to reject Ho")
        cat("Decision:", decision, "\n")
    }
    else {
        if (alternative == "two.sided") {
            critical_value_left <- qnorm(alpha/2, mean = 0, sd = 1, 
                lower.tail = TRUE)
            critical_value_right <- qnorm(alpha/2, mean = 0, 
                sd = 1, lower.tail = FALSE)
        }
        else {
            critical_value_left <- qnorm(alpha, mean = 0, sd = 1, 
                lower.tail = TRUE)
        }
        if (alternative == "two.sided") {
            test_statistic <- ((n_min + 0.5 - 0.5 * n))/(sqrt(n)/2)
        }
        else if (alternative == "less") {
            test_statistic <- ((n_plus + 0.5 - 0.5 * n))/(sqrt(n)/2)
        }
        else {
            test_statistic <- ((n_minus + 0.5 - 0.5 * n))/(sqrt(n)/2)
        }
        cat("Sample Size:", n, "\n")
        if (alternative == "two.sided") {
            cat("Critical Values (", alternative, "):", c(critical_value_left, 
                "and", critical_value_right), "\n")
        }
        else {
            cat("Critical Value (", alternative, "):", c(critical_value_left), 
                "\n")
        }
        cat("Test Statistic:", test_statistic, "\n")
        decision <- ifelse((alternative == "less" && test_statistic < 
            critical_value_left) || (alternative == "greater" && 
            test_statistic < critical_value_left) || (alternative == 
            "two.sided" && (test_statistic < critical_value_left || 
            test_statistic > critical_value_right)), "Reject Ho", 
            "Fail to reject Ho")
        cat("Decision:", decision, "\n")
    }
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
