\name{rank_sum}
\alias{rank_sum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{rank_sum
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rank_sum(var1, var2, alpha)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{var1}{
%%     ~~Describe \code{var1} here~~
}
  \item{var2}{
%%     ~~Describe \code{var2} here~~
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
function (var1, var2, alpha) 
{
    df_var1 <- data.frame(value = var1, title = "Group 1")
    df_var2 <- data.frame(value = var2, title = "Group 2")
    stacked_data <- rbind(df_var1, df_var2)
    sorted_data <- stacked_data[order(stacked_data$value), ]
    sorted_data$rank <- rank(sorted_data$value)
    n1 <- length(var1)
    n2 <- length(var2)
    print(sorted_data)
    cat("\n", "n1:", n1, "     n2:", n2, "\n", "\n")
    sum_ranks <- tapply(sorted_data$rank, sorted_data$title, 
        sum)
    smaller_sample_group <- names(sort(table(stacked_data$title))[1])
    sum_ranks_smaller_sample <- sum_ranks[smaller_sample_group]
    min_size <- min(length(var1), length(var2))
    max_size <- max(length(var1), length(var2))
    mean_ranks <- (min_size * (min_size + max_size + 1))/2
    stdev_ranks <- sqrt(((min_size * max_size) * (min_size + 
        max_size + 1))/12)
    z <- (sum_ranks_smaller_sample - mean_ranks)/stdev_ranks
    critical_value_left <- qnorm(alpha, lower.tail = TRUE)
    critical_value_right <- qnorm(alpha, lower.tail = FALSE)
    critical_value_two_tailed <- qnorm(alpha/2, lower.tail = FALSE)
    cat("Critical Values:", "\n")
    cat("Left Tail Test:", critical_value_left, "     Right Tail Test:", 
        critical_value_right, "\n")
    cat("Two Tail Test:  ", critical_value_two_tailed, " and ", 
        -1 * critical_value_two_tailed, "\n", "\n")
    cat("Test Statistic:", z, "\n", "\n")
    p_value_2Tail_Test <- 2 * (1 - pnorm(abs(z)))
    p_value_1Tail_test <- (1 - pnorm(abs(z)))
    result <- c(p_value_1Tail_test, p_value_2Tail_Test)
    names(result) <- c("One Tailed P-Value", "      Two Tailed P-Value")
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
