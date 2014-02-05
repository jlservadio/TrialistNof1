\name{treat.diff.change}
\alias{treat.diff.change}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
treat.diff.change(treat.diff, score.range)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{treat.diff}{
%%     ~~Describe \code{treat.diff} here~~
}
  \item{score.range}{
%%     ~~Describe \code{score.range} here~~
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
function (treat.diff, score.range) 
{
    treat.diff.change = treat.diff/score.range
    treat.diff.change.1 = as.vector(c(quantile(treat.diff.change, 
        0.025), median(treat.diff.change), quantile(treat.diff.change, 
        0.975)))
    treat.diff.change.2 = as.numeric(table(cut(treat.diff.change, 
        breaks = c(-1, -0.2, 0, 0.2, 1))))/length(treat.diff.change)
    return(list(treat.diff.change.1, treat.diff.change.2))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
