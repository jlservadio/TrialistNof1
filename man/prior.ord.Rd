\name{prior.ord}
\alias{prior.ord}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
prior.ord(Covs, betaprior, dcprior, c1prior, slopeprior = list("norm", 0, 0.1))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Covs}{
%%     ~~Describe \code{Covs} here~~
}
  \item{betaprior}{
%%     ~~Describe \code{betaprior} here~~
}
  \item{dcprior}{
%%     ~~Describe \code{dcprior} here~~
}
  \item{c1prior}{
%%     ~~Describe \code{c1prior} here~~
}
  \item{slopeprior}{
%%     ~~Describe \code{slopeprior} here~~
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
function (Covs, betaprior, dcprior, c1prior, slopeprior = list("norm", 
    0, 0.1)) 
{
    mean.beta = betaprior[[2]]
    prec.beta = betaprior[[3]]
    lower.dc = dcprior[[2]]
    upper.dc = dcprior[[3]]
    lower.c1 = c1prior[[2]]
    upper.c1 = c1prior[[3]]
    Prior.beta = paste("d", betaprior[[1]], "(mean.beta,prec.beta)", 
        sep = "")
    Prior.dc = paste("d", dcprior[[1]], "(lower.dc, upper.dc)", 
        sep = "")
    Prior.c1 = paste("d", c1prior[[1]], "(lower.c1, upper.c1)", 
        sep = "")
    if (!is.null(Covs)) {
        mean.slope = slopeprior[[2]]
        prec.slope = slopeprior[[3]]
        Prior.slope = paste("d", slopeprior[[1]], "(mean.slope,prec.slope)", 
            sep = "")
    }
    out <- list(mean.beta = mean.beta, prec.beta = prec.beta, 
        Prior.beta = Prior.beta, lower.dc = lower.dc, upper.dc = upper.dc, 
        Prior.dc = Prior.dc, lower.c1 = lower.c1, upper.c1 = upper.c1, 
        Prior.c1 = Prior.c1)
    if (!is.null(Covs)) {
        out[[1 + length(out)]] = mean.slope
        names(out)[[length(out)]] = "mean.slope"
        out[[1 + length(out)]] = prec.slope
        names(out)[[length(out)]] = "prec.slope"
        out[[1 + length(out)]] = Prior.slope
        names(out)[[length(out)]] = "Prior.slope"
    }
    return(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
