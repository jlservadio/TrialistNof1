\name{prior.norm}
\alias{prior.norm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
prior.norm(Covs, alphaprior, betaprior, slopeprior, varprior, varprior.params)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Covs}{
%%     ~~Describe \code{Covs} here~~
}
  \item{alphaprior}{
%%     ~~Describe \code{alphaprior} here~~
}
  \item{betaprior}{
%%     ~~Describe \code{betaprior} here~~
}
  \item{slopeprior}{
%%     ~~Describe \code{slopeprior} here~~
}
  \item{varprior}{
%%     ~~Describe \code{varprior} here~~
}
  \item{varprior.params}{
%%     ~~Describe \code{varprior.params} here~~
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
function (Covs, alphaprior, betaprior, slopeprior, varprior, 
    varprior.params) 
{
    mean.alpha = alphaprior[[2]]
    prec.alpha = alphaprior[[3]]
    mean.beta = betaprior[[2]]
    prec.beta = betaprior[[3]]
    Prior.alpha <- paste("d", alphaprior[[1]], "(mean.alpha,prec.alpha)", 
        sep = "")
    Prior.beta <- paste("d", betaprior[[1]], "(mean.beta,prec.beta)", 
        sep = "")
    if (!is.null(Covs)) {
        mean.slope = slopeprior[[2]]
        prec.slope = slopeprior[[3]]
        Prior.slope = paste("d", slopeprior[[1]], "(mean.slope,prec.slope)", 
            sep = "")
    }
    prec.1 = varprior.params[1]
    prec.2 = varprior.params[2]
    if (varprior[[1]] == "prec") {
        if (varprior[[2]] == "gamma") 
            Prior.prec = paste("dgamma(prec.1, prec.2)", sep = "")
        else return("Only Gamma currently supported for precision distribution")
    }
    else if (varprior[[1]] == "Sd") {
        if (varprior[[2]] == "unif") 
            Prior.prec = paste("dunif(prec.1, prec.2)", sep = "")
        else if (varprior[[2]] == "hn") 
            Prior.prec = paste("dnorm(0,1)I(", varprior[[3]][1], 
                ",", varprior[[3]][2], ")", sep = "")
        else return("Unsupported Sd distribution")
    }
    else if (varprior[[1]] == "var") {
        if (varprior[[2]] == "unif") 
            Prior.prec = paste("dunif(prec.1,prec.2)", sep = "")
        else if (varprior[[2]] == "hn") 
            Prior.prec = paste("dnorm(0,1),I(", varprior[[3]][1], 
                ",", varprior[[3]][2], ")", sep = "")
        else return("Unsupported var distribution")
    }
    out <- list(mean.alpha = mean.alpha, prec.alpha = prec.alpha, 
        mean.beta = mean.beta, prec.beta = prec.beta, Prior.alpha = Prior.alpha, 
        Prior.beta = Prior.beta, prec.1 = prec.1, prec.2 = prec.2, 
        Prior.prec = Prior.prec)
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
