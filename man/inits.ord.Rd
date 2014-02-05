\name{inits.ord}
\alias{inits.ord}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
inits.ord(Y, x, Treat, ncat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
%%     ~~Describe \code{Y} here~~
}
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{Treat}{
%%     ~~Describe \code{Treat} here~~
}
  \item{ncat}{
%%     ~~Describe \code{ncat} here~~
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
function (Y, x, Treat, ncat) 
{
    nobs = length(Y)
    p = rep(NA, ncat)
    c1 = c2 = c3 = rep(NA, ncat - 1)
    for (i in seq(ncat)) {
        p[i] = sum(Y == i)/nobs
        if (p[i] == 0) 
            p[i] = 0.05
    }
    if (sum(p) > 1) 
        p[max(which(p == max(p)))] = p[max(which(p == max(p)))] + 
            1 - sum(p)
    p1 = rmultz2(nobs, p)/nobs
    if (any(p1 == 0)) {
        p1[which(p1 == 0)] = 0.05
        p1[max(which(p1 == max(p1)))] = p1[max(which(p1 == max(p1)))] + 
            1 - sum(p1)
    }
    p2 = rmultz2(nobs, p)/nobs
    if (any(p2 == 0)) {
        p2[which(p2 == 0)] = 0.05
        p2[max(which(p2 == max(p2)))] = p2[max(which(p2 == max(p2)))] + 
            1 - sum(p2)
    }
    p3 = rmultz2(nobs, p)/nobs
    if (any(p3 == 0)) {
        p3[which(p3 == 0)] = 0.05
        p3[max(which(p3 == max(p3)))] = p3[max(which(p3 == max(p3)))] + 
            1 - sum(p3)
    }
    cumsum.p1 = cumsum(p1)
    cumsum.p2 = cumsum(p2)
    cumsum.p3 = cumsum(p3)
    for (i in seq(ncat - 1)) {
        c1[i] = logit(cumsum.p1[i])
        c2[i] = logit(cumsum.p2[i])
        c3[i] = logit(cumsum.p3[i])
    }
    dc1 = c(c1[1], c1[-1] - c1[-(ncat - 1)])
    dc2 = c(c2[1], c2[-1] - c2[-(ncat - 1)])
    dc3 = c(c3[1], c3[-1] - c3[-(ncat - 1)])
    if (is.null(x)) {
        fit <- summary(lm(Y ~ Treat))
    }
    else {
        slope <- se.slope <- rep(NA, dim(x)[2])
        fit <- summary(lm(Y ~ Treat + x))
        slope <- coef(fit)[2 + seq(dim(x)[2]), 1]
        if (!is.nan(fit$fstat[1])) 
            se.slope <- coef(fit)[2 + seq(dim(x)[2]), 2]
        else se.slope <- rep(1, length(se.slope))
    }
    beta = coef(fit)[2, 1]
    if (!is.nan(fit$fstat[1])) {
        se.beta = coef(fit)[2, 2]
    }
    else {
        se.beta = 1
    }
    inits.1 = list(dc1, beta + se.beta * rnorm(1))
    names(inits.1) = c("dc", "beta")
    inits.2 = list(dc2, beta + se.beta * rnorm(1))
    names(inits.2) = c("dc", "beta")
    inits.3 = list(dc3, beta + se.beta * rnorm(1))
    names(inits.3) = c("dc", "beta")
    if (!is.null(x)) {
        inits.1[[1 + length(inits.1)]] = slope + se.slope * rnorm(1)
        names(inits.1)[[length(inits.1)]] = "slope"
        inits.2[[1 + length(inits.2)]] = slope + se.slope * rnorm(1)
        names(inits.2)[[length(inits.2)]] = "slope"
        inits.3[[1 + length(inits.3)]] = slope + se.slope * rnorm(1)
        names(inits.3)[[length(inits.3)]] = "slope"
    }
    inInits <- list(inits.1, inits.2, inits.3)
    return(inInits)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
