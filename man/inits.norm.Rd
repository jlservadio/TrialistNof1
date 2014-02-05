\name{inits.norm}
\alias{inits.norm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
inits.norm(Y, x, Treat, varprior, ntreat = 2)
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
  \item{varprior}{
%%     ~~Describe \code{varprior} here~~
}
  \item{ntreat}{
%%     ~~Describe \code{ntreat} here~~
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
function (Y, x, Treat, varprior, ntreat = 2) 
{
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
    alpha = coef(fit)[1, 1]
    beta = coef(fit)[2, 1]
    if (!is.nan(fit$fstat[1])) {
        se.alpha = coef(fit)[1, 2]
        se.beta = coef(fit)[2, 2]
        tau <- fit$sigma^2
    }
    else {
        se.alpha = se.beta = tau = 1
    }
    inits.1 = list(alpha = alpha + se.alpha * rnorm(1))
    names(inits.1) = "alpha"
    inits.2 = list(alpha = alpha + se.alpha * rnorm(1))
    names(inits.2) = "alpha"
    inits.3 = list(alpha = alpha + se.alpha * rnorm(1))
    names(inits.3) = "alpha"
    inits.1[[1 + length(inits.1)]] = beta + se.beta * rnorm(1)
    names(inits.1)[[length(inits.1)]] = "beta"
    inits.2[[1 + length(inits.2)]] = beta + se.beta * rnorm(1)
    names(inits.2)[[length(inits.2)]] = "beta"
    inits.3[[1 + length(inits.3)]] = beta + se.beta * rnorm(1)
    names(inits.3)[[length(inits.3)]] = "beta"
    if (!is.null(x)) {
        inits.1[[1 + length(inits.1)]] = slope + se.slope * rnorm(1)
        names(inits.1)[[length(inits.1)]] = "slope"
        inits.2[[1 + length(inits.2)]] = slope + se.slope * rnorm(1)
        names(inits.2)[[length(inits.2)]] = "slope"
        inits.3[[1 + length(inits.3)]] = slope + se.slope * rnorm(1)
        names(inits.3)[[length(inits.3)]] = "slope"
    }
    random.ISigma1 = rchisq(1, fit$df[2])
    random.ISigma2 = rchisq(1, fit$df[2])
    random.ISigma3 = rchisq(1, fit$df[2])
    prec.1 = random.ISigma1/(tau * fit$df[2])
    prec.2 = random.ISigma2/(tau * fit$df[2])
    prec.3 = random.ISigma3/(tau * fit$df[2])
    if (varprior[[1]] == "prec") {
        inits.1[[1 + length(inits.1)]] = prec.1
        names(inits.1)[[length(inits.1)]] = "prec"
        inits.2[[1 + length(inits.2)]] = prec.2
        names(inits.2)[[length(inits.2)]] = "prec"
        inits.3[[1 + length(inits.3)]] = prec.3
        names(inits.3)[[length(inits.3)]] = "prec"
    }
    else if (varprior[[1]] == "Sd" & !is.null(x)) {
        inits.1[[1 + length(inits.1)]] = 0.5/sqrt(prec.1)
        names(inits.1)[[length(inits.1)]] = "Sd"
        inits.2[[1 + length(inits.2)]] = 0.5/sqrt(prec.2)
        names(inits.2)[[length(inits.2)]] = "Sd"
        inits.3[[1 + length(inits.3)]] = 0.5/sqrt(prec.3)
        names(inits.3)[[length(inits.3)]] = "Sd"
    }
    else if (varprior[[1]] == "var") {
        inits.1[[1 + length(inits.1)]] = 1/prec.1
        names(inits.1)[[length(inits.1)]] = "Var"
        inits.2[[1 + length(inits.2)]] = 1/prec.2
        names(inits.2)[[length(inits.2)]] = "Var"
        inits.3[[1 + length(inits.3)]] = 1/prec.3
        names(inits.3)[[length(inits.3)]] = "Var"
    }
    inInits <- list(inits.1, inits.2, inits.3)
    return(inInits)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
