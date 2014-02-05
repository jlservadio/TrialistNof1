\name{run.ord}
\alias{run.ord}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
run.ord(Y, Treat, Covs, ncat, model = "cumlogit", nChains = 3, conv.limit = 1.05, niters = 50000, nruns = 5000, setsize = 4000, betaprior, dcprior, c1prior, path)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
%%     ~~Describe \code{Y} here~~
}
  \item{Treat}{
%%     ~~Describe \code{Treat} here~~
}
  \item{Covs}{
%%     ~~Describe \code{Covs} here~~
}
  \item{ncat}{
%%     ~~Describe \code{ncat} here~~
}
  \item{model}{
%%     ~~Describe \code{model} here~~
}
  \item{nChains}{
%%     ~~Describe \code{nChains} here~~
}
  \item{conv.limit}{
%%     ~~Describe \code{conv.limit} here~~
}
  \item{niters}{
%%     ~~Describe \code{niters} here~~
}
  \item{nruns}{
%%     ~~Describe \code{nruns} here~~
}
  \item{setsize}{
%%     ~~Describe \code{setsize} here~~
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
  \item{path}{
%%     ~~Describe \code{path} here~~
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
function (Y, Treat, Covs, ncat, model = "cumlogit", nChains = 3, 
    conv.limit = 1.05, niters = 50000, nruns = 5000, setsize = 4000, 
    betaprior, dcprior, c1prior, path) 
{
    nobs = length(Y)
    if (!is.null(Covs)) 
        Covs = as.matrix(Covs)
    prior = prior.ord(Covs, betaprior, dcprior, c1prior, slopeprior = list("norm", 
        0, 0.1))
    inInits = inits.ord(Y, Covs, Treat, ncat)
    inData <- data.ord(Y, Covs, ncat, prior, Treat)
    model.ord(Covs, prior, path)
    pars.to.save <- c("beta", "or", "c", "p")
    if (!is.null(Covs)) 
        pars.to.save = c(pars.to.save, "slope")
    jags.out <- jags.fit(inData, inInits, pars.to.save, model, 
        "model.txt", nChains, niters, conv.limit, setsize, nruns, 
        Covs)
    burn.in <- jags.out[[1]]
    no.runs <- jags.out[[2]]
    samples <- jags.out[[3]]
    varnames <- dimnames(samples)[[3]]
    nvars <- dim(samples)[3]
    beta.vars <- grep("beta", varnames)
    beta <- as.vector(samples[, , beta.vars])
    or.vars <- grep("or", varnames)
    or <- as.vector(samples[, , or.vars])
    c.vars <- grep("c", varnames)
    c <- matrix(samples[, , c.vars], c(no.runs * nChains, length(c.vars)))
    p.vars <- grep("p", varnames)
    p <- array(matrix(samples[, , p.vars], c(no.runs * nChains, 
        length(p.vars))), c(no.runs * nChains, nobs, ncat))
    if (!is.null(Covs)) {
        slope.vars <- grep("slope", varnames)
        slope <- samples[, , slope.vars]
    }
    if (is.null(Covs)) {
        out <- list(burn.in, no.runs, Y, beta, or, c, p)
        names(out) <- c("Burn In", "Number runs per chain", "Y", 
            "beta", "or", "c", "p")
    }
    else {
        out <- list(burn.in, no.runs, Y, beta, or, p, slope)
        names(out) <- c("Burn In", "Number runs per chain", "Y", 
            "beta", "or", "p", "Slopes")
    }
    return(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
