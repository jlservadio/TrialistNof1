\name{run.norm}
\alias{run.norm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
run.norm(Y, Treat, Covs, model, var.model, nChains = 3, conv.limit = 1.05, niters = 50000, nruns = 5000, setsize = 4000, slopeprior, alphaprior, betaprior, varprior, varprior.params, path)
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
  \item{model}{
%%     ~~Describe \code{model} here~~
}
  \item{var.model}{
%%     ~~Describe \code{var.model} here~~
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
  \item{slopeprior}{
%%     ~~Describe \code{slopeprior} here~~
}
  \item{alphaprior}{
%%     ~~Describe \code{alphaprior} here~~
}
  \item{betaprior}{
%%     ~~Describe \code{betaprior} here~~
}
  \item{varprior}{
%%     ~~Describe \code{varprior} here~~
}
  \item{varprior.params}{
%%     ~~Describe \code{varprior.params} here~~
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
function (Y, Treat, Covs, model, var.model, nChains = 3, conv.limit = 1.05, 
    niters = 50000, nruns = 5000, setsize = 4000, slopeprior, 
    alphaprior, betaprior, varprior, varprior.params, path) 
{
    nobs = length(Y)
    if (!is.null(Covs)) 
        Covs = as.matrix(Covs)
    prior = prior.norm(Covs, alphaprior, betaprior, slopeprior, 
        varprior, varprior.params)
    inInits = inits.norm(Y, Covs, Treat, varprior, ntreat = 2)
    inData <- data.norm(Y, Covs, prior, Treat)
    model.norm(nobs, Covs, prior, varprior, path)
    pars.to.save <- c("alpha", "beta")
    if (!is.null(Covs)) 
        pars.to.save = c(pars.to.save, "slope")
    pars.to.save = c(pars.to.save, "Sd")
    jags.out <- jags.fit(inData, inInits, pars.to.save, model, 
        "model.txt", nChains, niters, conv.limit, setsize, nruns, 
        Covs)
    burn.in <- jags.out[[1]]
    no.runs <- jags.out[[2]]
    samples <- jags.out[[3]]
    varnames <- dimnames(samples)[[3]]
    nvars <- dim(samples)[3]
    alpha.vars <- grep("alpha", varnames)
    alpha <- as.vector(samples[, , alpha.vars])
    beta.vars <- grep("beta", varnames)
    beta <- as.vector(samples[, , beta.vars])
    Sd.vars <- grep("Sd", varnames)
    Sd <- as.vector(samples[, , Sd.vars])
    if (!is.null(Covs)) {
        slope.vars <- grep("slope", varnames)
        slope <- samples[, , slope.vars]
    }
    if (is.null(Covs)) {
        out <- list(burn.in, no.runs, Y, alpha, beta, Sd)
        names(out) <- c("Burn In", "Number runs per chain", "Y", 
            "alpha", "beta", "Sd")
    }
    else {
        out <- list(burn.in, no.runs, Y, alpha, beta, Sd, slope)
        names(out) <- c("Burn In", "Number runs per chain", "Y", 
            "alpha", "beta", "Sd", "Slopes")
    }
    return(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
