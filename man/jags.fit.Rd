\name{jags.fit}
\alias{jags.fit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
jags.fit(inData, inInits, pars.to.save, model, model.file, n.chains, niters, conv.limit, setsize, nruns, Covs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inData}{
%%     ~~Describe \code{inData} here~~
}
  \item{inInits}{
%%     ~~Describe \code{inInits} here~~
}
  \item{pars.to.save}{
%%     ~~Describe \code{pars.to.save} here~~
}
  \item{model}{
%%     ~~Describe \code{model} here~~
}
  \item{model.file}{
%%     ~~Describe \code{model.file} here~~
}
  \item{n.chains}{
%%     ~~Describe \code{n.chains} here~~
}
  \item{niters}{
%%     ~~Describe \code{niters} here~~
}
  \item{conv.limit}{
%%     ~~Describe \code{conv.limit} here~~
}
  \item{setsize}{
%%     ~~Describe \code{setsize} here~~
}
  \item{nruns}{
%%     ~~Describe \code{nruns} here~~
}
  \item{Covs}{
%%     ~~Describe \code{Covs} here~~
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
function (inData, inInits, pars.to.save, model, model.file, n.chains, 
    niters, conv.limit, setsize, nruns, Covs) 
{
    mod = jags.model(model.file, data = inData, inits = inInits, 
        n.chains = n.chains, n.adapt = 0)
    done.adapt = adapt(mod, n.iter = 2 * setsize, end.adaptation = FALSE)
    while (!done.adapt) done.adapt = adapt(mod, n.iter = 2 * 
        setsize, end.adaptation = FALSE)
    samples <- coda.samples(model = mod, variable.names = pars.to.save, 
        n.iter = setsize, thin = 1)
    nsamples = setsize
    varnames <- dimnames(samples[[3]])[[2]]
    alpha.vars <- grep("alpha", varnames)
    beta.vars <- grep("beta", varnames)
    Sd.vars <- grep("Sd", varnames)
    if (!is.null(Covs)) 
        slope.vars <- grep("slope", varnames)
    Mean.vars <- grep("theta", varnames)
    if (is.null(Covs)) {
        temp.samples.array = array(NA, c(niters, n.chains, length(alpha.vars) + 
            length(beta.vars) + length(Sd.vars)))
        temp.samples.array[seq(nsamples), 1, ] <- samples[[1]][, 
            c(alpha.vars, beta.vars, Sd.vars)]
        temp.samples.array[seq(nsamples), 2, ] <- samples[[2]][, 
            c(alpha.vars, beta.vars, Sd.vars)]
        temp.samples.array[seq(nsamples), 3, ] <- samples[[3]][, 
            c(alpha.vars, beta.vars, Sd.vars)]
        dimnames(temp.samples.array) <- list(NULL, NULL, varnames[c(alpha.vars, 
            beta.vars, Sd.vars)])
    }
    else {
        temp.samples.array = array(NA, c(niters, n.chains, length(alpha.vars) + 
            length(beta.vars) + length(Sd.vars) + length(slope.vars)))
        temp.samples.array[seq(nsamples), 1, ] <- samples[[1]][, 
            c(alpha.vars, slope.vars, beta.vars, Sd.vars)]
        temp.samples.array[seq(nsamples), 2, ] <- samples[[2]][, 
            c(alpha.vars, slope.vars, beta.vars, Sd.vars)]
        temp.samples.array[seq(nsamples), 3, ] <- samples[[3]][, 
            c(alpha.vars, slope.vars, beta.vars, Sd.vars)]
        dimnames(temp.samples.array) <- list(NULL, NULL, varnames[c(Sd.vars, 
            slope.vars, beta.vars, Sd.vars)])
    }
    max.bgrRatio = max(apply(temp.samples.array[seq(nsamples), 
        , , drop = F], 3, maxbgrRatio))
    check <- max.bgrRatio > conv.limit
    print(max.bgrRatio)
    flush.console()
    if (check) {
        count <- 1
        while (check & (count < niters/setsize)) {
            samples <- coda.samples(mod, variable.names = pars.to.save, 
                n.iter = setsize, thin = 1)
            count <- count + 1
            if (is.null(Covs)) {
                temp.samples.array[nsamples + seq(setsize), 1, 
                  ] = samples[[1]][, c(alpha.vars, beta.vars, 
                  Sd.vars)]
                temp.samples.array[nsamples + seq(setsize), 2, 
                  ] = samples[[2]][, c(alpha.vars, beta.vars, 
                  Sd.vars)]
                temp.samples.array[nsamples + seq(setsize), 3, 
                  ] = samples[[3]][, c(alpha.vars, beta.vars, 
                  Sd.vars)]
            }
            else {
                temp.samples.array[nsamples + seq(setsize), 1, 
                  ] = samples[[1]][, c(alpha.vars, slope.vars, 
                  beta.vars, Sd.vars)]
                temp.samples.array[nsamples + seq(setsize), 2, 
                  ] = samples[[2]][, c(alpha.vars, slope.vars, 
                  beta.vars, Sd.vars)]
                temp.samples.array[nsamples + seq(setsize), 3, 
                  ] = samples[[3]][, c(alpha.vars, slope.vars, 
                  beta.vars, Sd.vars)]
            }
            nsamples = nsamples + setsize
            max.bgrRatio = max(apply(temp.samples.array[seq(nsamples), 
                , , drop = F], 3, maxbgrRatio))
            check <- max.bgrRatio > conv.limit
            print(max.bgrRatio)
            flush.console()
        }
    }
    no.to.converge = nsamples
    print(max.bgrRatio)
    flush.console()
    no.to.keep <- no.to.converge/2
    if (nruns > no.to.keep) 
        samples = coda.samples(mod, variable.names = pars.to.save, 
            n.iter = nruns, thin = 1)
    else {
        thin <- floor(no.to.keep/nruns)
        samples <- coda.samples(mod, variable.names = pars.to.save, 
            n.iter = no.to.keep, n.thin = thin)
    }
    samples.array = array(NA, c(dim(samples[[1]])[1], n.chains, 
        length(varnames)))
    samples.array[, 1, ] = samples[[1]]
    samples.array[, 2, ] = samples[[2]]
    samples.array[, 3, ] = samples[[3]]
    dimnames(samples.array) = list(NULL, NULL, varnames)
    out <- list(no.to.converge, dim(samples.array)[1], samples.array)
    names(out) <- c("BurnIn", "No. Runs Per Chain", "Samples")
    return(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
