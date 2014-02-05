\name{analyze}
\alias{analyze}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
analyze(Pain, Fatigue, Drowsy, Sleep, Thinking, Constipation, Treat, Neuropain, score.range = c(30, 4, 5, 4, 4, 4, 30), Covs, slopeprior, nChains = 3, conv.limit = 1.05, niters = 50000, nruns = 5000, setsize = 4000, alphaprior, beta.norm.prior, beta.ord.prior, c1prior, dcprior, varprior, varprior.params, path = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Pain}{
%%     ~~Describe \code{Pain} here~~
}
  \item{Fatigue}{
%%     ~~Describe \code{Fatigue} here~~
}
  \item{Drowsy}{
%%     ~~Describe \code{Drowsy} here~~
}
  \item{Sleep}{
%%     ~~Describe \code{Sleep} here~~
}
  \item{Thinking}{
%%     ~~Describe \code{Thinking} here~~
}
  \item{Constipation}{
%%     ~~Describe \code{Constipation} here~~
}
  \item{Treat}{
%%     ~~Describe \code{Treat} here~~
}
  \item{Neuropain}{
%%     ~~Describe \code{Neuropain} here~~
}
  \item{score.range}{
%%     ~~Describe \code{score.range} here~~
}
  \item{Covs}{
%%     ~~Describe \code{Covs} here~~
}
  \item{slopeprior}{
%%     ~~Describe \code{slopeprior} here~~
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
  \item{alphaprior}{
%%     ~~Describe \code{alphaprior} here~~
}
  \item{beta.norm.prior}{
%%     ~~Describe \code{beta.norm.prior} here~~
}
  \item{beta.ord.prior}{
%%     ~~Describe \code{beta.ord.prior} here~~
}
  \item{c1prior}{
%%     ~~Describe \code{c1prior} here~~
}
  \item{dcprior}{
%%     ~~Describe \code{dcprior} here~~
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
function (Pain, Fatigue, Drowsy, Sleep, Thinking, Constipation, 
    Treat, Neuropain, score.range = c(30, 4, 5, 4, 4, 4, 30), 
    Covs, slopeprior, nChains = 3, conv.limit = 1.05, niters = 50000, 
    nruns = 5000, setsize = 4000, alphaprior, beta.norm.prior, 
    beta.ord.prior, c1prior, dcprior, varprior, varprior.params, 
    path = "") 
{
    nobs = length(Pain)
    stopifnot(length(Pain) == nobs, length(Fatigue) == nobs, 
        length(Drowsy) == nobs, length(Sleep) == nobs, length(Thinking) == 
            nobs, length(Constipation) == nobs, length(Neuropain) == 
            nobs, length(Treat) == nobs)
    stopifnot(length(varprior) == 2)
    stopifnot(length(alphaprior) == 3, length(beta.norm.prior) == 
        3, length(beta.ord.prior) == 3)
    stopifnot(length(dcprior) == 3, length(c1prior) == 3, length(varprior.params) == 
        2)
    varprior = as.list(varprior)
    alphaprior = as.list(alphaprior)
    beta.norm.prior = as.list(beta.norm.prior)
    beta.ord.prior = as.list(beta.ord.prior)
    dcprior = as.list(dcprior)
    outnames = c("Pain", "Fatigue", "Drowsy", "Sleep", "Thinking", 
        "Constipation", "Neuropathic Pain")
    names(score.range) = outnames
    Pain.out = run.norm(Pain, Treat, Covs, "normal", "hom", nChains, 
        conv.limit, niters, nruns, setsize, slopeprior, alphaprior, 
        beta.norm.prior, varprior, varprior.params, path = "")
    Fatigue.out = run.ord(Fatigue, Treat, Covs, score.range[2] + 
        1, "cumlogit", nChains, conv.limit, niters, nruns, setsize, 
        beta.ord.prior, dcprior, c1prior, path)
    Drowsy.out = run.ord(Drowsy, Treat, Covs, score.range[3] + 
        1, "cumlogit", nChains, conv.limit, niters, nruns, setsize, 
        beta.ord.prior, dcprior, c1prior, path)
    Sleep.out = run.ord(Sleep, Treat, Covs, score.range[4] + 
        1, "cumlogit", nChains, conv.limit, niters, nruns, setsize, 
        beta.ord.prior, dcprior, c1prior, path)
    Thinking.out = run.ord(Thinking, Treat, Covs, score.range[5] + 
        1, "cumlogit", nChains, conv.limit, niters, nruns, setsize, 
        beta.ord.prior, dcprior, c1prior, path)
    Constipation.out = run.ord(Constipation, Treat, Covs, score.range[6] + 
        1, "cumlogit", nChains, conv.limit, niters, nruns, setsize, 
        beta.ord.prior, dcprior, c1prior, path)
    Neuropain.out = run.norm(Neuropain, Treat, Covs, "normal", 
        "hom", nChains, conv.limit, niters, nruns, setsize, slopeprior, 
        alphaprior, beta.norm.prior, varprior, varprior.params, 
        path = "")
    Pain.treat.diff = Pain.out[["beta"]]
    Fatigue.treat.diff = treat.diffs(Fatigue.out[["p"]], Treat)
    Drowsy.treat.diff = treat.diffs(Drowsy.out[["p"]], Treat)
    Sleep.treat.diff = treat.diffs(Sleep.out[["p"]], Treat)
    Thinking.treat.diff = treat.diffs(Thinking.out[["p"]], Treat)
    Constipation.treat.diff = treat.diffs(Constipation.out[["p"]], 
        Treat)
    Neuropain.treat.diff = Neuropain.out[["beta"]]
    Pain.treat.diff.change = treat.diff.change(Pain.treat.diff, 
        score.range[1])
    Fatigue.treat.diff.change = treat.diff.change(Fatigue.treat.diff, 
        score.range[2])
    Drowsy.treat.diff.change = treat.diff.change(Drowsy.treat.diff, 
        score.range[3])
    Sleep.treat.diff.change = treat.diff.change(Sleep.treat.diff, 
        score.range[4])
    Thinking.treat.diff.change = treat.diff.change(Thinking.treat.diff, 
        score.range[5])
    Constipation.treat.diff.change = treat.diff.change(Constipation.treat.diff, 
        score.range[6])
    Neuropain.treat.diff.change = treat.diff.change(Neuropain.treat.diff, 
        score.range[1])
    change.interval = round(rbind(Pain.treat.diff.change[[1]], 
        Fatigue.treat.diff.change[[1]], Drowsy.treat.diff.change[[1]], 
        Sleep.treat.diff.change[[1]], Thinking.treat.diff.change[[1]], 
        Constipation.treat.diff.change[[1]], Neuropain.treat.diff.change[[1]]), 
        3)
    change.probs = round(rbind(Pain.treat.diff.change[[2]], Fatigue.treat.diff.change[[2]], 
        Drowsy.treat.diff.change[[2]], Sleep.treat.diff.change[[2]], 
        Thinking.treat.diff.change[[2]], Constipation.treat.diff.change[[2]], 
        Neuropain.treat.diff.change[[2]]), 3)
    output <- list()
    for (i in 1:nrow(change.interval)) {
        output[[i]] = list(interval = structure(as.list(change.interval[i, 
            ]), names = c("P025", "Median", "P975")), probs = structure(as.list(change.probs[i, 
            ]), names = paste("Proportion", c("< -0.2", "-0.2 - 0", 
            "0 - 0.2", "> 0.2"))))
    }
    names(output) = outnames
    return(output)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
