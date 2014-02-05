\name{var.hom}
\alias{var.hom}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
var.hom(prior, varprior, path)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{prior}{
%%     ~~Describe \code{prior} here~~
}
  \item{varprior}{
%%     ~~Describe \code{varprior} here~~
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
function (prior, varprior, path) 
{
    if (varprior[[1]] == "prec") {
        cat("\n            prec ~ ", prior$Prior.prec, "\n            sigma <- 1/prec", 
            file = paste(path, "model.txt", sep = ""), append = T, 
            sep = "")
    }
    else if (varprior[[1]] == "Sd") {
        cat("\n            Sd ~ ", prior$Prior.prec, "\n            prec <- 1/pow(Sd,2)\n            Sigma <- 1/prec", 
            file = paste(path, "model.txt", sep = ""), append = T, 
            sep = "")
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
