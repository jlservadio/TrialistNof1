\name{model.ord}
\alias{model.ord}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
model.ord(Covs, prior, path)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Covs}{
%%     ~~Describe \code{Covs} here~~
}
  \item{prior}{
%%     ~~Describe \code{prior} here~~
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
function (Covs, prior, path) 
{
    cat("model\n{", file = paste(path, "model.txt", sep = ""))
    cat("\n    for (i in 1:nobs) {\n       p[i,1] <- 1 - Q[i,1]\n       for (r in 2:(ncat-1)) {\n          p[i,r] <- Q[i,r-1] - Q[i,r]\n       }\n       p[i,ncat] <- Q[i,(ncat-1)]\n       for (r in 1:(ncat-1)) {\n          logit(Q[i,r]) <- beta*Treat[i] - c[r]\n       }\n\t   Y[i] ~ dcat(p[i,])\n    }\n    for (i in 2:(ncat-1)) {", 
        file = paste(path, "model.txt", sep = ""), append = T, 
        sep = "")
    cat("\n       dc[i] ~", prior$Prior.dc, file = paste(path, 
        "model.txt", sep = ""), append = T, sep = "")
    cat("\n    }\n    c[1] <- dc[1]\n    for (i in 2:(ncat-1)) {\n       c[i] <- c[i-1] + dc[i]\n    }", 
        file = paste(path, "model.txt", sep = ""), append = T, 
        sep = "")
    cat("\n       dc[1] ~", prior$Prior.c1, file = paste(path, 
        "model.txt", sep = ""), append = T, sep = "")
    cat("\n    beta ~ ", prior$Prior.beta, file = paste(path, 
        "model.txt", sep = ""), append = T, sep = "")
    cat("\n    or <- exp(beta)", file = paste(path, "model.txt", 
        sep = ""), append = T, sep = "")
    cat("\n    }", file = paste(path, "model.txt", sep = ""), 
        append = T, sep = "")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
