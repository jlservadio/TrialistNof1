treat.diffs <- function(p, Treat)
{
	expect.score = apply(p,1:2, function(x) sum(x*seq(length(x)))) #expected score matrix dim nsims x nobs
	treat.diff = apply(expect.score,1, function(x,Treat) sum(x*(2*Treat-1))/(length(x)/2), Treat) #dim = nsims
}

