treat.diffs <- function(p, Treat)
{
	expect.score = apply(p,1:2, function(x) sum(x*seq(length(x)))) #expected score matrix dim nsims x nobs
	
	expect.score.A = expect.score[ , Treat == 0]
	expect.score.B = expect.score[ , Treat == 1]
	
	mean.A = rowMeans(expect.score.A)
	mean.B = rowMeans(expect.score.B)
	
	n.A = dim(expect.score.A)[2]
	n.B = dim(expect.score.B)[2]
	
#	treat.diff = mean.B / n.B - mean.A / n.A

	treat.diff = mean.B - mean.A
}
