data.ord <- function (Y, Covs, ncat, prior, Treat, i) 
{
    inData = list(Treat = Treat, Y = Y, ncat = ncat, nobs = length(Y), 
                  mean.beta = prior$mean.beta, prec.beta = prior$prec.beta, 
                  lower.dc = prior$lower.dc, upper.dc = prior$upper.dc,
                  lower.c1 = prior$lower.c1, upper.c1 = prior$upper.c1)
    if (!is.null(Covs)) {
        inData[[1 + length(inData)]] = as.numeric(Covs)
        names(inData)[[length(inData)]] = "x"
        inData[[1 + length(inData)]] = prior$mean.slope
        names(inData)[[length(inData)]] = "mean.slope"
        inData[[1 + length(inData)]] = prior$prec.slope
        names(inData)[[length(inData)]] = "prec.slope"
    }
	
	if (i == 2) {
			x = inData$x
			j = k = l = x

			for (i in 2:length(x)) {
				if (is.na(j[i])) { j[i] = j[i - 1] }
			}

			for (i in (length(x) - 1):1) {
				if (is.na(l[i])) { l[i] = l[i + 1] }
			}

			j = j - .05
			l = l + .05		

			inData[[length(inData) + 1]] = j
			names(inData)[[length(inData)]] = "j"
			inData[[length(inData) + 1]] = l
			names(inData)[[length(inData)]] = "l"
		}
	
    return(inData)
}
