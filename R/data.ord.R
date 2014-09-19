data.ord <- function (Y, Covs, ncat, prior, Treat, mod.id) 
{
	inData = list(Treat = Treat, Y = Y, ncat = ncat, nobs = length(Y), 
		mean.beta = prior$mean.beta, prec.beta = prior$prec.beta, 
		lower.dc = prior$lower.dc, upper.dc = prior$upper.dc,
		lower.c1 = prior$lower.c1, upper.c1 = prior$upper.c1)
    if (!is.null(Covs)) {
		if (mod.id != 4) {
			inData[[1 + length(inData)]] = Covs
			names(inData)[[length(inData)]] = "x"
		}
        inData[[1 + length(inData)]] = prior$mean.slope
        names(inData)[[length(inData)]] = "mean.slope"
        inData[[1 + length(inData)]] = prior$prec.slope
        names(inData)[[length(inData)]] = "prec.slope"
    }
	
	if (mod.id == 2 || mod.id == 4.1) { tm = ncol(inData$x)
	} else if (mod.id == 5.2) { tm = 1 
	} else if (mod.id == 5.41) { tm = 2 }
	
	if (mod.id %in% c(2, 4.1, 5.2, 5.41)) {
		x = inData$x[ , tm]
		j = l = x

		if (is.na(j[1])) { j[1] = 1 }
		if (is.na(l[length(l)])) { l[length(l)] = l[length(l) - 1] + 1 }
		for (ix in 2:length(x)) {
			if (is.na(j[ix])) { j[ix] = j[ix - 1] }
		}
		for (ix in (length(x) - 1):1) {
			if (is.na(l[ix])) { l[ix] = l[ix + 1] }
		}
		for (i in (length(j) - 1):1) {
			if (is.na(j[i])) { j[i] = j[i + 1] - 1 }
		}
		for (i in 2:length(l)) {
			if (is.na(l[i])) { l[i] = l[i - 1] + 1 }
		}
		j = j - .05
		l = l + .05		

		inData[[length(inData) + 1]] = j
		names(inData)[[length(inData)]] = "j"
		inData[[length(inData) + 1]] = l
		names(inData)[[length(inData)]] = "l"
	}
	
	if (mod.id %in% c(4, 4.1, 4.2, 5.4, 5.41, 5.42)) {
	
		v = rep(0, ncat)
		for (i in 1:length(Y)) {
			if (!is.na(Y[i])) {
				v[Y[i]] = v[Y[i]] + 1
			}
		}
		v = v / sum(v)
		
#		v = c(0.1, 0.2, 0.4, 0.2, 0.1)
			
		inData[[length(inData) + 1]] = v
		names(inData)[[length(inData)]] = "v"
	}
	
    return(inData)
}
