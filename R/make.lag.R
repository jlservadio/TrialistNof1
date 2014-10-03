make.lag <- function(observations) {
	Pain2.lag = Fatigue2.lag = Drowsy2.lag = Sleep2.lag = Thinking2.lag = Constipation2.lag = Neuropain2.lag = 
		rep(NA, nrow(observations))
		
	for (i in 2:nrow(observations)) {
		Pain2.lag[i] = observations$Pain2[i - 1]
		Fatigue2.lag[i] = observations$Fatigue2[i - 1]
		Drowsy2.lag[i] = observations$Drowsy2[i - 1]
		Sleep2.lag[i] = observations$Sleep2[i - 1]
		Thinking2.lag[i] = observations$Thinking2[i - 1]
		Constipation2.lag[i] = observations$Constipation2[i - 1]
		Neuropain2.lag[i] = observations$Neuropain2[i - 1]
	}
	
	Lag.Covs = cbind(Pain2.lag, Fatigue2.lag, Drowsy2.lag, Sleep2.lag, Thinking2.lag, Constipation2.lag, Neuropain2.lag)
	colnames(Lag.Covs) = c("Pain2.lag", "Fatigue2.lag", "Drowsy2.lag", "Sleep2.lag", "Thinking2.lag", 
		"Constipation2.lag", "Neuropain2.lag")
		
	return(Lag.Covs)
}
