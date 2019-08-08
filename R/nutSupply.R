
nutSupply <- function(pH, SOC, Kex, Polsen, Ptotal=NA) {
	# SOC in g/kg, Kex in mmol/kg, P in mg/kg
	# Janssen et al., 1990. Geoderma 46: 299-318, Table 2
	# for recycling
	d <- cbind(as.vector(pH), as.vector(SOC), as.vector(Kex), as.vector(Polsen), as.vector(Ptotal))
	#pH <- d[,1]
	fN <- 0.25 * (d[,1] - 3)
	#SOC <- d[,2]
	N_base_supply <- fN * 6.8 * d[,2]
	fP <- 1 - 0.5 * (d[,1] - 6)^2
	#Polsen <- d[,4]
	P_base_supply <- fP * 0.35 * d[,2] + 0.5 * d[,4]
	#Ptotal <- d[,5]
	i <- which(!is.na(d[,5]))
	if (length(i) > 0) {
		P_base_supply[i] <- fP[i] * 0.014 * d[,5][i] + 0.5 * d[,4][i]
	}
	fK <- 0.625 * (3.4 - 0.4 * d[,1])
	#Kex <- d[,3]
	K_base_supply <- (fK * 400 * d[,3]) / (2 + 0.9 * d[,2])
	return(cbind(N_base_supply=N_base_supply, P_base_supply=P_base_supply, K_base_supply=K_base_supply))
}

