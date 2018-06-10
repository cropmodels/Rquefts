#Rcpp::compileAttributes('/home/rhijmans/bitbucket/models/quefts/Rquefts')


"soil<-" <- function(x, value) {
	parameters <- c("K_base_supply", "K_recovery", "N_base_supply", "N_recovery", "P_base_supply", "P_recovery", "UptakeAdjust")
	nms <- names(value)
	if (!all(parameters %in% nms)) stop(paste("parameters missing:", paste(parameters[!(parameters %in% nms)], collapse=", ")))

	value <- value[parameters]
	nms <- names(value)
	
	lapply(1:length(value), function(i) eval(parse(text = paste0("x$soil$", nms[i], " <- ", value[i]))))
	return(x)
}

"crop<-" <- function(x, value) {
	parameters <- c("KmaxStore", "KmaxVeg", "KminStore", "KminVeg", "Nfix", "NmaxStore", "NmaxVeg", "NminStore", "NminVeg", "PmaxStore", "PmaxVeg", "PminStore", "PminVeg", "Yzero")
	nms <- names(value)
	
	if (!all(parameters %in% nms)) stop(paste("parameters missing:", paste(parameters[!(parameters %in% nms)], collapse=", ")))
	value <- value[parameters]
	nms <- names(value)
	lapply(1:length(value), function(i) eval(parse(text = paste0("x$crop$", nms[i], " <- ", value[i]))))
	return(x)
}


"fert<-" <- function(x, value) {
	parameters <- c("N", "P", "K")
	value <- value[parameters]
	nms <- names(value)
	lapply(1:length(value), function(i) eval(parse(text = paste0("x$", nms[i], " <- ", value[i]))))
	return(x)
}

"biom<-" <- function(x, value) {
	parameters <- c("leaf_att", "stem_att", "store_att", "SeasonLength")
	nms <- names(value)
	if (!all(parameters %in% nms)) stop(paste("parameters missing:", paste(parameters[!(parameters %in% nms)], collapse=", ")))
	value <- value[parameters]
	nms <- names(value)
	lapply(1:length(value), function(i) eval(parse(text = paste0("x$", nms[i], " <- ", value[i]))))
	return(x)
}


soilNutrientSupply <- function(pH, SOC, Kex, Polsen, Ptotal=NA) {
	# SOC in g/kg, Kex in mmol/kg, P in mg/kg
	# Janssen et al., 1990. Geoderma 46: 299-318, Table 2

	# for recycling
	d <- cbind(pH, SOC, Kex, Polsen, Ptotal)
	pH <- d[,1]
	SOC <- d[,2]
	Kex <- d[,3]
	Polsen <- d[,4]
	Ptotal <- d[,5]

	fN <- 0.25 * (pH - 3)
	N_base_supply <- fN * 6.8 * SOC

	fP <- 1 - 0.5 * (pH - 6)^2

	P_base_supply <- fP * 0.35 * SOC + 0.5 * Polsen
	i <- which(!is.na(Ptotal))
	if (length(i) > 0) {
		P_base_supply[i] <- fP[i] * 0.014 * Ptotal[i] + 0.5 * Polsen[i]
	}
	fK <- 0.625 * (3.4 - 0.4 * pH)
	K_base_supply <- (fK * 400 * Kex) / (2 + 0.9 * SOC)

	return(cbind(N_base_supply=N_base_supply, P_base_supply=P_base_supply, K_base_supply=K_base_supply))
}


quefts_fert <- function() {
	list(N=60, P=10, K=60)
}


quefts_biom <- function() {
	list(leaf_att=2500, stem_att=2500, store_att=5000, SeasonLength=120);
}


quefts_soil <- function() {
	list(N_base_supply=60, P_base_supply=10, K_base_supply=60,
	     N_recovery=0.5, P_recovery=0.1, K_recovery=0.5,
		 #N_fertilizer=0, P_fertilizer=0, K_fertilizer=0,
		 UptakeAdjust = matrix(c(0, 0, 40, 0.4, 80, 0.7, 120, 1, 240, 1.6, 360, 2, 1000, 2), ncol=2, byrow=TRUE)
		)
}


quefts_crop <- function(name='') {

	if (name == '') {
		d <- list(	
			NminStore=0.0095, NminVeg=0.004,  NmaxStore=0.022,  NmaxVeg=0.0125, 
			PminStore=0.0017, PminVeg=0.0004, PmaxStore=0.0075, PmaxVeg=0.003, 
			KminStore=0.002,  KminVeg=0.005,  KmaxStore=0.006,  KmaxVeg=0.02, 
			Yzero=400, Nfix=0)	
	} else {
		f <- system.file("extdata/quefts_crop_pars.csv", package="Rquefts")
		x <- utils::read.csv(f, stringsAsFactors=FALSE)
		if (name %in% x$crop) {
			d <- as.list(x[x$crop == name, ])
		} else {
			stop(name, " not found. Choose from: ", paste(d$crop, collapse=', '))
		}
	}	
	return(d)
}


setMethod ('show' , 'Rcpp_QueftsModel', 
	function(object) {
		utils::str(object)
	}
)	


quefts <- function(soil, crop, fert, biom) {
	if (missing(soil)) { crop <- quefts_soil() }
	if (missing(crop)) { crop <- quefts_crop() }
	if (missing(fert)) { fert <- quefts_fert() }
	if (missing(biom)) { biom <- quefts_biom() }	
	q <- QueftsModel$new()
	crop(q) <- crop
	soil(q) <- soil
	fert(q) <- fert
	biom(q) <- biom
	q
}

