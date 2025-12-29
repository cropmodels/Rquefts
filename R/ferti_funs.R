
fertilizers <- function() {
	f <- system.file("extdata/fertilizers.csv", package="Rquefts")
	utils::read.csv(f)
}



nutrientRates <- function(supply, treatment) {
#	result <- matrix(nrow=ncol(supply)-1, ncol=ncol(treatment)-1)
	NPK <- supply[, c("N", "P", "K")] * treatment / 100
	apply(NPK, 2, sum)
}




# minimize total cost of a fertilizer treatment
# based on a function by Pieter Pypers
fertApp <- function(nutrients, fertilizers, price, exact=TRUE, retCost=FALSE){

	stopifnot(length(price) == nrow(fertilizers))
	name <- fertilizers$name
	supply <- t(as.matrix(fertilizers[,-1,drop=FALSE]))
	treatment <- as.matrix(nutrients)

	result <- matrix(nrow=ncol(supply), ncol=nrow(treatment))
	colnames(result) <- rownames(nutrients)
	if (!any(is.na(price))) {
		treatment <- treatment * 100
		for (i in 1:nrow(treatment)) {
			if (exact) {
				solution <- limSolve::linp(E=supply, F=treatment[i,], Cost=price)
			} else {
				solution <- limSolve::linp(G=supply, H=treatment[i,], Cost=price)		
			}
			if (solution$IsError) {
				result[,i] <- NA
			} else { 	
				result[,i] <- solution$X
			}
		}
	}
	if (retCost) {
		r <- colSums(result * price)
	} else {
		r <- data.frame(name, result)
		#r[apply(r[,-1,drop=FALSE], 1, function(i) !all(i==0)), ]	
	}
	r
}



optApp <- function(qm, fertilizers, dm_crop_value, min_use=0, max_inv=Inf) {

	stopifnot(all(c("name", "N", "P", "K", "price_kg") %in% names(fertilizers)))
	frt <- fertilizers[, c("name", "N", "P", "K", "price_kg")]
	frt <- stats::na.omit(frt)
	if (nrow(frt) == 0) stop("no complete fertilizer records")
	if (all(frt[, c("N", "P", "K")] < 1)) {
		stop("fertilizer NPK content should be expressed as a percentage")
	}
	# but we use a fraction
	frt[, c("N", "P", "K")] <- frt[, c("N", "P", "K")] / 100

	Rquefts::fert(qm) <- list(N=0, P=0, K=0)
	yield0 <- round(Rquefts::run(qm)[["store_lim"]])

	fopt <- function(p) {
		p <- round(p)
		p[p < min_use] <- 0
		cost <- sum(p * frt$pricekg)
		if (cost > max_inv) return(-1)
		Rquefts::fert(qm) <- as.list(colSums(p * frt[, c("N", "P", "K")]))
		yield <- Rquefts::run(qm)["store_lim"]
		#net revenue 
		(yield-yield0) * dm_crop_value - sum(p * frt[, "price_kg"])
	}
	
	nf <- nrow(frt)
	opt <- optim(par=rep(50, nf), fn=fopt, lower=rep(0, nf), method="L-BFGS-B", 
					control=list(fnscale=-.1, ndeps=rep(1, nf)))$par

	opt <- round(opt)
	opt[opt < min_use] <- 0

	NPK <- as.list(colSums(opt * frt[, c("N", "P", "K")]))
	fert(qm) <- NPK
	yield <- round(Rquefts::run(qm)[["store_lim"]])
	cost <- sum(opt * frt$price)
	NR <- ((yield-yield0) * dm_crop_value) - cost

	ferts <- data.frame(type=frt$name, amount=opt, price_kg=frt$price_kg)
	ferts <- ferts[ferts$amount > 0, ]
		
	d <- data.frame(NPK, prod0=yield0, prod=yield, cost=round(cost), NR=round(NR))
	d$fertilizer <- list(ferts)
	d
}

