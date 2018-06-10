\name{quefts}


\alias{quefts}
\alias{quefts_crop}
\alias{quefts_soil}
\alias{quefts_fert}
\alias{quefts_biom}
\alias{soil<-}
\alias{crop<-}
\alias{fert<-}
\alias{biom<-}


\title{
QUEFTS model
}

\description{
Create a QUEFTS model, set parameters, and run it to compute nutrient requirements and nutrient limited yield.

A number of default crop parameter sets are provided, as well as one example soil. You need to provide attainable (that is, yield without nutrient limitation), or target dry-matter biomass for leaves, stems and the storage organ (e.g. grain, root or tuber). Some crops are grown for the stems/leaves, in which case there is no relevant storage organ (e.g. sugarcane, jute). Attanable yield estimates can be obtained with a crop growht model. 
}

\usage{
quefts(soil, crop, fert, biom)
quefts_soil()
quefts_fert()
quefts_crop(name="")
quefts_biom()
crop(x) <- value
soil(x) <- value
fert(x) <- value
biom(x) <- value
}

\arguments{
 \item{soil}{list with named soil parameters. See Details. An example is returned by \code{quefts_soil()}}
 \item{crop}{list with named crop parameters. See Details. An example is returned by \code{quefts_crop()}} 
 \item{fert}{list with named fertilizer parameters (N, P and K). An example is returned by \code{quefts_fert()}} 
 \item{biom}{list with named biomass and growing season length parameters. An example is returned by \code{quefts_biom()}} 
 \item{name}{character. crop name} 
 \item{x}{QueftsModel object}
 \item{value}{a list with soil, crop, fertlizer, or yield parameters as above}
}


\details{


\tabular{lll}{
	\tab \bold{Input Parameters} \tab \bold{Explanation} \cr
	\tab \bold{Soil} \tab . \cr
	\tab \code{N_base_supply, P_base_supply, K_base_supply} \tab Potential supply (kg/ha) of N, P and K of the (unfertilzed) soil in a growing season of standard length (default is 120 days). See \code{\link{soilNutrientSupply}} to estimate that. \cr
	\tab \code{N_recovery, P_recovery, K_recovery} \tab Fertilzer recovey, that is, the fraction of applied fertilizer that can be taken up by the plant. \cr
	\tab \code{UptakeAdjust} \tab Two-colum matrix to compute the fraction uptake from soil supply as function of length of season. The default standard season length is 120 days. The first column is the length of the growing season, the second column is the fraction uptake. Intermediate values are computed by linear interpolation. \cr
	
	\tab \bold{Crop} \tab . \cr
	\tab \code{_minVeg, _maxVeg, _minStore, _maxStore} \tab minimum and maximum concentration of "\code{_}" (N, P, or K) in vegetative organs and in storage organs (kg/kg) \cr
	\tab \code{Yzero} \tab the maximum biomass of vegetative organs at zero yield of storage organs (kg/ha) \cr
	\tab \code{Nfix} \tab the fraction of a crop's nitrogen uptake supplied by biological fixation \cr

	\tab \bold{Managment} \tab . \cr
	\tab \code{N, P, K} \tab N, P, and K fertilzer applied. \cr

	\tab \bold{Crop yield} \tab . \cr
	\tab \code{leaf_att, stem_att, store_att} \tab Attainable (in the absence of nutirent limitation), or target crop biomass (dry-matter, kg/ha) for leaves, stems and storage organs.\cr
	\tab \code{SeasonLength} \tab Length of the growing season (days)\cr

	
	\tab . \tab \cr
	\tab . \tab \cr
	\tab \bold{Output Variables} \tab \bold{Explanation} \cr
	\tab \code{N_actual_supply, P_actual_supply, K_actual_supply} \tab nutrient uptake from soil (not fertilzer) (kg/ha) \cr
	\tab \code{leaf_lim, stem_lim, store_lim} \tab nutrient limited biomass of leaves, stems, and storage organ (kg/ha) \cr
	\tab \code{N_gap, P_gap, K_gap} \tab fertilizer required to reach the specified biomass (kg/ha) \cr
}
}


\value{
vector with output variables as described in the Details
}

\references{
Janssen et al., 1990. A system for the quantitative evaluation of tropical soils. Geoderma 46: 299-318

Sattari et al., 2014. Crop yield response to soil fertility and N, P, K inputs in different environments: Testing and improving the QUEFTS model. Field Crops Research 157:35-46
}


\examples{

library(Rquefts)

# create a QUEFTS model
# 1 get parameters
soiltype <- quefts_soil()
barley <- quefts_crop('Barley')
fertilizer <- list(N=0, P=0, K=0)
yld <- list(leaf_att=2200, stem_att=2700, store_att=4800, SeasonLength=110)

# create model
q <- quefts(soiltype, barley, fertilizer, yld)

# run the model
q$run()

# change some parameters
q$SeasonLength <- 162
q$leaf_att <- 2651
q$stem_att <- 5053
q$store_att <- 8208

q$N <- 100
q$P <- 50
q$K <- 50

q$run()


fert(q) <- list(N=0, P=0, K=0)
N <- 1:20*10

results <- matrix(nrow=length(N), ncol=12)
colnames(results) <- names(q$run())
for (i in 1:length(N)) {
	q$N <- N[i]
	results[i,] <- q$run()
}

yield <- results[,'store_lim']

par(mfrow=c(1,2))
plot(N, yield, type='l', lwd=2)
Efficiency = yield / N
plot(N, Efficiency, type='l', lwd=2)

f <- expand.grid(N=seq(0,400,10), P=seq(0,400,10), K=c(0,200))
x <- rep(NA, nrow(f))
for (i in 1:nrow(f)) {0
	q$N <- f$N[i] 
	q$P <- f$P[i] 
	q$K <- f$K[i] 
	x[i] <- q$run()['store_lim']
}
x <- cbind(f, x)

\dontrun{

library(raster  )
r0 <- rasterFromXYZ(x[x$K==0, -3])
r200 <- rasterFromXYZ(x[x$K==200, -3])

par(mfrow=c(1,2))
plot(r0, xlab='N fertilizer', ylab='P fertilizer', las=1, main="K=0")
contour(r0, add=TRUE)
plot(r200, xlab='N fertilizer', ylab='P fertilizer', las=1, main="K=200")
contour(r200, add=TRUE)
plot(stack(r0, r200))
}
}