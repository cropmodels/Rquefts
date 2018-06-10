\name{soilNutrientSupply}

\alias{soilNutrientSupply}

\title{
Soil nutrients supply for QUEFTS model
}


\description{
Computes the base (unfertilzied) soil supply of N, P and K according to Janssen et al., 1990, Table 2. For use with the QUEFTS model.
}

\usage{
soilNutrientSupply(pH, SOC, Kex, Polsen, Ptotal=NA)
}

\arguments{
  \item{pH}{soi pH}
  \item{SOC}{Soil organic carbon (g/kg)}
  \item{Kex}{Exchangeble K (mmol/kg)}
  \item{Polsen}{P-Olsen (mg/kg)}  
  \item{Ptotal}{Total P (mg/kg)}  
}



\value{
Three column matrix with Nsup, Psup and Ksup (kg/ha): the potential supply of N, P and K of the (unfertilzed) soil.
}

\references{
Janssen et al., 1990. A system for the quantitative evaluation of tropical soils. Geoderma 46: 299-318
}


\examples{
s <- soilNutrientSupply(6, c(23, 11, 35), 15, c(1.6, 2.6, 2.4))
s
}

