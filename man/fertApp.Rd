\name{fertApp}

\alias{fertApp}

\title{
Optimal fertilizer application
}


\description{
Compute the optimal fertilizer application rates given a target nutrient application and the available products (fertilizer blends) and their prices. 
}

\usage{
fertApp(nutrients, fertilizers, price)
}

\arguments{
  \item{nutrients}{data.frame with columns "N", "P", "K" in kg (per unit area)}
  \item{fertilizers}{data.frame with fertilizer products (see examples)}
  \item{price}{numeric. fertilizer product prices}
}

\author{Pieter Pypers}

\examples{
# fertilizer product list
fert <- fertilizers()
# shortening some of the names for display
fert[,2] = substr(fert[,2], 1, 20)
# contents are expressed as a percentage.
ferts <- fert[c(8,15,16), 2:5] 
ferts
npk <- data.frame("NPK", 20, 20, 20)
names(npk) <- names(ferts)
ferts <- rbind(ferts, npk)

x <- fertApp(data.frame(N=100, P=50, K=50), ferts, c(1, 1.5, 1.25, 1))
# show that it is correct
nutrientRates(ferts, x[,2])

fertApp(data.frame(N=seq(0,200,50), P=50, K=50), ferts, c(1, 1.5, 1.25, 0.75))
fertApp(data.frame(N=seq(0,200,50), P=50), ferts[,-3], c(1, 1.5, 1.25, 0.75))
fertApp(data.frame(N=seq(0,200,50), P=50), ferts[,-3], c(1, 1.5, 1.25, 5.75))
}

