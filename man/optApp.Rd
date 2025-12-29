\name{optApp}

\alias{optApp}

\title{
Compute the optimal fertilizer application for a crop and location.
}


\description{
Compute the optimal fertilizer application given a quefts model, the value of the crop product, and available fertilizer products, and their prices.

All computations are per ha and all prices should be in the same currency. Note that the value of the crop must be specified on a dry matter basis.

}

\usage{
optApp(qm, fertilizers, dm_crop_value, min_use=0, max_inv=Inf)
}

\arguments{
  \item{qm}{A quefts model}
  \item{fertilizers}{data.frame with fertilizer products, NPK content and prices (see examples). There must be at least these five variables: "name", "N", "P", "K" (nutrient content as percentage) and "price_kg" (price per kg of the product}
  \item{dm_crop_value}{numeric. The value of the crop product per kg dry matter (!). That is you need to divide the actual value with the fraction dry matter content. E.g. divide by ~.85 for grains and ~0.35 for cassava}
  \item{min_use}{numeric. The minimum amount allowed for a product, to avoid very low application rates}
  \item{max_inv}{positive number. The maximum allowed total investment}
}


\examples{
# fertilizer product list
fert <- fertilizers()
# shortening some of the names for display
fert[,2] = substr(fert[,2], 1, 20)
# contents are expressed as a percentage.
ferts <- fert[c(8,15:17), 2:5] 
ferts$price_kg <- c(1, 1.5, 1.25, 1.5)

soiltype <- quefts_soil()
barley <- quefts_crop("Barley")
fertilizer <- list(N=0, P=0, K=0)
att_yield <- list(leaf_att=2200, stem_att=2700, store_att=4800, SeasonLength=110)
q <- quefts(soiltype, barley, fertilizer, att_yield)

x <- optApp(q, ferts, dm_crop_value=.25)
x
x$fertilizer

y <- optApp(q, ferts, dm_crop_value=.75)
y
y$fertilizer

}

