\name{predict}

\alias{predict}
\alias{predict,Rcpp_QueftsModel-method}


\title{
Spatial QUEFTS model predictions
}

\description{
Make spatial predictions with a QUEFTS model. First create a model, then provide that to the predict methods, together with a SpatRaster object of soil properties as predictor.
}


\usage{
\S4method{predict}{Rcpp_QueftsModel}(object, supply, yatt, filename="", overwrite=FALSE, wopt=list(), ...)
}


\arguments{
  \item{object}{QUEFTSModel}
  \item{supply}{SpatRaster with nutrient supply data (Ns, Ps, Ks)}  
  \item{yatt}{SpatRaster with attainable yield}  
  \item{filename}{character. Output filename. Optional}
  \item{overwrite}{logical. If \code{TRUE}, \code{filename} is overwritten}
  \item{wopt}{list. Options for writing files as in \code{\link[terra]{writeRaster}}}
  \item{...}{addittional arguments. None implemented}  
}


\value{
SpatRaster
}

\examples{
library(terra)

ff <- list.files(system.file("sp", package="Rquefts"), full.names=TRUE)
r <- rast(ff)

soil <- r[[c("Tavg", "pH", "SOC", "Kex", "Pex", "Ptot")]]
supply <- lapp(soil, nutSupply2)
plot(supply)

ya <- rast(system.file("sp/Ya.tif", package="Rquefts"))

maize <- quefts_crop("Maize")
fertilizer <- list(N=0, P=0, K=0)
q <- quefts(crop=maize, fert=fertilizer)

p <- predict(q, supply, Ya)
plot(p)
}

