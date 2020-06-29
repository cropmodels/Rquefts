

setMethod("predict", signature("Rcpp_QueftsModel"), 
function(object, supply, yatt, filename="", overwrite=FALSE, wopt=list(), ...)  {

	stopifnot(inherits(supply, "SpatRaster"))
	stopifnot(inherits(yatt, "SpatRaster"))
	stopifnot(terra::nlyr(supply) == 3)
	stopifnot(terra::nlyr(yatt) == 1)
	nms <- toupper(substr(names(supply), 1, 1))
	if (!all(nms == c("N", "P", "K"))) {
		stop("the names of 'supply' must start with 'N', 'P', 'K'")
	}
	names(supply) <- c("Ns", "Ps", "Ks")
	terra::compareGeom(supply, yatt, lyrs=FALSE, crs=TRUE, warncrs=TRUE, ext=TRUE, rowcol=TRUE, res=FALSE)

	terra::readStart(supply)
	on.exit(terra::readStop(supply))
	terra::readStart(yatt)
	on.exit(terra::readStop(yatt))

	out <- terra::rast(yatt)
	names(out) <- "yield"
	
	nc <- ncol(yatt)
	b <- terra::writeStart(out, filename, overwrite, wopt)
	for (i in 1:b$n) {
		vs <- terra::readValues(supply, b$row[i], b$nrows[i], 1, nc, mat=TRUE)
		vy <- terra::readValues(yatt, b$row[i], b$nrows[i], 1, nc)
		v <- run(object, vs, vy)
		terra::writeValues(out, v, b$row[i], b$nrows[i])
	}
	out <- terra::writeStop(out)
	return(out)
}
)

