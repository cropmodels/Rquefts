

setMethod("predict", signature("Rcpp_QueftsModel"), 
function(object, supply, yatt, filename="", overwrite=FALSE, wopt=list(), ...)  {

	stopifnot(inherits(supply, "SpatRaster"))
	stopifnot(terra::nlyr(supply) == 3)
	stopifnot(terra::hasValues(supply))
	stopifnot(inherits(yatt, "SpatRaster"))
	stopifnot(terra::nlyr(yatt) == 1)
	stopifnot(terra::hasValues(yatt))
	
	nms <- toupper(substr(names(supply), 1, 1))
	if (!all(nms == c("N", "P", "K"))) {
		stop("the names of 'supply' must start with 'N', 'P', 'K'")
	}
	names(supply) <- c("Ns", "Ps", "Ks")
	terra::compareGeom(supply, yatt, lyrs=FALSE, crs=TRUE, warncrs=TRUE, ext=TRUE, rowcol=TRUE, res=FALSE)

	terra::readStart(supply)
	terra::readStart(yatt)
	
	out <- terra::rast(yatt)
	nc <- ncol(out)
	if (is.null(wopt$names)) wopt$names <- "yield"
	b <- terra::writeStart(out, filename, overwrite, wopt)
	for (i in 1:b$n) {
		vs <- terra::readValues(supply, b$row[i], b$nrows[i], 1, nc, mat=TRUE)
		vy <- terra::readValues(yatt, b$row[i], b$nrows[i], 1, nc)
		v <- run(object, vs, vy)
		terra::writeValues(out, v, b$row[i], b$nrows[i])
	}
	terra::readStop(supply)
	terra::readStop(yatt)
	out <- terra::writeStop(out)
	return(out)
}
)

