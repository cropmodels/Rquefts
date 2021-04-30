

setMethod("predict", signature("Rcpp_QueftsModel"), 
function(object, supply, yatt, var="yield", filename="", overwrite=FALSE, ...)  {

	stopifnot(var %in% c("yield", "gap"))
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
	gap <- FALSE
	wopt <- list(...)
	if (var == "gap") {
		gap <- TRUE
		terra::nlyr(out) <- 3
		if (is.null(wopt$names)) wopt$names <- c("Ngap", "Pgap", "Kgap")
	} else {
		if (is.null(wopt$names)) wopt$names <- "yield"
	}
	b <- terra::writeStart(out, filename, overwrite, wopt=wopt)
	for (i in 1:b$n) {
		vs <- terra::readValues(supply, b$row[i], b$nrows[i], 1, nc, mat=TRUE)
		vy <- terra::readValues(yatt, b$row[i], b$nrows[i], 1, nc)
		v <- object$runbatch(vs[,1], vs[,2], vs[, 3], vy, var)	
		if (gap) {
			v <- as.vector(matrix(v, ncol=3, byrow=TRUE))
		} 
		terra::writeValues(out, v, b$row[i], b$nrows[i])
		
	}
	terra::readStop(supply)
	terra::readStop(yatt)
	out <- terra::writeStop(out)
	return(out)
}
)

