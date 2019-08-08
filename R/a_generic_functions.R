
if (!isGeneric("crop<-")) { setGeneric("crop<-", function(x, value) standardGeneric("crop<-")) }	
if (!isGeneric("soil<-")) { setGeneric("soil<-", function(x, value) standardGeneric("soil<-")) }	
if (!isGeneric("biom<-")) { setGeneric("biom<-", function(x, value) standardGeneric("biom<-")) }	
if (!isGeneric("fert<-")) { setGeneric("fert<-", function(x, value) standardGeneric("fert<-")) }	

if (!isGeneric("run")) { setGeneric("run", function(x, ...) standardGeneric("run")) }	

#if (!isGeneric("quefts")) { setGeneric("quefts", function(soil, crop, fert, biom) standardGeneric("quefts")) }	
