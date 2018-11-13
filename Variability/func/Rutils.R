# My R Utilities

# convert a data frame to a vector
avm <- function(x) {
	as.vector(as.matrix(x))
}

# list files in a way suitable for pasting into other code
lsc <- function(pattern) {
	if(missing(pattern)) {
		x <- ls(envir = .GlobalEnv)
		x
	} else {
		x <- ls(pattern=pattern, envir = .GlobalEnv)
	}
	cat(paste(x, collapse=", "), "\n")
	invisible(x)
}

# make a data frame into a SpatialGridDataFrame using the first two cols as coords
mkgrid <- function(x) {
    sp::coordinates(x) <- c(1, 2)
    sp::gridded(x) <- TRUE
    sp::fullgrid(x) <- TRUE
    x
}

# parse a comma-separated series of arguments
# useful when debugging functions with a lot of defaults values
parg <- function(x) {
    x <- gsub(",", ";", x)
    eval(parse(text=x), envir = .GlobalEnv)
    invisible()
}


