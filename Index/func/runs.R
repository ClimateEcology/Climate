# Trim the ends before measuring runs

runtrim <- function(dat) {

    startfun <- function(y) {
        if(is.na(y[1]))
           startitem <- min(which(!is.na(y)))
        else
            startitem <- min(which(y != y[1]))
        startitem
    }

    endfun <- function(y) {
        n <- length(y)
        if(is.na(y[n]))
            enditem <- max(which(!is.na(y)))
        else 
            enditem <- max(which(y != y[n]))
        enditem
    }

	# trims ends of data to eliminate spurious runs
	# should work on any type of values
	# works on a vector or a matrix
    # returns a logical vector keep/discard
	if(is.vector(dat)) {
		startitem <- startfun(dat)
		enditem <- endfun(dat)
        keepitems <- rep(FALSE, length(dat))
	} else {
		orig.class <- class(dat)
		dat <- as(dat, "matrix")
		startitem <- min(apply(dat, 1, startfun))
		enditem   <- max(apply(dat, 1, endfun))
        keepitems <- rep(FALSE, nrow(dat))
    }
    
    keepitems[seq(startitem, enditem)] <- TRUE
    keepitems
}

# calculate run lengths

runs <- function(..., trim=TRUE) {

    if(length(list(...)) == 1) {
        dat <- ..1
        all.levels <- unique(dat)
    } else {
        # want to identify missing combinations
        all.levels <- list(...)
        all.levels <- lapply(all.levels, function(x)sort(unique(as.character(x))))
        all.levels <- expand.grid(all.levels)
        all.levels <- do.call(paste, all.levels)

        dat <- do.call(paste, list(...))
    }


    # if trim is a vector, use it to truncate dat
    # if it's logical TRUE, call runtrim()
    # otherwise, leave dat alone
    if(length(trim) > 1) {
        dat <- dat[keepitems]
    } else {
        if(trim) {
            dat <- dat[runtrim(dat)]
        }
    }

    dat <- rle(dat)
    split(dat$len, factor(dat$values, levels=all.levels))

}


