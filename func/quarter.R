quarter <- function(x, tmaxcol="tmax", tmincol="tmin", tmeancol="tmean", precipcol="precip", period = 3, rotate=FALSE) {
	# calculate statistics on a rolling period 
    # if rotate, will add beginning data to the end to get a complete set
    # - this is how climates::bioclim() handles yearly data

    if(rotate) {
        x <- rbind(x, x[seq(1, period - 1), ])
    }

	tmax <- x[[tmaxcol]]
	tmin <- x[[tmincol]]
	precip <- x[[precipcol]]

	if(!(tmeancol %in% colnames(x)))
		tmean <- (tmax + tmin) / 2
    else
        tmean <- x[[tmeancol]]

	maxrow <- length(tmax) - period + 1


	dat <- matrix(NA, nrow=maxrow, ncol=4)

	for(i in seq(1, maxrow)) {
		thisrows <- seq(i, i + period - 1)

        thisvar <- tmax[thisrows]
        thisvar <- thisvar[!is.na(thisvar)]
		dat[i, 1] <- fastmean(thisvar)

        thisvar <- tmin[thisrows]
        thisvar <- thisvar[!is.na(thisvar)]
		dat[i, 2] <- fastmean(thisvar)

        thisvar <- tmean[thisrows]
        thisvar <- thisvar[!is.na(thisvar)]
		dat[i, 3] <- fastmean(thisvar)

        thisvar <- precip[thisrows]
        thisvar <- thisvar[!is.na(thisvar)]
		dat[i, 4] <- sum(thisvar)
	}

	dat <- data.frame(dat)
	colnames(dat) <- c("tmax", "tmin", "tmean", "precip")

	dat
}

