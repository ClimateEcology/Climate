gslength <- function(tmin, base = 0) {
	# calculate growing season / frost-free period
	# base temperature in C

	frostdays <- tmin < base

	if(all(frostdays)) {
		# never above base temperature
		# return NA, NA, length = 0
		gslength <- 0
		lastinspring <- NA
		firstinautumn <- NA
	} else {
		if(all(!frostdays)) {
			# never below base temperature
			gslength <- length(tmin)
			lastinspring <- 0
			firstinautumn <- gslength + 1
		} else {
			# has some above and some below base temperature
			# growing season is longest period above base temperature
			frostdays.rle <- rle(frostdays)
            gslength <- max(frostdays.rle$length[frostdays.rle$values == FALSE], na.rm=TRUE)


            tempid <- min(which(frostdays.rle$length == gslength & frostdays.rle$values == FALSE), na.rm=TRUE)
			if(tempid == 1) {
				lastinspring <- 0
			} else {
				lastinspring <- sum(frostdays.rle$lengths[seq(1, tempid - 1)], na.rm=TRUE)
			}
			if(tempid == length(frostdays.rle$length)) {
				firstinautumn <- length(tmin) + 1
			} else {
            	firstinautumn <- lastinspring + gslength + 1
			}

		}
	}

	list(gslength = gslength, lastinspring = lastinspring, firstinautumn = firstinautumn)
}


