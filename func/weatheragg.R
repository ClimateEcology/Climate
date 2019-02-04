

# function to calculate aggregated weather statistics over a given set of data

weatheragg <- function(dat, byvar, tmaxcol="TMAX.VALUE", tmincol="TMIN.VALUE", precipcol="PRCP.VALUE", preciplim = 0) {

    owarn <- options()$warn
    options(warn = -1)

	dat <- data.frame(dat)

    if(missing(byvar))
        byvar <- rep(1, nrow(dat))

	if(is.null(dim(byvar)) & length(byvar) < nrow(dat))
        indexdat  <- dat[, byvar, drop=FALSE]
    else
        indexdat  <- data.frame(byvar, stringsAsFactors=FALSE)

    tempdat   <- dat[, c(tmaxcol, tmincol)]
	colnames(tempdat) <- c("tmax", "tmin")
	precipdat <- dat[, c(precipcol), drop=FALSE]
	colnames(precipdat) <- "precip"

    indexdat.levels <- aggindex(indexdat)

	# missing data and numbers of days
	ndays <- cbind(is.na(tempdat), is.na(precipdat), rep(1, nrow(dat)))
	colnames(ndays) <- c("tmax.NA", "tmin.NA", "precip.NA", "n")
	ndays <- aggfun(ndays, indexdat, FUN=sum)

	# extreme minimum temperature
	tmin.min <- aggfun(tempdat$tmin, indexdat, FUN=min)
    tmin.min[is.infinite(tmin.min)] <- NA

	# extreme maximum temperature
	tmax.max <- aggfun(tempdat$tmax, indexdat, FUN=max)
    tmax.max[is.infinite(tmax.max)] <- NA
    
    # temperature standard deviations
    tmin.sd <- aggfun(tempdat$tmin, indexdat, FUN=sd)
    tmax.sd <- aggfun(tempdat$tmax, indexdat, FUN=sd)

    # temperature averages
	tmean <- data.frame(tempdat, tmean = (tempdat$tmin + tempdat$tmax)/2, tdiff = tempdat$tmax - tempdat$tmin)
	tmean <- aggfun(tmean, indexdat, FUN=mean)

    # precipitation summaries
	precip <- data.frame(precipdat, nprecip = precipdat$precip > preciplim)
	precip <- aggfun(precip, indexdat, FUN=sum) 

    pstats <- t(aggfun(precipdat$precip, indexdat, FUN=precipstats, llim=preciplim))

    options(warn = owarn)

	results <- data.frame(indexdat.levels, ndays, tmin.min, tmax.max, tmean, tmin.sd, tmax.sd, precip, pstats, stringsAsFactors=FALSE)
    results[complete.cases(ndays), ] # may want to remove this for more complex/problematic datasets
}

