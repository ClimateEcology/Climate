# wrapper function for climdex.pcic


climdex <- function(x, tmaxcol="TMAX.VALUE", tmincol="TMIN.VALUE", precipcol="PRCP.VALUE", datecol="Date", yearcol="YEAR", monthcol="MONTH", daycol="DAY", base.range=c(1981, 2010)) {

	# works on raw daily data; column names are GHCN defaults from VFS::read.dly()
	# can take a date column or year, month, day columns

	tmax <- x[[tmaxcol]]
	tmin <- x[[tmincol]]
	precip <- x[[precipcol]]

	# get dates and convert into PCICt format
	if(datecol %in% colnames(x)) {
		date <- x[[datecol]]
	} else {
		date <- as.Date(paste(x[[yearcol]], x[[monthcol]], x[[daycol]], sep="-"))
	}
	date <- as.PCICt(format(date, "%Y-%m-%d"), cal="gregorian")

	cdat <- climdexInput.raw(tmax=tmax, tmin=tmin, prec=precip, tmax.dates=date, tmin.dates=date, prec.dates=date, base.range=base.range)

    func.names <- climdex.get.available.indices(cdat)

    thisclimdex <- lapply(seq_along(func.names), function(x)do.call(func.names[x], list(cdat)))
    names(thisclimdex) <-  func.names

    clen <- sapply(thisclimdex, length)
    thisclimdex.year <- do.call(data.frame, thisclimdex[clen == length(unique(format(date, "%Y")))])
    thisclimdex.month <- do.call(data.frame, thisclimdex[clen == length(unique(format(date, "%Y")))*12])


    list(year=thisclimdex.year, month=thisclimdex.month)

}

