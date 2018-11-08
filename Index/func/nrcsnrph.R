# Calculate NRCS climate indices from
# National Range and Pasture Handbook
# for one year of data


nrcsnrph <- function(x, tmaxcol="TMAX.VALUE", tmincol="TMIN.VALUE", precipcol="PRCP.VALUE", datecol="Date", yearcol="YEAR", monthcol="MONTH", daycol="DAY") {

	# works on raw daily data; column names are GHCN defaults from VFS::read.dly()
	# can take a date column or year, month, day columns
    # assumes a full year, in calendrical order
    #
    # expects temperature in C, precip in mm

	# get dates and convert into PCICt format
	if(datecol %in% colnames(x)) {
		date <- x[[datecol]]
	} else {
		date <- as.Date(paste(x[[yearcol]], x[[monthcol]], x[[daycol]], sep="-"))
	}
    month <- format(date, "%m")

	tmax <- x[[tmaxcol]]
	tmin <- x[[tmincol]]
    tmean <- (tmax + tmin) / 2
	precip <- x[[precipcol]]

    # 1. freeze-free period (28F) in days
    # 2. last killing freeze in spring (28F) date
    # 3. first killing freeze in fall (28F) date
    # returns 365/6, NA, NA if full year is freeze-free
    # returns 0, NA, NA if full year is below 28F
    # if there are multiple freeze-free periods of identical length, takes the first

    dayT.28 <- tmin < FtoC(28)
    if(all(dayT.28)) {
        # Never freezes
        dayT.28 <- length(dayT.28)
        dayT.28.last <- NA
        dayT.28.first <- NA
    } else {
        # No growing season
        if(all(!dayT.28)) {
            dayT.28 <- 0
            dayT.28.last <- NA
            dayT.28.first <- NA
        } else {
            dayT.28.rle <- rle(dayT.28)
            dayT.28 <- max(dayT.28.rle$length[dayT.28.rle$values == FALSE], na.rm=TRUE)
            tempid <- min(which(dayT.28.rle$length == dayT.28 & dayT.28.rle$values == FALSE), na.rm=TRUE)
            dayT.28.last <- sum(dayT.28.rle$lengths[seq(1, tempid - 1)], na.rm=TRUE)
            dayT.28.first <- dayT.28.last + dayT.28 + 1
        }
    }


    
    # 4. length of growing season (32 °F) in days
    # 5. last frost in spring (32 °F) date
    # 6. first frost in fall (32 °F) date
    # returns 365/6, NA, NA if full year is frost-free
    # returns 0, NA, NA if full year is below 32F
    # if there are multiple frost-free periods of identical length, takes the first

    dayT.32 <- tmin < FtoC(32)
    if(all(dayT.32)) {
        # Never frosts
        dayT.32 <- length(dayT.32)
        dayT.32.last <- NA
        dayT.32.first <- NA
    } else {
        # All frost
        if(all(!dayT.32)) {
            dayT.32 <- 0
            dayT.32.last <- NA
            dayT.32.first <- NA
        } else {
            dayT.32.rle <- rle(dayT.32)
            dayT.32 <- max(dayT.32.rle$length[dayT.32.rle$values == FALSE], na.rm=TRUE)
            tempid <- min(which(dayT.32.rle$length == dayT.32 & dayT.32.rle$values == FALSE), na.rm=TRUE)
            dayT.32.last <- sum(dayT.32.rle$lengths[seq(1, tempid - 1)], na.rm=TRUE)
            dayT.32.first <- dayT.32.last + dayT.32 + 1
        }
    }

    # 7. growing degree-days (base 40F) in C
    gdd40C <- sum(ifelse(tmean < FtoC(40), 0, tmean - FtoC(40)), na.rm=TRUE)

    # 8. growing degree-days (base 50F) in C
    gdd50C <- sum(ifelse(tmean < FtoC(50), 0, tmean - FtoC(50)), na.rm=TRUE)


    # 9. extreme annual minimum temperature
    minmin <- min(tmin, na.rm=TRUE)

    # 10. extreme annual max temperature
    maxmax <- max(tmax, na.rm=TRUE)

    # 11. average July temperature (°C)
    JulyT <- mean(tmean[month == "07"], na.rm=TRUE)

    # 12. average within-month temperature range (C)
    rangeT <- mean(tapply(tmax - tmin, month, mean, na.rm=TRUE), na.rm=TRUE)

    # 13. annual precipitation
    yearP <- sum(precip, na.rm=TRUE)

    # 14. growing season precipitation; used 32F
    if(dayT.32 >= 365) {
        gsP <- yearP
    } else { 
        if(is.na(dayT.32)) {
            gsP <- NA
        } else {
            gsP <- sum(precip[seq(dayT.32.last, dayT.32.first)], na.rm=TRUE)
        }
    }

    # 15. greatest between-month precipitation range
    rangeP <- max(tapply(precip, month, sum, na.rm=TRUE), na.rm=TRUE) - min(tapply(precip, month, sum, na.rm=TRUE), na.rm=TRUE)

    # 16. average number of days between 0.1 inch or greater rain events
    llim <- 0.1 * 25.4
    dayP.1 <- precip > llim
    dayP.1 <- rle(dayP.1)
    dayP.1 <- mean(dayP.1$length[dayP.1$values == FALSE], na.rm=TRUE)

    # 17. average number of days between 0.3 inch or greater rain events
    llim <- 0.3 * 25.4
    dayP.3 <- precip > llim
    dayP.3 <- rle(dayP.3)
    dayP.3 <- mean(dayP.3$length[dayP.3$values == FALSE], na.rm=TRUE)

    results <- c(dayT.28, dayT.28.last, dayT.28.first, dayT.32, dayT.32.last, dayT.32.first, gdd40C, gdd50C, minmin, maxmax, JulyT, rangeT, yearP, gsP, rangeP, dayP.1, dayP.3)
    names(results) <- c("ff28", "ff28.last", "ff28.first", "ff32", "ff32.last", "ff32.first", "gdd40C", "gdd50C", "minmin", "maxmax", "JulyT", "rangeT", "yearP", "gsP", "rangeP", "dayP.1", "dayP.3")

    results
}
