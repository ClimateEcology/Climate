precipstats <- function(x, llim=0, na.rm=TRUE) {
	# calculate detailed statistics on precipitation data
	# pulled out precip component of VFS::wth.param()

    x.pos <- x[x > 0 & !is.na(x)]

    if(length(x.pos) > 0) {
        prmean <- fastmean(x.pos)
        prmax <- max(x.pos, na.rm=FALSE)
        prsd <- fastsd(x.pos)
        prskew <- fastskew(x.pos)


        dayP.wet <- ifelse(x > llim, 1, 0)
        dayP.wet[is.na(dayP.wet)] <- 0
        prdays <- sum(dayP.wet)/length(dayP.wet)

        Ptoday <- c(dayP.wet, 0)
        Pyest <- c(0, dayP.wet)
        Pyt <- paste0(Pyest, Ptoday)
        Pyt <- Pyt[-length(Pyt)]

        prww <- sum(Pyt == "11") / sum(Pyt == "11" | Pyt == "10")
        prdw <- sum(Pyt == "01") / sum(Pyt == "01" | Pyt == "00")


        results <- c(prcpmean = prmean, prcpmax = prmax, prcpsd = prsd, 
            prcpskew = prskew, prcpwet = prdays, prcpww = prww, 
            prcpdw = prdw)
    } else {
        results <- c(prcpmean = NA, prcpmax = NA, prcpsd = NA, 
            prcpskew = NA, prcpwet = NA, prcpww = NA, 
            prcpdw = NA)
    }
    results
}


