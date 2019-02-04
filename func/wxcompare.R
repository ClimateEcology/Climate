# whatever appeals to me by way of looking at the different weather sets vs each other

wxcompare <- function(basename, varname, years) {
    ghcn <- get(paste("wx", basename, "ghcn", sep="."))
    prism <- get(paste("wx", basename, "prism", sep="."))
    maurer <- get(paste("wx", basename, "maurer", sep="."))
    livneh <- get(paste("wx", basename, "livneh", sep="."))

    if(missing(years)) {
        minyear <- min(c(ghcn$date, prism$date, maurer$date, livneh$date))
        maxyear <- max(c(ghcn$date, prism$date, maurer$date, livneh$date))
    } else {
        minyear <- as.Date(paste(years[1], "01", "01", sep="-"))
        maxyear <- as.Date(paste(years[2], "12", "31", sep="-"))
    }

    ghcn <- subset(ghcn, date >= minyear & date <= maxyear)
    prism <- subset(prism, date >= minyear & date <= maxyear)
    maurer <- subset(maurer, date >= minyear & date <= maxyear)
    livneh <- subset(livneh, date >= minyear & date <= maxyear)

    yrange <- range(c(ghcn[[varname]], prism[[varname]], maurer[[varname]], livneh[[varname]]), na.rm=TRUE)

    par(mfrow=c(2, 2))
    qqplot(ghcn[[varname]], prism[[varname]], xlab="GHCN", ylab="PRISM", main=paste(basename, varname), xlim=yrange, ylim=yrange)
    abline(c(0, 1), col="red")

    qqplot(ghcn[[varname]], maurer[[varname]], xlab="GHCN", ylab="Maurer", main=paste(basename, varname), xlim=yrange, ylim=yrange)
    abline(c(0, 1), col="red")

    qqplot(ghcn[[varname]], livneh[[varname]], xlab="GHCN", ylab="Livneh", main=paste(basename, varname), xlim=yrange, ylim=yrange)
    abline(c(0, 1), col="red")

    invisible()
}


