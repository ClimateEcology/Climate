## recode the DOY and YEAR to match an arbitrary month start date
## always starts on the first day of a month, but eg.
## July 1 for continuous winter
## October 1 for water year

anydoy <- function(date, startmonth=1) {

    if(!class(date) == "Date")
        date <- as.Date(date)

    doy <- as.numeric(format(date, "%j"))

    year <- as.numeric(format(date, "%Y"))

    splitdoy <- paste(year, sprintf("%02d", startmonth), "01", sep="-")
    splitdoy <- as.numeric(format(as.Date(splitdoy), "%j"))

    newdoy <- doy - splitdoy + 1

    newyear <- year
    newyear[newdoy <= 0] <- newyear[newdoy <= 0] - 1

    ndays <- rep(365, length(doy))
    ndays[((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0)] <- 366

    newdoy[newdoy <= 0] <- newdoy[newdoy <= 0] + ndays[newdoy <= 0]

    data.frame(date, doy, year, newdoy, newyear)
}


    #########


anyweek <- function(date, startmonth = 1) {
    # if startmonth = 1, conforms to ISO standard
    # adapted from ISOweek::ISOweek()

    if(!class(date) == "Date")
        date <- as.Date(date)

    nearest_thursday <- ISOweek:::thursday0(date)
    year <- anydoy(nearest_thursday, startmonth)$newyear

    start04 <- as.Date(paste(year, sprintf("%02d", startmonth), "04", sep="-"))
    start_thursday <- ISOweek:::thursday0(start04)

    diffdays <- difftime(nearest_thursday, start_thursday, units="days")

    week <- as.integer(diffdays)%/%7 + 1

    data.frame(date, newweek = week, newyear = year)
}



