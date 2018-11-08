speiall <- function(x, lat, ref.start=c(1981, 1), ref.end=c(2010, 12), monthcol="month", yearcol="year", tmeancol="tmean", precipcol="precip") {

    pr <- x[[precipcol]]
    tave <- x[[tmeancol]]

    startyear <- as.numeric(x[[yearcol]][1])
    startmonth <- as.numeric(x[[monthcol]][1])

    thistw <- thornthwaite(tave, lat)

    spei01tw <- spei(ts(pr - thistw, freq=12, start=c(startyear, startmonth)), scale= 1, ref.start=ref.start, ref.end=ref.end)
    spei03tw <- spei(ts(pr - thistw, freq=12, start=c(startyear, startmonth)), scale= 3, ref.start=ref.start, ref.end=ref.end)
    spei06tw <- spei(ts(pr - thistw, freq=12, start=c(startyear, startmonth)), scale= 6, ref.start=ref.start, ref.end=ref.end)
    spei09tw <- spei(ts(pr - thistw, freq=12, start=c(startyear, startmonth)), scale= 9, ref.start=ref.start, ref.end=ref.end)
    spei12tw <- spei(ts(pr - thistw, freq=12, start=c(startyear, startmonth)), scale=12, ref.start=ref.start, ref.end=ref.end)
    spei18tw <- spei(ts(pr - thistw, freq=12, start=c(startyear, startmonth)), scale=18, ref.start=ref.start, ref.end=ref.end)
    spei24tw <- spei(ts(pr - thistw, freq=12, start=c(startyear, startmonth)), scale=24, ref.start=ref.start, ref.end=ref.end)
    spei36tw <- spei(ts(pr - thistw, freq=12, start=c(startyear, startmonth)), scale=36, ref.start=ref.start, ref.end=ref.end)

    list(thornthwaite = thistw, 
        spei01tw = spei01tw,
        spei03tw = spei03tw,
        spei06tw = spei06tw,
        spei09tw = spei09tw,
        spei12tw = spei12tw, 
        spei18tw = spei18tw, 
        spei24tw = spei24tw, 
        spei36tw = spei36tw)

}


