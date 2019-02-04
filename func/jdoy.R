
jdoy <- function(x) {
	# julian day of year
	if(!inherits(head(x), "Date"))
		x <- as.Date(x)

	as.numeric(format(x, "%j"))
}



