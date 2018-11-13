climstat <- function(x, xsub) {
    if(!missing(xsub)) {
        x <- x[xsub]
    }
    z <- try(zyp::zyp.trend.vector(x), silent=TRUE)
    if(inherits(z, "try-error")) 
        z <- c(lbound = NA_real_, trend = NA_real_, trendp = NA_real_, ubound = NA_real_, 
tau = NA_real_, sig = NA_real_, nruns = NA_real_, autocor = NA_real_, 
valid_frac = NA_real_, linear = NA_real_, intercept = NA_real_
)

    c(mean=mean(x, na.rm=TRUE), 
      quantile(x, probs=c(.1, .5, .9), na.rm=TRUE), 
      NAs=sum(is.na(x)), z)
}

#


climchunk <- function(x, inc=5, span=30) {
    # takes a data frame; calculates sequential quantiles
	n <- nrow(x)
	seqs <- data.frame(start=seq(1, n-span+1, by=inc), end=seq(span, n, by=inc))
    result <- vector(nrow(seqs), mode="list")
	for(i in seq_len(nrow(seqs))) {
		result[[i]] <- apply(x, 2, climstat, xsub = seq(seqs$start[i], seqs$end[i]))
		names(result)[i] <- paste0("q", rownames(x)[seqs$start[i]])
	}

    # add overall summary
    result[["all"]] <- apply(x, 2, climstat)


	result
}

#

