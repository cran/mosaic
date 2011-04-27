statTally <-
function (sample, rdata, FUN, direction = NULL, 
	stemplot = dim(rdata)[direction] < 201, q = c(0.5, 0.9, 0.95, 0.99), fun, ...) 
{
	if (missing(FUN)) {
		FUN = fun
	}
	if ( is.null(direction) ) {
		if ( dim(rdata) [1] == length(sample) ) {
			direction <- 2
		} else if ( dim(rdata) [2] == length(sample) ) {
			direction <- 1
		} else {
			stop("sample and rdata have incompatible dimensions")
		}
	}
    dstat <- FUN(sample)
    cat("Test Stat function: ")
	cat(deparse(substitute(FUN)))
    cat("\n\n")
    stats <- apply(rdata, direction, FUN)
    cat("\nTest Stat applied to sample data = ")
    cat(signif(dstat, 4))
    cat("\n\n")
    cat("Test Stat applied to random data:\n\n")
    print(quantile(stats, q))
    if (stemplot) {
        stem(stats)
    }
    plot1 <- xhistogram(~stats, groups=stats >= dstat, ...) 
    cat("\nOf the random samples")
    cat("\n\t", paste(sum(stats < dstat), "(", round(100 * 
        sum(stats < dstat)/length(stats), 2), "% )", "had test stats <", 
        signif(dstat, 4)))
    cat("\n\t", paste(sum(stats == dstat), "(", round(100 * 
        sum(stats == dstat)/length(stats), 2), "% )", "had test stats =", 
        signif(dstat, 4)))
    cat("\n\t", paste(sum(stats > dstat), "(", round(100 * 
        sum(stats > dstat)/length(stats), 2), "% )", "had test stats >", 
        signif(dstat, 4)))
    cat("\n")
    invisible(plot1)
}
