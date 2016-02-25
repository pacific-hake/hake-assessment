#load all.r like you are going to build the document
#see beamer-hake-requests.rnw file

selexByAge <- function(model, start.yr, tv.end.yr=2015,
	                   ages, ylims=c(0,1)) {
	tv.selex <- calc.tv.selex(model,
	                               start.yr = start.yr,
	                               end.yr = tv.end.yr,
	                               ages = ages,
	                               probs = c(0.025, 0.975))
	x <- tv.selex$median
	yrs <- as.numeric(colnames(x))
	cols <- rich.colors.short(5)
	plot(yrs, x[1,], type="l",ylim=ylims, col=cols[1], xlim=c(min(yrs),(max(yrs)+1)),lwd=2, xlab="Year", ylab="Selectivity")
	for(i in 2:5) {
		lines(yrs, x[i,], col=cols[i], lwd=2)
	}
	text(max(yrs)+1, c(x[1:5, "2015"], 1), c(1:5,"Age"))
}

#selexByAge(model, start.yr=tv.selex.start.yr, tv.end.yr=2015,ages=tv.selex.ages)
