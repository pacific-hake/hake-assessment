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

if(FALSE){
  # these objects could be changed here and in the lines below
  model66 <- SS_output('../../models/66_Sen45_block_sel_2016_SRG_request2')
  model67 <- SS_output('../../models/67_Sen45_block_sel_2016_SRG_request3')

  # make mountains plot for MLEs, depends on sourcing
  # updated /doc/r/r-functions/figures-selex.r
  make.tv.selex.plot(base.model, mcmc=FALSE)
  make.tv.selex.plot(sens.models.6[[1]], mcmc=FALSE) # model59, phi = 0.03 for all years
  make.tv.selex.plot(model66, mcmc=FALSE) # phi = 0.03 up to 2008, and 0.20 for 2009-2016
  make.tv.selex.plot(model67, mcmc=FALSE) # phi = 0.03 up to 2015, and 0.20 for 2016
}
