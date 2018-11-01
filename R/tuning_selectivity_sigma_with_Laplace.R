# Applying Laplace approximation to estimate sigma parameter
# for semi-parametric selectivity
#
# Jan 26, 2018

if(FALSE){
  
# get development branch of r4ss to get revised Laplace approx functions
# changes presumably will get into trunk of r4ss before long 
devtools::install_github('r4ss/r4ss', ref='development')



#### optimizing for model with semi-parametric selectivity
# define full path because wd is changed in function
direc <- file.path(models.dir, '2018.19.10_semiPara_tvSelect_Laplace')
direc <- file.path(models.dir, 'sel_tests/2018.19.10_semiPara_tvSelect_Laplace_check')

# note: "2018.19.10_semiPara_tvSelect_Laplace" is a copy of
#       "2018.19.10_semiPara_tvSelect" to avoid messing up the original

# delete record of any past optimizations
file.remove(file.path(direc,"Optimization_record.txt"))
file.remove(file.path(direc,"Iteration.txt"))

# run optimization
Opt <- optimize(f=NegLogInt_Fn,
                interval=c(0.05, 2.0),
                maximum=FALSE,
                File=direc,
                CTL_linenum_List=list(243), # parameter line in ctl file
                ESTPAR_num_List=list(91:225), # par numbers of devs in .cor file
                Int_Group_List=1,
                PAR_num_Vec=NA,
                StartFromPar=FALSE,
                Intern=TRUE)

# read output from optimization using function Jim Thorson posted to r4ss issue #44:
# https://github.com/r4ss/r4ss/issues/44
Extract_Trace_Fn = function(dir){
  ### function to parse the output file created by optimizing a variance parameter
  ### using the Laplace Approximation calculations in NegLogInt_Fn.R

  ### written by Jim Thorson
  # Print conditions
  message("This function is only designed for univariate optimization")
  # Read lines
  Lines = readLines(file.path(dir,"Optimization_record.txt",sep=""))
  # Make save object
  Niter = length(grep("Iteration", Lines))
  Results = data.frame( matrix(NA, nrow=Niter, ncol=5, dimnames=list(NULL,c("Iteration","SD_Group_Vec","LnLike","NegLnDet","Ln_Integral"))) )
  # Loop across iterations
  for(IterI in 1:Niter){
    Results[IterI,"Iteration"] = as.numeric(strsplit(Lines[grep("Iteration", Lines)[IterI]], " ")[[1]][2])
    Results[IterI,"SD_Group_Vec"] = as.numeric(strsplit(Lines[grep("SD_Group_Vec", Lines)[IterI]], " ")[[1]][2])
    Results[IterI,"LnLike"] = as.numeric(strsplit(Lines[grep("LnLike", Lines)[IterI]], " ")[[1]][2])
    Results[IterI,"NegLnDet"] = as.numeric(strsplit(Lines[grep("NegLnDet", Lines)[IterI]], " ")[[1]][2])
    Results[IterI,"Ln_Integral"] = as.numeric(strsplit(Lines[grep("Ln_Integral", Lines)[IterI]], " ")[[1]][2])
  }
  Diff = abs(Results[,'LnLike']+Results[,'NegLnDet']/2 - Results[,'Ln_Integral'] + log(2*pi))
  if( any(Diff[!is.na(Diff)]>0.01) ){
    warning("Warning something may be wrong with the optimization results")
  }
  return(Results)
}

### run function above to get optimization results
Res <- Extract_Trace_Fn(direc)  # parse Optimization_record.txt
Res <- Res[!is.na(Res$LnLike),] # filter out any NA values (maybe not converged)
## Res <- Res[Res$Ln_Integral > -1e7,]   # filter out some crazy high values
Res <- Res[order(Res$SD_Group_Vec),] # sort by x-variable
# best estimate for sigma_s
sigma_s <- Res$SD_Group_Vec[which(Res$Ln_Integral==max(Res$Ln_Integral))]

### plot of results
# second plot showing just Ln_Integral
plot(0, xlim=range(Res$SD_Group_Vec),
     type='n', yaxs='i', ylab="Change in value",
     ylim=c(0, 1.1*max(Res$Ln_Integral-min(Res$Ln_Integral))),
     xlab=expression(paste("SD of selectivity deviations,  ", sigma[s])))
abline(v=sigma_s, lty=3)
legend('bottomleft', bg='white', box.col='white', lwd=3, col=c(1,2,4),
       legend=c("log penalized likelihood",
           "half the negative log determinant of the Hessian",
           "log likelihood to be maximized (sum of other 2 values)"))
lines(Res$SD_Group_Vec, Res$Ln_Integral-min(Res$Ln_Integral),
      type='o', col=4, lwd=3)
lines(Res$SD_Group_Vec, Res$LnLike-min(Res$LnLike),
      type='o', col=1, lwd=3)
lines(Res$SD_Group_Vec, (Res$NegLnDet-min(Res$NegLnDet))/2,
      type='o', col=2, lwd=3)
axis(1, at=round(sigma_s,3))
box()

} # end if(FALSE)
