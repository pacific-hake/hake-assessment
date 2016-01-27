make.long.parameter.estimates.table <- function(model,                ## model is an mcmc run and is the output of the r4ss package's function SSgetMCMC
                                                digits = 3,           ## number of decimal points for the estimates
                                                xcaption = "default", ## Caption to use
                                                xlabel   = "default", ## Latex label to use
                                                font.size = 9,        ## Size of the font for the table
                                                space.size = 10       ## Size of the spaces for the table
                                                ){
  ## Returns an xtable in the proper format for the parameter estimates

  j <- model$par
  p.names <- rownames(j)
  ## Should use the global key.posteriors here but the name of SR_LN.R0. is different here
  key.posts <- c("NatM_p_1_Fem_GP_1",
                 "SR_LN(R0)",
                 "SR_BH_steep",
                 "Q_extraSD_2_Acoustic_Survey")
  df <- as.data.frame(cbind(key.posts, j[p.names %in% key.posts,]$Value))
  names(df) <- c("parameter", "post.med")

  ## Add all Early_InitAge parameters
  ei <- j[grep("Early_InitAge_[0-9]+", p.names),]
  ei <- as.data.frame(cbind(ei$Label, ei$Value))
  names(ei) <- c("parameter", "post.med")
  df <- rbind(df, ei)

  ## Add all Recruitment deviation parameters
  ##rec <- j[grepl("(.*_RecrDev_[0-9]+)(.*ForeRecr_[0-9]+)", p.names, perl = TRUE),]
  rec <- j[union(grep(".*_RecrDev_[0-9]+", p.names),
           grep("ForeRecr_[0-9]+", p.names)),]
  rec <- as.data.frame(cbind(rec$Label, rec$Value))
  names(rec) <- c("parameter", "post.med")
  df <- rbind(df, rec)

  ## Add all AgeSel
  a.sel <- j[grep("AgeSel_.*", p.names),]
  a.sel <- a.sel[a.sel$Value != 0,]
  a.sel <- as.data.frame(cbind(a.sel$Label, a.sel$Value))
  names(a.sel) <- c("parameter", "post.med")
  df <- rbind(df, a.sel)

  ## Format the values
  df[,2] <- fmt0(as.numeric(levels(df[,2])[df[,2]]), digits)

  ## Make the underscores in the names have a preceeding \
  param.names <- levels(df[,1])[df[,1]]
  df[,1] <- gsub("\\_", "\\\\_", param.names)

  names(df) <- c("\\textbf{Parameter}", "\\textbf{Posterior median}")

  addtorow          <- list()
  addtorow$pos      <- list()
  addtorow$pos[[1]] <- c(0)
  addtorow$command  <- c(paste("\\hline \n",
                               "\\endhead \n",
                               "\\hline \n",
                               "{\\footnotesize Continued on next page} \n",
                               "\\endfoot \n",
                               "\\endlastfoot \n",sep=""))
  ## Make the size string for font and space size
  size.string <- paste0("\\fontsize{",font.size,"}{",space.size,"}\\selectfont")
  return(print(xtable(df, caption=xcaption, label=xlabel, align=get.align(ncol(df)), digits=digits),
               caption.placement = "top", table.placement="H", tabular.environment="longtable",
               include.rownames=FALSE, sanitize.text.function=function(x){x},
               size=size.string, add.to.row = addtorow, hline.after=c(-1)))
}
