#' Run Standard Sensitivities for Pacific Hake
#' 
#' The SRG requests that the following sensitivities are run as standard
#' protocol for the base model on an annual basis. 
#' 
#' @param dirbase A directory where the base model is located. A full
#' path is preferred. 
#' @param run A logical value specifying if the sensitivities should
#' be ran using the Stock Synthesis executable found in the folder.
#' If \code{FALSE}, then the folders will be created but the analyses
#' are not started. The default is to just copy the files. 
#' @param niters An integer value specifying how many times to perform 
#' the iterative process to weight the composition data when running
#' the McAllister-Ianelli and Francis weighting approaches. The default
#' is to run each four times. 
#' 
#' @author Kelli Faye Johnson
#' @return The function writes files to the disk and does not return 
#' anything. 
#' 
runs_sens_base <- function(dirbase, run = FALSE, niters = 4) {
  # 2019.03.01_h_prior_mean_low
  # 2019.03.02_h_fix_high
  # 2019.03.03_sigmaR_fix_low
  # 2019.03.04_sigmaR_fix_high
  # 2019.03.05_M_0.2SD
  # 2019.03.06_M_0.3SD
  # 2019.03.07_age1Survey
  # 2019.03.08_compWeight_HarmonicMean
  # 2019.03.09_compWeight_Francis
  
  number <- paste(strsplit(strsplit(
    basename(dirbase), "_")[[1]][1], "\\.")[[1]][1:2],
    collapse = ".")
  main <- dirname(dirbase)
  aa <- file.path(main, paste0(number, ".01_h_prior_mean_low"))
  dir.create(aa, showWarnings = FALSE)
  filesget <- dir(dirbase, pattern = "[aelt].ss\\b|.exe|ter.ss\\b")
  ignore <- sapply(filesget, function(x) file.copy(file.path(dirbase, x), 
    file.path(aa, x), overwrite = TRUE, recursive = FALSE))
  filename <- dir(aa, pattern = "control", full.names = TRUE)
  lines <- readLines(filename)
  line <- grep("SR_BH_steep", lines)
  text <- strsplit(gsub("^\\s+", "", gsub("\\s+", " ", lines[line])), " ")[[1]]
  text[4] <- 0.5
  lines[line] <- paste(text, collapse = " ")
  writeLines(lines, filename)
  
  aa <- file.path(main, paste0(number, ".02_h_fix_high"))
  dir.create(aa, showWarnings = FALSE)
  filesget <- dir(dirbase, pattern = "[aelt].ss\\b|.exe|ter.ss\\b")
  ignore <- sapply(filesget, function(x) file.copy(file.path(dirbase, x), 
    file.path(aa, x), overwrite = TRUE, recursive = FALSE))
  filename <- dir(aa, pattern = "control", full.names = TRUE)
  lines <- readLines(filename)
  line <- grep("SR_BH_steep", lines)  
  text <- strsplit(gsub("^\\s+", "", gsub("\\s+", " ", lines[line])), " ")[[1]]
  text[2:3] <- 1
  text[7] <- -4
  lines[line] <- paste(text, collapse = " ")
  writeLines(lines, filename)
  
  aa <- file.path(main, paste0(number, ".03_sigmaR_fix_low"))
  dir.create(aa, showWarnings = FALSE)
  filesget <- dir(dirbase, pattern = "[aelt].ss\\b|.exe|ter.ss\\b")
  ignore <- sapply(filesget, function(x) file.copy(file.path(dirbase, x), 
    file.path(aa, x), overwrite = TRUE, recursive = FALSE))
  filename <- dir(aa, pattern = "control", full.names = TRUE)
  lines <- readLines(filename)
  line <- grep("SR_sigmaR", lines)  
  text <- strsplit(gsub("^\\s+", "", gsub("\\s+", " ", lines[line])), " ")[[1]]
  text[3] <- 1
  lines[line] <- paste(text, collapse = " ")
  writeLines(lines, filename)
  
  aa <- file.path(main, paste0(number, ".04_sigmaR_fix_high"))
  dir.create(aa, showWarnings = FALSE)
  filesget <- dir(dirbase, pattern = "[aelt].ss\\b|.exe|ter.ss\\b")
  ignore <- sapply(filesget, function(x) file.copy(file.path(dirbase, x), 
    file.path(aa, x), overwrite = TRUE, recursive = FALSE))
  filename <- dir(aa, pattern = "control", full.names = TRUE)
  lines <- readLines(filename)
  line <- grep("SR_sigmaR", lines)  
  text <- strsplit(gsub("^\\s+", "", gsub("\\s+", " ", lines[line])), " ")[[1]]
  text[3] <- 1.8
  lines[line] <- paste(text, collapse = " ")
  writeLines(lines, filename)
  
  aa <- file.path(main, paste0(number, ".05_M_0.2SD"))
  dir.create(aa, showWarnings = FALSE)
  filesget <- dir(dirbase, pattern = "[aelt].ss\\b|.exe|ter.ss\\b")
  ignore <- sapply(filesget, function(x) file.copy(file.path(dirbase, x), 
    file.path(aa, x), overwrite = TRUE, recursive = FALSE))
  filename <- dir(aa, pattern = "control", full.names = TRUE)
  lines <- readLines(filename)
  line <- grep("NatM", lines)  
  text <- strsplit(gsub("^\\s+", "", gsub("\\s+", " ", lines[line])), " ")[[1]]
  text[5] <- 0.2
  lines[line] <- paste(text, collapse = " ")
  writeLines(lines, filename)
  
  aa <- file.path(main, paste0(number, ".06_M_0.3SD"))
  dir.create(aa, showWarnings = FALSE)
  filesget <- dir(dirbase, pattern = "[aelt].ss\\b|.exe|ter.ss\\b")
  ignore <- sapply(filesget, function(x) file.copy(file.path(dirbase, x), 
    file.path(aa, x), overwrite = TRUE, recursive = FALSE))
  filename <- dir(aa, pattern = "control", full.names = TRUE)
  lines <- readLines(filename)
  line <- grep("NatM", lines)  
  text <- strsplit(gsub("^\\s+", "", gsub("\\s+", " ", lines[line])), " ")[[1]]
  text[5] <- 0.3
  lines[line] <- paste(text, collapse = " ")
  writeLines(lines, filename)

  # 2019.03.07_age1Survey
  aa <- file.path(main, paste0(number, ".07_age1Survey"))
  dir.create(aa, showWarnings = FALSE)
  filesget <- dir(dirbase, pattern = "[aelt].ss\\b|.exe|ter.ss\\b")
  ignore <- sapply(filesget, function(x) file.copy(file.path(dirbase, x), 
    file.path(aa, x), overwrite = TRUE, recursive = FALSE))
  filename <- dir(aa, pattern = "data", full.names = TRUE)
  lines <- readLines(filename)
  line <- grep("#_Nfleets", lines)  
  lines[line] <- paste(as.numeric(
    regmatches(lines[line], regexpr("\\d", lines[line]))) + 1, 
    "#_Nfleets")
  lines <- append(lines, 
    "3 7 1 2 0 Age1Index  # 3",
    after = grep("_Catch", lines) - 1)
  lines <- append(lines, 
    "3 0 0 0 # Age1Index",
    after = grep("# Acoustic_Survey", lines))
  # todo: remove hardwire to survey
  age1 <- c("1995" = 231940,      
  "1998" = 107000,      
  "2003" = 24266.583,   
  "2005" = 8651.674,    
  "2007" = 1029090.399, 
  "2009" = 3396343.372, 
  "2011" = 5948669.130, 
  "2012" = 64440.675,   
  "2013" = 421579.674,  
  "2015" = 4665070.061, 
  "2017" = 1238038.108)
  endyr <- as.numeric(gsub("\\D", "", grep("#_EndYr", lines, value = TRUE)))
  line <- grep("terminator for survey observations", lines) - 1
  if (grepl("#", lines[line])) line <- line - 1
  lines <- append(lines, apply(
    merge(
      data.frame("year" = as.numeric(names(age1)), "month" = 7, "survey" = 3,
      "est" = age1, "se" = 0.5),
      data.frame("year" = (1995:endyr)[!1995:endyr %in% names(age1)],
      "month" = 7, "survey" = -3, "est" = 1, "se" = 1), all = TRUE), 1, 
    paste, collapse = " "), line)
  lines <- append(lines, 
    "-1 0.001 0 0 0 0 0.001 #_fleet:3_Age1Index",
    after = grep("# sex codes:", lines)[1] - 1)
  lines <- append(lines, 
    "-1 0.001 0 0 1 2 0.001 #_fleet:3_Age1Index",
    after = grep("Lbin_method_for_Age_Data", lines) - 1)
  writeLines(lines, filename)  
  # Control file
  filename <- dir(aa, pattern = "control", full.names = TRUE)
  lines <- readLines(filename)
  line <- grep("extra_se", lines) + 1
  if (!any(grepl("2", strsplit(lines[line], "\\s+")[[1]][1:2]))) {
    stop("The survey is not fleet two, or something is wrong in\n",
      "  writing the age-1 survey to the control file.")
  }
  lines <- append(lines,
    "3 1 0 1 0 1 # Age 1 Index Survey",
    line)
  line <- grep("\\d.+LnQ_base_", lines)
  temp <- lines[line:(line + 1)]
  temp <- gsub("Acoustic_Survey.+", "Age1Index\\(3\\)", temp)
  lines <- append(lines, temp, line + 1)
  line <- grep("Pattern Discard Male Special", lines) + 2
  for (ii in 1:2){
    lines <- append(lines,
      c(" 0 0 0 0 # 3 Age 1 Index", " 11 0 0 0 # 3 Age 1 Index")[ii], 
      (grep("Pattern Discard Male Special", lines) + 2)[ii])
  }
  lines <- append(lines, 
    c("1 1 1 -1 0.01 0 -2 0 0 0 0 0 0 0  #  AgeSel_P22_Age1Index_Minage(3)",
      "1 1 1 -1 0.01 0 -2 0 0 0 0 0 0 0  #  AgeSel_P23_Age1Index_Maxage(3)"),
    grep("Dirichlet-Multinomial parameters", lines) - 1)
  writeLines(lines, filename)

  # 2019.03.08_compWeight_HarmonicMean
  # 2019.03.09_compWeight_Francis
  aa <- file.path(main, paste0(number, ".08_compWeight_HarmonicMean"))
  bb <- file.path(main, paste0(number, ".09_compWeight_Francis"))
  dir.create(aa, showWarnings = FALSE)
  dir.create(bb, showWarnings = FALSE)
  filesget <- dir(dirbase, pattern = "[aelt].ss\\b|.exe|ter.ss\\b")
  ignore <- sapply(filesget, function(x) {
    file.copy(file.path(dirbase, x), 
    file.path(aa, x), overwrite = TRUE, recursive = FALSE)
    file.copy(file.path(dirbase, x), 
    file.path(bb, x), overwrite = TRUE, recursive = FALSE)
  })
  filename <- dir(aa, pattern = "data", full.names = TRUE)
  lines <- readLines(filename)
  line <- grep("Lbin_method_for_Age_Data", lines)
  temp <- do.call("rbind", strsplit(lines[(line - 2):(line - 1)], "\\s+"))
  temp[, 5:6] <- 0
  lines[(line - 2):(line - 1)] <- apply(temp, 1, paste, collapse = " ")
  writeLines(lines, dir(aa, pattern = "data", full.names = TRUE))
  writeLines(lines, dir(bb, pattern = "data", full.names = TRUE))
  filename <- dir(aa, pattern = "control", full.names = TRUE)
  lines <- readLines(filename)
  line <- grep("# Dirichlet-Multinomial parameters", lines)
  lines <- lines[-(line:(line + 2))]
  lines <- append(lines, 
    c("5 1 0.15", "5 2 0.45"),
    grep("_7=mult_by_generalized_sizecomp", lines))
  writeLines(lines, dir(aa, pattern = "control", full.names = TRUE))
  writeLines(lines, dir(bb, pattern = "control", full.names = TRUE))

  if (run) {
    oldwd <- getwd()
    on.exit(setwd(oldwd), add = TRUE)
    dir <- dir(main, include.dirs = TRUE, pattern = "03\\.0[1-9]", 
      full.names = TRUE)
    for (ii in dir) {
      setwd(ii)
      print(ii);flush.console()
      system("ss3")
    }
    setwd(oldwd)

    for (ii in grep("compWeight", dir)){
      for (iii in 1:niters) {
      filename <- dir(dir[ii], pattern = "_control", full.names = TRUE)
      out <- r4ss::SS_output(dir[ii], verbose = FALSE, printstats = FALSE)
      age <- out$Age_comp_Eff_N_tuning_check
      lines <- readLines(filename)
      line <- grep(" #_7=mult_by_generalized_sizecomp", lines)
      if (ii == 8) temp <- out$Age_comp_Eff_N_tuning_check$Recommend_Var_Adj
      if (ii == 9) temp <- r4ss::SS_tune_comps(out, write = FALSE)$New_Var_adj
      lines[c(line + 1, line + 2)] <- apply(cbind(do.call("rbind", 
        strsplit(lines[c(line + 1, line + 2)], "\\s"))[, 1:2],
        temp),
        1, paste, collapse = " ")
      writeLines(lines, filename)
      setwd(dir[ii])
      system("ss3")
      setwd(oldwd)
    }}
    compare <- lapply(dir(main,
      include.dirs = TRUE, 
      pattern = "03\\.0[1-9]|base$",
      #pattern = "03\\..{+}Selec|base$,
      full.names = TRUE),
      r4ss::SS_output, verbose = FALSE, printstats = FALSE)
    r4ss::SSplotComparisons(r4ss::SSsummarize(compare), pdf = TRUE, 
      plotdir = main,
      legendlabels = gsub(".+\\d_", "", dir(main,
        include.dirs = TRUE, 
        pattern = "03\\.0[1-9]|base$")))
        #pattern = "03\\..{+}Selec|base$")))
  }

}
