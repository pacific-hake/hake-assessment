#' Remove multiple header/datarows found in posteriors files to leave just one
#'
#' @param dir Directory where the files reside
#'
#' @return If the file contains only one header/datarow (i.e is of correct format)
#' then return a 1-row dataframe of the contents
#' @export
fix.posteriors <- function(dir){

  do.it <- function(file){
    posts <- read.table(file.path(dir, file),
                        header = TRUE,
                        fill = TRUE,
                        stringsAsFactors = FALSE)
    if(all(grepl("^[[:digit:]]", posts[,1]))){
      return(posts)
    }
    write.table(posts[1:(grep("\\D+", posts[,1])[1] - 1),],
                file.path(dir, file),
                quote = FALSE,
                row.names = FALSE)
  }
  do.it(posts_file_name)
  do.it(derposts_file_name)
}

#' Load all the SS files for output and input, and return the model object
#'
#' @details If MCMC directory is present, load that and perform calculations for mcmc parameters.
#'
#' @param model.dir Directory the model reesides in
#' @param key_posts Vector of key posteriors used to create key posteriors file
#' @param key_posts_fn Name of the key posteriors file
#' @param nuisance_posts_fn Name of the nuisance posteriors file
#' @param printstats Print info on each model loaded via [r4ss::SS_output()]
#'
#' @return A model object representing the output from the SS model
#' @export
load_ss_files <- function(model_path = NA,
                          key_posts = c("NatM",
                                        "SR_LN",
                                        "SR_BH_steep",
                                        "Q_extraSD",
                                        "ln.EffN_mult._1",
                                        "ln.EffN_mult._2"),
                          key_posts_fn = "keyposteriors.csv",
                          nuisance_posts_fn = "nuisanceposteriors.csv",
                          printstats = FALSE,
                          ...){

  stopifnot(!is.na(model_path))

  # Load MPD results
  model <- tryCatch({
    SS_output(dir = model_path,
              verbose = FALSE,
              printstats = printstats,
              covar = FALSE,
              wtfile = "wtatage.ss")
  }, error = function(e){
    SS_output(dir = model_path,
              verbose = FALSE,
              printstats = printstats,
              covar = FALSE,
              forecast = FALSE,
              wtfile = "wtatage.ss")
  })

  # Load the data file and control file for the model
  # Get the file whose name contains "_data.ss" and "_control.ss"
  # If there is not exactly one of each, stop with error.
  model_path_listing <- tolower(dir(model_path))
  dat_fn_ind <- grep("_data.ss", model_path_listing)
  ctl_fn_ind <- grep("_control.ss", model_path_listing)
  par_fn_ind <- grep("ss.par", model_path_listing)
  if(!length(dat_fn_ind)){
    stop("Error in model ", model_path,
         ", there is no data file. A data file is any file whose name end with _data.ss.\n",
         call. = FALSE)
  }
  if(length(dat_fn_ind) > 1){
    stop("Error in model ", model_path,
         ", there is more than one data file. A data file is any file whose name ends with _data.ss.\n\n",
         call. = FALSE)

  }
  if(!length(ctl_fn_ind)){
    stop("Error in model ", model_path,
         ", there is no control file. A control file is any file whose name ends with _control.ss.\n\n",
         call. = FALSE)

  }
  if(length(ctl_fn_ind) > 1){
    stop("Error in model ", model_path,
         ", there is more than one control file. A control file is any file whose name ends with _control.ss.\n\n",
         call. = FALSE)
  }
  model$path <- model_path
  model$dat_file <- file.path(model_path, model_path_listing[dat_fn_ind])
  model$ctl_file <- file.path(model_path, model_path_listing[ctl_fn_ind])
  model$par_file <- file.path(model_path, model_path_listing[par_fn_ind])
  model$dat <- SS_readdat(model$dat_file, verbose = FALSE)
  model$ctl <- readLines(model$ctl_file)
  # model$par <- readLines(par_fn)
  # Set default mcmc members to NA. Later code depends on this.
  model$mcmc <- NA
  # Set the mcmc and extra mcmc paths and record their existence
  model$mcmc_path <- file.path(model_path, "mcmc")
  model$mcmc_exists <- dir.exists(model$mcmc_path)
  model$extra_mcmc_path <- file.path(model$mcmc_path, "sso")
  model$extra_mcmc_exists <- dir.exists(model$extra_mcmc_path)
  # Save the posterior names from the mcmc output. This is necessary for the function `plot_mcmc_param_stats()`
  posteriors_dir <- ifelse(model$extra_mcmc_exists, model$extra_mcmc_path, model$mcmc_path)

  # If it has an mcmc sub-directory, load that as well
  if(dir.exists(posteriors_dir)){
    tmp <- readLines(file.path(posteriors_dir, "posteriors.sso"), n = 1)
    tmp <- stringr::str_split(tmp, "[:space:]")[[1]]
    # Remove Empty string, Iter and Objective_function as they are not parameters
    model$post_names <- tmp[!tmp %in% c("", "Iter", "Objective_function")]
    fix.posteriors(posteriors_dir)
    model$mcmc <- SSgetMCMC(dir = posteriors_dir,
                            writecsv = FALSE,
                            verbose = FALSE)
    # replace any SPB with SSB
    names(model$mcmc) <- gsub(pattern="SPB", replacement="SSB", names(model$mcmc))
    create.key.nuisance_posteriors_files(model,
                                         key_posts,
                                         key_posts_fn,
                                         nuisance_posts_fn)
    # Do the mcmc calculations, e.g. quantiles for SB, SSB, DEPL, RECR, RECRDEVS
    model$mcmccalcs <- calc.mcmc(model$mcmc)

  }
  model
}

#' Return a list of mcmc calculations, e.g. quantiles for various values
#'
#' @param mcmc The output of the [r4ss::SS_getMCMC()] function as a data.frame
#' @param lower Lower quantile value
#' @param upper Upper quantile value
#' @param biomass.scale Scale the biomass by this amount. The default for
#'  2021 and before was 2e6, but is not 1e6 due to changes in SS3 (hake Issue
#'   #866). Biomass will be shown in the millions of tonnes (and is females only).
#' @param recruitment.scale Scale the recruitment by this amount. The default is 1e6
#' because recruitment will be shown in millions of tonnes
#'
#' @return
#' @export
calc.mcmc <- function(mcmc,
                      lower = 0.025,
                      upper = 0.975,
                      biomass.scale = 1e6,
                      recruitment.scale = 1e6){

  ssb <- mcmc[,grep("SSB",names(mcmc))] / biomass.scale
  svirg <- quantile(ssb[,names(ssb) == "SSB_Virgin"],
                    c(lower, 0.5, upper))
  sinit <- quantile(ssb[,names(ssb) == "SSB_Initial"],
                    c(lower, 0.5, upper))

  # sinit.post exists so that depletion calculations can be done for each posterior
  sinit.post <- ssb[,names(ssb) == "SSB_Initial"]

  names(ssb) <- gsub("SSB_", "", names(ssb))
  cols.to.strip <- c("Virgin", "Initial")
  ssb <- strip.columns(ssb, cols.to.strip)

  slower <- apply(ssb, 2, quantile, prob = lower, na.rm = TRUE)
  smed   <- apply(ssb, 2, quantile, prob = 0.5, na.rm = TRUE)
  supper <- apply(ssb, 2, quantile, prob = upper, na.rm = TRUE)

  depl   <- apply(ssb, 2, function(x){x / sinit.post})
  dlower <- apply(depl, 2, quantile, prob = lower, na.rm = TRUE)
  dmed   <- apply(depl, 2, quantile, prob = 0.5, na.rm = TRUE)
  dupper <- apply(depl, 2, quantile, prob = upper, na.rm = TRUE)

  ## 1e6 used here because recruitment will be shown in the millions of tonnes
  recr <- mcmc[,grep("Recr_", names(mcmc))] / recruitment.scale
  recr <- recr[,-grep("Fore", names(recr))]
  names(recr) <- gsub("Recr_", "", names(recr))
  rvirg <- quantile(recr[,names(recr) == "Virgin"],
                    c(lower, 0.5, upper))
  rinit <- quantile(recr[,names(recr) == "Initial"],
                    c(lower, 0.5, upper))
  runfished <- quantile(recr[,grepl("unfished", names(recr), ignore.case = TRUE)],
                        c(lower, 0.5, upper))

  cols.to.strip <- c("Virgin", "Initial",
    grep("unfished", names(recr), ignore.case = TRUE, value = TRUE))
  recr <- strip.columns(recr, cols.to.strip)

  rmed <- apply(recr, 2, quantile, prob = 0.5, na.rm = TRUE)
  rmean <- apply(recr, 2, mean, na.rm = TRUE)
  rlower <- apply(recr, 2, quantile,prob = lower, na.rm = TRUE)
  rupper <- apply(recr, 2, quantile,prob = upper, na.rm = TRUE)

  dev <- mcmc[,c(grep("Early_InitAge_", names(mcmc)),
                 grep("Early_RecrDev_", names(mcmc)),
                 grep("Main_RecrDev_", names(mcmc)),
                 grep("Late_RecrDev_", names(mcmc)),
                 grep("ForeRecr_", names(mcmc)))]

  names(dev) <- gsub("Early_RecrDev_", "", names(dev))
  names(dev) <- gsub("Main_RecrDev_", "", names(dev))
  names(dev) <- gsub("Late_RecrDev_", "", names(dev))
  names(dev) <- gsub("ForeRecr_", "", names(dev))

  # Change the Early_Init names to be the correct preceeding years
  start_yr <- as.numeric(min(names(dev)))
  early <- grep("Early_InitAge_", names(dev))
  num.early.yrs <- length(early)
  early.yrs <- seq(start_yr - num.early.yrs, start_yr - 1, 1)
  late.yrs <- names(dev[-early])
  names(dev) <- c(as.character(early.yrs), late.yrs)

  devlower <- apply(dev, 2, quantile, prob = lower, na.rm = TRUE)
  devmed <- apply(dev, 2, quantile, prob = 0.5, na.rm = TRUE)
  devupper <- apply(dev, 2, quantile, prob = upper, na.rm = TRUE)

  spr <- mcmc[,grep("SPRratio_", names(mcmc))]
  names(spr) <- gsub("SPRratio_", "", names(spr))

  plower <- apply(spr, 2, quantile, prob = lower, na.rm = TRUE)
  pmed <- apply(spr, 2, quantile, prob = 0.5, na.rm = TRUE)
  pupper <- apply(spr, 2, quantile, prob = upper, na.rm = TRUE)

  f <- mcmc[,grep("F_", names(mcmc))]
  names(f) <- gsub("F_", "", names(f))
  flower <- apply(f, 2, quantile, prob = lower, na.rm = TRUE)
  fmed   <- apply(f, 2, quantile, prob = 0.5, na.rm = TRUE)
  fupper <- apply(f, 2, quantile, prob = upper, na.rm = TRUE)

  # Calculations for the reference points table
  probs <- c(lower, 0.5, upper)

  unfish.fem.bio <-
    f(round(quantile(mcmc$SSB_Virgin,
                     prob = probs) / biomass.scale, 3) * 1000,
      0)
  unfish.recr <-
    f(round(quantile(mcmc$Recr_Virgin,
                     prob = probs) / recruitment.scale, 3) * 1000,
      0)
  f.spawn.bio.bf40 <-
    f(round(quantile(mcmc$SSB_SPR,
                     prob = probs) / biomass.scale, 3) * 1000,
      0)
  spr.msy.proxy <- c(latex.bold("--"),
                     "40\\%",
                     latex.bold("--"))
  exp.frac.spr <-
    paste0(f(100 * quantile(mcmc$annF_SPR,
                            prob = probs),
             1),
           "\\%")
  yield.bf40 <-
    f(round(quantile(mcmc$Dead_Catch_SPR,
                     prob = probs) / recruitment.scale, 3) * 1000,
      0)
  fem.spawn.bio.b40 <-
    f(round(quantile(mcmc$SSB_Btgt,
                     prob = probs) / biomass.scale, 3) * 1000,
      0)
  spr.b40 <-
    paste0(f(100 * quantile(mcmc$SPR_Btgt,
                            prob = probs),
             1),
           "\\%")
  exp.frac.b40 <-
    paste0(f(100 * quantile(mcmc$annF_Btgt,
                            prob = probs),
             1),
           "\\%")
  yield.b40 <-
    f(round(quantile(mcmc$Dead_Catch_Btgt,
                     prob = probs) / recruitment.scale, 3) * 1000,
      0)
  fem.spawn.bio.bmsy <-
    f(round(quantile(mcmc$SSB_MSY,
                     prob = probs) / biomass.scale, 3) * 1000,
      0)
  spr.msy <- paste0(f(100 * quantile(mcmc$SPR_MSY,
                                     prob = probs),
                      1),
                    "\\%")
  exp.frac.sprmsy <-
    paste0(f(100 * quantile(mcmc$annF_MSY,
                            prob = probs),
             1),
           "\\%")
  msy <-
    f(round(quantile(mcmc$Dead_Catch_MSY,
                     prob = probs) / recruitment.scale, 3) * 1000,
      0)

  # Return a list of the calculated values
  sapply(c("svirg",
           "sinit",
           "slower",
           "smed",
           "supper",
           "dlower",
           "dmed",
           "dupper",
           "rvirg",
           "rinit",
           "runfished",
           "rlower",
           "rmed",
           "rupper",
           "rmean",
           "devlower",
           "devmed",
           "devupper",
           "plower",
           "pmed",
           "pupper",
           "flower",
           "fmed",
           "fupper",
           ## Reference points
           "unfish.fem.bio",
           "unfish.recr",
           "f.spawn.bio.bf40",
           "spr.msy.proxy",
           "exp.frac.spr",
           "yield.bf40",
           "fem.spawn.bio.b40",
           "spr.b40",
           "exp.frac.b40",
           "yield.b40",
           "fem.spawn.bio.bmsy",
           "spr.msy",
           "exp.frac.sprmsy",
           "msy"),
         function(x){get(x)})
}

create.key.nuisance_posteriors_files <- function(model,
                                                 posterior_regex,
                                                 key_post_file,
                                                 nuisance_post_file){
  ## Creates the two files for key and nuisance posteriors
  if(model$extra_mcmc_exists){
    key_file <- here::here(model$extra_mcmc_path, key_post_file)
    nuisance_file <- here::here(model$extra_mcmc_path, nuisance_post_file)
  }else{
    key_file <- here::here(model$mcmc_path, key_post_file)
    nuisance_file <- here::here(model$mcmc_path, nuisance_post_file)
  }

  mc <- model$mcmc
  mc_names <- names(mc)
  mcmc_grep <- unique(grep(paste(posterior_regex, collapse="|"), mc_names))
  mcmc_names <- mc_names[mcmc_grep]
  keys <- mc[, mcmc_grep]
  nuisances <- mc[, -mcmc_grep]
  write.csv(keys, key_file, row.names = FALSE)
  write.csv(nuisances, nuisance_file, row.names = FALSE)
}

#' Load models from files created using [create_rds_file()]
#'
#' @details Load model(s) and return as a list if more than one. If only one,
#'  return that object or if ret.single.list is TRUE, return a 1-element list.
#'
#' @param model_dirs A vector of model directory names
#' @param ret_single_list See details
#'
#' @return A list of model objects
#' @export
#'
#' @examples
#' base <- load_models("base")
load_models <- function(model_dirs,
                        ret_single_list = FALSE){
  ret_list <- NULL
  model_rds_files <- file.path(rootd_models, model_dirs, paste0(model_dirs, ".rds"))
  if(!all(file.exists(model_rds_files))){
    stop("The following files do not exist, run build_rds() on the associated directories:\n",
         paste(model_rds_files[!file.exists(model_rds_files)], collapse = "\n"),
         call. = FALSE)
  }
  for(i in 1:length(model_rds_files)){
    small_file <- file.path(rootd_models, model_dirs[i], paste0("small_", model_dirs[i], ".rds"))
    if(file.exists(small_file)){
      #message("Trying ", small_file)
      #gzfile(small_file, "rb")
      ret_list[[i]] <- readRDS(small_file)
      message("Loaded small RDS file: ", small_file)
    }else{
      ret_list[[i]] <- readRDS(model_rds_files[i])
      message("Loaded large RDS file: ", model_rds_files[i])
    }
  }
  if(length(model_dirs) == 1){
    if(ret_single_list){
      ret_list
    }else{
      ret_list[[1]]
    }
  }else{
    ret_list
  }
}
