#' Read in NUTS run data files ('hake.Rdata' files), fix the parameter names
#' in the `nuts_updated` `samples` table, save the `nuts_updated` object as an
#' RDS file and return that object
#'
#' @param dr The directory in which the model subdirectories reside
#' @param mdl_nms A vector of the model directory names
#'
#' @return A list of the fit objects, each of which can be used in a call to
#' [adnuts::launch_shinyadmb()]
#' @export
save_nuts_fits <- function(dr = "/srv/hake/models/2024/02-version/05-test-models",
                           mdl_nms = c("03-burnin-1",
                                       "04-burnin-50",
                                       "05-burnin-100",
                                       "06-burnin-150",
                                       "07-burnin-200",
                                       "08-burnin-250",
                                       "09-burnin-300",
                                       "10-burnin-350",
                                       "11-burnin-400",
                                       "12-burnin-450",
                                       "13-burnin-500",
                                       "14-long-base")){

  walk(mdl_nms, ~{
    rdata_fn <- file.path(dr, .x, "mcmc", "hake.Rdata")
    nuts_fn <- file.path(dr, .x, "nuts-fit.rds")

    if(file.exists(rdata_fn)){
      load(rdata_fn)

      # Fix parameter names so that ShinySTAN shows parameter names
      parnms <- nuts_updated$mle$par_names
      parnms <- c("Objfun", parnms)
      dimnames(nuts_updated$samples)[[3]] <- parnms

      saveRDS(nuts_updated, nuts_fn)
      message("Wrote file: ", nuts_fn)
    }
  })

  fits <- map(mdl_nms, ~{
    nuts_fn <- file.path(dr, .x, "nuts-fit.rds")
    if(file.exists(nuts_fn)){
      readRDS(nuts_fn)
    }else{
      NA
    }
  })

  fits
}
