##'  Save some results as `.rds` files to then be imported into the pacea R
##'  package.
##'
##' Need to first have results loaded in, so just run this after building the
##'   document (function does use `assess_yr` which will have been defined
##'   already). Function creates a directory and places the files in there.
##'   See pacea/data-raw/groundfish/hake.R for next steps. Some results were
##'   automatically already saved for the assessment (e.g. recruitment), but
##'   some had to be calculated here (e.g. recruitment scaled by R0).
##' @param model_lst List of models, but only works for one, just change if want
##'   to save results from a different model. Code keep the list format to
##'   remain similar to other `hake` package functions.
##' @param model_names List of model names, again only one will work.
##' @param dir_to_save Directory to put the resulting `.rds` files into, gets
##'   created if doesn't exist. The default (gets built if `NULL`) is somewhat
##'   tailored for Andy's machine, so check the function code and set the value
##'   if needed. You'll have to manually include `assess_yr`.
##' @return Nothing, but saves the `.rds` files in the appropriate directory.
##' @export
##' @author Andrew Edwards
pacea_save <- function(model_lst = list(base_model),
                       model_names = list(base_model_name),
                       dir_to_save = NULL){

  # Keeping inputs to be similar to other functions, but this only works for a
  #  single model:
  if(length(model_lst) != 1){stop("model_lst can only have one model")}

  # default is for Andy's machine - note directory is PACea not pacea.
  if(is.null(dir_to_save)){
    dir_to_save <- paste0(here::here(),
                          "/../PACea/data-raw/groundfish/hake-",
                          assess_yr)
  }

  if(!dir.exists(dir_to_save)) {dir.create(dir_to_save)}

  # -- Recruitment in absolute numbers --

  recr <- create_group_df_recr(model_lst,
                               model_names)$d %>%
                                          dplyr::select(-c("model",
                                                           "rmean"))

  stopifnot(colnames(recr) == c("year",
                                "rlower",
                                "rmed"   ,
                                "rupper"))

  # Use pacea names
  colnames(recr) <- c("year",
                      "low",
                      "median",
                      "high")

  recr_name <- paste0("hake_recruitment_", assess_yr)

  assign(recr_name,
         recr)

  saveRDS(get(recr_name),
          file = paste0(dir_to_save, "/", recr_name, ".rds"))


  # -- Recruitment scaled by that in 2010 --

  recr_over_2010 <- create_group_df_recr(model_lst,
                                         model_names,
                                         relative = TRUE)$d %>%
                                                    dplyr::select(-c("model",
                                                                     "r_rel_mean"))

  stopifnot(colnames(recr_over_2010) == c("year",
                                          "r_rel_lower",
                                          "r_rel_med"   ,
                                          "r_rel_upper"))

  # Use pacea names
  colnames(recr_over_2010) <- c("year",
                                "low",
                                "median",
                                "high")

  recr_over_2010_name <- paste0("hake_recruitment_over_2010_", assess_yr)

  assign(recr_over_2010_name,
         recr_over_2010)

  saveRDS(get(recr_over_2010_name),
          file = paste0(dir_to_save, "/", recr_over_2010_name, ".rds"))

  # -- Recruitment scaled by R0 recruitment --

  # Need to do from the full mcmc's so a bit different to above (i.e. not already
  #  automatically calculated). This is adapted from sandbox/andy/pacea-save/pacea-save.R
  dat <- as_tibble(model_lst[[1]]$mcmc) %>%
    select(c("Recr_Virgin",
             paste0("Recr_", start_yr):paste0("Recr_", end_yr))) / 1e6
  # converts from thousands to billions, though rescaling here anyway

  dat <- dat / dat$"Recr_Virgin"

  dat <- select(dat, -"Recr_Virgin")

  names(dat) = gsub(pattern = "Recr_",
                    replacement = "",
                    x = names(dat))

  low <- apply(dat,
               2,
               quantile,
               probs = 0.025)

  median <- apply(dat,
                  2,
                  median)

  high <- apply(dat,
                2,
                quantile,
                probs = 0.975)

  recr_over_R0 <- tibble("year" = as.numeric(names(dat)),
                         "low" = low,
                         "median" = median,
                         "high" = high)

  recr_over_R0_name <- paste0("hake_recruitment_over_R0_", assess_yr)

  assign(recr_over_R0_name,
         recr_over_R0)

  saveRDS(get(recr_over_R0_name),
          file = paste0(dir_to_save, "/", recr_over_R0_name, ".rds"))


  # -- Recruitment deviations --

  recr_devs <- create_group_df_recr(model_lst,
                                    model_names,
                                    devs = TRUE)$d %>%
               dplyr::select(-c("model"))

  stopifnot(colnames(recr_devs) == c("year",
                                     "devlower",
                                     "devmed"   ,
                                     "devupper"))

  # Use pacea names
  colnames(recr_devs) <- c("year",
                           "low",
                           "median",
                           "high")

  recr_devs_name <- paste0("hake_recruitment_deviations_", assess_yr)

  assign(recr_devs_name,
         recr_devs)

  saveRDS(get(recr_devs_name),
          file = paste0(dir_to_save, "/", recr_devs_name, ".rds"))

  # -- Spawning biomass --

  biomass <- create_group_df_biomass(model_lst,
                                     model_names)$d %>%
                                                dplyr::select(-c("model"))

  stopifnot(colnames(biomass) == c("year",
                                   "slower",
                                   "smed",
                                   "supper"))

  # Use pacea names
  colnames(biomass) <- c("year",
                         "low",
                         "median",
                         "high")

  biomass_name <- paste0("hake_biomass_", assess_yr)

  assign(biomass_name,
         biomass)

  saveRDS(get(biomass_name),
          file = paste0(dir_to_save, "/", biomass_name, ".rds"))

  # -- Total biomass of age-1 hake --

  # Need to do from the full mcmc's, adapted from hake_recruitment_over_R0 above.

  # Does not match Table 18, not sure what batage is then:
  # dat <- as_tibble(model_lst[[1]]$batage) %>%
  #   dplyr::filter(`Beg/Mid` == "B",
  #          Yr <= assess_yr) %>%  # Projections are 0 anyway
  #   select(Yr,
  #          `1`)

  # This matches Table 18, these are kilotonnes
  dat <- model_lst[[1]]$extra_mcmc$batage_med %>%
    select(c("yr",
             `1`)) %>%
    mutate("low" = as.numeric(NA),
           "high" = as.numeric(NA)) %>%
    relocate(year = yr,
             low,
             median = `1`,
             high)

  age_1_biomass_name <- paste0("hake_total_biomass_age_1_",
                               assess_yr)
  assign(age_1_biomass_name,
         dat)

  saveRDS(get(age_1_biomass_name),
          file = paste0(dir_to_save, "/", age_1_biomass_name, ".rds"))

  return()
}
