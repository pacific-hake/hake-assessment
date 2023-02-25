#' Make a table of values for cohorts as they pass through their life
#'
#' @param model A model as output by [create_rds_file()]
#' @param end_yr The last year to use
#' @param cohorts A vector of cohorts (years) to use
#' @param csv_dir Directory for output of a file containing the table data
#' @param xcaption The table caption
#' @param xlabel The TeX label for the table
#' @param digits The number of decimal point digits to use in the table
#' @param font_size Size of text in the table
#' @param space_size Amount of vertical space betwen text in the table
#'
#' @return An [xtable::xtable()]
#' @export
cohort_table <- function(model,
                         end_yr,
                         cohorts,
                         csv_dir = "out-csv",
                         xcaption = "default",
                         xlabel   = "default",
                         digits = 1,
                         font_size = 9,
                         space_size = 10){

  caa <- model$extra.mcmc$catage_median
  # All data have the same start and end year, the exact same dimensions.
  # They were built that way in extra-mcmc.R
  min_yr <- min(caa$Yr)
  max_yr <- max(caa$Yr)
  max_yr <- ifelse(end_yr > max_yr, max_yr, end_yr)

  caa <- model$extra.mcmc$catage_median %>%
    filter(Yr <= max_yr)
  naa <- model$extra.mcmc$natage_median %>%
    mutate_at(.vars = vars(-Yr), ~{.x * 1e3}) %>%
    filter(Yr <= max_yr)
  naa_next <- model$extra.mcmc$natage_median %>%
    mutate_at(.vars = vars(-Yr), ~{.x * 1e3}) %>%
    filter(Yr %in% (min_yr + 1):(max_yr + 1)) %>%
    mutate(Yr = Yr - 1)
  caa_b <- model$extra.mcmc$catage_biomass_median %>%
    filter(Yr <= max_yr)
  baa <- model$extra.mcmc$batage_median %>%
    filter(Yr <= max_yr)
  waa <- model$wtatage %>%
    as_tibble() %>%
    filter(Fleet == 1) %>%
    select(-(2:6)) %>%
    filter(Yr <= max_yr)

  # Get the diagonals of the cohort data from the data frame
  #
  # @param d Data frame with the -at-age data
  # @param cohorts A vector of cohorts (years) to extract
  # @return A list of length of cohort vector with a vector of the cohort data for each one
  get_coh <- function(d, cohorts){
    yrs <- d %>% pull(Yr)
    d_noyr <- d %>% select(-Yr)

    coh_inds <- as.character(which(yrs %in% cohorts) - 1)
    delta <- row(d_noyr) - col(d_noyr)
    coh_lst <- split(as.matrix(d_noyr), delta)
    map(coh_inds, ~{get(.x, coh_lst)})
  }

  coh_baa <- get_coh(baa, cohorts)
  coh_caa_b <- get_coh(caa_b, cohorts) %>%
    map(~{
      tmp <- .x / 1e3
      tmp[-length(.x)]
      })
  coh_naa <- get_coh(naa, cohorts)
  coh_naa_next <- get_coh(naa_next, cohorts - 1) %>% map2(seq_along(cohorts), ~{
      tmp <- .x[-1]
      if(.y == 1) tmp else tmp[-length(tmp)]
    })
  coh_waa <- get_coh(waa, cohorts) %>% map(~{.x[-length(.x)]})
  if(length(coh_naa_next[[1]]) != length(coh_waa[[1]])){
    coh_naa_next[[1]] <- head(coh_naa_next[[1]], -1)
  }
  coh_survive_b <- map2(coh_naa_next, coh_waa, ~{
    .x * .y
  }) %>% map(~{.x / 1e3})
  coh_m <- map(seq_along(cohorts), function(.x, ba, ca, surv){
    ba[[.x]] <- ba[[.x]][-length(ba[[.x]])]
    ba[[.x]] - surv[[.x]] - ca[[.x]]
  }, ba = coh_baa, ca = coh_caa_b, surv = coh_survive_b)

  # Pad all vectors in the list with `NA`s so that they are all `num` long
  # @param lst A list of vectors
  # @param num The length to make all vectors
  pad_vects <- function(lst, num){
    map(lst, ~{.x[1:num]})
  }

  # Make a list of length 4, one list for each element which contains one of the four value types
  lst <- map2(list(coh_baa, coh_caa_b, coh_m, coh_survive_b), c("baa", "caa", "m", "surv"), ~{
    pad_vects(.x, length(coh_baa[[1]])) %>%
      `names<-`(paste0(.y, "_", cohorts))
  }) %>%
    `names<-`(c("baa", "caa", "m", "surv"))

  # Make a list of length of number of cohorts, each with a list of 4 value types. This is getting the
  # list into the correct structure for the table
  df <- map(seq_along(cohorts), ~{
    map_df(lst, ~{
      .x[[.y]]
    }, .y = .x)
  }) %>%
    `names<-`(cohorts)

  # Make unique names for the cohort baa, caa, m, and surv columns and bind the data frames into one
  df <- map2(df, cohorts, ~{
    names(.x) <- paste0(names(.x), "_", .y)
    .x
  }) %>%
    bind_cols

  # Add the Age column
  age_df <- enframe(0:(nrow(df) - 1), name = NULL, value = "Age")
  df <- bind_cols(age_df, df)

  # Table constructed, write to a CSV
  csv_headers <- c("Age", map(seq_along(cohorts), ~{
    c(paste(cohorts[.x], "Start Biomass"),
      paste(cohorts[.x], "Catch Weight"),
      paste(cohorts[.x], "M Weight"),
      paste(cohorts[.x], "Surviving Biomass"))
  }) %>% unlist)
  csv_out <- df %>% mutate_all(~{as.character(.x)})
  colnames(csv_out) <- csv_headers
  csv_out[is.na(csv_out)] <- ""

  csv_dir_full <- here::here("doc", csv_dir)
  if(!dir.exists(csv_dir_full)){
    dir.create(csv_dir_full)
  }
  write_csv(csv_out,
            file.path(csv_dir_full, "cohort-effects.csv"))

  # Change the number of decimal points displayed and remove NAs
  df <- df %>%
    mutate_at(.vars = vars(-Age), ~{f(.x, digits)}) %>%
    map_df(~{gsub("NA", "", .x)})

  # Apply LaTeX
  colnames(df) <- c(latex.bold("Age"),
                    rep(c(latex.mlc(c("Start",
                                      "Biomass",
                                      "000s t")),
                          latex.mlc(c("Catch",
                                      "Weight",
                                      "000s t")),
                          latex.mlc(c("M",
                                      "000s t")),
                          latex.mlc(c("Surviving",
                                      "Biomass",
                                      "000s t"))),
                        length(cohorts)))

  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  addtorow$command <- latex.hline
  for(i in 1:length(cohorts)){
    addtorow$command <-
      paste0(addtorow$command,
             latex.amp(),
             latex.mcol(4,
                        "c|",
                        latex.bold(paste(cohorts[i],
                                         " cohort"))))
  }
  addtorow$command <- paste0(addtorow$command, latex.nline)
  size_string <- latex.size.str(font_size, space_size)
  al <- get.align(ncol(df))
  al[2] <- paste0("|", al[2], "|")
  for(i in 3:length(al)){
    if(i %% 4 == 2){
      al[i] <- paste0(al[i], "|")
    }
  }
  print(xtable(df,
               caption = xcaption,
               label = xlabel,
               align = al),
        caption.placement = "top",
        add.to.row = addtorow,
        table.placement = "H",
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size_string)
}