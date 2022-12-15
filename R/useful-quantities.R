#' Get recruitment information for a cohort from two models with
#' an CI envelope % difference calculation
#'
#' @param model1 A model, typically last year's model
#' @param model2 A model, typically this year's model
#' @param year The cohort to extract
#' @param probs The 3-vector of probabilities to be passes to the [quantile()] function.
#' @param scale Scaling factor
#'
#' @return A list of length 3: Item 1 is a vector of the `model1` recruitment lower,
#' median, upper, and size of credible interval (`env_diff` column). Item 2 is the same type object
#' for `model2`, and item 3 is the ratio of the `model2` `env_diff` divided by that of `model1`
#' as a percentage.
#' @export
get_rec_ci <- function(model1,
                       model2,
                       year,
                       probs = c(0.025, 0.5, 0.975),
                       scale = 1e6,
                       decimals = 3,
                       perc.decimals = 0){
  col <- paste0("Recr_", year)
  rec1 <- quantile(model1$mcmc[[col]] / scale, probs)
  rec2 <- quantile(model2$mcmc[[col]] / scale, probs)
  rec1.env <- rec1[3] - rec1[1]
  rec2.env <- rec2[3] - rec2[1]
  rec.diff.perc <- rec2.env / rec1.env * 100
  rec1 <- c(rec1, rec1.env)
  names(rec1)[4] <- "env_diff"
  rec2 <- c(rec2, rec2.env)
  names(rec2)[4] <- "env_diff"
  list(f(rec1, decimals), f(rec2, decimals), f(rec.diff.perc, perc.decimals))
}

#' Get the median proportions of biomass-at-ages for the final year (MCMC) for all cohorts
#'
#' @param model A model as loaded by [load_ss_files()]
#' @param curr_yr is the current year. The years will decrease from this to give the cohorts year values
#'
#' @return A single-column tibble representing the proportion of biomass-at-ages for the terminal year
#' @export
get_baa <- function(model, curr_yr, probs = c(0.025, 0.5, 0.975)){
  model$extra.mcmc$natselwt.prop %>%
    map_df(function(x){
      quantile(x, probs = c(0.025, 0.5, 0.975))
    }) %>%
    t() %>%
    as.data.frame() %>%
    as_tibble() %>%
    mutate(Cohort = curr_yr - row_number() + 1) %>%
    rename(`Lower CI` = V1,
           Median = V2,
           `Upper CI` = V3) %>%
    select(Cohort,
           `Lower CI`,
           Median,
           `Upper CI`)
}

baa_table <- function(d,
                      xcaption = NULL,
                      xlabel   = NULL,
                      font.size = 13,
                      space.size = 14,
                      decimals = 3){
  tab <- d %>%
    mutate_at(vars(-Cohort), ~{f(.x, decimals)})

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(tab, caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab),
                                 first.left = FALSE,
                                 just = "r"),
               digits = rep(0, ncol(tab) + 1)),
        caption.placement = "top",
        include.rownames = FALSE,
        table.placement = "H",
        sanitize.text.function = function(x){x},
        size = size.string)
}

#' Calculate Biomass-at-age for the years given for the given model
#'
#' @details Uses numbers-at-age multiplied by weight-at-age. If any years are missing
#' the weight-at-age, the average will be used (Assumed to be in the 1966 year slot for SS input)
#'
#' @param model A model as loaded by [load_ss_files()]
#' @param yrs The years to return the values for
#' @param scale Scaling factor for the output values
#'
#' @return A data frame of the biomass-at-ages
#' @export
#'
#' @examples
#' baa <- get_baa(base_model)
get_baa_mle <- function(model, yrs = NULL, scale = 1e3){

  naa <- model$natage
  waa <- model$wtatage
  wt.at.age <- waa[waa$Fleet == 1, ]
  max.age <- max(suppressWarnings(as.numeric(names(naa))),
                 na.rm=TRUE)
  if(is.null(yrs)){
    yrs <- unique(naa$Yr)
  }

  n.age.b <- naa[naa$"Beg/Mid" == "B", names(naa) %in% c("Yr", as.character(0:max.age))]
  n.age.b <- n.age.b[n.age.b$Yr %in% yrs,]
  n.age.m <- naa[naa$"Beg/Mid" == "M", names(naa) %in% c("Yr", as.character(0:max.age))]
  n.age.m <- n.age.m[n.age.m$Yr %in% yrs,]

  dat <- n.age.b
  dat <- dat[,-1]
  ## If needed, add years using mean across years
  missing.yrs <- as.numeric(yrs)[!as.numeric(yrs) %in% abs(wt.at.age$Yr)]

  if(length(missing.yrs) > 0){
    ## get mean vector (assuming it is the one associated with -1966)
    mean.wt.at.age <- wt.at.age[wt.at.age$Yr == 1966,]
    ## loop over missing years (if any present)
    for(iyr in 1:length(missing.yrs)){
      mean.wt.at.age$Yr <- missing.yrs[iyr]
      wt.at.age <- rbind(wt.at.age, mean.wt.at.age)
    }
    ## sort by year just in case the missing years weren't contiguous
    wt.at.age <- wt.at.age[order(abs(wt.at.age$Yr)),]
  }
  wt.at.age <- wt.at.age[wt.at.age$Yr %in% yrs,]
  wt.at.age <- wt.at.age[, names(wt.at.age) %in% as.character(0:max.age)]
  dat <- dat * wt.at.age
  dat <- cbind(yrs, dat)
  names(dat)[1] <- "Year"
  ## Apply division by weight factor and formatting
  dat[, -1] <- apply(dat[, -1], c(1, 2), function(x) {x / scale})
  dat
}

#' Plot cohort recruitment MLE estimates with retrospectives
#'
#' @param model A model as loaded by [load_ss_files()] with MLE retrospective runs
#' @param end_yr End year for the plot
#' @param cohorts The cohort years to show
#'
#' @return A [ggplot2] object
#' @export
#'
#' @examples
#' get_rec_ci_mle(base_model, end_yr = 2019, cohorts = c(1999, 2010, 2014, 2016))
get_rec_ci_mle <- function(model,
                           end_yr = NULL,
                           cohorts = NULL){

  m_lst <- list()
  m_lst[[1]] <- model
  for(i in seq_along(model$retros)){
    m_lst[[i + 1]] <- model$retros[[i]]
  }

  recr <- lapply(m_lst, function(x){
    mle_parms <- x$derived_quants
    rec <- mle_parms %>%
      #filter(Label %in% paste0("Recr_", yrs)) %>%
      filter(grepl("^Recr_[0-9]+", Label)) %>%
      rename(Year = Label)
    rec$Year <- gsub("Recr_", "", rec$Year)
    rec <- rec %>%
      select(Year, Value, StdDev) %>%
      as_tibble() %>%
      mutate(ci = 2 * StdDev)
  }) %>%
    bind_cols()

  if(is.null(end_yr)){
    end_yr <- as.numeric(tail(recr, 1)$Year)
  }
  start_yr <- end_yr - length(m_lst) + 1
  yrs <- start_yr:end_yr

  recr_vals <- recr %>%
    select(matches("^Year$|^Value")) %>%
    filter(Year %in% yrs) %>%
    select(-Year) %>%
    as.matrix()

  recr_ci <- recr %>%
    select(matches("^Year$|^ci")) %>%
    filter(Year %in% yrs) %>%
    select(-Year) %>%
    as.matrix()

  ## Get all diagonals of the CI matrix (cohorts)
  rci_ind <- row(recr_ci) - col(recr_ci)
  rci <- split(recr_ci, rci_ind)
  # rci_ind <- row(recr_vals) - col(recr_vals)
  # rci <- split(recr_vals, rci_ind)
  rci_positive <- as.numeric(names(rci)) >= 0
  rci <- rci[rci_positive]

  ## Make ragged vectors all the same length, with NA's filling in the two ends
  yr_r <- as.numeric(names(rci))
  rci <- map(seq_along(rci), function(x){
    x <- c(rep(NA, yr_r[x]), rci[[x]])
    length(x) <- nrow(recr_ci)
    x
  })

  names(rci) <- yr_r
  rci <- bind_cols(enframe(yrs), rci) %>%
    select(-name) %>%
    rename(Year = value) %>%
    melt(id.var = "Year") %>%
    as_tibble() %>%
    filter(Year %in% yrs) %>%
    mutate(variable = as.numeric(variable) + start_yr - 1)

  color_count <-  length(yr_r)
  get_palette <- colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))

  if(!is.null(cohorts[1])){
    rci <- rci %>%
      filter(variable %in% cohorts)
  }
  g <- ggplot(rci, aes(x = Year, y = value, color = factor(variable))) +
    geom_line(size = 1.5) +
    scale_color_manual(values = get_palette(color_count))
  g
}

function(){
  get_rec_ci_mle(base_model, end_yr = 2019, cohorts = c(1999, 2010, 2014, 2016))
  get_rec_ci_mle(base_model, end_yr = 2019, cohorts = c(2000:2009, 2011:2013, 2015, 2017:2019))
  get_rec_ci_mle(base_model, end_yr = 2019, cohorts = 1999:2019)
}
