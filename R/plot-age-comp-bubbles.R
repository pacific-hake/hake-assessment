#' Make an age composition bubble plot
#'
#' @param model A model object as returned from [create_rds_file()]
#' @param type Either `fishery` or `survey`
#' @param inc_maen_age_line Logical. If `TRUE`, show a line across years
#' representing the mean age for each year
#' @param ... Additional parameters passed to [plot_bubbles()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_age_comp_bubbles <- function(model,
                                  type = c("fishery", "survey"),
                                  mcmc = FALSE,
                                  inc_mean_age_line = TRUE,
                                  ...){

  type <- match.arg(type)
  if(type == "fishery"){
    type_code <- 1
  }else if(type == "survey"){
    type_code <- 2
  }

  # Ages to show in the plot
  if(mcmc){
    if(type == "survey"){
      stop("`survey` Numbers-at-age for MCMC not available. You would ",
           "need to add that to the extra-mcmc extraction code first",
           call. = FALSE)
    }
    d <- model$extra_mcmc$natage_med |>
      rename(Year = yr)
  }else{
    pat <- "^a(\\d+)$"
    age_inds <- grep(pat, names(model$dat$agecomp))
    ages_chr <- gsub(pat, "\\1", names(model$dat$agecomp[age_inds]))
    if(!length(ages_chr)){
      stop("Problem with object `model$agecomp`, no ages were found as ",
           "column names in the data frame. Check to make sure they are ",
           "there and not prepended by an 'a', e.g. 'a15'",
           call. = FALSE)
    }
    ages <- as.numeric(ages_chr)
    ages_cols <- paste0("a", ages)

    nms <- c("Year", ages)
    d <- model$dat$agecomp |>
      as_tibble() |>
      filter(FltSvy == type_code) |>
      select(Yr, all_of(ages_cols)) |>
      set_names(nms)
  }

  d <- d %>%
    mutate(row_sum = rowSums(.[-1])) %>%
    mutate_at(vars(-Year), ~(. / row_sum)) |>
    select(-row_sum) |>
    pivot_longer(-Year,
                 names_to = "Age",
                 values_to = "Proportion") |>
    mutate(Age = as.numeric(Age)) |>
    mutate(Age = factor(Age))

  if(inc_mean_age_line){
    d_mean <- d |>
      split(~Year) |>
      map_dbl(~{
        j <- .x |>
          mutate(val = as.numeric(as.character(Age)) * Proportion)
        mean_age <- sum(j$val) / sum(j$Proportion)}) |>
      enframe(name = "Year", value = "Age") |>
      mutate(Year = as.numeric(Year))
    g <- plot_bubbles(d, mean_age = d_mean, ...)
  }else{
    g <- plot_bubbles(d, ...)
  }

  g
}
