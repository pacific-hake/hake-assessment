#' Make an age composition bubble plot
#'
#' @param model A model object as returned from [create_rds_file()]
#' @param type Either `fishery` or `survey`
#' @param proportions Logical. If `TRUE`, If `TRUE` plot the input data
#' proportions-at-age. If `FALSE` plot the estimated median numbers-at-age
#' @param inc_mean_age_line Logical. If `TRUE`, plot the mean age line
#' over the bubbles
#' @param ... Additional parameters passed to [plot_bubbles()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_age_comp_bubbles <- function(model,
                                  type = c("fishery", "survey"),
                                  proportions = TRUE,
                                  inc_mean_age_line = TRUE,
                                  ...){

  type <- match.arg(type)
  if(type == "fishery"){
    type_code <- 1
  }else if(type == "survey"){
    type_code <- 2
  }

  # Ages to show in the plot
  val_col_nm <- ifelse(proportions, "Proportion", "Numbers (billions)")
  val_col_sym <- sym(val_col_nm)
  if(proportions){
    pat <- "^a(\\d+)$"
    age_inds <- grep(pat, names(model$dat$agecomp))
    ages_chr <- gsub(pat, "\\1", names(model$dat$agecomp[age_inds]))
    if(!length(ages_chr)){
      stop("Problem with object `model$agecomp`, no ages were found as ",
           "column names in the data frame. Check to make sure they are ",
           "there and not prepended by an 'a', e.g. 'a15'")
    }
    ages <- as.numeric(ages_chr)
    ages_cols <- paste0("a", ages)

    nms <- c("Year", ages)
    d <- model$dat$agecomp |>
      as_tibble() |>
      dplyr::filter(fleet == type_code) |>
      select(year, all_of(ages_cols)) |>
      set_names(nms) %>%
      mutate(n = rowSums(.[-1])) %>%
      mutate_at(vars(-Year), ~(. / n)) |>
      select(-n) |>
      pivot_longer(-Year,
                   names_to = "Age",
                   values_to = val_col_nm) |>
      mutate(Age = as.numeric(Age))
  }else{
    if(type == "survey"){
      stop("`survey` Numbers-at-age for MCMC not available. You would ",
           "need to add that to the extra-mcmc extraction code first")
    }

    d <- model$extra_mcmc$natage_med |>
      rename(Year = yr) |>
      pivot_longer(-Year,
                   names_to = "Age",
                   values_to = val_col_nm) |>
      mutate(Age = as.numeric(Age)) |>
      mutate(!!val_col_sym := !!val_col_sym / 1e3)
  }

  if(inc_mean_age_line){

    d_mean <- d |>
      split(~Year) |>
      map_dbl(~{
        j <- .x |>
          mutate(val = as.numeric(as.character(Age)) * !!val_col_sym)
        mean_age <- sum(j$val) / sum(j[[val_col_nm]])}) |>
      enframe(name = "Year", value = "Age") |>
      mutate(Year = as.numeric(Year))
    g <- plot_bubbles(d,
                      mean_age = d_mean,
                      val_col_nm = val_col_nm,
                      ...)
  }else{
    g <- plot_bubbles(d,
                      val_col_nm = val_col_nm,
                      ...)
  }

  g
}
