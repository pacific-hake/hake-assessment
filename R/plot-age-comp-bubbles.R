#' Make an age composition bubble plot
#'
#' @param model A model object as returned from [create_rds_file()]
#' @param type Either `fishery` or `survey`
#' @param ... Additional parameters passed to [plot_bubbles()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_age_comp_bubbles <- function(model,
                                  type = c("fishery", "survey"),
                                  ...){

  type <- match.arg(type)
  if(type == "fishery"){
    type <- 1
  }else if(type == "survey"){
    type <- 2
  }

  d <- model$dat$agecomp |>
    filter(FltSvy == type) |>
    select(Yr, starts_with("a", ignore.case = FALSE)) %>%
    setNames(gsub("a", "", names(.))) |>
    rename(Year = Yr) %>%
    mutate(n = rowSums(.[-1])) %>%
    mutate_at(vars(-Year), ~(./n)) |>
    select(-n) |>
    pivot_longer(-Year, names_to = "Age", values_to = "Proportion") |>
    mutate(Age = as.numeric(Age)) |>
    mutate(Age = factor(Age))

  g <- plot_bubbles(d, ...)

  g
}
