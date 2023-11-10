#' Create a density plot for a particular parameter
#'
#' @param model A model object, created by [create_rds_file()]
#' @param post_regex A regular expression that matches one parameter name
#' and nothing else
#' @param fill The fill color under the density curve
#' @param alpha The alpha value for under the density curve
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_density <- function(model,
                         post_regex,
                         fill = "royalblue",
                         alpha = 0.3){

  mtch <- grep(post_regex, names(model$mcmc))

  if(!length(mtch)){
    stop("The regex `", post_regex, "` did not match any names in the mcmc ",
         "data frame")
  }

  if(length(mtch) > 1){
    stop("The regex `", post_regex, "` matched more than one name in the ",
         "mcmc data frame")
  }

  mc <- model$mcmc |>
    select(matches(post_regex)) |>
    as_tibble() %>%
    transmute(Value = .[[1]])

  g <- ggplot(mc, aes(x = Value)) +
    geom_density(fill = fill,
                 alpha = alpha) +
    ylab("Density")

  g
}
