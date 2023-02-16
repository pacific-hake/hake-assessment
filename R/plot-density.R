#' Create a density plot for a particular parameter
#'
#' @param model A model list, created by [create_rds_file()]
#' @param post_regex A regular expression that matches one parameter name
#' and nothing else
#' @param den_alpha The alpha value for under the density curve
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_density <- function(model,
                         post_regex,
                         den_alpha = 0.2){

  mtch <- grep(post_regex, names(model$mcmc))

  if(!length(mtch)){
    stop("The regex `", post_regex, "` did not match any names in the mcmc ",
         "data frame",
         call. = FALSE)
  }

  if(length(mtch) > 1){
    stop("The regex `", post_regex, "` matched more than one name in the ",
         "mcmc data frame",
         call. = FALSE)
  }

  mc <- model$mcmc |>
    select(matches(post_regex)) |>
    as_tibble() %>%
    transmute(Value = .[[1]])

  g <- ggplot(mc, aes(x = Value)) +
    geom_density(fill = "black", alpha = den_alpha) +
    ylab("Density")

  g
}
