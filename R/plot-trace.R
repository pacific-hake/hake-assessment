#' Create a trace plot for a particular parameter
#'
#' @param model A model list, created by [create_rds_file()]
#' @param post_regex A regular expression that matches one parameter name
#' and nothing else
#' @param color The color for the trace lines
#' @param alpha The transparency for the trace lines
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_trace <- function(model,
                       post_regex,
                       color = "blue",
                       alpha = 1){

  mtch <- grep(post_regex, names(model$mcmc))

  if(!length(mtch)){
    stop("The regex `", post_regex, "` did not match any names in the mcmc ",
         "data frame")
  }

  if(length(mtch) > 1){
    stop("The regex `", post_regex, "` matched more than one name in the ",
         "mcmc data frame")
  }

  model$mcmc |>
    select(matches(post_regex)) |>
    as_tibble(rownames = "Iteration") %>%
    transmute(Iteration = as.numeric(Iteration),
              Value = .[[2]]) |>
    ggplot(aes(x = Iteration, y = Value)) +
    geom_path(color = color,
              alpha = alpha) +
    scale_x_continuous(labels = comma)
}
