#' Create a trace plot for a particular parameter
#'
#' @param model A model list, created by [create_rds_file()]
#' @param post_regex A regular expression that matches one parameter name
#' and nothing else
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_trace <- function(model,
                       post_regex){

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

  model$mcmc |>
    select(matches(post_regex)) |>
    as_tibble(rownames = "Iteration") %>%
    transmute(Iteration = as.numeric(Iteration),
              Value = .[[2]]) |>
    ggplot(aes(x = Iteration, y = Value)) +
    geom_path() +
    scale_x_continuous(labels = comma)
}
