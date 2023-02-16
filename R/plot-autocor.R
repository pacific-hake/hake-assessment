#' Create an autocorelation plot for a particular parameter
#'
#' @param model A model list, created by [create_rds_file()]
#' @param post_regex A regular expression that matches one parameter name
#' and nothing else
#' @param lag_max See [coda::autocorr.plot()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_autocor <- function(model,
                         post_regex,
                         lag_max = 20,
                         y_lim = c(-1, 1),
                         bar_width = 0.5){

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


  xacf <- acf(coda:::as.ts.mcmc(mc),
              lag.max = lag_max,
              plot = FALSE)

  lag_df <- tibble(Lag = xacf$lag[, 1, 1],
                   Autocorrelation = xacf$acf[, 1, 1])

  g <- ggplot(lag_df, aes(x = Lag, y = Autocorrelation)) +
    geom_col(width = bar_width) +
    ylim(y_lim)

  g
}
