.onLoad <- function(libname, pkgname){
  # Options which will save your sanity
  # max.print - Stop tibbles from showing 1e3 and 1e6
  # warnPartialMatchDollar - Don't allow partial matches using the
  #   dollar operator
  #`dplyr.summarise.inform - Stop messages like the following:
  #  `summarize()` has grouped output by 'Fleet'. You can override using
  #   the `.groups` argument.
  options(max.print = 999999,
          warnPartialMatchDollar = TRUE,
          dplyr.summarise.inform = FALSE,
          xtable.comment = FALSE)
}
