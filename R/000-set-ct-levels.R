#' Here is where you fill in the forecast list. This will be used throughout
#' the package for forecasts catch levels
#'
#' @param nf The number of forecast years
#'
#' @return A list of 2 lists:
#' List 1 is a list of lists of length-3 vectors. Each list represents a catch
#' level, each vector's three elements are:
#' 1 - A vector of length `nf` of catch levels
#' 2 - The nice name for the catch level scenario
#' 3 - The directory name for the catch level scenario
#' List 2 is a list of important values and indices referenced in the document.
#' Be sure to update this each year if forecasts are added and/or removed
#'
#' @export
set_ct_levels <- function(nf){
  ret <- list()
  ret$ct_levels <-
    list(list(rep(0.01, nf), "No Fishing", "01-0"),
         list(rep(180000, nf), "180,000 t", "02-180000"),
         list(rep(225000, nf), "225,000 t", "03-225000"),
         list(rep(270000, nf), "270,000 t", "04-270000"),
         list(c(320000, 288000, 259200, 233280),
              "320,000 t 10% red.",
              "05-320000-10"),
         list(rep(325000, nf), "2022 catch: 325,000 t", "06-325000"),
         list(rep(350000, nf), "350,000 t", "07-350000"),
         list(c(350000, 315000, 283500, 255150),
              "350,000 t 10% red.",
              "08-350000-10"),
         list(rep(380000, nf), "380,000 t", "09-380000"),
         list(c(380000, 342000, 307800, 277020),
              "380,000 t 10% red.",
              "10-380000-10"),
         list(rep(430000, nf), "430,000 t", "11-430000"),
         list(rep(545000, nf), "2022 TAC: 545,000 t", "12-545000"),
         list(rep(NA, nf), "FI=100%", "13-spr-100"),
         list(rep(NA, nf), "Default Harvest Policy", "14-default-hr"),
         list(rep(NA, nf), "Stable Catch", "15-stable-catch"))

  ret$ct_levels_vals <- list(
    ct_levels_num = length(ret$ct_levels),
    ct_actual_ind = 6,
    ct_tac_ind = 12,
    ct_spr100_ind = 13,
    ct_default_policy_ind = 14,
    ct_stable_ind = 15,
    ct_reduction_rows = c(5, 8, 10),
    ct_constant_rows = c(1, 2, 3, 4, 6, 7, 9, 11, 12),
    # Copy/paste values of `ct_constant_rows` in here
    ct_constant_str = paste(letters[c(1, 2, 3, 4, 6, 7, 9, 11, 12)],
                            collapse = ", "),
    # Copy/paste values of `ct_reduction_rows` in here
    ct_reduction_str = paste(letters[c(5, 8, 10)],
                             collapse = ", "))

  ret
}