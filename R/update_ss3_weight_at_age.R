update_ss3_weight_at_age <- function(dir) {
  load(fs::path(hakedata_wd(), "length-weight-age", "LWAdata.Rdata"))
  filenameforss <- file.path(
    dir,
    paste0("wtatage_", max(yrs), "created_", format(Sys.time(), "%d-%b-%Y_%H.%M"), ".ss")
  )
  unlink(x = c(filenameforss))
  write_wtatage_file(
    file = filenameforss,
    data = withforecast,
    maturity = maturity
  )
  return(filenameforss)
}
