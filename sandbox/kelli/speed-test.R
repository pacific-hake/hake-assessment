dir_use <- "test_ss3"
fs::dir_create(dir_use)
api_files <- httr::content(httr::GET(
  "https://api.github.com/repositories/803404557/contents/inst/extdata/ss3"
))
files_lines <- purrr::map(
  api_files,
  .f = \(x) utils::download.file(
    x[["download_url"]],
    destfile = fs::path(dir_use, x[["name"]])
  )
)
r4ss::get_ss3_exe(dir = dir_use)
current_wd <- getwd()
setwd(dir_use)
shell("time ss3")
setwd(current_wd)
unlink(dir_use, recursive = TRUE)
# Laptop          real    1m00.601s
# hake-precision  real    0m47.765s
# Kelli's desktop real    0m58.110s
# leafcutter      real    0m56.502s
