# Install R packages for hake assessment docker image

install.packages("remotes")
library(remotes)
library(purrr)
library(usethis)

# These are the packages on GitHub, in alphabetical order
github_pac_lst <- c("cgrandin/adnuts",
                    "cgrandin/kableExtra",
                    "pacific-hake/hakedataUSA",
                    "pbs-assess/gfdata",
                    "pbs-assess/gfplot",
                    "ropensci/rnaturalearthhires",
                    "r4ss/r4ss")

walk(github_pac_lst, \(pkg){
  install_github(pkg)
})

# These are packages on CRAN. Alphabetical order, each line has the same
# starting letter for each package (new line for new starting letter)
pac_lst <- c(
  "bookdown",
  "car", "carData", "clipr", "coda", "corrplot", "cowplot",
  "crayon", "data.table", "data.tree", "date",
  "fs", "future", "furrr",
  "gfplot", "GGally", "ggh4x", "ggnewscale", "ggpubr", "ggrepel", "ggsci",
  "ggsignif", "glue", "grDevices", "grid", "gridExtra", "gridGraphics", "gtable",
  "here", "hexbin",
  "knitr",
  "magick", "marmap", "MatrixModels", "microbenchmark", "minpack.lm",
  "pacman", "parallelly", "pbkrtest", "polynom",
  "quantreg",
  "RColorBrewer", "readr", "rgl", "rnaturalearth", "rstatix",
  "scales", "SparseM", "sf",
  "tidyselect", "tictoc", "tools",
  "utils",
  "with")

install.packages(pac_lst)
