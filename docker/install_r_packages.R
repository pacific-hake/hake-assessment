# Install R packages for hake assessment docker image

install.packages("remotes")
library(remotes)
library(purrr)
library(usethis)

github_pac_lst <- c("cgrandin/adnuts",
                    "cgrandin/kableExtra",
                    "pacific-hake/hakedataUSA",
                    "pbs-assess/gfdata",
                    "pbs-assess/gfplot",
                    "ropensci/rnaturalearthhires",
                    "r4ss/r4ss",
                    "tylermorganwall/rayshader")

walk(github_pac_lst, \(pkg){
  install_github(pkg)
})

# This assumes you are starting with rocker/tidyverse, so there are no
# tidyverse packages here

pac_lst <- c(
  "bookdown",
  "car", "carData", "clipr", "coda", "corrplot", "cowplot",
  "crayon", "data.table", "data.tree", "date",
  "fs", "future", "furrr",
  "gfplot", "GGally", "ggh4x", "ggnewscale", "ggpubr", "ggrepel", "ggsci",
  "ggsignif", "glue", "grDevices", "grid", "gridExtra", "gridGraphics", "gtable",
  "here",
  "knitr",
  "marmap", "MatrixModels", "microbenchmark", "minpack.lm",
  "pacman", "parallelly", "pbkrtest", "polynom",
  "quantreg",
  "RColorBrewer", "readr", "rgl", "rnaturalearth", "rstatix",
  "scales", "SparseM", "sf",
  "tidyselect", "tictoc", "tools",
  "utils",
  "with")

install.packages(pac_lst)
