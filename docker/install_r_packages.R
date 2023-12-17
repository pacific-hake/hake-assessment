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
                    "r4ss/r4ss@bioscale",
                    "tylermorganwall/rayshader")

walk(github_pac_lst, \(pkg){
  install_github(pkg)
})

# This assumes you are starting with rocker/tidyverse, so there are no
# tidyverse packages here

pac_lst <- c(
  "bookdown", "clipr", "coda", "cowplot", "crayon", "data.table",
  "fs", "future", "furrr", "gfplot", "GGally", "ggh4x",
  "ggnewscale", "ggrepel", "glue", "grDevices", "grid",
  "gridExtra", "gtable", "here", "knitr", "marmap",
  "microbenchmark", "minpack.lm", "pacman", "parallelly",
  "RColorBrewer", "readr", "rgl", "rnaturalearth",
  "scales", "sf", "tidyselect", "tools",
  "utils", "with")

install.packages(pac_lst)
