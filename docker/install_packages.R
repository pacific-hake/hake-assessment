# Install packages for hake assessment docker image

install.packages(c(
  "aws.s3", "cowplot", "data.tree", "date", "furrr", "future", "ggpubr",
  "ggrepel", "here", "maps", "maptools", "matrixcalc",
  "rgeos", "rnaturalearth", "rstan", "shinystan", "snowfall",
  "tictoc"))

devtools::install_github("cgrandin/r4ss")
devtools::install_github("ropensci/rnaturalearthhires")
# 2021 assessment SHA for adnuts (Jan 12, 2021 - base run was on
# Jan 14 so this was the most recent)
devtools::install_github("cgrandin/adnuts@hake2021")

tinytex::tlmgr_install(c(
  "amsmath", "adjustbox", "algorithmicx", "algorithms", "appendix",
  "booktabs", "caption", "chngcntr", "cite", "collectbox", "courier",
  "datetime", "datetime2", "dvips", "ec", "enumitem", "epstopdf-pkg",
  "etex-pkg", "etoolbox", "fancyhdr", "fancyvrb", "float",
  "fmtcount", "framed", "geometry", "hyperref", "iftex",
  "import", "infwarerr", "kvoptions", "lastpage", "linegoal",
  "lineno", "marginnote", "multirow", "natbib", "ncctools",
  "nowidow", "pdflscape", "pdfcomment", "pdftexcmds", "psnfss",
  "rsfs", "soulpos", "soulutf8", "subfig", "times",
  "tocloft", "tracklang", "zref"))

