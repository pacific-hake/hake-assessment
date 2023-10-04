# Install packages for hake assessment docker image

devtools::install_github("cgrandin/r4ss")
devtools::install_github("cgrandin/adnuts")
devtools::install_github("ropensci/rnaturalearthhires")

install.packages(c(
  "aws.s3", "cowplot", "data.tree", "date", "dlookr",
  "furrr", "future", "ggpubr", "ggrepel", "here", "kableExtra",
  "maps", "matrixcalc", "rnaturalearth",
  "rstan", "shinystan", "snowfall", "tictoc", "tidyverse"))

tinytex::tlmgr_install(c(
  "amsmath", "adjustbox", "algorithmicx", "algorithms", "appendix",
  "booktabs", "caption", "chngcntr", "cite", "collectbox", "courier",
  "datetime", "datetime2", "dvips", "ec", "enumitem", "epstopdf-pkg",
  "etex-pkg", "etoolbox", "fancyhdr", "fancyvrb", "float",
  "fmtcount", "framed", "geometry", "hyperref", "iftex",
  "import", "infwarerr", "k+voptions", "lastpage", "linegoal",
  "lineno", "listings", "marginnote", "multirow", "natbib", "ncctools",
  "nowidow", "parskip", "pdflscape", "pdfcomment", "pdftexcmds", "psnfss",
  "rsfs", "soulpos", "soulutf8", "subfig", "times",
  "tocloft", "tracklang", "zref"))

