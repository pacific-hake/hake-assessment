# Install packages for hake assessment docker image

# 2022
devtools::install_github("cgrandin/r4ss")
devtools::install_github("cgrandin/adnuts@hake2021")
devtools::install_github("haozhu233/kableExtra@9e459")
devtools::install_github("ropensci/rnaturalearthhires")

# 2021
#devtools::install_github("r4ss/r4ss@7e15b")
#devtools::install_github("ropensci/rnaturalearthhires@2ed7a")
#devtools::install_github("Cole-Monnahan-NOAA/adnuts@df246")

install.packages(c(
  "aws.s3", "cowplot", "data.tree", "date", "dlookr",
  "furrr", "future", "ggpubr", "ggrepel", "here",
  "maps", "maptools", "matrixcalc", "rgeos", "rnaturalearth",
  "rstan", "shinystan", "snowfall", "tictoc", "tidyverse"))

tinytex::tlmgr_install(c(
  "amsmath", "adjustbox", "algorithmicx", "algorithms", "appendix",
  "booktabs", "caption", "chngcntr", "cite", "collectbox", "courier",
  "datetime", "datetime2", "dvips", "ec", "enumitem", "epstopdf-pkg",
  "etex-pkg", "etoolbox", "fancyhdr", "fancyvrb", "float",
  "fmtcount", "framed", "geometry", "hyperref", "iftex",
  "import", "infwarerr", "kvoptions", "lastpage", "linegoal",
  "lineno", "listings", "marginnote", "multirow", "natbib", "ncctools",
  "nowidow", "parskip", "pdflscape", "pdfcomment", "pdftexcmds", "psnfss",
  "rsfs", "soulpos", "soulutf8", "subfig", "times",
  "tocloft", "tracklang", "zref"))

