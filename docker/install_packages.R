# Install packages for hake assessment docker image

install.packages(c(
  "aws.s3", "cowplot", "data.tree", "date", "furrr", "future", "ggpubr",
  "ggrepel", "here", "maps", "maptools", "matrixcalc",
  "rgeos", "rnaturalearth", "rstan", "shinystan", "snowfall",
  "tictoc"))

devtools::install_github("cgrandin/r4ss") # Commit 106da0ab8a611818a2ed72d45ff8d2af7ae58c55
devtools::install_github("ropensci/rnaturalearthhires") # Commit 2ed7a937f3cca4f44b157098c472f6b3ae8cd9f3
devtools::install_github("cgrandin/adnuts@hake2021") # Commit 8f7d8004a059fa3d4a6948bc896d3293d6b3930e

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

