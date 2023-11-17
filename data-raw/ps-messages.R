# Source this file to apply changes. Don't forget to add documentation for
# any new package data to file R/data.R
library(usethis)

create_data_hake("parallelism_warning",
  paste0("Your operating system does not support\n",
  "forking, so multicore parallelism cannot be used. Use a Mac or\n",
  "Linux machine if you want this to run in parallel. Even if you\n",
  "are on a Mac or Linux machine, you cannot use multicore\n",
  "parallelism if you are in an Rstudio terminal, you must run\n",
  "the code using Rscript (via bash file perhaps) or terminal R\n\n"))
