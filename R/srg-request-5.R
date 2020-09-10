# 1. make a copy of the 2020.01.31_nutsMCMC folder
# 2. rename the copy 2020.01.50_nutsMCMC_thin_2000
# 3. delete the catch-levels, extra-mcmc, forecasts, and retrospectives directories so that
#    making the Rdata file doesn't take forever
# 4. make a copy of the 2020.01.50_nutsMCMC_thin_2000 folder
# 5. rename the copy 2020.01.51_nutsMCMC_thin_1000
# 4. source this file

thin_posts <- function(model_dir = here::here("models", "2020.01.50_nutsMCMC_thin_2000", "mcmc"),
                       num_samples = 2000,
                       thin_factor = 3){

  thin_file <- function(file){
    posts <- read_table2(file)
    if(nrow(posts) <= num_samples){
      message("The file ", file, " has ", nrow(posts),
              " rows and you asked to thin to ", num_samples, " rows so that file was not changed.")
    }else{
      p <- posts %>%
        filter(row_number() %% thin_factor == 0) %>%
        slice(1:num_samples)
      write_delim(p, file)
    }
  }

  posts_file <- file.path(model_dir, posts_file_name)
  derposts_file <- file.path(model_dir, derposts_file_name)
  thin_file(posts_file)
  thin_file(derposts_file)
}

thin_posts(here::here("models", "2020.01.50_nutsMCMC_thin_2000", "mcmc"), num_samples = 2000, thin_factor = 3)
thin_posts(here::here("models", "2020.01.51_nutsMCMC_thin_1000", "mcmc"), num_samples = 1000, thin_factor = 7)
build_rds(.model_list = "2020.01.50_nutsMCMC_thin_2000", .ovwrt_rdata = TRUE)
build_rds(.model_list = "2020.01.51_nutsMCMC_thin_1000", .ovwrt_rdata = TRUE)

