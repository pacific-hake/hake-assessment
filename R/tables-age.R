#' Make an at-age table showing percentage of each age by year
#'
#' @param model The model to make the table for
#' @param fleet The fishing fleet (1 = commercial, 2 = survey)
#' @param start_yr First year in table
#' @param end_yr Last year in table
#' @param csv.dir Directory for output of the table in csv format
#' @param xcaption Caption for the table
#' @param xlabel Latex label to use in the main document for this table
#' @param font.size Point size of font
#' @param space.size Vertical space between rows of table
#' @param placement Latex table placement string
#' @param decimals Number if decimal points to show
#'
#' @return an [xtable::xtable()]
make.input.age.data.table <- function(model,
                                      fleet = 1,
                                      start_yr,
                                      end_yr,
                                      csv.dir = "out-csv",
                                      xcaption = "default",
                                      xlabel = "default",
                                      font.size = 9,
                                      space.size = 10,
                                      placement = "htbp",
                                      decimals = 2){

  if(!dir.exists(csv.dir)){
    dir.create(csv.dir)
  }

  # Get ages from header names
  age.df <- model$dat$agecomp
  if(fleet == 2){
    age.df <- age.df[ , names(age.df) != "a1"]
  }
  nm <- colnames(age.df)
  yr <- age.df$Yr
  flt <- age.df$FltSvy
  n.samp <- age.df$Nsamp
  # Get ages from column names
  ages.ind <- grep("^a[[:digit:]]+$", nm)
  ages.num <- gsub("^a([[:digit:]]+)$", "\\1", nm[ages.ind])
  ages.num[length(ages.num)] <- paste0(ages.num[length(ages.num)], "+")
  # Make all bold
  ages <- latex_bold(ages.num)
  # Put ampersands in between each item and add newline to end
  ages.tex <-latex_paste(ages)

  # Construct age data frame
  age.df <- age.df[, ages.ind]
  age.df <- t(apply(age.df,
                    1,
                    function(x){
                      as.numeric(x) / sum(as.numeric(x))
                    }))
  #age.headers <- paste0(latex_mcol(1, "c", ages), latex_amp())
  age.df <- cbind(yr, n.samp, flt, age.df)

  # Fishery or survey?
  age.df <- age.df[age.df[,"flt"] == fleet,]
  # Remove fleet information from data frame
  age.df <- age.df[,-3]
  # Extract years
  age.df <- age.df[age.df[,"yr"] >= start_yr & age.df[,"yr"] <= end_yr,]

  # Make number of samples pretty
  age.df[,2] <- f(as.numeric(age.df[,2]))
  # Make percentages for age proportions
  age.df[,-c(1,2)] <- as.numeric(age.df[,-c(1,2)]) * 100
  age.df[,-c(1,2)] <- f(as.numeric(age.df[,-c(1,2)]), decimals)

  # Add the extra header spanning multiple columns
  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1

  addtorow$command <-
    paste0(latex_hline,
           latex_bold("Year"),
           latex_amp(),
           latex_mlc(c("Number",
                       "of samples")),
           latex_amp(),
           latex_mcol(length(ages),
                      "c",
                      latex_bold("Age (\\% of total for each year)")),
           latex_nline,
           latex_amp(2),
           ages.tex,
           latex_nline,
           latex_hline)

  addtorow$command <- paste0(addtorow$command,
                             latex_continue(ncol(age.df), addtorow$command))

  size.string <- latex_size_str(font.size, space.size)
  # Write the CSV
  cnames <- colnames(age.df)
  cnames[3:length(cnames)] <- ages.num
  # Add + for plus group
  cnames[length(cnames)] <- paste0(cnames[length(cnames)], "+")
  colnames(age.df) <- cnames

  write.csv(age.df,
            file.path(csv.dir,
                      ifelse(fleet == 1,
                             "fishery-input-age-proportions.csv",
                             "survey-input-age-proportions.csv")),
            na = "")

  print(xtable(age.df,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(age.df))),
        caption.placement = "top",
        include.rownames = FALSE,
        include.colnames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        add.to.row = addtorow,
        table.placement = placement,
        tabular.environment = "longtable",
        hline.after = NULL)
}

make.can.age.data.table <- function(dat,
                                    dat_num_fish,
                                    fleet = 1,
                                    start_yr,
                                    end_yr,
                                    xcaption = "default",
                                    xlabel = "default",
                                    font.size = 9,
                                    space.size = 10,
                                    placement = "H",
                                    decimals = 2){
  ## Returns an xtable in the proper format for the main tables section for
  ##  Canadian age data.
  ##
  ## fleet - 1 = Can-Shoreside, 2 = Can-FT, 3 = Can-JV
  ## start_yr - start the table on this year
  ## end_yr - end the table on this year
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table
  ## placement - latex code for placement of table
  ## decimals - number of decimals in the numbers in the table

  ages.df <- dat[[fleet]]
  n.trip.haul <- as.numeric(dat[[fleet + 3]])
  ages.df <- cbind(n.trip.haul, ages.df)
  dat <- cbind(as.numeric(rownames(ages.df)), ages.df)
  dat <- dat[dat[,1] >= start_yr & dat[,1] <= end_yr,]
  colnames(dat)[1] <- "year"
  dat <- as_tibble(dat)
  dat_num_fish <- as_tibble(dat_num_fish)

  dat <- dat %>%
    left_join(dat_num_fish, by = "year") %>%
    select(year, num_fish, n.trip.haul, everything()) %>%
    mutate(n.trip.haul = as.numeric(f(n.trip.haul))) %>%
    mutate(year = as.character(year))

  # Make percentages
  dat[, -c(1, 2, 3)] <- dat[, -c(1, 2, 3)] * 100
  dat[, -c(1, 2, 3)] <- apply(dat[, -c(1, 2, 3)], 2, function(x) f(x, decimals))
  dat[, 2:3] <- apply(dat[, 2:3], 2, function(x) f(x))
  # Add the extra header spanning multiple columns
  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  age.headers <- names(dat)[grep("^[[:digit:]].*", names(dat))]
  ages <- 1:length(age.headers)
  ages[length(ages)] <- paste0(ages[length(ages)], "+")
  ages.tex <- latex_paste(latex_bold(ages))

  if(fleet == 2 | fleet == 3){
    mlc <- latex_mlc(c("Number",
                       "of hauls"))
  }else{
    mlc <- latex_mlc(c("Number",
                       "of trips"))
  }
  num_fish <- latex_mlc(c("Number",
                          "of fish"))

  addtorow$command <-
    paste0(latex_hline,
           latex_bold("Year"),
           latex_amp(),
           num_fish,
           latex_amp(),
           mlc,
           latex_amp(),
           latex_mcol(length(age.headers),
                      "c",
                      latex_bold("Age (\\% of total for each year)")),
           latex_nline,
           latex_amp(3),
           ages.tex,
           latex_nline,
           latex_hline)

  addtorow$command <- paste0(addtorow$command,
                             latex_continue(ncol(dat), addtorow$command))

  size.string <- latex_size_str(font.size, space.size)
  print(xtable(dat,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(dat))),
        caption.placement = "top",
        include.rownames = FALSE,
        include.colnames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        add.to.row = addtorow,
        table.placement = placement,
        tabular.environment = "longtable",
        hline.after = NULL)
}

#' Makes a table of the estimated -at-age values for 5 different values:
#' Numbers-at-age, Catch-at-age, Biomass-at-age, Exploitation-at-age,
#' and Catch-at-age-biomass
#'
#' @param model A model in this project
#' @param start_yr Start year for the table
#' @param end_yr End year for the table
#' @param table_type 1 = Numbers-at-age, 2 = Exploitation-rate-at-age, 3 = Catch-at-age-number
#' 4 = Catch-at-age-biomass, 5 = Biomass-at-age
#' @param digits Number of decimal points
#' @param csv_dir Directory for CSV output
#' @param xcaption Table caption
#' @param xlabel The label used to reference the table in latex
#' @param font_size Size of the font for the table
#' @param space_size Size of the vertical spaces for the table
#'
#' @return An [xtable::xtable()]
#' @export
atage_table <- function(model,
                        start_yr = NA,
                        end_yr = NA,
                        table_type = 1,
                        digits = 0,
                        csv_dir = "out-csv",
                        xcaption = "default",
                        xlabel   = "default",
                        font_size = 9,
                        space_size = 10){

  csv_dir_full <- here::here("doc", csv_dir)
  if(!dir.exists(csv_dir_full)){
    dir.create(csv_dir_full)
  }

  tbl <- switch (table_type,
                 model$extra_mcmc$natage_median,
                 model$extra_mcmc$expatage_median,
                 model$extra_mcmc$catage_median,
                 model$extra_mcmc$catage_biomass_median,
                 model$extra_mcmc$batage_median)
  fn <- switch (table_type,
                file.path(csv_dir_full, out_est_naa_file),
                file.path(csv_dir_full, out_est_eaa_file),
                file.path(csv_dir_full, out_est_caa_file),
                file.path(csv_dir_full, out_est_caa_bio_file),
                file.path(csv_dir_full, out_est_baa_file))

  yrs_in_table <- sort(unique(tbl$Yr))
  min_yr <- min(yrs_in_table)
  max_yr <- max(yrs_in_table)
  start_yr <- ifelse(is.na(start_yr), min_yr, start_yr)
  end_yr <- ifelse(is.na(end_yr), max_yr, end_yr)
  if(start_yr > end_yr){
    start_yr <- min_yr
    end_yr <- max_yr
  }
  start_yr <- ifelse(start_yr < min_yr, min_yr, start_yr)
  end_yr <- ifelse(end_yr > max_yr, max_yr, end_yr)
  yrs <- start_yr:end_yr

  dat <- tbl %>%
    filter(Yr %in% yrs) %>%
    rename(Year = Yr) %>%
    mutate(Year = as.character(Year))
  write_csv(dat, fn)
  dat <- dat %>%
    mutate_at(.vars = vars(-Year), ~{f(.x, digits)})
  names(dat)[length(names(dat))] <- paste0(names(dat)[length(names(dat))], "+")

  # Add latex headers
  ages <- colnames(dat)[-1]
  ages_tex <- map_chr(ages, latex_bold)
  ages_tex <- paste0(latex_paste(ages_tex), latex_nline)

  # Add the extra header spanning multiple columns
  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1

  addtorow$command <-
    paste0(latex_hline,
           latex_bold("Year"),
           latex_amp(),
           latex_mcol(ncol(dat) - 1,
                      "c",
                      latex_bold("Age")),
           latex_nline,
           latex_amp(),
           ages_tex,
           latex_hline)

  addtorow$command <- paste0(addtorow$command,
                             latex_continue(ncol(dat), addtorow$command))

  # Make the size string for font and space size
  size_string <- latex_size_str(font_size, space_size)
  return(print(xtable(dat,
                      caption = xcaption,
                      label = xlabel,
                      align = get.align(ncol(dat))),
               caption.placement = "top",
               include.rownames = FALSE,
               include.colnames = FALSE,
               sanitize.text.function = function(x){x},
               size = size_string,
               add.to.row = addtorow,
               table.placement = "H",
               tabular.environment = "longtable",
               #latex.environments = "center",
               hline.after = NULL))

}
