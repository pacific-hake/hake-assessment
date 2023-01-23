#' Extract the vectors from a list into a [tibble::tibble()]
#'
#' @param reps_lst A list of vectors, all the same length and structure,
#' typically extracted as a portion of a Report.sso file
#' @param header A vector of column names for the new table
#' @param verbose Logical. If `TRUE`, show message about which iteration
#' and row range is being populated from the loop
#'
#' @return A [tibble::tibble()] representing one row for each of the list
#'  elements found in `reps_lst`. A new column called `Iter` is prepended and
#'  represents the list element number that the data for each row came from.
#'  List elements that are NA will not be included in the table.
extract_rep_table <- function(reps_lst,
                              header,
                              verbose = TRUE){

  num_iters <- length(reps_lst)
  nrow_per_iter <- map_dbl(reps_lst, ~{length(.x)})
  tot_num_rows <- sum(nrow_per_iter)

  mat <- matrix(, nrow = tot_num_rows, ncol = length(header) + 1)
  row_ind_start <- 1
  row_ind_end <- nrow_per_iter[1]
  for(i in seq_along(reps_lst)){
    vecs <- map(str_split(reps_lst[[i]], " +"), ~{
      # Convert 'Era' VIRG, INIT, TIME, and FORE to numbers
      # 1, 2, 3, and 4 respectively
      # Biomass table
      .x[.x == "VIRG"] <- 1
      .x[.x == "INIT"] <- 2
      .x[.x == "TIME"] <- 3
      .x[.x == "FORE"] <- 4
      .x[.x == "_"] <- 9999
      .x[.x == "NA"] <- 9999
      # Selectivity table
      .x[.x == "Asel"] <- 9999
      asel_ind <- grep("Asel", .x)
      if(length(asel_ind)){
        .x[asel_ind] <- 9999
      }
      # Vulnerable bioimass table
      .x[.x == "sel*wt"] <- 8888
      lbl_ind <- grep("sel", .x)
      if(length(lbl_ind)){
        .x[lbl_ind] <- 9999
      }
      # Numbers at age table (also has time period FOR, TIME etc
      # Which is taken care of already
      .x[.x == "B"] <- 9999
      .x[.x == "M"] <- 8888
      # Catch-at-age table (also has time period FOR, TIME etc
      .x[.x == "dead"] <- 9999
      # Pearson Residuals table
      .x[.x == "AGE"] <- 9999
      as.numeric(.x)
    })
    mtch_end <- i * nrow_per_iter[1]
    if(i %% 500 == 0 && verbose)
      cat("Iter: ", i, " -> Matrix rows: ", row_ind_start, "--", row_ind_end, "\n")
    vecs <- map(vecs, ~{c(i, .x)})
    iter <- 1
    for(j in row_ind_start:row_ind_end){
      if(length(mat[j, ]) > length(vecs[[iter]])){
        length(vecs[[iter]]) <- length(mat[j, ])
      }
      mat[j, ] <- vecs[[iter]]
      iter <- iter + 1
    }
    row_ind_start <- row_ind_end + 1
    row_ind_end <- row_ind_start + nrow_per_iter[i] - 1
  }
  colnames(mat) <- c("Iter", header)
  df <- mat |>
    as_tibble()

  if("Era" %in% names(df)){
    df <- df |>
      mutate(Era = ifelse(Era == 1, "VIRG",
                          ifelse(Era == 2, "INIT",
                                 ifelse(Era == 3, "FORE",
                                        Era))))
  }
  if("obs_cat:_1" %in% names(df)){
    df <- df |>
      mutate(`obs_cat:_1` = ifelse(`obs_cat:_1` == 9999,
                                   "_",
                                   `obs_cat:_1`))
  }
  if("ABC_buffer" %in% names(df)){
    df <- df |>
      mutate(ABC_buffer = ifelse(ABC_buffer == 9999,
                                 NA_real_,
                                 ABC_buffer))

  }
  if("Factor" %in% names(df)){
    df <- df |>
      mutate(Factor = ifelse(Factor == 9999,
                             "Asel",
                             ifelse(Factor == 8888,
                                    "sel*wt",
                                    Factor)))
  }
  if("Beg/Mid" %in% names(df)){
    df <- df |>
      mutate(`Beg/Mid` = ifelse(`Beg/Mid` == 9999,
                             "B",
                             ifelse(`Beg/Mid` == 8888,
                                    "M",
                                    `Beg/Mid`)))
  }
  if("Type" %in% names(df)){
    df <- df |>
      mutate(Type = ifelse(Type == 9999,
                           "dead",
                           Type))
  }
  if("Kind" %in% names(df)){
    df <- df |>
      mutate(Type = ifelse(Kind == 9999,
                           "AGE",
                           Kind))
  }

  df
}
