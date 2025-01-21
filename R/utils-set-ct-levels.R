#' Reads in the forecast description data and creates the list containing
#' the catch levels and extra information used for the assessment document
#'
#' @param fn The name of the file containing the forecast description data
#' @param inc_fi_and_stable_catch Logical. If `TRUE`, include the Fishing
#' intensity = 100% and the Stable Catch scenarios
#'
#' @return A list of 2 lists:
#' List 1 is a list of lists of length-3 vectors. Each list represents a catch
#' level, each vector's three elements are:
#' 1 - A vector of catch levels
#' 2 - The nice name for the catch level scenario
#' 3 - The directory name for the catch level scenario
#' List 2 is a list of important values and indices referenced in the document.
#' Be sure to update this each year if forecasts are added and/or removed
#'
#' @export
set_ct_levels <- function(fn = here(doc_path, forecast_descriptions_fn),
                          inc_fi_and_stable_catch = FALSE){

  ret <- list()

  # Need fread() here because there are commas in the description field
  # TODO: Check this again and remove data.table dependency if possible
  ct_levels <- fread(fn) |>
    as_tibble()

  if(!inc_fi_and_stable_catch){
    # Descriptions of the catch streams
    inds <- grep("FI|Stable", ct_levels$description)
    if(length(inds)){
      ct_levels <- ct_levels[-inds, ]
    }
  }

  # Columns with forecast catch
  ct_inds <- grep("^catch_year[0-9]+$", names(ct_levels))
  desc_ind <- grep("description", names(ct_levels))
  dir_name_ind <- grep("directory", names(ct_levels))

  # Convert the table format into a list structure that can makes run
  # code simpler
  ret$ct_levels <- ct_levels |>
    pmap(~{
      row <- c(...) |>
        set_names(NULL)
      list(as.numeric(row[ct_inds]),
           row[desc_ind],
           row[dir_name_ind])})

  ret$ct_levels_vals <- list(
    ct_levels_num = length(ret$ct_levels),
    ct_actual_ind = grep("actual", ct_levels$special),
    ct_tac_last_ind = grep("tac_last", ct_levels$special),
    ct_tac_ind = grep("^tac$", ct_levels$special),
    ct_spr100_ind = grep("spr100", ct_levels$special),
    ct_default_policy_ind = grep("default_hr", ct_levels$special),
    ct_stable_ind = grep("stable_catch", ct_levels$special),
    ct_reduction_rows = grep("reduction", ct_levels$forecast_type),
    ct_constant_rows = grep("constant", ct_levels$forecast_type),
    # Copy/paste values of `ct_constant_rows` in here
    ct_constant_str =
      paste(letters[grep("constant", ct_levels$forecast_type)],
            collapse = ", "),
    # Copy/paste values of `ct_reduction_rows` in here
    ct_reduction_str =
      paste(letters[grep("reduction", ct_levels$forecast_type)],
            collapse = ", "))

  ret
}
