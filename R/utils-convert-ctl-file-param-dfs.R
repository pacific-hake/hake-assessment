#' Convert a control file parameter data frame into a format usable in this
#' project
#'
#' @param ctl The control file list as read in by [r4ss::SS_readctl()]
#' @param df_nm The name of the data frame found in the `ctl` list
#'
#' @return A [tibble::tibble()] with modified format and containing only the
#' columns we need
#' @export
convert_ctl_file_param_dfs <- function(ctl, df_nm){

  ctl[[df_nm]] |>
    as_tibble(rownames = "param") %>%
    setNames(tolower(names(.))) |>
    mutate(across(everything(), as.character)) |>
    select(param, hi, lo, pr_type, phase, init, prior, pr_sd) |>
    rename(type = pr_type,
           mean = prior,
           sd = pr_sd) |>
    mutate(param = tolower(param)) |>
    mutate(latex_nm = NA_character_,
           num_param = NA_character_,
           bounds = NA_character_,
           prior_str = NA_character_)
}