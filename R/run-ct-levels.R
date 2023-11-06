#' A wrapper to run the catch levels determination routines
#'
#' @param model_path The model directory name
#' @param ct_levels_path The catch levels list, which is a list of lists
#' of length 3
#' @param ... Absorbs arguments intended for other functions
#'
#' @return [base::invisible()]
#' @export
run_ct_levels <- function(model_path,
                          ct_levels_path = ct_levels_path,
                          ...){

  model <- load_ss_files(model_path)

  ct_levels_path <- file.path(model_path, ct_levels_path)
  dir.create(ct_levels_path, showWarnings = FALSE)
  unlink(file.path(ct_levels_path, "*"), recursive = TRUE)

  plan("multisession")
  future_map(1:3, function(x = .x,
                           ct_levels_path = ct_levels_path,
                           default_hr_path = default_hr_path,
                           spr_100_path = spr_100_path,
                           stable_catch_path = stable_catch_path,
                           forecast_yrs = forecast_yrs,
                           ct_levels_spr_tol = ct_levels_spr_tol,
                           ct_levels_catch_tol = ct_levels_catch_tol,
                           ct_levels_max_iter = ct_levels_max_iter){
    if(x == 1){
      model <- load_ss_files(model_path)
      default_hr_path <- file.path(ct_levels_path, default_hr_path)
      dir.create(default_hr_path, showWarnings = FALSE)
      run_ct_levels_default_hr(model,
                               default_hr_path,
                               forecast_yrs)
    }else if(x == 2){
      model <- load_ss_files(model_path)
      spr_100_path <- file.path(ct_levels_path, spr_100_path)
      dir.create(spr_100_path, showWarnings = FALSE)
      run_ct_levels_spr_100(model,
                            spr_100_path,
                            forecast_yrs,
                            ct_levels_spr_tol,
                            ct_levels_catch_tol,
                            ct_levels_max_iter)
    }else{
      model <- load_ss_files(model_path)
      stable_catch_path <- file.path(ct_levels_path, stable_catch_path)
      dir.create(stable_catch_path, showWarnings = FALSE)
      run_ct_levels_stable_catch(model,
                                 stable_catch_path,
                                 forecast_yrs,
                                 ct_levels_catch_tol,
                                 ct_levels_max_iter)
    }
  },
  ct_levels_path = ct_levels_path,
  default_hr_path = default_hr_path,
  spr_100_path = spr_100_path,
  stable_catch_path = stable_catch_path,
  forecast_yrs = forecast_yrs,
  ct_levels_spr_tol = ct_levels_spr_tol,
  ct_levels_catch_tol = ct_levels_catch_tol,
  ct_levels_max_iter = ct_levels_max_iter,
  .options = furrr_options(
    globals = c(f = f,
                load_ss_files = load_ss_files,
                SS_output = SS_output,
                fix.posteriors = fix.posteriors,
                posts_fn = posts_fn,
                derposts_fn = derposts_fn,
                create_kn_files = create_kn_files,
                calc_mcmc = calc_mcmc,
                get_os = get_os,
                run_ct_levels_default_hr = run_ct_levels_default_hr,
                run_ct_levels_spr_100 = run_ct_levels_spr_100,
                run_ct_levels_stable_catch = run_ct_levels_stable_catch,
                latex_bold = latex_bold,
                forecast_fn = forecast_fn,
                ss_executable = ss_executable,
                show_ss_output = show_ss_output,
                starter_fn = starter_fn,
                modify_starter_mcmc_type = modify_starter_mcmc_type,
                system_ = system_)))
  plan()
}
