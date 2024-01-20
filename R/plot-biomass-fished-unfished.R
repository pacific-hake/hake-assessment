#' Plot the fished biomass vs. the unfished biomass for a model
#'
#' @param model A model to plot
#' @param model_nms A vector of length 2 containing the names to use in the
#' legend for the fished/unfished trajectories
#' @param rel Logical. If `TRUE`, plot the biomass relative to dynamic B0
#' biomass
#' @param ... Arguments passed to [plot_biomass()]
#'
#' @return A [ggplot2::ggplot()] object
#' @export
plot_biomass_fished_unfished <- \(model,
                                  model_nms = c("Base model - Fished",
                                                "Base model - Unfished"),
                                  rel = FALSE,
                                  ...){

  # Hack a copy of the model, replacing the biomass time series with
  # the unfished biomass time series so we can use the already-made
  # `plot_biomass()` and `plot_rel_biomass()`
  model_unf <- model
  if(rel){
    model_unf$mcmccalcs$dlower <- model$mcmccalcs$rel_dyn_slower
    model_unf$mcmccalcs$dmed <- model$mcmccalcs$rel_dyn_smed
    model_unf$mcmccalcs$dupper <- model$mcmccalcs$rel_dyn_supper
  }else{
    model_unf$mcmccalcs$slower <- model$mcmccalcs$dyn_slower
    model_unf$mcmccalcs$smed <- model$mcmccalcs$dyn_smed
    model_unf$mcmccalcs$supper <- model$mcmccalcs$dyn_supper
  }

  model_lst <- list(model, model_unf)

  if(rel){
    plot_rel_biomass(model_lst,
                     model_nms,
                     ...)
  }else{
    plot_biomass(model_lst,
                 model_nms,
                 ...)
  }
}