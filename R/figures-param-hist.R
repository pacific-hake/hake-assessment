#' Make a histogram and density plot of posterior samples for a given parameter and model (or set of models)
#'
#' @param model The SS model output as loaded by [load_ss_files()] or a list of models (see examples)
#' @param model.name model name or set of model names
#' @param param_name The SS name for a given parameter
#' @param legend.pos Position for the legend, e.g., top or none for no legend
#' @param vert.line Location on x-axis of vertical line, used to show single estimate with distribution 
#' @param xlabel The label used for the x-axis
#'
#' @return Nothing
#' @export
#'
#' @examples
#' plot_hist(model=list(base_model), model.name=base_model_name,param_name = "Late_RecrDev_2019",xlabel = "RecDev_2019",legend.pos="none")
#' plot_hist(model=c(list(base_model), list(sens_models_7),sens_models_2),model.name=c(base_model_name,"rwMH",sens_model_dir_names_2))
#' plot_hist(model=list(base_model), model.name=base_model_name,param_name = "Recr_2019",xlabel = "Rec_2019",legend.pos="none",vert.line=16149000)
#' 
plot_hist <- function(model=list(base_model),model.name=base_model_name,param_name = "SR_LN(R0)",
                            legend.pos="top",xlabel = expression(ln(R[0]))){
  
  #can use legend.pos = "none" for no legend
  
  sums <- SSsummarize(model)
  didit <- dplyr::bind_rows(lapply(sums$mcmc, "[", param_name), .id = "Model")
  colnames(didit)[2] <- "value"
  didit[["Model"]] <- factor(didit[["Model"]],
                             labels = model.name)
  # https://www.datanovia.com/en/blog/ggplot-histogram-with-density-curve-in-r-using-secondary-y-axis/
  # 1. Create the histogram plot
  phist <- gghistogram(didit,
                       x = "value",
                       xlab = xlabel,
                       add = "median",
                       rug = TRUE,
                       title = "",
                       fill = "Model",
                       palette = c("#00AFBB", "#E7B800","#FFDB6D", "#C4961A", "#F4EDCA", 
                                   "#D16103", "#C3D7A4", "#52854C", "#4E84C4", "#293352")) +
    theme(legend.position = legend.pos)
    
  # 2. Create the density plot with y-axis on the right
  # Remove x axis elements
  pdensity <- ggdensity(didit,
                        x = "value",
                        xlab = xlabel,
                        title = "",
                        color= "Model",
                        palette = c("#00AFBB", "#E7B800","#FFDB6D", "#C4961A", "#F4EDCA", 
                                    "#D16103", "#C3D7A4", "#52854C", "#4E84C4", "#293352"),
                        alpha = 0) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                       position = "right")  +
    theme_half_open(11, rel_small = 1) +
    rremove("x.axis")+
    rremove("xlab") +
    rremove("x.text") +
    rremove("x.ticks") +
    rremove("legend")
  # 3. Align the two plots and then overlay them.
  aligned_plots <- align_plots(phist, pdensity, align = "hv", axis = "tblr")
  ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]])
  
  # Code for ggplot (not as good)
  # ggplot(didit,
  #   aes(x=R0, col=factor(model, labels = ))
  #   )+
  # geom_density(lwd=1.5,show.legend=FALSE)+
  # stat_density(geom="line",position="identity")+
  # labs(colour="Model")+
  # theme(legend.position=c(0.8,.8),legend.box.background=element_rect(linetype=0))+
  # scale_colour_grey()
}
