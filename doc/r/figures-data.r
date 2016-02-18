make.data.overview.plot <- function(model,               ## model contains the output of SS_output
                                    show.title = FALSE){ ## Show the title?
  ## Make a plot of the data used in the assessment
  SSplotData(model, both = FALSE, show.title = show.title)
}
