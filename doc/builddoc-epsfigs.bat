Rscript -e "source(here::here('R/all.r'));build_doc(png_figs = FALSE)" 1> latex_output.log 2>&1
mv hake-assessment.pdf hake-assessment-eps.pdf