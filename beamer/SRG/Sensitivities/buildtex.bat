Rscript -e "library(knitr);knit('./beamer-hake-sensitivities.rnw')" 1> knitrOutput.log 2>&1

(@latex -synctex=1 "beamer-hake-sensitivities.tex" && bibtex "beamer-hake-sensitivities" && latex "beamer-hake-sensitivities.tex" && latex "beamer-hake-sensitivities.tex" && dvips "beamer-hake-sensitivities.dvi" && ps2pdf "beamer-hake-sensitivities.ps") 1> latexOutput.log 2>&1
