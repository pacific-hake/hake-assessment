Rscript -e "library(knitr);knit('./beamer-hake-assessment.rnw')" 1> knitrOutput.log 2>&1

(@latex -synctex=1 "beamer-hake-assessment.tex" && latex "beamer-hake-assessment.tex" && latex "beamer-hake-assessment.tex" && dvips "beamer-hake-assessment.dvi" && ps2pdf "beamer-hake-assessment.ps") 1> latexOutput.log 2>&1
