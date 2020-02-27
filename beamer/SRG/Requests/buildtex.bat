Rscript -e "library(knitr);knit('./beamer-hake-requests-day-2.rnw')" 1> knitrOutput.log 2>&1

(@latex -synctex=1 "beamer-hake-requests-day-2.tex" && latex "beamer-hake-requests-day-2.tex" && latex "beamer-hake-requests-day-2.tex" && dvips "beamer-hake-requests-day-2.dvi" && ps2pdf "beamer-hake-requests-day-2.ps") 1> latexOutput.log 2>&1
