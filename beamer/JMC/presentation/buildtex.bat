Rscript -e "library(knitr);knit('./beamer-hake-management.rnw')" 1> knitrOutput.log 2>&1

(@latex -synctex=1 "beamer-hake-management.tex" && bibtex "beamer-hake-management" && latex "beamer-hake-management.tex" && latex "beamer-hake-management.tex" && dvips "beamer-hake-management.dvi" && ps2pdf "beamer-hake-management.ps") 1> latexOutput.log 2>&1
