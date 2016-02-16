Rscript -e "library(knitr);knit('./beamer-hake-data.rnw')" 1> knitrOutput.log 2>&1

(@latex -synctex=1 "beamer-hake-data.tex" && bibtex "beamer-hake-data" && latex "beamer-hake-data.tex" && latex "beamer-hake-data.tex" && dvips "beamer-hake-data.dvi" && ps2pdf "beamer-hake-data.ps") 1> latexOutput.log 2>&1
