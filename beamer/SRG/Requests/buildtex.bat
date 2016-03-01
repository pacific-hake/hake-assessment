Rscript -e "library(knitr);knit('./beamer-hake-requests.rnw')" 1> knitrOutput.log 2>&1

(@latex -synctex=1 "beamer-hake-requests.tex" && bibtex "beamer-hake-requests" && latex "beamer-hake-requests.tex" && latex "beamer-hake-requests.tex" && dvips "beamer-hake-requests.dvi" && ps2pdf "beamer-hake-requests.ps") 1> latexOutput.log 2>&1
