Rscript -e "library(knitr);knit('./hake-assessment.rnw')" 1> knitrOutput.log 2>&1

(@latex -synctex=1 "hake-assessment.tex" && bibtex "hake-assessment" && latex "hake-assessment.tex" && latex "hake-assessment.tex" && dvips "hake-assessment.dvi" && ps2pdf "hake-assessment.ps") 1> latexOutput.log 2>&1
