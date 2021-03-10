Rscript -e "library(knitr);knit('./beamer-hake-JMC-Canada.rnw')" 1> knitrOutput.log 2>&1

(@latex -synctex=1 "beamer-hake-JMC-Canada.tex" && latex "beamer-hake-JMC-Canada.tex" && latex "beamer-hake-JMC-Canada.tex" && dvips "beamer-hake-JMC-Canada.dvi" && ps2pdf "beamer-hake-JMC-Canada.ps") 1> latexOutput.log 2>&1
