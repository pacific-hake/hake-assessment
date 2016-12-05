Rscript -e "library(knitr);knit('./beamer-hake-JMC.rnw')" 1> knitrOutput.log 2>&1

(@latex -synctex=1 "beamer-hake-JMC.tex" && latex "beamer-hake-JMC.tex" && latex "beamer-hake-JMC.tex" && dvips "beamer-hake-JMC.dvi" && ps2pdf "beamer-hake-JMC.ps") 1> latexOutput.log 2>&1
