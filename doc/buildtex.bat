Rscript -e "library(knitr);knit('./hake-assessment.rnw')" 1> knitrOutput.log 2>&1

(@pdflatex "hake-assessment.tex" && pdflatex "hake-assessment.tex" && bibtex "hake-assessment.aux" && pdflatex "hake-assessment.tex"&& pdflatex "hake-assessment.tex") 1> latexOutput.log 2>&1
