____
# hake-assessment-latex-knitr

hake-assessment-latex-knitr is latex and knitr code used to build a PDF of the US/Canadian Hake assessment. The repository was set
up for the first time on Jan 4, 2016.

_____________________________________________________________


## Prerequisites
* MikTex for Windows
* R (version 3.22 "Fire Safety" or later)
* R Package 'knitr' and its dependencies.
* Other R packages used: r4ss, xtable
* Rscript.exe must be on your PATH.

---
## How to run the code and create hake-assessment.pdf

- Using the command line, navigate to the doc subdirectory and run the buildtex.bat file

- To clean up the build, run the cleantex.bat file.

- To remove the cached figures, delete the knitr-cache/ directory and all its contents.

- To see the output from the knitr part of the process, look at the file knitrOutput.log.

- To see the output from the Latex part of the process, look at the file latexOutput.log.

---
