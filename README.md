____
# hake-assessment

hake-assessment is latex and knitr code used to build a PDF of the US/Canadian Hake assessment. The repository was set
up for the first time on Jan 4, 2016.

_____________________________________________________________


## Prerequisites
* MikTex for Windows - the first time you try to run, many packages will be installed automatically.
  This takes some time so make sure you have a fast connection.
* R (version 3.22 "Fire Safety" or later)
* R Packages 'knitr', 'xtable', and 'r4ss' and their dependencies.
* Rscript.exe must be on your PATH.

---
## How to run the code and create hake-assessment.pdf

- Using the command line, navigate to the doc subdirectory and run the buildtex.bat file.

- To clean up the build, run the cleantex.bat file.

- To remove the cached figures, delete the knitr-cache/ directory and all its contents.
  If you don't do this, tables and figures built previously will be used.

- To see the output from the knitr part of the process, look at the file knitrOutput.log.

- To see the output from the Latex part of the process, look at the file latexOutput.log.
  If the compilation seems to hang, check the latexOutput.log file to see where it stopped.

---
