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

- To remove the cached figures, delete the **knitr-cache** directory and all its contents.
  If you don't do this, tables and figures built previously will be used.

- To see the output from the knitr part of the process, look at the file **knitrOutput.log**.

- To see the output from the Latex part of the process, look at the file **latexOutput.log**.
  If the compilation seems to hang, check the **latexOutput.log** file to see where it stopped.

---

## How the R environment is set up

- To reload the models and data files, and any changes to the R code, open an R terminal and change it's working directory to
  the **doc/r** directory, then type **source("all.r")**. This will in turn source other r code
  files, which load data and model outputs. The model outputs are put into a list, with one element for each
  sub-directory found in the **models** directory. Close the R session, and make sure to save the workspace when it asks you.
  Saving the workspace will create a file called **.RData** in the **doc/r** directory. Make sure this exists before trying to run the
  **buildtex.bat ** script, as that is what is read in by knitr.

- The **models** directory should only have valid model
  directories in it; there should be no empty directories or extraneous files.

- Each model sub-directory may have an **mcmc** directory, which itself contains all the files used to run
  the model in an mcmc configuration. These will be loaded during the load phase and attached
  as the object **mcmc** to it's parent model object. If there is no **mcmc** directory, or it failed to load,
  the **mcmc** object will be set to **NULL**.

The following depicts the object structure of the **models** list:

    models[[1]] - First directory found, typically of format like: 00_Last_years_model
    models[[2]] - Second directory found, typically of format like: 01_Last_years_model
    ...
    models[[N]] - The Nth directory found; each unique member of this models list contains
      models[[N]]$mcmc - mcmc output from the model as read in by the SSgetMCMC function in the r4ss package or NULL if none for this model
      models[[N]]$dat  - data file as read in by the SS_readdat function in the r4ss package
      models[[N]]$     - All the objects as read in by the SS_output function in the r4ss package

---

## How knitr deals with the R environment

- The file **doc/hake-assessment.rnw** has the initial knitr code chunk in it, where the R environment is loaded. Once this is loaded,
  knitr has full access throughout the document to the environment, and calls to plot or create tables can be made.
  This is true of child documents as well, e.g. **doc/executive-summary/executive-summary.rnw**.

- The list structure of the model scenarios is crucial to keep things organized, and to ensure sensitivity plots are easy to implement.

---
