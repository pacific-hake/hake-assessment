____
# hake-assessment

*A framework which uses latex and knitr code to build the US/Canadian Pacific hake assessment.*
_____________________________________________________________

## What's new for 2020/2021

* All model runs, including sensitivities and retrospectives, were done done using the [ADNUTS](https://github.com/Cole-Monnahan-NOAA/adnuts) MCMC algorithm. MLE runs were done only for the bridging models.

* Model outputs are now stored using `RDS` files instead of `RData` files. RDS files are smaller due to serialization, and can be assigned to a variable in code instead of being loaded into the global environment.

* The assessment is now built using `PNG` figures, which are necessary to conform to web accessibility rules, in particular for the *Automatic reader* function found in PDF viewers.

* Alternative text for figures was incorporated as tooltips, to conform to web accessibility standards.

* Moved some older content off this README page into Wiki pages:
  * [Old methods (pre-2017)](https://github.com/pacific-hake/hake-assessment/wiki/Older-methods-from-pre-2017)
  * [MCMC commands](https://github.com/pacific-hake/hake-assessment/wiki/MCMC-Commands)

## Prerequisites for 2020/2021

* R version 4.0.2 "Taking Off Again", released June 22, 2020
* Install the [tinytex](https://yihui.org/tinytex) package - If the directory `C:/Users/username/AppData/Roaming/TinyTex` exists, delete it before re-installing. See https://github.com/pbs-assess/csasdown/wiki/LaTeX-installation-for-csasdown for more information.
* Rscript.exe must be on your PATH if you want to use
  **Method 1** for building the document (explained below).
* The R package `r4ss`, on the `bioscale` branch:

```
remotes::install_github("r4ss/r4ss", ref = "bioscale")
```

---
## How to create the RDS files required for the document to build

* Place all model directories in the `models` directory. The base model must have an `mcmc` subdirectory; its main directory holds the MPD run and the `mcmc` subdirectory holds the mcmc run for the same model.

* Navigate to the `R` directory and setup the model by editing the files `model-setup.R` and `forecast-catch-levels.R`

* To run all forecasts, retrospectives, and extra-mcmc calculations (required to get posterior survey index trajectories) for the base model, and then build the `RDS` files for the base model, bridge models, and sensitivity models included in the assessment, do the following  [note that `build_rds()` has to be run from your `hake-assessment/` folder, which is given by `here::here()`]:

```R
source(here::here("R/all.r"))

# Run forecasts, retrospectives, catch-levels, and extra-mcmc for the base model only
build_rds(model_dirs = "2021.00.04_base_v1",
          run_catch_levels = TRUE,
          run_forecasts = TRUE,
          run_retrospectives = TRUE,
          run_extra_mcmc = TRUE)

# Build the RDS files for all folders in model-setup.R, but do not run any of the model extras
# (forecasts, retrospectives, catch-levels, and extra-mcmc). Presence of mcmc directories in
# sensitivity model directories will not trigger running of any of the above when called
# this way.
build_rds()
```

* The retrospectives will be run by default using the ADNUTS MCMC routine set to 8000 samples with a warm-up of 250. This takes a very long time. To change the way this is run you would call `build_rds()` like this:

```R
source(here::here("R/all.r"))

# Run retrospectives with half the default samples and half the default warmup samples
# for the base model only
build_rds(model_dirs = "2021.00.04_base_v1",
          run_retrospectives = TRUE,
          retro_n_final = 4000,
          retro_warmup_final = 125)

# Run retrospectives using MLE only (No ADNUTS MCMC) for the base model only
build_rds(model_dirs = "2021.00.04_base_v1",
          run_retrospectives = TRUE,
          retro_mcmc = FALSE)
```

* The `model_list` as defined in the `R/model-setup.R` file is what is used by default by `build_rds()`. You can also use any list of model directory names, or a single directory name. If you wanted to run retrospectives only for a model called **test-model** you would call it like this:

```R
source(here::here("R/all.r"))
build_rds("test-model", run_retrospectives = TRUE)
```

<span style="color:red">**Careful!**</span> - **ALL** directories in the `model_list` with an `mcmc` subdirectory will have the full gamut of forecasts, retrospectives, and extra-mcmc runs done if you set those arguments to `TRUE`. This can take a very long time if you have `mcmc` output you don't need. It is worth checking and renaming any `mcmc` subdirectories (to `mcmc1` or something) which are not going to have mcmc outputs used in the document, or calling `build_rds()` on a directory-by-directory basis.

* Once finished, you can see that each model defined in `model-setup.R` now has an `RDS` file inside its directory with the same name.

* To delete all existing RDS files and rebuild them again from the model outputs, run the following. This assumes you have previously done all the forecasting, retrospectives, and extra-mcmc calculations:
```R
source(here::here("R/all.r"))
delete_rds_files()
build_rds()
```

---
## How to create hake-assessment.pdf
**The `RDS` files must have been created using the method above before the document can be built.**

* **Method 1**
  
  Run the batch file `builddoc.bat`. Doing it this way will allow you to continue to work in your R session while the document builds.

  * To see the output from this see `latex_output.log`
  * You can look at the log file while it is building to see where in the build process you are using a non-locking editor such as Emacs
  * If the compilation seems to hang, check the two log files to see where it stopped

* **Method 2**

  * Run this in an R session:
  ```R
  source(here::here("R/all.r"))
  build_doc()
  ```
  
  * After the first time you do this, the models will be loaded into the R workspace and any subsequent builds will be a little faster.

* **Method 3 (Andy)**

  * Run this in an R session:
  ```R
  knitr::knit("hake-assessment.rnw")
  ```
  To add alternative text (after running the above without a cache):
  ```R
  add_alt_text(alt_fig_text = alt_fig_text)
  ```

  Then in a terminal:
  ```
  pdflatex hake-assessment
  ```
  repeating it if needed (if labels have changed). As necessary do:
  ```R
  ispell hake-assessment.tex
  bibtex hake-assessment
  ```

## How to clean up the `doc` directory after an erroneous build
* To remove everything from the build, including cached figures and table data, run the batch file `freshdoc.bat`
* To remove all the Latex files but keep the cached figures and table data, run the batch file `cleandoc.bat`

## How to debug functions used in the knitr chunks in the `.rnw` files

* Open R and use this one-liner so that you can use the R history (up and down-arrows)
  This is important while debugging the R code, because you will need to run this each
  time you make a change in the code and want to test it, or if you insert a `browser()` command somewhere:
  ```R
	source(here::here("R/all.r"));load_models_rds();source(here::here("R/custom-knitr-variables.r"))
  ```
* Cut-and-paste the figure/table code from the knitr chunk you want to debug into R and the output will be exactly
  what will appear in the document.

---
## How to create adnuts-diagnostics.pdf
**The `RDS` files must have been created using the method above before the document can be built.**

This document shows a set of diagnostics figures for the base model and each of the sensitivity models runs defined in the sensitivity model groups in `model-setup.R`. If the assessment document builds then this file should build as the `RDS` files already exist. For 16 sensitivity analyses, using `.eps` files created a 100Mb `.pdf`, but using `.png` only created a 20Mb `.pdf`, so the methods here only use `.png` files. (Revert commit c3dacd09 to make `.eps`). 

Document build took about 20 minutes for the `knitr` component (it looks like it hangs at 86% done, but it isn't!), and a few minutes to build the `.pdf`. The following build methods are adapted from those above. Ignore the latex warnings about having `multiply-defined labels`.

* **Method 1**
  
  Run the batch files `doc-adnuts-diagnostics/builddoc-adnuts.bat`.

* **Method 2**

  * Run this in an R session:
  ```R
  setwd(here::here("doc"))
  source(here::here("R/all.r"))
  build_adnuts_doc()
  ```

  * After the first time you do this, the models will be loaded into the R workspace and any subsequent builds will be a little faster.

* **Method 3**
  * Run this in an R session in `doc-adnuts-diagnostics/`:
  ```
  knitr::knit("adnuts-diagnostics.rnw")
  ```
  then this in a terminal:
  ```
  pdflatex hake-assessment.tex
  ```
  * And run that again to get Table of Contents correct (the only cross-referencing in the document) if necessary.

## How to clean up the `doc` directory after an erroneous build (Andy not fully tested, but has updated)
* To remove everything from the build, including cached figures and table data, run the batch file
  * `doc-adnuts-diagnostics/freshdoc-png.bat`

* To remove all the Latex files but keep the cached figures and table data, run one of the batch files
  * `doc-adnuts-diagnostics/cleandoc-png.bat`

---
## How the R environment is set up

* When the document is built, all of the model RDS files which were previously built are loaded into the workspace that is seen by knitr. All the lengthy R processes are done ahead of time from the `build_rds()` function to make the document building quicker.

The following depicts the object structure of each model's RDS file:

```R
model$          - All the objects as read in by the SS_output function in the r4ss package
model$retros    - A list of MLE retrospective outputs from SS_output
model$retros[[1]] - Model run with one year removed
model$retros[[2]] - Model run with two years removed
...
model$retros[[N]] - Model run with N years removed (depends on user input when sourcing all.r)

model$forecasts - A list of forecasts, 1 for each year from the mcmc run of the model
model$forecasts[[1]] - A list, one element for each catch level forecasted for the first year forecast
model$forecasts[[1]][[1]] - A list of 4 items (see below) for the first forecast level for the first year forecast
model$forecasts[[1]][[2]] - A list of 4 items (see below) for the second forecast level for the first year forecast
...
model$forecasts[[1]][[N]] - A list of 4 items (see below) for the last forecast level for the first year forecast
  model$forecasts[[1]][[N]]$outputs   - List of mcmc outputs from the forecast models as read in by the SSgetMCMC function
  model$forecasts[[1]][[N]]$mcmccalcs - Calculations done on the mcmc outputs for this forecast model. Same structure as below.
  model$forecasts[[1]][[N]]$biomass   - Forecasts for biomass. The rows are labelled by forecast year.
  model$forecasts[[1]][[N]]$spr       - Forecasts for SPR. The rows are labelled by forecast year.
model$risks     - A list, one element for each forecast year except the last year
  model$risks[[1]] - Holds the risk values for the first year of forecasts
  model$risks[[2]] - Holds the risk values for the second year of forecasts
  ...
  model$risks[[N]] - Holds the risk values for the last year - 1 of forecasts
model$extra.mcmc- Extra MCMC output obtained by running MLE once for each MCMC sample and extracting output
  model$extra.mcmc$agedbase$Exp         - median of posterior for expected value for age comps
  model$extra.mcmc$agedbase$Exp.025     - 2.5% of posterior for expected value for age comps
  model$extra.mcmc$agedbase$Exp.975     - 97.5% of posterior for expected value for age comps
  model$extra.mcmc$agedbase$Pearson     - median of posterior for pearson residuals for age comps
  model$extra.mcmc$agedbase$Pearson.025 - 2.5% of posterior for pearson residuals for age comps
  model$extra.mcmc$agedbase$Pearson.975 - 97.5% of posterior for pearson residuals for age comps
  model$extra.mcmc$cpue.table           - Table of cpue index values for all posteriors (survey)
  model$extra.mcmc$cpue.median          - median of posterior for cpue index values (survey)
  model$extra.mcmc$cpue.025             - 2.5% of posterior for cpue index values (survey)
  model$extra.mcmc$cpue.975             - 97.5% of posterior for cpue index values (survey)
  model$extra.mcmc$like.info            - Likelihood values for all posteriors
model$path      - The path where this model is located
model$ctl.file  - control file name for this model
model$dat.file  - data file name for this model
model$dat       - data file as read in by the SS_readdat function in the r4ss package
model$mcmc      - mcmc output from the model as read in by the SSgetMCMC function or NULL if none for this model
model$mcmcpath  - The path where this mcmc model is located
model$mcmccalcs - calculations done on the mcmc outputs for this model
  model$mcmccalcs$svirg     - SSB virgin biomass, vector of length 3 (2.5%, 50%, 97.5%)
  model$mcmccalcs$sinit     - SSB initial biomass, vector of length 3 (2.5%, 50%, 97.5%)
  model$mcmccalcs$slower    - SSB lower confidence (2.5%)
  model$mcmccalcs$smed      - SSB median (50%)
  model$mcmccalcs$supper    - SSB upper confidence (97.5%)
  model$mcmccalcs$dlower    - Depletion lower confidence (2.5%)
  model$mcmccalcs$dmed      - Depletion median (50%)
  model$mcmccalcs$dupper    - Depletion upper confidence (97.5%)
  model$mcmccalcs$rvirg     - Virgin recruitment, vector of length 3 (2.5%, 50%, 97.5%)
  model$mcmccalcs$rinit     - Initial recruitment, vector of length 3 (2.5%, 50%, 97.5%)
  model$mcmccalcs$runfished - Unfished recruitment, vector of length 3 (2.5%, 50%, 97.5%)
  model$mcmccalcs$rlower    - Recruitment lower confidence (2.5%)
  model$mcmccalcs$rmed      - Recruitment median (50%)
  model$mcmccalcs$rupper    - Recruitment upper confidence (97.5%)
  model$mcmccalcs$devlower  - Recruitment deviations lower confidence (2.5%)
  model$mcmccalcs$devmed    - Recruitment deviations median (50%)
  model$mcmccalcs$devupper  - Recruitment deviations upper confidence (97.5%)
  model$mcmccalcs$plower    - SPR lower confidence (2.5%)
  model$mcmccalcs$pmed      - SPR median (50%)
  model$mcmccalcs$pupper    - SPR upper confidence (97.5%)
  model$mcmccalcs$flower    - Fishing mortality lower confidence (2.5%)
  model$mcmccalcs$fmed      - Fishing mortality median (50%)
  model$mcmccalcs$fupper    - Fishing mortality upper confidence (97.5%)
```

There are the other variables in the global workspace. These can be directly referenced using \Sexpr{} in inline latex code, or in a knitr code chunk. They are declared in the `custom-knitr-variables.r` file.

---
### To take a quick look at model output without making an RDS file

Open R within the model's folder and use the R commands:

```
library(r4ss)
SS_plots(SS_output("./"))
```

This creates figures and an HTML page with tabs for sets of figures. This is useful for quickly looking at results, especially when MCMCs have not yet been run and so the assessment document will not build yet.

---
## Survey map

For 2018, Julia Clemons produced the multi-year panel plots from the surveys. Andy converted to `.eps` using

```
pdf2ps <filename>.pdf <filename>.eps
```

which is a ghostscript command. Seems to be fine in document (may not be a properly encapsulated .eps, but we resize it anyway and it looks good and is zoomable). Presumably used that for 2020 assessment also. For 2021 we need .png files, and these worked just fine:
```
magick <filename>.pdf <filename>.png
magick <filename>.eps <filename>.png

```
So Julia can continue to give us a `.pdf` if she can't make a `.png`.

---
## GitHub workflow

All authors push to a single master branch and use the `rebase` method to merge in changes.
```
git fetch
git rebase
```
If we get a conflict when rebasing, abort the rebase and fix the conflict manually:
```
git rebase --abort
git merge origin/master
```
Once the conflict is fixed, the modified file must be added again like this:
```
git add <filename>
git commit ... [as usual]
git merge origin/master
git push
```
This method should avoid confusion since rebasing is not intuitive when you get conflicts.