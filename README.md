____
# hake-assessment

*An R package which uses Bookdown and Rmarkdown to build the US/Canadian Pacific hake assessment document*
_____________________________________________________________

## 2023/2024 Update - Complete rewrite of code
* Now a true R package.
* Converted all document code from Sweave with LaTeX to Rmarkdown. See the
 [Rmarkdown reference guide](https://www.rstudio.com/wp-content/uploads/2015/03/rmarkdown-reference.pdf) for help on this syntax.
* Standardize all code filenames by category and one function per file.
* All document text rewritten to conform to Rmarkdown standards.
* Incorporates `bookdown` package for document building.
* `r4ss` package now only used for loading of outputs, not for any figures
  or tables.
* Table of Contents has been improved with uniform spacing and numbering,
  and with the Appendix section appearing the same as the main section.
* All figure functions standardized to return `ggplot2:ggplot()` objects,
  and utilize package global variables for many plot attributes for
  standardization.
* All tables standardized to return `knitr::kbl()` objects.
* post-processor code injection removes all LaTeX from the core document
  text for ease of writing.
* All data tables pre-loaded into global package variables to shorten build
  time and aggregate loading code.
* Decision tables now have the same format as the rest of the tables in the
  document. There is also more room in them so there are more complete
  descriptions in the left column.
* All old, unused functions and other code removed.
* Use of Rmarkdown means that no special LaTeX markup needs to be used any
  longer. For example:
  - The fancy backward and forward quotes that come before and after quoted
    text are simpler. With LaTeX we had to write quotes like this:
    `` `some quote' `` or ```` ``some quote''````. Now we can just write
    `'some quote'` or `"some quote"` to get the same result.
  - Backslashes before special characters are no longer necessary. Previously
    we would have to write something like `the 97.5\% quantile`. Now we would
    simply write `the 97.5% quantile`. This has always been a thorn in our
    side as forgetting one backslash broke the build broken.
---
## How to create the hake assessment PDF document
**The `RDS` files must have been created before the document can be built.**

* Load the hake package like this: `devtools::load_all(".")`
* Render the PDF like this: `render()`
* The Tex file must now be run through LaTeX again, using `LuaLatex` to give us
  the final document. Go to an Operating system terminal window and run the
  following. Note that `lualatex` is run twice. This is to ensure all
  references are set correctly. If you run it only once you will find many
  question marks in the document for figure and table references.
  - `lualatex hake.tex; lualatex hake.tex`

**Details of the `render()` function**

The `render()` function is a two-step process:

1. Calls the `bookdown::render_book()` function to generate the PDF.
1. Runs the post processing step (`post_process()`) which will:
    - Insert the Table of Contents
    - Move any figures or tables around that need to be (Add latex
      position variables such as [H], [bt] [!H]). The setup file for this
      is `doc/object-placement.csv`
    - Customize longtables so they have "continued on next page..." and
      "Continued from previous page..." on them
    - Process landscape figures and tables
    - Align table captions that need it
    - Add figure and table numbers
    - Remove vertical space before section headings
    - Place a marker in the file saying that it's been post-processed
      so post-processing cannot be run on it a second time

## Debugging a figure or table, or anything else

* If you haven't already done so in your current R session, run
  `devtools::load_all(".")` while in the hake package working directory.

* If you are using Rstudio:
    - Copy the chunk or chunks of Rmarkdown code you want to test to the
      clipboard.
    - Run `gotest()`, which will create a temporary directory containing all
      the files necessary to run a pared-down version of the document, and
      switch you to that directory. If your repository directory is not the
      default ("~/github/pacific-hake/hake"), you will have to include the
      `repo_dr` argument in the call to `gotest()`.
    - Click the gear-arrow-down icon ![](gear-arrow-down.png) in the Files
      window (bottom right panel in Rstudio) and select
      `Go to working directory`.
    - Open the `005-text.rmd` file, delete everything in that file if it
      contains anything, and paste your chunk(s) of code. Save the file.
    - In the R terminal, build the document using `render()`. The PDF will
      be built in the current temporary directory, and contain only your test
      figure(s) or table(s).
    - Make changes to your code in the temporary file, and when satisfied with
      your code, copy the code to the clipboard for pasting into the real
      document.

* If you are not using Rstudio:
    - Run `gotest()`, which will create a temporary directory containing all
      the files necessary to run a pared-down version of the document, and
      switch you to that directory.
    - Run `getwd()` to find out the name of the temporary directory you are
      now in. Copy the name of the directory to the clipboard.
    - Go to the directory using a program of your choice, pasting the
      directory name into it to get there fast.
    - Copy the chunk or chunks of Rmarkdown code you want to test to the
      clipboard.
    - Open the `005-text.rmd` file from that directory, delete everything in
      it if it contains anything, and paste your chunk(s) of code.
      Save the file.
    - In the R terminal, build the document using `render()`. The PDF will
      be built in the current temporary directory, and contain only your test
      figure(s) or table(s).
    - Make changes to your code in the temporary file, and when satisfied with
      your code, copy the code to the clipboard for pasting into the real
      document.

* To go back to the real document, run `goback()`. Run `getwd()` to make sure
  you are in the correct directory. In Rstudio, click the gear-down-arrow icon
  ![](gear-arrow-down.png) in the Files window (bottom right panel in Rstudio)
  and select `Go to working directory`. **Be careful here that you've copied
  your new code to the clipboard. It may be lost once you've left the
  temporary directory.**

## 2023 Assessment cycle (Jan - Mar 2023)

* Model runs were done on an Ubuntu 22.04 LTS server with 80 Xeon Gold CPUs and 128 GB of RAM

* All model runs, including the base, bridging, sensitivities, and
  retrospectives, were done using the **main** branch of the
  [ADNUTS](https://github.com/cgrandin/adnuts) MCMC algorithm, which is a Fork.
  
* `extra-mcmc must be and was enabled for ALL models`

## Server setup for 2023

* Operating system: Ubuntu 22.04 LTS (jammy)

* R version: 4.2.2 (2022-11-10 r83330)

* TexLive version: 2022 (tlmgr version: 63068, 2022-04-18 07:58:07 +0200)

* The R packages listed [here](https://github.com/pacific-hake/hake-assessment/blob/356f1a069ddc1f806f0c151d6b15e59e2efe92ec/R/all.R#L20)

* The TEX packages listed [here](https://github.com/pacific-hake/hake-assessment/blob/356f1a069ddc1f806f0c151d6b15e59e2efe92ec/docker/install_packages.R#L21)


---
# How the models are run

* There are bash scripts which launch R functions. The scripts are used to
  allow OS-level control over the parallelism, which allows us to view
  the processes running for each and every model, in addition it is faster than
  using an R package to distribute model runs which are calling `system()` to
  run each model
  
* The bash scripts are located [here](https://github.com/pacific-hake/hake-assessment/tree/master/bash-scripts)

## Base model bash script

* The base model is special and has its own bash script. It is
  `run-base-model.sh`. This needs to be edited each year before beginning.
    
* Check all the variables and make sure they are correct. Change the
   `year_path` to the new assessment year (Whatever year that January is in)

* If run on the server, the `models_path` must be set to `/srv/hake/models`
  
* If run on a local machine, set `models_path` to the location of your
  `models` directory. This is typically a subdirectory of the repository,
  and if so the variable would set like this: `models_path=$repo_path/models`.
  Note that there cannot be spaces around the `=` sign in bash scripts.
    
* The base model has more steps that other models (calculation of catch levels
  for forecasting, and the forecasting itself). There are clear chunk of code
  for these options in `models_path`. Currently you have to comment out what
  you don't want to run (comment character is #)
    
* Some chunks delete files. This is to save space.
  [This chunk](https://github.com/pacific-hake/hake-assessment/blob/356f1a069ddc1f806f0c151d6b15e59e2efe92ec/bash-scripts/run-base-model.sh#L50) for example, deletes all output files in the `forecasts`
  directory except those necessary to run the forecasts
    
## Forecasts for the base model

* Either leave the forecasting chunks in `run-base-model.sh` uncommented or run
  `run-base-forecasts.sh` after the base model has already been run. Note that
  you need to edit this file also before running it, as it has the year,
  etc. in it
  
## Retrospectives for base model

* To run these, first run the base model, then edit `run-retrospectives.sh`. The
  `year_path` will have to be changed each year and also `models_path` if you
  are running on a different machine than the server. See the section
  on running the base model for directions on this variable

## Other models bash scripts

* The bash scripts that start with `run-` are for running models. Each of
  them calls `generic-run-models.sh` which contains the script that actually
  distributes parallel processes and runs the models
  
* The `run-` scripts must have their `project_path` set as follows
  (same as how the base model `models_path` is set):
  
  - If run on a local machine, set `project_path` to the parent directory
    of your `models` directory. This is typically the repository location,
    and if so the variable would set like this:
    
    `` project_path=`Rscript -e "cat(dirname(here::here()))"` ``
    
    Note that there cannot be spaces around the `=` sign in bash scripts

  - `generic-run-models.sh` contains the **year_path** variable that needs to be
    changed each year
    
  - It also contains `version_path` which should usually be **01-version**
    but if the base model is scrapped after some time, then a new
    version will be started called **02-version** and that would have to be
    entered here or you will still be using the old version of the models.
    
## Other bash scripts

* The `create-` scripts are for creating RDS files (if you need or want to do
  this manually after running the models). The `run-` scripts will create the
  RDS files automatically though so you may never need these scripts
  
* The `create-sensitivity-dirs.sh` runs code that calls an R function to create
  the standard set of sensitivities for hake, and insert the files into the
  correct directory structure. This needs to be run every year

---
# To take a quick look at model output without making an RDS file

Open R within the model's folder and use the command:

```
r4ss::SS_plots(SS_output("./"))
```

This creates figures and an HTML page with tabs for sets of figures. This is useful for quickly looking at results, especially when MCMCs have not yet been run and so the assessment document will not build yet.

