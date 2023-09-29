____
# hake-assessment

*An R package which uses Bookdown and Rmarkdown to build the US/Canadian Pacific hake assessment document*
_____________________________________________________________

Read NEWS.md for important details on the major code rewrite that took place
in 2023.

---
## How to create the hake assessment PDF document
**The `RDS` files must have been created before the document can be built.**

* Load the hake package by running `devtools::load_all(".")` while in the
  hake package working directory.

* Create the `hake.tex` file by running `render()`. This will create the
  `hake.tex` file along with the `hake.pdf` file. ***The PDF file created
  in this step is not formatted correctly, and the following step is required
  to rebuild it using `lualatex`***.

* The `hake.tex` file must now be run through LaTeX, using `lualatex` to
  give us the final document. Go to an Operating System terminal window
  and run the following in the `doc` directory (where the `hake.tex` file is
  located). Note that `lualatex` is run twice. This is to ensure all references
  are set correctly. If you run it only once you will find many question marks
  in the document for figure and table references.
  - `lualatex hake.tex; lualatex hake.tex`

The file `hake.pdf` will contain the final document.

## Debugging a figure or table, or anything else

* If you haven't already done so in your current R session, run
  `devtools::load_all(".")` while in the hake package working directory.

* **If you are using Rstudio:**
  - Copy the chunk or chunks of Rmarkdown code you want to test to the
    clipboard.
  - Run `gotest()`, which will create a temporary directory containing all
    the files necessary to run a pared-down version of the document, and
    switch you to that directory. If your repository directory is not the
    default (`~/github/pacific-hake/hake`), you will have to include the
    `repo_dr` argument in the call to `gotest()`.
  - Click the gear-arrow-down icon ![](gear-arrow-down.png) in the Files
    window (bottom right panel in Rstudio) and select
    `Go to working directory`. This will take the Rstudio file manager to
    the temporary directory, and show you the files that have been copied
    there by the `gotest()` function.
  - Open the `005-text.rmd` file, delete everything in that file if it
    contains anything, and paste your chunk(s) of code, or create a new
    Rmarkdown chunk. Save the file.
  - In the R terminal, build the document using `render()`. The PDF will
    be built in the current temporary directory, and contain only your test
    figure(s) or table(s).
  - Make changes to your code in the temporary `005-text.rmd` file, and
    when satisfied with your code, copy the code to the clipboard for
    pasting into the real document.

* **If you are not using Rstudio:**
  - Run `gotest(copy_tmpdir = TRUE)`, which will create a temporary
    directory containing all the files necessary to run a pared-down
    version of the document, switch you to that directory, and copy the name
    of the directory to the clipboard. If your repository directory is not
    the default (`~/github/pacific-hake/hake`), you will have to also
    include the `repo_dr` argument in the call to `gotest()`.
  - Go to the temporary directory using a program (file manager) of your
    choice, pasting the directory name from the clipboard into it so you
    can get there without having to remember the (ugly) name.
  - Copy the chunk(s) of Rmarkdown code you want to test to the clipboard
    from the original file.
  - Open the `005-text.rmd` file from that directory, delete everything in
    it if it contains anything, and paste your chunk(s) of code, or create
    a new Rmarkdown chunk. Save the file.
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

## Adding new data to data tables <a name="dt"></a>

This is a bit different than it was previously, because the data tables
are now package data and have to be built in a different way to update the
package data.

1. Open the CSV file you want to add data to in the `data-tables` directory.
1. Add the new data row, and save the file.
1. Do the first two steps with as many data tables as you want to update, then
   do then next step once only.
1. Source the `data-raw/data-tables.R` file to update the data tables:
   `source(here::here(data-raw/data-tables.R))`
1. Make sure to include the changes to the RDA files in the GitHub repo by
   committing the changes in Git. This will be obvious as there will be several
   dozen RDA files changed. Commit all of the changes.
   
Try not to do steps 4 and 5 for every change you make, rather make as many
changes as you can to data tables at one time, then run steps 4 and 5 once to
incorporate all the changes (they are binary and its better to keep binary
file changes to a minimum).

## 2024 Assessment cycle (Jan - Mar 2024)

* Model runs were done on an Ubuntu 22.04 LTS server with 80 Xeon Gold CPUs
  and 405 GB of RAM.

* All model runs, including the base, bridging, sensitivities, and
  retrospectives, were done using the **main** branch of the
  [ADNUTS](https://github.com/cgrandin/adnuts) MCMC algorithm, which is
  a Fork.
  
* `extra-mcmc must be and was enabled for ALL models`

## Server setup for 2024

* Operating system: Ubuntu 22.04 LTS (jammy)

* R version: 4.3.1 (2023-06-16 "Beagle Scouts")

* TexLive version: 2023 (tlmgr revision 66457 (2023-03-08 00:07:12 +0100))

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
  you don't want to run (comment character is #).
    
* Some chunks delete files. This is to save space.
  [This chunk](https://github.com/pacific-hake/hake-assessment/blob/356f1a069ddc1f806f0c151d6b15e59e2efe92ec/bash-scripts/run-base-model.sh#L50)
  for example, deletes all output files in the `forecasts` directory except
  those necessary to run the forecasts.
    
## Forecasts for the base model

* Either leave the forecasting chunks in `run-base-model.sh` uncommented or run
  `run-base-forecasts.sh` after the base model has already been run. Note that
  you need to edit this file also before running it, as it has the year,
  etc. in it.
  
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

  - `generic-run-models.sh` contains the **year_path** variable that needs to
    be changed each year.
    
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

