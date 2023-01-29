____
# hake-assessment

*A framework which uses latex and knitr code to build the US/Canadian Pacific hake assessment.*
_____________________________________________________________

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

* The base model is special and has it's own bash script. It is
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
## How to create hake-assessment.pdf
**The `RDS` files must have been created before the document can be built.**

### Method 1

* Run the following in an R session:
```R
source(here::here("R/all.R"))
build_doc()
```
  
* After the first time you do this, the models will be loaded
  into the R workspace and any subsequent builds will be a little faster
  
* This method has the benefit of one-click type build but if there is
  an error on the **LaTeX** part it can hang up your R session

### Method 2 (best when working on figures)

* Change to the `doc` subdirectory in the repository and run this in R:
```R
knitr::knit("hake-assessment.rnw")
```
To add alternative text (after running the above without a cache):
```R
add_alt_text(alt_fig_text = alt_fig_text)
```

Then in a bash terminal:
```
lualatex hake-assessment # To create the PDF
lualatex hake-assessment  # If you notice ?? references in the PDF
bibtex hake-assessment # To link the bibliography references
```

## How to work on figures created in the knitr chunks:

* Use `knitr::knit("hake-assessment.rnw")` from within the `doc` directory
  to knit the document. Once it has been knit, the knitr cache will contain
  PNG files with the knitr chunk name as the file name, one for each figure
  
  - Open the figure and make it fullscreen so you can see details. Assuming
    it still needs work:
  
  - Go back the the function call inside the knitr chunk and change that
    code however you need to to fix the problem. This will be very fast
    
  - Knit the document again with `knitr::knit("hake-assessment.rnw")` and
    switch back to the open image file and see your change happen to the figure
    
  - Iterate these steps until your figure looks how you want it
  
  - If you don't change code in the chunk itself, but code in the function
    that is being called in the chunk, you have to delete all the files in the
    `knitr-cache` that have the name of the chunk in it. If you don't, the
    figure won't be rebuilt and you will see no changes
  
* This method has the benefit of you seeing exactly what the figure will
  look like in the document. If you just call the function code in R,
  you do not get an accurate depiction of the figure and will have to redo
  it again later.
  
* It is really fast, assuming you leave all the other files alone in the 
  `knitr-cache`. If you delete the `knitr-cache` or unrelated files within
  it, this method will become very slow.
    
* In **Microsoft Windows** you may not be able to leave the figure viewer
  open when changing the file

---
# To take a quick look at model output without making an RDS file

Open R within the model's folder and use the command:

```
r4ss::SS_plots(SS_output("./"))
```

This creates figures and an HTML page with tabs for sets of figures. This is useful for quickly looking at results, especially when MCMCs have not yet been run and so the assessment document will not build yet.

