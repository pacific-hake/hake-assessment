____
# hake-assessment  <img src="doc/image-files/hake-logos.png" align="right" height="139" />

>An R package which uses Bookdown and Rmarkdown to build the US/Canadian
>Pacific hake assessment document

![GitHub commit activity (branch)](https://img.shields.io/github/commit-activity/y/pacific-hake/hake-assessment)
![GitHub last commit (by committer)](https://img.shields.io/github/last-commit/pacific-hake/hake-assessment)
![GitHub contributors](https://img.shields.io/github/contributors/pacific-hake/hake-assessment)
![Codecov](https://img.shields.io/codecov/c/github/pacific-hake/hake-assessment)

>Quicklinks (press alt-left arrow to come back here):  
>- [How to create the hake assessment PDF document](#create-doc)  
>- [Debugging chunks of code and Rmarkdown text](#debugging)  
>- [Adding new data to data tables](#add-new-data)  
>- [Reference point text markup](#settings-ref-pt)  
>- [Plot settings](#settings-plot)  
>- [2024 Assessment cycle (Jan - Mar 2024)](#assessment-cycle)  
>- [Server setup for 2024](#server-setup)  
>- [How the models are run](#how-to-run-models)  
>- [Base model bash script](#base-model-bash)  
>- [Forecasts for the base model](#forecasts)  
>- [Retrospectives for base model](#retrospectives)  
>- [Other models bash scripts](#bash-scripts-run)  
>- [Other bash scripts](#bash-scripts-other)  
>- [To take a quick look at model output without making an RDS file](#quick-look)  
_____________________________________________________________

In 2023, the project code used for building the assessment document and all the
presentations was switched over from the original Sweave-based approach to the
[Bookdown](https://bookdown.org/) system. Read
[NEWS.md](https://github.com/pacific-hake/hake-assessment/blob/package-dev/NEWS.md)
for important details on this.

The assessment document is built using the following software packages:
* [R Markdown](https://rmarkdown.rstudio.com/lesson-1.html): A simple yet
  powerful markup language designed to remove the need for tedious LaTeX
  macros embedded throughout the text. See the [Rmarkdown reference guide](https://www.rstudio.com/wp-content/uploads/2015/03/rmarkdown-reference.pdf)
  for help on syntax.
* [knitr](https://yihui.org/knitr/): This R package is responsible for creating
  figures and tables during builds, and for dealing with inline R code chunks
  to create reproducible paragraphs of text. `knitr` chunks are written in a
  special `R Markdown` format.
* [Pandoc](https://pandoc.org/): A very powerful document converter which in
  this case converts the R Markdown to LaTeX code.
* [LuaTeX](https://www.luatex.org/): The LaTeX compiler which converts the
  LaTeX created by `Pandoc` into the final PDF document.
* [YAML](https://yaml.org/spec/1.2.2/#chapter-1-introduction-to-yaml): This 
  is a simple configuration file format used to keep the project in order.
  It is used by the `bookdown` package.
* [bookdown](https://bookdown.org/): An R package that facilitates writing
  complex documents by integrating all of the above packages into a single
  package, using a YAML configuration file.

---
<a name="create-doc"></a>
## How to create the hake assessment PDF document
>**The same method works for creating the beamer presentation PDFs**  
>The `RDS` files must have been created before the document can be built

1. Install the hake package:
   - If you are on the Linux server, the `hake` package is already installed,
     go to step 2.
   - If you are on a local machine, open an R session, and install the hake
     package from GitHub:
     `remotes::install_github("pacific-hake/hake-assessment")`
1. Clone this GitHub repository:
   `git clone https://github.com/pacific-hake/hake-assessment`
1. If using Rstudio, open `hake.Rproj`. If not, open an R console however you
   like to and navigate to the hake project root directory. Either way, change
   your working directory to the `doc` directory (*for beamer presentations
   change your working directory to the one containing all the RMD files*):  
   `setwd(here::here("doc"))`
1. Create the PDF document (*same command for beamer presentations*):  
   `hake::render()`
1. The `hake.pdf` document will be rendered in the `doc` directory. The first
   time rendering the document in a new R session will take about 7-8 minutes
   because the model RDS files have to be loaded. After that, the build will
   be much quicker.
1. Clean the directory up from time to time by running `hake::clean()`. This
   will remove all files and subdirectories created by the `render()`
   function. This command also works for the beamer presentation directories.
   If you get an error stating that the variable `assess_yr` could not be
   found, run `clean()`.

Alternatively, in RStudio you can click the `knit` button while the file
`000-launcher.rmd` is open in the editor window. This will be much slower
though because it starts its own new R session, which means all the models
have to be loaded every time you render the document this way.

For details on the `hake::render()` function, see
[NEWS.md](https://github.com/pacific-hake/hake-assessment/blob/package-dev/NEWS.md).

<a name="debugging"></a>
## Debugging chunks of code and Rmarkdown text

Run `gotest()` to enter a customized debug directory, paste the code in
and run `render()`. When done, run `goback()` to go back to the directory you
were in originally. This works for beamer presentations as well.

**Details:**  
* If you haven't already done so in your current R session, run
  `devtools::load_all()` while your working directory is somewhere within
   the hake repo directory structure.
* Run `gotest()`, which will create a temporary directory containing 
  copies of all files needed to do a minimal document build, and
  switch you to that directory.
  - If in RStudio, click the gear-arrow-down icon ![](gear-arrow-down.png) in
    the Files window and select `Go to working directory`. This will take
    the Rstudio file manager to the temporary directory, and show you
    the files that have been copied there by the `gotest()` function.
  - If not in RStudio, type `dirclip()`, which will copy the temporary
    directory name to the clipboard. You can now go to a file manager of
    your choice and paste the directory name into it, and it will take you
    to the temporary working directory.
* Open the `005-test.rmd` file, and paste your chunk(s) of code into it.
  Save the file.
* Build the document by running `render()`. The PDF (`hake.pdf`) will be
  built in the current temporary directory, and contain only the output
  from your test code. If you haven't built the document yet in the current
  R session, this will take 7-8 minutes because all the mode files have
  to be loaded.
* Iteratively make changes to your code in the temporary `005-test.rmd`
  file and build the document, until satisfied with your code. Copy the
  code to the clipboard for pasting into the real document. Be careful,
  once you leave the temporary directory your code in `005-test.rmd` will be
  gone and unrecoverable.
* To go back to the directory you were in before testing, run `goback()`.
  - If in Rstudio, click the gear-down-arrow icon ![](gear-arrow-down.png) in
    the Files window (bottom right panel in Rstudio) and select
    `Go to working directory`.

<a name="add-new-data"></a>
## Adding new data to data tables

Data tables are package data and can be accessed directly from within
the package like in this example, which gives you the U.S. at-sea bottom
depth table:  
`hake::us_atsea_bottom_depth_df`

To see a list of all package data available in the `hake` package:  
`data(package = "hake")`

To update any package data, for example if we add a new rows to the CSV files
found in the `data-tables` directory:

1. Open the CSV file from the `data-tables` directory that you want to add
   data to.
1. Add the new data row(s), and save the file.
1. Do the first two steps with as many data tables as you want to update, then
   do then next steps as few times as possible.
1. Source the `data-raw/data-tables.R` file to update the data tables:
   `source(here::here(data-raw/data-tables.R))`. If you're using RStudio
   you can just press `Ctrl-Shift-Enter` with the file in focus to do this.
   This will update the `*.rda` files which are the binary package data files.
1. Make sure to include the changes to the `*.rda` files in the GitHub repo by
   committing those files in Git. This will be obvious as there will be
   several dozen `*.rda` files changed when you run `git status`. Commit all
   of the changes.
   
Try not to do steps 4 and 5 for every change you make, rather make as many
changes as you can to data tables at one time, then run steps 4 and 5 once to
incorporate all the changes. The `*.rda` files are binary and its better to
keep binary file changes to a minimum when using Git as it can introduce
repository bloating.

<a name="settings-ref-pt"></a>
## Reference point text markup

The reference points and other values which are referred to in text in
numerous places and require complex markup and/or latex are located in the
file `data-raw/reference-points.R`. They are stored as package data so can be
referred to like this (example for *F*<sub>SPR=40%</sub>):  
- `hake::fspr_40_10` for Rmarkdown (inside text in the document) or
- `hake::fspr_40_10_for_latex_table` for tables (which require LaTeX code)

To add to this list or change anything, follow the same method as laid out in
the `Adding new data to data tables (done annually)` section above. Test the
new expression by using the `gotest()/goback()` debugging method.

<a name="settings-plot"></a>
## Plot settings

There are many standardized project-wide plot settings which are also
package data. These can be found in the file `data-raw/plot-settings.R`.
If any are modified or new ones are added, follow the same method as laid
out in the `Adding new data to data tables (done annually)` section above.
For example, if you wanted to change the cohort diagonal line color in all
age bubble plots from dark green to red you would find this line of code:  

File: `data-raw/plot-settings.R`  
Line: `create_data_hake("age_diag_linecolor", "darkgreen")`  

and change darkgreen to red. Then source the file and reload the package
using:  
`devtools::load_all()`

<a name="assessment-cycle"></a>
## 2024 Assessment cycle (Jan - Mar 2024)

* Model runs were done on an Ubuntu 22.04 LTS server with 80 Xeon Gold CPUs
  and 404 GB of RAM.

* All model runs, including the base, bridging, sensitivities, and
  retrospectives, were done using the **main** branch of the
  [ADNUTS](https://github.com/cgrandin/adnuts) MCMC algorithm.
  
* `extra-mcmc must be and was enabled for ALL models`

<a name="server-setup"></a>
## Server setup for 2024

* Operating system: Ubuntu 22.04 LTS (Jammy Jellyfish)

* R version: 4.3.1 (2023-06-16 "Beagle Scouts")

* TexLive version: 2023 (tlmgr revision 66457 (2023-03-08 00:07:12 +0100))

* The R packages listed
  [here](https://github.com/pacific-hake/hake-assessment/blob/356f1a069ddc1f806f0c151d6b15e59e2efe92ec/R/all.R#L20)

* The TEX packages listed
  [here](https://github.com/pacific-hake/hake-assessment/blob/356f1a069ddc1f806f0c151d6b15e59e2efe92ec/docker/install_packages.R#L21)

---
<a name="how-to-run-models"></a>
# How the models are run

* There are bash scripts which launch R functions. The scripts are used to
  allow OS-level control over the parallelism, which allows us to view
  the processes running for each and every model, in addition it is faster than
  using an R package to distribute model runs which are calling `system()` to
  run each model
  
* The bash scripts are located
  [here](https://github.com/pacific-hake/hake-assessment/tree/master/bash-scripts)

<a name="base-model-bash"></a>
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
    
<a name="forecasts"></a>
## Forecasts for the base model

* Either leave the forecasting chunks in `run-base-model.sh` uncommented or run
  `run-base-forecasts.sh` after the base model has already been run. Note that
  you need to edit this file also before running it, as it has the year,
  etc. in it.
  
<a name="retrospectives"></a>
## Retrospectives for base model

* To run these, first run the base model, then edit `run-retrospectives.sh`. The
  `year_path` will have to be changed each year and also `models_path` if you
  are running on a different machine than the server. See the section
  on running the base model for directions on this variable

<a name="bash-scripts-run"></a>
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
    
<a name="bash-scripts-other"></a>
## Other bash scripts

* The `create-` scripts are for creating RDS files (if you need or want to do
  this manually after running the models). The `run-` scripts will create the
  RDS files automatically though so you may never need these scripts
  
* The `create-sensitivity-dirs.sh` runs code that calls an R function to create
  the standard set of sensitivities for hake, and insert the files into the
  correct directory structure. This needs to be run every year

---
<a name="quick-look"></a>
# To take a quick look at model output without making an RDS file

Open R within the model's folder and use the command:

```
r4ss::SS_plots(SS_output("./"))
```

This creates figures and an HTML page with tabs for sets of figures. This is useful for quickly looking at results, especially when MCMCs have not yet been run and so the assessment document will not build yet.

