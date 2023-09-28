<!-- This HTTML tag ensures all web links will open in a new tab when clicked -->
<base target="_blank">

____
# hake-assessment

*An R package which uses Bookdown and Rmarkdown to build the US/Canadian Pacific hake assessment document*
_____________________________________________________________

## 2023/2024 Update - Complete rewrite of code

The following list contains the details of all the changes that were
implemented in the hake assessment codebase between the 2023 and 2024
assessment seasons.

* Now a true R package. This compartmentalized the code, and allows for the
  documentation of functions and package data to be monitored through
  software tools.

* Converted all document code from Sweave with embedded LaTeX to Rmarkdown.
  See the [Rmarkdown reference guide](https://www.rstudio.com/wp-content/uploads/2015/03/rmarkdown-reference.pdf)
  for help on this syntax.

* Uses the `bookdown` package to create the assessment document. This brings
  with it the _bookdown.yml file which makes it much easier to see which
  files are to be included in the build. See the file here: [_bookdown.yml](https://github.com/pacific-hake/hake-assessment/blob/package-dev/doc/_bookdown.yml).

* Uses a post-processor to inject tedious LaTeX code after the building
  of the TeX file is complete with `bookdown`. This means there is almost no
  LaTeX code found in the Rmarkdown code files. It make it easier to read
  and write the actual document text.

* Standardized all code filenames by category and one function per file.

* `r4ss` package now only used for loading of outputs, not for any figures
  or tables.

* All document text rewritten to conform to Rmarkdown standards. This means
  re-writing all inline R code into Rmarkdown format (Global replacement of
  `\Sexpr{r_code_here}` with `` `r r_code_here` ``)
* Table of Contents has been improved with uniform spacing and numbering,
  and with the Appendix section appearing the same as the main section.
  There is a post-processing function where the TOC can be modified very
  easily.

* Executive Summary needs a tag at the end of it to signal the post-processor
  to restart numbering of tables and figures and to switch from letters to
  numbers. This tag is `Executive summary EOF`. See
  [Executive summary tag](https://github.com/pacific-hake/hake-assessment/blob/87af60178be68153dc27728935026160fd17a3b7/doc/006-executive-summary.rmd#L832).
* Tables
  - All tables standardized to return `knitr::kbl()` objects.
  - To make any table landscape, just pipe the output to the `landscape()`
    function. See this [landscape table example](https://github.com/pacific-hake/hake-assessment/blob/05247ca8fa97e98e84d42db10fd2a45fd4fbfec2/doc/013-tables.rmd#L55). The post-processing step injects the
    LaTeX landscape tags around the table code.
    
* Figures
  - All figures standardized to return `ggplot2::ggplot()` objects.
  - The method to make a figure landscape is different than that for a table.
    You must place `BEGIN LANDSCAPE` and `END LANDSCAPE`  tags around the
    figure code a blank line between them and the figure code.
    See this [landscape figure example](https://github.com/pacific-hake/hake-assessment/blob/87af60178be68153dc27728935026160fd17a3b7/doc/014-figures.rmd#L32-L36.)
  - Utilizes package data variables so all figures have the same look and
    feel. These can be found in [`data-raw/plot-settings.R`](https://github.com/pacific-hake/hake-assessment/blob/package-dev/data-raw/plot-settings.R).

* All data tables pre-loaded into global package variables to shorten build
  time and aggregate loading code.
* Decision tables now have the same format as the rest of the tables in the
  document. There is also more room in them so there are more complete
  descriptions in the left column.
* Use of Rmarkdown means that no special LaTeX markup needs to be used any
  longer. For example:
  - The degree symbol used to need a LaTeX variable, escaped in code like
    this `/degree/` or this `/degree` depending if there was to be a space
    after it or not. In Rmarkdown we just embed the degree symbol `°`.
  - The fancy backward and forward quotes that come before and after quoted
    text are simpler. With LaTeX we had to write quotes like this:
    `` `some quote' `` or ```` ``some quote''````. Now we can just write
    `'some quote'` or `"some quote"` to get the same result.
  - Backslashes before special characters are no longer necessary. Previously
    we would have to write something like `the 97.5\% quantile`. Now we would
    simply write `the 97.5% quantile`. This has always been a thorn in our
    side as forgetting one backslash broke the whole build, with no easy way
    to find what was causing it to break.
* All weights in the document text, figures, and tables are in metric (kt, Mt)
  instead of thousands of tonnes, '000 tonnes, x 1,000 tonnes, millions of
  tonnes and any other phrases that were present throughout previously.
* Forecast descriptions have been placed into a CSV file so that we can more
  easily find and edit them. These are the values that will appear in the
  decision tables. The file is found at `doc/forecast-descriptions.csv`
* Many figure settings have been standardized across all figures. These
  can be found in `data-raw/plot-settings.R` If these are be changed,
  follow the same procedure as laid out in the
  [Adding new data to data tables](#dt) section below, but using the
  `data-raw/plot-settings.R` instead.
* Some document settings have been made into package data. See the file
  `data-raw/document-settings.R` and change the same way as explained in the
  point above.
* Some SS filename settings have been made into package data. See the file
  `data-raw/ss-filenames.R` and change the same way as explained in the
  points above.
* The key and nuisance posteriors have been made into package data. See the
  file `data-raw/key-posteriors.R` and change the same way as explained in the
  points above.
* All old, unused functions and other code has been removed.
  
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

