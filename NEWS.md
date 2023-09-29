## hake assessment repository news

## Complete rewrite of code in 2023

The following list contains the details of all the changes that were
implemented in the hake assessment codebase in 2023 between the 2023 and
2024 assessment seasons.

* Now a true R package. This compartmentalized the code, and allows for the
  documentation of functions and package data to be monitored through
  software tools.

* Converted all document code from Sweave with embedded LaTeX to Rmarkdown.
  See the [Rmarkdown reference guide](https://www.rstudio.com/wp-content/uploads/2015/03/rmarkdown-reference.pdf)
  for help on this syntax.

* Uses the `bookdown` package to create the assessment document. This brings
  with it the _bookdown.yml file which makes it much easier to see which
  files are to be included in the build. See the file here:
  [_bookdown.yml](https://github.com/pacific-hake/hake-assessment/blob/package-dev/doc/_bookdown.yml).

* Uses a post-processor to inject tedious LaTeX code after the building
  of the TeX file is complete with `bookdown`. This means there is almost no
  LaTeX code found in the Rmarkdown code files. It make it easier to read
  and write the actual document text.

* Standardized all code filenames by category and one function per file.

* `r4ss` package now only used for loading of outputs, not for any figures
  or tables.

* All document text rewritten to conform to Rmarkdown standards. This means
  re-writing all inline R code into Rmarkdown format (Global replacement of
  `\Sexpr{r_code_here}` with `` `r r_code_here` ``). Also, all lines of text
  are standardized to less than 80 columns long and there is a line break
  between every line. Some equation lines and URL lines that could not
  be broken remain greater than 80 columns long.

* Table of Contents has been improved with uniform spacing and numbering,
  and with the Appendix section appearing the same as the main section.
  There is a post-processing function where the TOC can be modified very
  easily.

* Executive Summary needs a tag at the end of it to signal the post-processor
  to restart numbering of tables and figures and to switch from letters to
  numbers. This tag is `Executive summary EOF`. See
  [Executive summary tag](https://github.com/pacific-hake/hake-assessment/blob/87af60178be68153dc27728935026160fd17a3b7/doc/006-executive-summary.rmd#L832).

* Tables
  - All tables have been standardized to return `knitr::kbl()` objects.
    Every table function was rewritten from scratch.
  - To render any table in landscape orientation in the PDF file, pipe
    the output to the `landscape()` function. See this
    [landscape table example](https://github.com/pacific-hake/hake-assessment/blob/05247ca8fa97e98e84d42db10fd2a45fd4fbfec2/doc/013-tables.rmd#L55).
    `landscape()` is a function from the `kableExtra` package.
    
* Figures
  - All figures have been standardized to return `ggplot2::ggplot()` objects.
    Every plot function was rewritten from scratch.
  - The method to render a figure in landscape orientation in the PDF file
    is different than that for a table. You must place `BEGIN LANDSCAPE` and
    `END LANDSCAPE`  tags around the figure code a blank line between them
    and the figure code. It will work with either embedded graphic files or
    `knitr` chunks that create plots. See this
    [landscape figure example](https://github.com/pacific-hake/hake-assessment/blob/87af60178be68153dc27728935026160fd17a3b7/doc/014-figures.rmd#L32-L36.).
    The post-processor injects the appropriate LaTeX wrappers around the
    figure code.
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

## Details of the `render()` function

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
