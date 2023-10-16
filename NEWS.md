## hake assessment repository news

## Complete rewrite of code in 2023

The following list contains the details of all the changes that were
implemented in the hake assessment codebase in 2023 between the 2023 and
2024 assessment seasons.

* The document is built using [Bookdown](https://bookdown.org/), an R package
  that facilitates writing complex documents using
  [R Markdown](https://rmarkdown.rstudio.com/). See the [Rmarkdown reference guide](https://www.rstudio.com/wp-content/uploads/2015/03/rmarkdown-reference.pdf)
  for help on syntax. The R Markdown is converted to a PDF with [Pandoc](https://pandoc.org/),
  but this is a part of the `bookdown` package and invisible for the most
  part to end users.
* Converted ALL document code from Sweave to Rmarkdown.
* Now a true R package. This compartmentalized the code, and allows for the
  documentation of functions and package data to be monitored through
  software tools.
* The `bookdown` package uses the project-specific `_bookdown.yml` file which
  contains the names of all files that are to be included in the build.
  See the file here: [_bookdown.yml](https://github.com/pacific-hake/hake-assessment/blob/package-dev/doc/_bookdown.yml).
  The layout of this file allows for extremely easy commenting-out of sections
  while you are working on a particular section and need to build just that
  section.
* Uses a post-processor to inject tedious LaTeX code after the building
  of the TeX file is complete with `bookdown`. This means there is almost no
  LaTeX code found in the Rmarkdown code files. It make it easier to read
  and write the actual document text. This post-processing step is invisible
  to the authors when building the document (no extra steps required).
* Standardized all code filenames by category and one function per file.
* `r4ss` package now only used for loading of outputs, not for any figures
  or tables.
* All document text rewritten to conform to Rmarkdown standards. This means
  re-writing all inline R code into Rmarkdown format (Global replacement of
  `\Sexpr{r_code_here}` with `` `r r_code_here` ``). Also, all lines of text
  are standardized to less than 80 columns long and there is a line break
  between every line. Some equation lines and URL lines that could not
  be broken remain greater than 80 columns long.
* The Table of Contents has been improved with uniform spacing and numbering,
  and with the Appendix section having the same format as the main section.
  There is a post-processing function
  ([post_process_table_of_contents](https://github.com/pacific-hake/hake-assessment/blob/package-dev/R/post-process-table-of-contents.R))
  which allows the TOC to be modified very easily. There is a line in
  `000-launcher.rmd` that reads
  `TABLE OF CONTENTS GOES HERE`. This is a required tag for the post-processor
  which will inject the table of contents code there.
* The Executive Summary requires a tag (`Executive summary EOF`) at the end
  of it to signal the post-processor to restart numbering of tables and
  figures and to switch from letters to numbers. See
  [Executive summary tag](https://github.com/pacific-hake/hake-assessment/blob/87af60178be68153dc27728935026160fd17a3b7/doc/006-executive-summary.rmd#L832).
* All data tables pre-loaded into global package variables to shorten build
  time and aggregate loading code.
* Tables
  - All tables have been standardized to return `knitr::kbl()` objects.
    Every table function was rewritten from scratch.
  - To render any table in landscape orientation in the PDF file, pipe
    the output to the `landscape()` function. See this
    [landscape table example](https://github.com/pacific-hake/hake-assessment/blob/05247ca8fa97e98e84d42db10fd2a45fd4fbfec2/doc/013-tables.rmd#L55).
    `landscape()` is a function from the `kableExtra` package.
  - Many tables modified so they are easier to make accessibility-compliant.
    This involved making one-row headers instead of two or more.
  - Decision tables now have the same format as the rest of the tables in the
    document. There is also more room in them so there are more complete
    descriptions in the left column. These tables will be easier to make
    accessibility-compliant.
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
    feel. These can be found in [`data-raw/plot-settings.R`](https://github.com/pacific-hake/hake-assessment/blob/package-dev/data-raw/plot-settings.R). For example, points on all single time-series plots in the document
    (spawning biomass, recruitment, recruitment deviations, etc.) can be
    changed to a different type and size by modifying the package data
    variables `ts_pointshape` and `ts_pointsize` found in the file
    `data-raw/plot-settings.R`.
  - In the catches bar plot showing catch data by year, moved the larger US
    catch to the bottom, with smaller catch on top of the bars. It is much
    easier to see the small catches when they are on top.
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
  decision tables. The file is found at
  [doc/forecast-descriptions.csv](https://github.com/pacific-hake/hake-assessment/blob/package-dev/doc/forecast-descriptions.csv).
* Many figure settings have been standardized across all figures. These
  can be found in [data-raw/plot-settings.R](https://github.com/pacific-hake/hake-assessment/blob/package-dev/data-raw/plot-settings.R).
  If these are be changed, follow the same procedure as laid out in the
  [Adding new data to data tables](https://github.com/pacific-hake/hake-assessment/blob/079becf0680c53301f4984edbfa2f053606ce9d8/README.md?plain=1#L90) section of the README.md file, but using `data-raw/plot-settings.R` instead.
* Some document settings have been made into package data. See the file
  [data-raw/document-settings.R](https://github.com/pacific-hake/hake-assessment/blob/package-dev/data-raw/document-settings.R)
  and change the same way as explained in the
  [Adding new data to data tables](https://github.com/pacific-hake/hake-assessment/blob/079becf0680c53301f4984edbfa2f053606ce9d8/README.md?plain=1#L90)
   section of the README.md file.
* Some SS filename settings have been made into package data. See the file
  [data-raw/ss-filenames.R](https://github.com/pacific-hake/hake-assessment/blob/package-dev/data-raw/ss-filenames.R)
  and change the same way as explained in the the
  [Adding new data to data tables](https://github.com/pacific-hake/hake-assessment/blob/079becf0680c53301f4984edbfa2f053606ce9d8/README.md?plain=1#L90)
   section of the README.md file.
* The key and nuisance posteriors have been made into package data. See the
  file [data-raw/key-posteriors.R](https://github.com/pacific-hake/hake-assessment/blob/package-dev/data-raw/key-posteriors.R)
  and change the same way as explained in the
  [Adding new data to data tables](https://github.com/pacific-hake/hake-assessment/blob/079becf0680c53301f4984edbfa2f053606ce9d8/README.md?plain=1#L90)
  section of the README.md file.
* The way in which web accessibility including the alternative text
  injection is done has been changed, it no longer uses knitr hooks which
  means a full build from scratch is not required to inject these features.
  Also, an external file is no longer used to keep track of these captions.
  In addition, inline R chunks can be used in the alternative text
  definitions. These are not parsed by knitr but by a function in the post
  processor.
* All landscape pages have header and footer lines and text, so they match
  their portrait counterparts.
* All old, unused functions and other code has been removed.

## Details of the `render()` function

The `render()` function performs two-steps internally:

1. Calls the `bookdown::render_book()` function to generate the PDF.
1. Runs the post processing step (`post_process()`) on the LaTeX file
   before creating the PDF which:
  - Inserts the Table of Contents
  - Moves any figures or tables around that need to be (Adds latex
    position variables such as [H], [bt] [!H]). The setup file for this
    is `doc/object-placement.csv`
  - Customizes longtables so they have "continued on next page..." and
    "Continued from previous page..." on them
  - Processes landscape figures and tables
  - Aligns table captions to the left margin if they need it
  - Adds figure and table numbers
  - Removes vertical space before section headings
  - Adds the web-accessibility features, including the alternative text
