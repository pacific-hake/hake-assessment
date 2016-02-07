____
# hake-assessment

hake-assessment is latex and knitr code used to build a PDF of the US/Canadian Hake assessment. The repository was set
up for the first time on Jan 4, 2016.

_____________________________________________________________


## Prerequisites
* MikTex for Windows - the first time you try to run, many packages will be installed automatically.
  This takes some time so make sure you have a fast connection.
* R (version 3.22 "Fire Safety" or later)
* R Packages 'knitr', 'xtable', 'r4ss', nwfscSurvey, nwfscMapping, data and their dependencies.
* Rscript.exe must be on your PATH.

---
## How to run the code and create hake-assessment.pdf

- You **MUST** load the R environment first. To do this, navigate to the doc/r directory,
  start RGui in that directory, and type **source("all.r")**. If it is the first time or you
  are running this, you must answer *y* to the three questions. Once this is finished, do a save.image()
  to sdave the .RData file in the doc/r directory.

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
    models[[2]] - Second directory found, typically of format like: 01_This_years_model
    ...
    models[[N]] - The Nth directory found; each unique member of this models list contains
      models[[N]]$          - All the objects as read in by the SS_output function in the r4ss package
      models[[N]]$retros    - A list of MLE retrospective outputs from SS_output
      models[[N]]$retros[[1]] - Model run with one year removed
      models[[N]]$retros[[2]] - Model run with two years removed
      ...
      models[[N]]$retros[[N]] - Model run with N years removed (depends on user input when sourcing all.r)
      models[[N]]$forecasts - A list of forecasts from the mcmc run of the model (for decision tables)
        models[[N]]$forecasts$outputs   - List of mcmc outputs from the forecast models as read in by the SSgetMCMC function
        models[[N]]$forecasts$mcmccalcs - Calculations done on the mcmc outputs for this forecast model. Same structure as below.
        models[[N]]$forecasts$biomass   - Forecasts for biomass. The rows are labelled by forecast year.
        models[[N]]$forecasts$spr       - Forecasts for SPR. The rows are labelled by forecast year.
      models[[N]]$risks     - The risk calculations for the sxecutive summary decision table (e.g. P(B2016<B2015))
        models[[N]]$risks[[1]] - Holds the risk values for the second year of forecasts - the first year
        models[[N]]$risks[[2]] - Holds the risk values for the third year of forecasts - the second year
        ...
        models[[N]]$risks[[N]] - Holds the risk values for the N+1th year of forecasts - the Nth year
      models[[N]]$path      - The path where this model is located
      models[[N]]$ctl.file  - control file name for this model
      models[[N]]$dat.file  - data file name for this model
      models[[N]]$dat       - data file as read in by the SS_readdat function in the r4ss package
      models[[N]]$mcmc      - mcmc output from the model as read in by the SSgetMCMC function or NULL if none for this model
      models[[N]]$mcmckey   - contents of the mcmc keyposteriors.csv file
      models[[N]]$mcmcnuc   - contents of the mcmc nuisanceposteriors.csv file
      models[[N]]$mcmcpath  - The path where this mcmc model is located
      models[[N]]$mcmccalcs - calculations done on the mcmc outputs for this model
        models[[N]]$mcmccalcs$svirg     - SPB virgin biomass, vector of length 3 (2.5%, 50%, 97.5%)
        models[[N]]$mcmccalcs$sinit     - SPB initial biomass, vector of length 3 (2.5%, 50%, 97.5%)
        models[[N]]$mcmccalcs$slower    - SPB lower confidence (2.5%)
        models[[N]]$mcmccalcs$smed      - SPB median (50%)
        models[[N]]$mcmccalcs$supper    - SPB upper confidence (97.5%)
        models[[N]]$mcmccalcs$dlower    - Depletion lower confidence (2.5%)
        models[[N]]$mcmccalcs$dmed      - Depletion median (50%)
        models[[N]]$mcmccalcs$dupper    - Depletion upper confidence (97.5%)
        models[[N]]$mcmccalcs$rvirg     - Virgin recruitment, vector of length 3 (2.5%, 50%, 97.5%)
        models[[N]]$mcmccalcs$rinit     - Initial recruitment, vector of length 3 (2.5%, 50%, 97.5%)
        models[[N]]$mcmccalcs$runfished - Unfished recruitment, vector of length 3 (2.5%, 50%, 97.5%)
        models[[N]]$mcmccalcs$rlower    - Recruitment lower confidence (2.5%)
        models[[N]]$mcmccalcs$rmed      - Recruitment median (50%)
        models[[N]]$mcmccalcs$rupper    - Recruitment upper confidence (97.5%)
        models[[N]]$mcmccalcs$devlower  - Recruitment deviations lower confidence (2.5%)
        models[[N]]$mcmccalcs$devmed    - Recruitment deviations median (50%)
        models[[N]]$mcmccalcs$devupper  - Recruitment deviations upper confidence (97.5%)
        models[[N]]$mcmccalcs$plower    - SPR lower confidence (2.5%)
        models[[N]]$mcmccalcs$pmed      - SPR median (50%)
        models[[N]]$mcmccalcs$pupper    - SPR upper confidence (97.5%)
        models[[N]]$mcmccalcs$flower    - Fishing mortality lower confidence (2.5%)
        models[[N]]$mcmccalcs$fmed      - Fishing mortality median (50%)
        models[[N]]$mcmccalcs$fupper    - Fishing mortality upper confidence (97.5%)


These are the other variables in the global workspace. These can be directly referenced using \Sexpr{} in inline latex code,
or in a knitr code chunk:

    base.model              - The base model object. Same as models[[base.model.ind]].
    base.model.ind          - Index of the base model as found in the directory.
    unfished.eq.yr          - Unfished equilibrium year. For hake, this is before the start year.
    start.yr                - Start year for the model.
    end.yr                  - End year for the model.
    survey.start.yr         - First survey year included in the model.
    survey.end.yr           - Last survey year included in the model.
    assess.yr               - The current assessment year.
    last.assess.yr          - The last year in which an assessment was done.
    forecast.yrs            - A vector of years to forecast for decision tables (e.g. 2015:2017).
    catch.levels            - A list of vectors of length equal to the number of years in forcast.yrs.
    catch.levels.names      - A vector of names that describe the catch.levels list elements.
    catch.levels.dir.names  - A vector of OS_friendly names to use for the catch levels' directory names.
    catch.default.policy    - A vector of catch limits for the forecast years which corresponds to the default harvest rate.
    data.path               - The absolute path to the data folder, which holds catch and tac tables.
    models.path             - The absolute path to the models folder, which holds sub-directories for the models which have been run.

There are additional elements for model-partest, which is created by running **run.partest.model**. It is saved in a file called
**model-partest.RData**. It is a copy of base.model with the following additions:

    model-partest$agedbase$Exp           - median of posterior for expected value for age comps
    model-partest$agedbase$Exp.025       - 2.5% of posterior for expected value for age comps
    model-partest$agedbase$Exp.975       - 97.5% of posterior for expected value for age comps
    model-partest$agedbase$Pearson       - median of posterior for pearson residuals for age comps
    model-partest$agedbase$Pearson.025   - 2.5% of posterior for pearson residuals for age comps
    model-partest$agedbase$Pearson.975   - 97.5% of posterior for pearson residuals for age comps
    model-partest$cpue.table             - Table of cpue index values for all posteriors (survey)
    model-partest$cpue.median            - median of posterior for cpue index values (survey)
    model-partest$cpue.025               - 2.5% of posterior for cpue index values (survey)
    model-partest$cpue.975               - 97.5% of posterior for cpue index values (survey)
    model-partest$like.info              - Likelihood values for all posteriors

---

## How knitr deals with the R environment

- The file **doc/hake-assessment.rnw** has the initial knitr code chunk in it, where the R environment is loaded. Once this is loaded,
  knitr has full access throughout the document to the environment, and calls to plot or create tables can be made.
  This is true of child documents as well, e.g. **doc/executive-summary/executive-summary.rnw**.

- The list structure of the model scenarios is crucial to keep things organized, and to ensure sensitivity plots are easy to implement.

---

## How Andy is running it (and see Chris's notes above)

- download from Hake JTC Google Drive the model runs, and put in **hake-assessment\models\** (then 'unzip to here', then remove the .zip file so that **models\** just has the required subdirectories).
- **source("all.r")** to reload models and data files and for any changes to R code.
- **save.image()** to create the **.RData** file, or close R (and **save** workspace).
- delete **knitr-cache** directory if any tables or figures need to be updated
- **knit("hake-assessment.rnw")** [or use Chris's batch file - at first I just want to see the warnings]
- **latex hake-assessment.tex** and **dvips** and **bibtex** if necessary
- **ispell hake-assessment.tex** periodically


__GitHub workflow__

- I forked Chris's master repository, and did **git remote add cgrandin https://...** [and he added me to his] so that we can merge each other's commits. **git remove -v** shows that.
- **Allan/Aaron**: to merge my commits (for when Chris isn't on top of it) do:
-  
       git remote add aedwards https://github.com/andrew-edwards/hake-assessment
  
- just once. 
- Then do **git fetch** and **merge** as described below, but with **aedwards** instead of **cgrandin**. Note that **aedwards** is just what you call my repository on your machine, it doesn't have to match my user name.
 
  
- **git com** and **git push** often [I'm using Chris's **git-workshop** shortcuts]
- **git fetch cgrandin** - fetches his latest version
- **git diff cgrandin/master** shows me the differences between his and mine. :
-- + green is on mine not his, red is his not mine [seems like it can look like I've added something but really Chris has removed it; and when merging it should base it on the most recent commits]
- **git merge cgrandin/master** merges our versions. 
- remove **knitr-cache** directory, re-run **source("all.r")** and re-run **knitr** to make sure it all still works (I kept forgetting this before pushing).
- Then **push** (I think a merge automatically does a **commit**?).
- when get a conflict, open the file in emacs and it has <<<<<<   for the start of a conflicting part, and ========= at the end, so manually fix it. Then **git add <filename>** to confirm that's the one you want (not completely obvious), then commit. See <https://help.github.com/articles/resolving-a-merge-conflict-from-the-command-line/>  
- We will try and work on different files so that there are no conflicts when we merge.
 

**Undoing a merge**

Just merged Chris's stuff (27/1/16) in but rebuilding the models doesn't work, I think because he mentioned that he had to change some structure (and he's left for the day). I tried fixing, but I don't think it worked (I'll commit this edit to the readme.md file to double check), so easiest just to ask Chris tomorrow as I need to leave soon anyway. I tried:

From http://stackoverflow.com/questions/2389361/undo-a-git-merge  trying the answer:

"Strange that the simplest command was missing. Most answers work, but undoing the merge you just did, this is the easy and safe way:

git reset --merge ORIG_HEAD

The ref ORIG_HEAD will point to the original commit from before the merge."

So I get ORIG_HEAD from doing  git lg

git reset --merge 33489f0

**Running MCMC in SS**
Copy and paste all model output files into new mcmc/ directory.

ss3 -mcmc 999 -mcsave 1 

ss3 -mceval

[Allan says probably 1000 for the first one since it discards the first sample - presumably only when -mcsave 1 is there would we need 1000]



__Andy's other notes__

- Network graph - I just [25 Jan 2016, commit number 7e25a5c] merged Chris's, but this doesn't show up on his or my Network graphs, I think because he had merged all my commits earlier, and I hadn't committing anything since. So it's not really merging (and there was no possibility for a conflict because I hadn't changed anything since he merged mine), just updating.

- Use text in main document of last year's, and start converting to .tex.
- Table 1 and 2 of last year's .pdf -- values should be in **catches**, see **make-catches-table.r** in **catches.r** for an earlier table, and modify to make new ones.
- if just editing the placement (and maybe more) of a table that is in, say, executive-summary.rnw, then no need to delete knitr-cache. Probably.

Helpful git commands I didn't know:

**git lg1**   [or lg2] - shows commit numbers (codes)

**git log .\doc\hake-assessment.rnw**  - show revision history for a file (syntax not quite right there)

**git checkout <enough numbers of the commit reference to make it unique> .\doc\[filename.rnw] **  - revert back to that version of that file, I think...  

[I can delete this once I know it all automatically] GitHub Colors are explained under The Prompt in the README shown at https://github.com/dahlbyk/posh-git/ . To summarize:

- Cyan means the branch matches its remote
- Green means the branch is ahead of its remote (green light to push)
- Red means the branch is behind its remote
- Yellow means the branch is both ahead of and behind its remote

The +~-! status represents added/modified/removed/conflicted file count in your index (dark green) and/or working directory (dark red).
