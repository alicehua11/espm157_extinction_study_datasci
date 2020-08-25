
[![Build Status](https://travis-ci.com/espm-157/climate-alicehua11.svg?token=7gSxV1VqqHz7TUXHHWGp&branch=master)](https://travis-ci.com/espm-157/climate-alicehua11)

This repository is a report on current climate change data completed under the first module of ESPM 157. The report utilizes R library to make visualizations followed by text analysis to understand how the earth's climate is warming. 

## assignment

All work for this assignment is in the `assignment` directory.  Code is contained in the `climate.Rmd` notebook, and final rendered output files (`climate.md` and associated files) are in the `assignment` directory as well. The general rubric you will be graded on is found in the `Rubric.md` file. 

#### Climate Report

Look at the `climate.rmd` notebook for **final report**. Here are the sections:

* Warm-up: CO2 trends.
* Exercise 1: Temperature Data.
* Exercise 2: Melting Ice Sheets.
* Exercise 3: Rising Sea Levels.
* Excercise 4: Longer term trends in CO2 Records
  
## Special files

Additionally this repository, and all team repositories, includes most of the special files found here:

#### Common files

- `README.md` this file, a general overview of the repository in markdown format.  
- `.gitignore` Optional file, ignore common file types we don't want to accidentally commit to GitHub. Most projects should use this. 
- `climate-template.Rproj` an R-Project file created by RStudio for it's own configuration of the repo files.  Some people prefer to `.gitignore` this file, it is optional for team repos to ignore or commit their own `<REPO-NAME>.Rproj` file. 

#### Infrastructure for Testing

- `.travis.yml`: A configuration file for automatically running [continuous integration](https://travis-ci.com) checks to verify reproducibility of all `.Rmd` notebooks in the repo.  If all `.Rmd` notebooks can render successfully, the "Build Status" badge above will be green (`build success`), otherwise it will be red (`build failure`).  
- `DESCRIPTION` a metadata file for the repository, based on the R package standard. It's main purpose here is as a place to list any additional R packages/libraries needed for any of the `.Rmd` files to run.
- `tests/render_rmds.R` an R script that is run to execute the above described tests, rendering all `.Rmd` notebooks. 

## **For more information on the course visit:**

[![](assignment/classpic.JPG)](https://espm-157.carlboettiger.info/)


