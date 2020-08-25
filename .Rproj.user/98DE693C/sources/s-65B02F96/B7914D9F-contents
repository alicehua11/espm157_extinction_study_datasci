
[![Build Status](https://travis-ci.com/espm-157/extinction-quam-hua.svg?token=7gSxV1VqqHz7TUXHHWGp&branch=master)](https://travis-ci.com/espm-157/extinction-quam-hua)

## Team Members:

- Alice Hua, [@alicehua11](https://github.com/alicehua11)
- Chauncey Quam, [@ChanceyQuam](https://github.com/ChaunceyQuam)

This assignment examines whether we are undergoing a sixth mass extinction based on data from the [International Union for Conservation of Nature (IUCN) redlist](https://www.iucnredlist.org/). 
Throughout this assignment, we try to replicate a study by Gerardo Ceballos and colleagues in assessing the current rates of vertebrate extictions, particularly, the mammals, birds, vertebrates and other vertebrates. While Caballos compares the their findings to the background extinction rates, which are assumed to be between 0.1 - 1 species extinction per million species per year E/MSY, we are interested in comparing the extinction rates computed by slightly different parameters: with NA values, without NA values, an interpolated estimate of NA values in our final findings.
We will use RESTful API to pull data about species and regular expression to extract their extinction dates. 
Lastly, we transform the data into "rectangular" data to make our final graphs and draw comparisons.

## assignment

All work for this assignment should be in the `assignment` directory.  You will work in the `.Rmd` notebook, and commit your rendered output files (`.md` and associated files) in the `assignment` directory as well.

## Special files

All team repositories will also include most of the special files found here:

### Common files
- `extinction-assigment_cache` is an intermediate cache folder that we used to store our large GET responses so that we don't need to rerun our code chunk later when we rerender our markdown. However, if it is your first time running the code chunk, there is no cached results and therefore, it will take a while to run. Meditate. 
- `massive_resp.rds` stored as an R object, it contains the narratives that we used to extract the extinction dates. Due to the large size of this file, we git ignored it when pushing this repo to Github. When rerunning our codes locally, they will automatically download the data and stored this large file locally on your comp. Make sure to not push this to Github to avoid a foreseeable unfortunate event.
- `README.md` this file, a general overview of the repository in markdown format.  
- `.gitignore` Optional file, ignore common file types we don't want to accidentally commit to GitHub. Most projects should use this. 
- `extinction-template.Rproj` Optional, an R-Project file created by RStudio for it's own configuration.  Some people prefer to `.gitignore` this file.


### Infrastructure for Testing

- `.travis.yml`: A configuration file for automatically running [continuous integration](https://travis-ci.com) checks to verify reproducibility of all `.Rmd` notebooks in the repo.  If all `.Rmd` notebooks can render successfully, the "Build Status" badge above will be green (`build success`), otherwise it will be red (`build failure`).  
- `DESCRIPTION` a metadata file for the repository, based on the R package standard. It's main purpose here is as a place to list any additional R packages/libraries needed for any of the `.Rmd` files to run.

### This project is a part of our course on [ESPM 157: Data Science for Global Change Ecology](https://espm-157.carlboettiger.info/):
[![](images/classpic.JPG)](https://espm-157.carlboettiger.info/)



