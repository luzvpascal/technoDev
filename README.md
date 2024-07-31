# When to stop developing new technologies ?

This repository contains the code used to produce the results for the paper *When to stop developing new technologies?*. 
Here we provide instructions on how to use the code, and reproduce results.

**WARNING** (long run time for experiments): We produced the results of the paper in 12h, on a DELL laptop (CPU	11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, Base speed:	1.80 GHz, Sockets:	1,	Cores:	4, Logical processors:	8).

## Table of contents
* [Requirements](#requirements)
* [Run experiments](#run-experiments)

## Requirements
This code is implemented in R and calls an external executable programmed in C++ (MCUAMS folder).
To run the numerical experiments, users can decide to compile or not the C++ code. The succesful compilation of the MCUAMS algorithm allows to run all experiments from scratch. Alternatively, the user can directly use the outcomes of the MCUAMS algorithm (stored in `res/mean_params` and `res/priors`) by setting the variable `runMCUAMS` to `FALSE` in the file `variables of problem.R`.

### C++ requirements (MCUAMS folder - optional compilation)
The MCUAMS folder contains the C++ implementation of the *Universal Adaptive Management Solver* [see publication](https://ojs.aaai.org/index.php/AAAI/article/view/17747). This algorithm is used to predict the possible outcomes of technology deployment.

Tested compiler:
- CLion 2022 under Windows. Toolchain: MinGW (version w64 9.0); Cmake 3.22.3; C++14.

Quick start on Windows:
```
$ cd MCUAMS/src
$ make
```

Set the variable `file_MCUAMS` in the file `variables of problem.R` to the location of the compiled algorithm.


### R requirements
Users need to make sure that their version of R is at least 4.3.1. We recommend running the following code in R to preinstall all the necessary packages:
```r
# List of packages
packages_to_install <- c("sarsop", "prodlim", "MDPtoolbox", "RColorBrewer", 
                         "tidyverse", "foreach", "parallel", "doParallel", 
                         "mgcv", "fmsb", "dplyr", "tidyverse", "ggpubr", 
                         "Matrix", "latex2exp", "svglite", "ragg", "grid", 
                         "markovchain")

# Install packages if not already installed
install_if_not_installed <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
  }
}

# Apply the function to install packages
invisible(lapply(packages_to_install, install_if_not_installed))
```
## Run experiments

To run all experiments, run in R the code in `main.R`. This code operates as follows:
- install and load necessary packages
- load necessary functions (`load_functions.R`) and variables (`variables of problem.R`)
- Run the following code to generate Fig 3 (influence of initial belief technology is beneficial for the ecosystem)
```r
source("2 initial belief benefits deployment/GBR heatmap + expected AM + strategy + figures.R")
```
- Run the following code to generate Fig 4 and Fig S7 (influence of initial belief technology is beneficial for the ecosystem accross 4 profiles of ecosystems)
```r
source("3 degradation recovery profiles/4 profiles.R") #4 profiles figure
source("3 degradation recovery profiles/general profiles.R") #Supp info figure
```
- Run the following code to generate Fig S6 (influence of initial belief technology development will be successful on Tmax)
```r
source("4 initial belief feasibility and costs/graph maxTime.R")
source("4 initial belief feasibility and costs/graph maxTime analytical.R")
source("4 initial belief feasibility and costs/graph maxTime figure paper.R")
```
- Run the following code to generate Fig 2 (influence of net benefits on Tmax)
```r
source("5 Tmax vs Rdeploy/Tmax vs rdeploy.R")
```
All figures are saved in `res/figures paper/`.
