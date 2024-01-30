# When to stop developing new technologies ?

This repository contains the code used to produce the results for the paper *When to stop developing new technologies?*. 
Here we provide instructions on how to use the code, and reproduce results.

WARNING (long run time for experiments): We produced the results of the paper in 12h, on a DELL laptop (CPU	11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz, Base speed:	1.80 GHz, Sockets:	1,	Cores:	4, Logical processors:	8).

## Table of contents
* [Requirements](#requirements)

## Requirements:
This code is implemented in R and calls an external executable programmed in C++ (MCUAMS folder).
To run the numerical experiments, users can decide to compile or not the C++ code. 

### C++ requirements (MCUAMS folder)
The MCUAMS folder contains the C++ implementation of the *Universal Adaptive Management Solver* [see publication](https://ojs.aaai.org/index.php/AAAI/article/view/17747).

Tested compiler:
- CLion 2022 under Windows. Toolchain: MinGW (version w64 9.0); Cmake 3.22.3; C++14.

```
$ cd MCUAMS/src
$ make
```


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
