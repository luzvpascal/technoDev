# When to stop developing new technologies ?

This repository contains the code used to produce the results for the paper When to stop developing new technologies?

# REQUIREMENTS:
This code is implemented in R and calls external packages. Users need to make sure that their version of R is at least 4.3.1.
We recommend running the following code in R to preinstall all the necessary packages:
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
