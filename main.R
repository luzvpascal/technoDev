# # List of packages
# packages_to_install <- c("sarsop", "prodlim", "MDPtoolbox", "RColorBrewer",
#                          "tidyverse", "foreach", "parallel", "doParallel",
#                          "mgcv", "fmsb", "dplyr", "tidyverse", "ggpubr",
#                          "Matrix", "latex2exp", "svglite", "ragg", "grid",
#                          "markovchain")
#
# # Install packages if not already installed
# install_if_not_installed <- function(package) {
#   if (!requireNamespace(package, quietly = TRUE)) {
#     install.packages(package, dependencies = TRUE)
#   }
# }
#
# # Apply the function to install packages
# invisible(lapply(packages_to_install, install_if_not_installed))

## libraries ####
library(sarsop)
library(prodlim)
library(MDPtoolbox)
library(RColorBrewer)
library(tidyverse)
library(foreach)
library(parallel)
library(doParallel)
library(mgcv)
library(fmsb)
library(dplyr)
library(tidyverse)
library(ggpubr)
library(Matrix)
library(latex2exp)
library(svglite)
library(ragg)
library(grid)
library(markovchain)
library(ggnewscale)

## useful functions ####
source("0 functions/load_functions.R")
## load global variables ####
source("variables of problem.R")
