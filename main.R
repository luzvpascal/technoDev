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
# source("MCUAMS/mean_parameters.R")

#MDP
source("MDP/MDP_full_info.R")

#POMDP
source("POMDP/transition_functions.R")
source("POMDP/possible_models.R")
source("POMDP/write_hmMDP_mcuams.R")
source("POMDP/read solutions.R")
source("POMDP/build_matrices_hmMDP.R")
source("POMDP/solving AM.R")
source("POMDP/solving tech dev.R")

#analytical solution
source("analytical solution/analytical functions AM.R")
source("analytical solution/analytical functions tech dev.R")

#simulations
source("simulations/simulations.R")

## load global variables ####
source("variables of problem.R")
source("files_names_functions.R")

# ## RD vs no RD ####
# source("RD vs no RD/heatmap value no RD vs value RD.R")
