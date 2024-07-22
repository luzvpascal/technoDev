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

## libraries ####
library(sarsop)
library(cowplot)
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

# ## Run experiments ####
#around 1h run time
source("2 initial belief benefits deployment/GBR heatmap + expected AM + strategy + figures.R")
# source("2 initial belief benefits deployment/GBR heatmap + expected AM + strategy + figures analytical.R")

#around 4h run time
source("3 degradation recovery profiles/4 profiles.R")
source("3 degradation recovery profiles/general profiles.R")

#around 10 sec run time
source("4 initial belief feasibility and costs/graph maxTime.R")
source("4 initial belief feasibility and costs/graph maxTime analytical.R")
source("4 initial belief feasibility and costs/graph maxTime figure paper.R")

#around
source("5 Tmax vs Rdeploy/Tmax vs rdeploy.R")

#around 1 sec run time
source("6 insights analytical/analytical vs numerical Tmax.R")
