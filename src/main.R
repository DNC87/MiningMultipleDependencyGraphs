# ...................................................
# Main program ##################

# ...................................................
# Libraries #####################
packages <- c("rstudioapi","stringr", "dplyr" ,"igraph", "visNetwork", "ggplot2", "methods","intergraph","dplyr")

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())), dependencies = T)
}
lapply(packages, require, character.only = TRUE)

# Get the current path of the main.R file and set it as working directory.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# ...................................................

# Load global vars and default settings
source("conf.R")

# Load the set of functions used during program 
source("utils.R")

# Load trials data and transform it into adjacent matrices and igraph structures   
source("ETL.R")

# Load our mining algorithms 
source("algorithms.R")

# Load the experiments 
source("experiments.R") 
# ...................................................
