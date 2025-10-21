#load packages
library(renv)

#initialize renv
renv::init()

#where are the packages
.libPaths()
renv::snapshot()

