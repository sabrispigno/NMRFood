# #load packages
# library(renv)
# 
# #initialize renv
# renv::init()
# 
# #where are the packages
# .libPaths()
# renv::snapshot()
# 

library(targets)
use_targets()
library(tarchetypes)

renv::clean(prompt = FALSE)  # removes packages no longer referenced by renv.lock
renv::status()
# 1) Prune lockfile entries not required by your code
renv::snapshot(prune = TRUE, prompt = FALSE)

# 2) Remove packages from the project library that aren't in the lockfile
renv::clean(prompt = FALSE)

# 3) Re-check
renv::status()
