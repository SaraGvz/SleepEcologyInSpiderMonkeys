#### Installing required packages ####

install.packages(c("devtools","miniCRAN","pacman"), dependencies = TRUE, type = "source")
devtools::install_github("wrathematics/getPass")

#Check if required packages and their dependencies need installation or updates
list_of_required_packages <- c("plyr", "dplyr", "purrr","readr", "stringr", "data.table,",
                               "lme4", "Matrix", "maditr", "reshape2", "magrittr", "ggplot2", "ggpubr", "fitdistrplus", "tidyr",
                               "mgcv", "insight", "httr", "plotrix", "lubridate", "suncalc", "sp",
                               "rgdal", "raster", "rgeos", "move", "multcomp","entropy", "ggspatial", "gapminder")

check_if_needs_install=as.character(miniCRAN::pkgDep(list_of_required_packages, suggests = TRUE, enhances = TRUE))
check_if_needs_update=as.character(pacman::p_update(FALSE))

new_packages <- check_if_needs_install[!(check_if_needs_install %in% installed.packages()[,"Package"])]
packages_to_update=check_if_needs_install[check_if_needs_install %in% check_if_needs_update]

packages_to_install=c(new_packages,packages_to_update)


install.packages(packages_to_install, type = "source")
install.packages(c("lubridate", "data.table"), dependencies = TRUE)
install.packages('ggubr', dependencies = TRUE)
install.packages('sf', dependencies = TRUE)


######## installing the newest version of rstan and brms#######
remove.packages(c("StanHeaders", "rstan"))
install.packages("StanHeaders", repos = c("https://mc-stan.org/r-packages/", getOption("repos")),
                 type = "source")
install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")),
                 type = "source")

devtools::install_github("paul-buerkner/brms", force = TRUE)
install.packages("igraph")