# set directory to where your function file is located!
setwd("D:/GitHub/Fu_WeatherWhatever")
rm(list = ls())

# install.packages('devtools')
# install.packages('roxygen2')
# install.packages('usethis')

library(devtools)
library(roxygen2)
library(usethis)

# 1. create the R package skeleton, choose a name for your R package
devtools::create('WeatherWhatever')

# set working directory to *inside* your R package
setwd("D:/GitHub/Fu_WeatherWhatever/WeatherWhatever")

# populate the DESCRIPTION file
# never touch your NAMESPACE file

# # 2. add a license
usethis::use_ccby_license()

# 3. move your R file with your functions into the R directory of your package

# 4.1 Define which functions should be available to your users
# use "#' @export" for that, add it to your .R function file
# 4.2 Use the document() function to make the functions available
# to users
devtools::document()

# 5. Do your R functions depend on other packages?
# (1) mark those functions clearly in your R file, by adding the package name + :: 
# in front of your functions (e.g., ggplot2::ggplot())
# (2) add the packages to your dependencies in the Description file, e.g.,
# Imports:
#   ggplot2

# 6. build your package
# creates an installable file with ending "tar.gz"
devtools::build()

# 7. your package can now be installed (by you and others!)
rm(list = ls())
devtools::install()
library(WeatherWhatever)
plot_temperature('2024-05-01','2024-05-31','Amsterdam')
?plot_temperature
