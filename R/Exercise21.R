install.packages("remotes")
remotes::install_github("robjhyndman/fpp3-package")

library(dplyr)
library(tsibble)

y <- tsibble(Year = 2015:2019, Observation = c(123,39,78,52,110), index = Year)

