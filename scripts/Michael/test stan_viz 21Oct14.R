
setwd("C:/Users/Michael Andreae/Dropbox/Columbia/courses/Fall 2014/Stat Communication/project/Hello-stan/scripts/Michael")

## Need only one for the installation of the SHINYstan package 
install.packages("devtools")
library("devtools") 
install_github("jgabry/SHINYstan")

library(SHINYstan)
launch_SHINYstan_demo()
launch_SHINYstan_demo(binormal)


