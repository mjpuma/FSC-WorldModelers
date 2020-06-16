#################################
### INSTALL REQUIRED PACKAGES ###
#################################

repo <- "http://cran.us.r-project.org"

cat("Installing required packages....\n")

install.packages("tidyr", repos = repo)
install.packages("dplyr", repos = repo)
install.packages("reshape2", repos = repo)
install.packages("stringr", repos = repo)
install.packages("igraph", repos = repo)
install.packages("ggraph", repos = repo)
install.packages("tidyverse", repos = repo)
install.packages("netrankr", repos = repo)
install.packages('abind', repos = repo

cat("Setup completed!")