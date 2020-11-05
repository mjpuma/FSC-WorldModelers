#################################
### INSTALL REQUIRED PACKAGES ###
#################################

repo <- "http://cran.us.r-project.org"

cat("Installing required packages....\n")

install.packages("tidyr", repos = repo, INSTALL_opts = '--no-lock')
install.packages("dplyr", repos = repo, INSTALL_opts = '--no-lock')
install.packages("reshape2", repos = repo, INSTALL_opts = '--no-lock')
install.packages("stringr", repos = repo, INSTALL_opts = '--no-lock')
install.packages("igraph", repos = repo, INSTALL_opts = '--no-lock')
install.packages("ggraph", repos = repo, INSTALL_opts = '--no-lock')
install.packages("tidyverse", repos = repo, INSTALL_opts = '--no-lock')
install.packages("netrankr", repos = repo, INSTALL_opts = '--no-lock')

cat("Setup completed!")