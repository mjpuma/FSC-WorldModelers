#################################
### INSTALL REQUIRED PACKAGES ###
#################################

repo <- "http://cran.us.r-project.org"

cat("Installing required packages....\n")

install.packages("tidyr", repos = repo)
install.packages("dplyr", repos = repo)
install.packages("reshape2", repos = repo)
install.packages("igraph", repos = repo)
install.packages("stringr", repos = repo)

cat("Setup completed!")