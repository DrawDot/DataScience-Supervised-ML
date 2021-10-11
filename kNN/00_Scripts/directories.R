# Create directories 
library(fs)
dir_create("kNN")

getwd()
setwd("/Users/seunghyunsung/Desktop/R_DS/Github/DataScience-Supervised-ML/kNN/")

make_project_dir <- function() {
  dir_names <- c("00_Data",
                 "00_Scripts",
                 "01_Business_Understanding",
                 "02_Visualisation",
                 "03_Functions_iteration",
                 "04_Modelling",
                 "05_Evaluation")
  dir_create(dir_names)
  
  dir_ls()
}
make_project_dir()