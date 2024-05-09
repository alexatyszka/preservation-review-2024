#Created by AST, July 5 2023; edited May 8 2024.
#This script takes multiple smaller .csv files containing GBIF information and concatenates them into a larger .csv file, if downloading coordinates in batches.

library(dplyr)
setwd("~/exampledirectory")
file_names <- dir() #where you have your files, the directory must have only these files in there

r <- NULL
input <- NULL
load_csvs <- function(infile){
  try(
    input <- read.csv(infile, header = T, colClasses=c("scrubbed_species_binomial"="character", 
                                                     "date_collected" = "character",
                                                     "datasource" = "character",
                                                     "dataowner" = "character",
                                                     "collection_code" = "character",
                                                     "custodial_institution_codes" = "character",
                                                     "dataset" = "character"
                                                     ))
  )
  message(infile)
  r <<- bind_rows(r, input)
  #print(input$scrubbed_species_binomial)
  #print(length(r$scrubbed_species_binomial))
}

for (i in file_names){
  load_csvs(i)
}

write.csv(r, file="../obs_compiled.csv")
