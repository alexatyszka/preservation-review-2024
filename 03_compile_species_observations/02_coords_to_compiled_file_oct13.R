library(dplyr)
#this file is run from the server as of jul 5 2023
setwd("~")
setwd("../../scratch/alexa/pres_rev_figs/01_data/rbien_dl_csvs/coords_sept20/nonzero/")

file_names <- dir() #where you have your files, must have only these files in there



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

write.csv(r, file="../obs_compiled_rbien_oct_13.csv")
