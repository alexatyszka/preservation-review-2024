####server copy, most recent edition,edited on sept 19 2023 for BIEN visualizations
#cleaned on Dec 14 2023 for github repo
#this script downloads all the data from RBIEN for each catalogued species


library(BIEN)

setwd("~")
setwd("../../scratch/alexa/pres_rev_figs/01_data/rbien_dl_csvs/")
#old from country based approach, this is what we have downloaded previously
#load(file = "species_vector_dl_4_21_23.rdata")
#if you have a species vector to search use it here
#load(file = "rbien_to_search_sept20.rdata")
#here we use the list_all function from RBIEN
Blistall<-BIEN_list_all()
#only create the directory once:
#dir.create("rbien_coords_dec_14")
#setwd("rbien_coords_dec_14/")
##with try error handling, courtesy of https://stackoverflow.com/questions/31999808/retry-for-loop-r-loop-if-error
compiled_results <- NULL
n <- 10 #species for each query
m <- length(Blistall$species) #total number of species
startnum <- 0 #which index we start at, useful if server run is interrupted
for (x in seq(from=startnum, to=m, by=n)) {
  checknum <- 0 # setup for retry error
  message(x, " out of ", m, ", ",x/m ,"%") #print progress
  queries <- na.omit(Blistall$species[x:(x+(n-1))]) #get queries from db
  while(TRUE){ 
    download_try <- try(results <- BIEN_occurrence_species(species = queries,observation.type=T), silent=TRUE)
    if(is(download_try, 'try-error')){
      message("WARNING! timeout on: ", x, "\t retry attempt:", checknum)
      Sys.sleep(20)
      checknum = checknum + 1
    }
    else{
      break
      }
    }
  write.csv(results, file=paste("rbien_coords_dec_14/rbien_dl_from_", x, ".csv", sep=""))
}
