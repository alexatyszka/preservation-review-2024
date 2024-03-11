
library("BIEN")
library(rgbif)
setwd("~")
setwd("../../scratch/alexa/pres_rev_figs/01_data/rbien_dl_csvs/")
sras <- read.csv("../01_search_SRA_RNA/runinfo.csv")
results <- read.csv(file="../../species_observations_197533_species")
#remove anything except RNAseq
sras_rnaseq <- sras[sras$LibraryStrategy == "RNA-Seq",]
u_all <- unique(results$scrubbed_species_binomial)
#u_sp <- unique(sras_rnaseq$ScientificName)
#sum(u_sp %in% u_all)
name_checklist <- name_backbone_checklist(u_sp)
checked <- name_checklist[name_checklist$rank == "SPECIES",]
checked <- checked[checked$status == "ACCEPTED",]
checked_in_all <- checked$canonicalName[unique(checked$canonicalName) %in% u_all]
#length(unique(checked$canonicalName))
write.csv(checked_in_all, "checked_sra_names_in_results_oct_16.csv")
