library(BIEN)
examplepath <- "file/path"
setwd(examplepath)
sras <- read.csv("Oryza_example_SraRunInfo.csv")
#remove anything except RNAseq
sras_rnaseq <- sras[sras$LibraryStrategy == "RNA-Seq",]
unique_species <- unique(sras_rnaseq$ScientificName)
unique_species
write.csv(unique_species, "Output_orzya_example_unique_species.csv")
