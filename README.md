# Information

This repository contains code and workflow data for an ongoing project titled "Sequencing historical RNA: unrealized potential to increase understanding of the plant tree of life".
The aim of this repository is to produce publication-quality figures summarizing large amounts of coordinate data from green plants, using [RBIEN](https://bien.nceas.ucsb.edu/bien/tools/rbien/). 

> Maitner BS, Boyle B, Casler N, et al. The bien r package: A tool to access the Botanical Information and Ecology Network (BIEN) Database. Methods Ecol Evol. 2018; 9: 373–379. https://doi.org/10.1111/2041-210X.12861

My hope is that this repository can also serve as a tutorial for downloading data from the BIEN database, working with coordinate data in R, and creating figures that display species ranges.

# Outline of files included

1. Obtaining a list of species of interest from the NCBI SRA
2. Downloading species occurrences using RBIEN
3. Compiling occurrence data into a larger file, cross-referencing names using GBIF backbone
4. Range map creation using square grids
5. Range map creation using Uber's H3 hexagon mapping


# Acknowledgements #

> This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. 2236870.


# Citations #
Citations for all packages and software used to collect and analyze data

> Maitner BS, Boyle B, Casler N, et al. The bien r package: A tool to access the Botanical Information and Ecology Network (BIEN) Database. Methods Ecol Evol. 2018; 9: 373–379. https://doi.org/10.1111/2041-210X.12861

> R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna,  Austria. URL https://www.R-project.org/.

> Brian Maitner (2023). BIEN: Tools for Accessing the Botanical Information and Ecology Network Database. R package version 1.2.6. https://CRAN.R-project.org/package=BIEN

> Hadley Wickham, Romain François, Lionel Henry, Kirill Müller and Davis Vaughan (2023). dplyr: A Grammar of Data Manipulation. R package version 1.1.4. https://CRAN.R-project.org/package=dplyr

> Chamberlain S, Barve V, Mcglinn D, Oldoni D, Desmet P, Geffert L, Ram K (2024). _rgbif: Interface to the Global Biodiversity Information Facility API_. R package version 3.8.1, <URL: https://CRAN.R-project.org/package=rgbif>.
