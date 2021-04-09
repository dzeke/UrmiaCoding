
# TestScholar.r
# From https://www.rdocumentation.org/packages/scholar/versions/0.1.7

### Clear any existing data or functions.
rm(list=ls())

if (!require(scholar)) { 
  install.packages("scholar", repos="http://cran.r-project.org") 
  library(scholar) 
}


# Define the id for Richard Feynman
id <- 'B7vSqZsAAAAJ'
#Rosenberg
id <- 'GfYKtkIAAAAJ'

#Comapre coauthors
coauthor_network <- get_coauthors(id)
plot_coauthors(coauthor_network)