# TestCircle.r
# To test code to generate a circle diagram of keyword relations for interdisciplinary work
# https://www.r-graph-gallery.com/123-circular-plot-circlize-package-2.html
#
# David E Rosenberg
# August 12, 2020
# Utah State University
# david.rosenberg@usu.edu
#
#
### Notes to self:
# Next steps:
#   - Figure out how to code data from 21 papers
#   - Figure out how to harness keywords from all links.

install.packages("circlize")
library(circlize)

## Example 1
# Create an adjacency matrix: 
# a list of connections between 20 origin nodes, and 5 destination nodes:
numbers <- sample(c(1:1000), 100, replace = T)
data <- matrix( numbers, ncol=5)
rownames(data) <- paste0("orig-", seq(1,20))
colnames(data) <- paste0("dest-", seq(1,5))

# Load the circlize library
library(circlize)

# Make the circular plot
chordDiagram(data, transparency = 0.5)

## Example 2
# Create an edge list: a list of connections between 10 origin nodes, and 10 destination nodes:
origin <- paste0("orig ", sample(c(1:10), 20, replace = T))
destination <- paste0("dest ", sample(c(1:10), 20, replace = T))
data <- data.frame(origin, destination)

# Transform input data in a adjacency matrix
adjacencyData <- with(data, table(origin, destination))

# Charge the circlize library
library(circlize)

# Make the circular plot
chordDiagram(adjacencyData, transparency = 0.5)