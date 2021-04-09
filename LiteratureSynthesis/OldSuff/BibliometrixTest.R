# BibliometrixTest.r
#
# Following the directions at https://cran.r-project.org/web/packages/bibliometrix/vignettes/bibliometrix-vignette.html
# To test the Bibliometrix package
#
# ## Aria, M. & Cuccurullo, C. (2017) bibliometrix: An R-tool for comprehensive science mapping analysis, Journal of Informetrics, 11(4), pp 959-975, Elsevier.
#
# David E. Rosenberg
# August 14, 2019
#

library(bibliometrix)   ### load bibliometrix package

# Load the data
D <- readFiles("http://www.bibliometrix.org/datasets/savedrecs.bib")

# Convert to dataframe
M <- convert2df(D, dbsource = "isi", format = "bibtex")

# Conduct the bilio analysis
results <- biblioAnalysis(M, sep = ";")

options(width=100)
S <- summary(object = results, k = 10, pause = FALSE)

# Plot the results
plot(x = results, k = 10, pause = FALSE)

#Obtain most frequent citations
CR <- citations(M, field = "article", sep = ";")
cbind(CR$Cited[1:10])

#To obtain the most frequent cited first authors:
  
CR <- citations(M, field = "author", sep = ";")
cbind(CR$Cited[1:10])

# Most frequent local citations
CR <- localCitations(M, sep = ";")
CR$Authors[1:10,]
CR$Papers[1:10,]

#Top authors productivity over time
topAU <- authorProdOverTime(M, k = 10, graph = TRUE)

# Bipartite networks
A <- cocMatrix(M, Field = "SO", sep = ";")
sort(Matrix::colSums(A), decreasing = TRUE)[1:5]

# Citation network
A <- cocMatrix(M, Field = "CR", sep = ".  ")
sort(Matrix::colSums(A), decreasing = TRUE)[1:5]

# Author keyword network
A <- cocMatrix(M, Field = "DE", sep = ";")

NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "references", sep = ".  ")
net=networkPlot(NetMatrix,  normalize = "salton", weighted=NULL, n = 100, Title = "References' Coupling", type = "fruchterman", size=5,size.cex=T,remove.multiple=TRUE,labelsize=0.8,label.n=10,label.cex=F)


NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "authors", sep = ";")
net=networkPlot(NetMatrix,  normalize = "salton", weighted=NULL, n = 100, Title = "Authors' Coupling", type = "fruchterman", size=5,size.cex=T,remove.multiple=TRUE,labelsize=0.8,label.n=10,label.cex=F)

# Visualize networks
# Create a country collaboration network

M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, n = dim(NetMatrix)[1], Title = "Country Collaboration", type = "circle", size=TRUE, remove.multiple=FALSE,labelsize=0.7,cluster="none")

#Create keyword co-occurrences network

NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Keyword Co-occurrences", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)

# Conceptual Structure using keywords (method="CA")
CS <- conceptualStructure(M,field="ID", method="CA", minDegree=4, clust=5, stemming=FALSE, labelsize=10, documents=10)

# Create a historical citation network
options(width=130)
histResults <- histNetwork(M, min.citations = 10, sep = ";")

## Articles analysed   84

# Plot a historical co-citation network
net <- histPlot(histResults, n=15, size = 10, labelsize=5)
