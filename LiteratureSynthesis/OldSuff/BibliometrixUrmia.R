# BibliometrixUrmia.r
#
# Bibliometrix analysis of Scopus Papers with Keywords Lake Urmia
#
# Following the directions at https://cran.r-project.org/web/packages/bibliometrix/vignettes/bibliometrix-vignette.html
# To test the Bibliometrix package
#
# ria, M. & Cuccurullo, C. (2017) bibliometrix: An R-tool for comprehensive science mapping analysis, Journal of Informetrics, 11(4), pp 959-975, Elsevier.
#
# David E. Rosenberg 
# Utah State Univeristy
# Logan, Utah
# August 14, 2019
# david.rosenberg@usu.edu
#

library(bibliometrix)   ### load bibliometrix package
library(ggplot2)
library(dplyr)
library(lubridate)
library(plyr)
library(data.table)
library(stringr)

# Number of items to print
lPrint <- 20

# Load the data
# From CSV

# The TiTLE, Abstract, or Keyword Query String
tArticleQuery <- 'TITLE-ABS-KEY ( "Lake"  AND  ( "Urmia"  OR  "Ormayeh"  OR  "Orumiyeh" ) )' 

sFileName <- "Scopus-LakeUrmia-Aug8-2019"
#Read as a .bib file
fScopusFile <- readFiles(paste0(sFileName,".bib"))

D <- fScopusFile
#D <- readFiles("http://www.bibliometrix.org/datasets/savedrecs.bib")

# Convert to dataframe
M <- convert2df(D, dbsource = "scopus", format = "bibtex")

# Conduct the bilio analysis
results <- biblioAnalysis(M, sep = ";")

#Echo some summary results
results$Articles   #Number of articles
results$nAuthors   #Number of authors
results$AuMultiAuthoredArt #Number of multi-authored papers
results$AuSingleAuthoredArt #Number of single authored papers

# Articles over time
#Group years. Create as a new factor YearType
dfArticleData <- M

cPeriods <- c('Before ULRP', 'After ULRP')
cPeriodBreak <- c(2013,2030)
dfArticleData$YearPeriod <- cPeriods[3]

for (i in 2:1) {
  index <- dfArticleData$PY <= cPeriodBreak[i]
  dfArticleData$YearPeriod[index] = cPeriods[i]
}
#Find the max year


barlines <- "#1F3552" #Black outline
cFills <- c('#6baed6', '#3182bd', '#08519c') #Fill colors for bars. Light blue, medium blue, dark blue

minYear <- min(dfArticleData$PY)
minYear <- 1984
maxYear <- max(dfArticleData$PY)

### Figures coded by DER
### Figure 1 - Bar graph of number of journals per year by period
#Create version of the data set with YearType ordered so the legend and bar colors correctly plot.
# See https://ilari.scheinin.fi/ggplot-2-0-and-the-missing-order-aesthetic/ for particulars
dfArticleOrdered <- dfArticleData %>% filter(PY >= minYear) %>%
  mutate(YearPeriod=factor(YearPeriod, levels=cPeriods, ordered=TRUE))

#Similarly order the datafame
dfArticleOrdered %>%
  ungroup() %>%
  arrange(-as.integer(YearPeriod))


#Plot barplot
ggplot(data = dfArticleOrdered, aes(x = dfArticleOrdered$PY)) + 
  geom_bar(breaks=seq(minYear,maxYear, by=1), 
           colour = barlines,
           position = "identity",
           aes(fill=dfArticleOrdered$YearPeriod)) +
  
  labs(x="", y="Articles\n(number per year)") +
  scale_x_continuous(breaks = seq(minYear,maxYear,1)) +
  scale_fill_manual(values=cFills, name="Period", labels=cPeriods) +
  theme_bw() +
  theme(text = element_text(size=20), legend.text=element_text(size=18),
        axis.text.x = element_text(size=18, angle=90,hjust=1.1,vjust=0.5)) #,angle=90,

# ----------------------
# Figure 2 Query and arrange the top citation counts

dfArticleCites <- dfArticleData %>% 
  ungroup() %>%
  arrange(desc(dfArticleData$TC))

#Formulate the paper lable : Author (Year) or Author et al (Year)
# Count the number of authors by the number of commas in the author field
dfArticleCites$NumAuthors <- str_count(dfArticleCites$AU,";") + 1
dfArticleCites$FirstAuthorLN <- paste(word(dfArticleCites$AU,start=1,end=1," ")," ")
dfArticleCites$SecondAuthorLN <- paste(word(dfArticleCites$AU,start=2,end=2,";")," ")
#Strip off final space and first initial
dfArticleCites$SecondAuthorLN <- paste(word(dfArticleCites$SecondAuthorLN,start=1,end=1," ")," ")


#Single author => Last Name (Year)
dfArticleCites$PaperDesc <- paste0(dfArticleCites$FirstAuthorLN,"(",dfArticleCites$PY,")")
#Two author => First and Last (Year)
dfArticleCites$PaperDesc[dfArticleCites$NumAuthors == 2] <- paste0(dfArticleCites$FirstAuthorLN[dfArticleCites$NumAuthors ==2],"and ",dfArticleCites$SecondAuthorLN[dfArticleCites$NumAuthors == 2], "(",dfArticleCites$PY[dfArticleCites$NumAuthors == 2],")")
# Multi author => Last Name et al (Year)
dfArticleCites$PaperDesc[dfArticleCites$NumAuthors > 2] <- paste0(dfArticleCites$FirstAuthorLN[dfArticleCites$NumAuthors > 2],"et al. ", "(",dfArticleCites$PY[dfArticleCites$NumAuthors > 2],")")

#Plot barplot of Cited By
ggplot(dfArticleCites[0:lPrint,], aes(x=reorder(PaperDesc,TC), y=TC)) +
  geom_col() +
  
  labs(y="Citations (count)", x="") +
  theme_bw() +
  theme(text = element_text(size=20), legend.text=element_text(size=18),
        axis.text.x = element_text(size=18, angle=90,hjust=1.1,vjust=0.5)) +
  coord_flip()

# ------------------------------

# Figure 3. Plot number of authors
dfNumAuthors <- ddply(dfArticleCites, .(NumAuthors), nrow) 
ggplot(data = dfNumAuthors, aes(x=NumAuthors,y=V1)) + 
  geom_bar( 
    stat = "identity",
    position = "identity") +
  
  labs(x="Number of Authors", y="Artiles (count)") +
  #scale_x_continuous(breaks = seq(minYear,maxYear,1)) +
  #scale_fill_manual(values=cFills, name="Period", labels=cPeriods) +
  theme_bw() +
  theme(text = element_text(size=20), legend.text=element_text(size=18),
        axis.text.x = element_text(size=18, angle=90,hjust=1.1,vjust=0.5))


## Sort the data frame from largest to smallest number of authors
dfArticleAuthorSort <- dfArticleCites %>% arrange(-NumAuthors, AU)

write.csv(dfArticleAuthorSort, "UrmiaArticlesSorted.csv")

# ------------------------------------------
### Figure 4 - Most frequent journals
# To obtain the most frequent cited manuscripts:

dfJournalCounts <- ddply(dfArticleData, .(SO), nrow) 

dfJournalCounts <- dfJournalCounts %>%
  ungroup() %>%
  arrange(desc(dfJournalCounts$V1))

#Grab the first 6 words for the source
dfJournalCounts$SourceTitleShort <- str_trunc(dfJournalCounts$SO,50,side="right")

#Plot barplot
ggplot(data = dfJournalCounts[0:lPrint,], aes(x=reorder(SourceTitleShort,V1),y=V1)) + 
  geom_bar( 
    stat = "identity",
    position = "identity") +
  
  labs(x="Journal", y="Articles (count)") +
  #scale_x_continuous(breaks = seq(minYear,maxYear,1)) +
  #scale_fill_manual(values=cFills, name="Period", labels=cPeriods) +
  theme_bw() +
  theme(text = element_text(size=20), legend.text=element_text(size=18),
        axis.text.x = element_text(size=18, angle=90,hjust=1.1,vjust=0.5)) +
  coord_flip()#,angle=90,

# -------------------------------------
### Key word frequency

### Key word frequency FROM BIBLIOMETRIX
dfKeywords <- as.data.frame(results$DE, stringsAsFactors = FALSE)
#Grab the first 6 words for the source
#dfJournals$SourceTitleShort <- str_trunc(dfJournals$SO,50,side="right")

# Introduce arbitrary faceting to plot more keywords on the chart
nKeys <- nrow(dfKeywords)
nGroups <- round(nrow(dfKeywords)/lPrint,0)
for(i in 1:nGroups) {
  dfKeywords$Group[(lPrint*(i-1)+1):nKeys]   <- i
}

#Plot barplot
ggplot(data = dfKeywords[3:(3*lPrint),], aes(x=reorder(Tab,Freq),y=Freq)) + 
  geom_bar( 
    stat = "identity",
    position = "identity") +
  
  facet_wrap(~ Group, scales="free_y") +
  labs(x="Keyword", y="Publications (count)") +
  #scale_x_continuous(breaks = seq(minYear,maxYear,1)) +
  #scale_fill_manual(values=cFills, name="Period", labels=cPeriods) +
  theme_bw() +
  theme(text = element_text(size=20), legend.text=element_text(size=18),
        axis.text.x = element_text(size=16, angle=90,hjust=1.1,vjust=0.5),
        axis.text.y = element_text(size=14)) +
  coord_flip()#,angle=90,





### Remaaining Figures by Bibliometrix


#Obtain most frequent citations
CR <- citations(M, field = "article", sep = ";")
cbind(CR$Cited[1:lPrint])

#To obtain the most frequent cited first authors:

# Doesn't work  
CR <- citations(M, field = "author", sep = ";")
cbind(CR$Cited[1:lPrint])

# Most frequent local citations
CR <- localCitations(M, sep = ";")
CR$Authors[1:lPrint,]
CR$Papers[1:lPrint,]

#Top authors productivity over time
topAU <- authorProdOverTime(M, k = lPrint, graph = TRUE)

# Bipartite networks
A <- cocMatrix(M, Field = "SO", sep = ";")
dfJournalSort <- sort(Matrix::colSums(A), decreasing = TRUE)[1:lPrint]

# Citation network
A <- cocMatrix(M, Field = "CR", sep = ".  ")
sort(Matrix::colSums(A), decreasing = TRUE)[1:lPrint]

# Author keyword network
#A <- cocMatrix(M, Field = "DE", sep = ";")

#NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "DE", sep = ".  ")
#net=networkPlot(NetMatrix,  normalize = "salton", weighted=NULL, n = 100, Title = "References' Coupling", type = "fruchterman", size=5,size.cex=T,remove.multiple=TRUE,labelsize=0.8,label.n=10,label.cex=F)


NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "authors", sep = ";")
net=networkPlot(NetMatrix,  normalize = "salton", weighted=NULL, n = 100, Title = "Authors' Coupling", type = "fruchterman", size=5,size.cex=T,remove.multiple=TRUE,labelsize=0.8,label.n=10,label.cex=F)

# Visualize networks
# Create a country collaboration network

#M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
#NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")

# Plot the network
#net=networkPlot(NetMatrix, n = dim(NetMatrix)[1], Title = "Country Collaboration", type = "circle", size=TRUE, remove.multiple=FALSE,labelsize=0.7,cluster="none")

#Create keyword co-occurrences network

NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Keyword Co-occurrences", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)

# Conceptual Structure using keywords (method="CA")
#CS <- conceptualStructure(M,field="ID", method="CA", minDegree=4, clust=5, stemming=FALSE, labelsize=10, documents=10)

# Create a historical citation network
#options(width=130)
histResults <- histNetwork(M, min.citations = 10, sep = ";")

## Articles analysed   84

# Plot a historical co-citation network
net <- histPlot(histResults, n=25, size = 10, labelsize=5)
