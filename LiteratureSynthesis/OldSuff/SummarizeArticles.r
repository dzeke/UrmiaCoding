# Read Journal Articles from Scopus CSV file and make summary plots
# 
# 1.	Journal articles by year – bar chart -- color coded by period: pre-last Review, Pre-ULRP, After ULRP
# 2.	Articles by journal / impact fact – bar chart – sorted largest to smallest

# David E. Rosenberg 
# Utah State Univeristy
# Logan, Utah
# August 2019
# david.rosenberg@usu.edu

### Clear any existing data or functions.
rm(list=ls())

# Load packages needed to plot

xPackages <- c("ggplot2","devtools","dplyr","lubridate", "sqldf","plyr","htmlTable",
               "magrittr","rmarkdown", "data.table", "stringr")
lapply(xPackages,require, character.only = TRUE)
lapply(xPackages,library, character.only = TRUE)

devtools::install_github("muschellij2/rscopus")
library(rscopus)

xPackages2 <- c("factoextra", "FactoMineR", "igraph", "Matrix", "SnowballC", "stringr")
lapply(xPackages2,require, character.only = TRUE)
lapply(xPackages2,library, character.only = TRUE)

install.packages("bibliometrix", dependencies=TRUE) 
library(bibliometrix)

#STEP 1: Query from SCOPUS (https://github.com/muschellij2/rscopus)

# The TiTLE, Abstract, or Keyword Query String
tArticleQuery <- 'TITLE-ABS-KEY ( "Lake"  AND  ( "Urmia"  OR  "Ormayeh"  OR  "Orumiyeh" ) )' 

#Set Elsevier key to Rosenberg
set_api_key("a3d6a6b695e2f1a7e6463c3c6e16c5bc")

# Simple example query
res = author_df(last_name = "Muschelli", first_name = "John", verbose = FALSE, general = FALSE)
names(res)

nMaxReturn <- 500

if (have_api_key()) {
  res = scopus_search(query = tArticleQuery, max_count = nMaxReturn,
                      count = nMaxReturn, view="COMPLETE", verbose=TRUE)
  df = gen_entries_to_df(res$entries)
  head(df$df)
  
  dfArticleData <- df$df
  
  res <- metadata_retrieval(query = tArticleQuery, verbose=TRUE)
  df <- gen_entries_to_df(res$entries)
  
  # Having trouble here to conver the loaded data into a data frame
}



# Read CSV into R

sFileName <- "Scopus-LakeUrmia-Aug8-2019"

# Columns A-X (add Abstract and author key works on download)
 fArticles <- fread(file=paste0(sFileName,".csv"), sep=",")
 dfArticleData <- as.data.frame(fArticles)

sapply(dfArticleData, class)

#Group years. Create as a new factor YearType
cPeriods <- c('Before ULRP', 'After ULRP')
cPeriodBreak <- c(2013,2030)
dfArticleData$YearPeriod <- cPeriods[3]

for (i in 2:1) {
  index <- dfArticleData$Year <= cPeriodBreak[i]
  dfArticleData$YearPeriod[index] = cPeriods[i]
}
#Find the max year


barlines <- "#1F3552" #Black outline
cFills <- c('#6baed6', '#3182bd', '#08519c') #Fill colors for bars. Light blue, medium blue, dark blue

minYear <- min(dfArticleData$Year)
minYear <- 1984
maxYear <- max(dfArticleData$Year)

### Figure 1 - Bar graph of number of journals per year by period
#Create version of the data set with YearType ordered so the legend and bar colors correctly plot.
# See https://ilari.scheinin.fi/ggplot-2-0-and-the-missing-order-aesthetic/ for particulars
dfArticleOrdered <- dfArticleData %>% filter(Year >= minYear) %>%
  mutate(YearPeriod=factor(YearPeriod, levels=cPeriods, ordered=TRUE))

#Similarly order the datafame
dfArticleOrdered %>%
  ungroup() %>%
  arrange(-as.integer(YearPeriod))

paste("Number of articles = ", nrow(dfArticleData))

#Plot barplot
ggplot(data = dfArticleOrdered, aes(x = dfArticleOrdered$Year)) + 
  geom_bar(breaks=seq(minYear,maxYear, by=1), 
                 colour = barlines,
                 position = "identity",
                 aes(fill=dfArticleOrdered$YearPeriod)) +
  
  labs(x="", y="Publications\n(number per year)") +
  scale_x_continuous(breaks = seq(minYear,maxYear,1)) +
  scale_fill_manual(values=cFills, name="Period", labels=cPeriods) +
  theme_bw() +
  theme(text = element_text(size=20), legend.text=element_text(size=18),
        axis.text.x = element_text(size=18, angle=90,hjust=1.1,vjust=0.5)) #,angle=90,
  
### STEP 2. Use bibliometrix library to further analyze (https://cran.rstudio.com/web/packages/bibliometrix/vignettes/bibliometrix-vignette.html)

fScopusFile <- readFiles(paste0(sFileName,".bib"))
mArticles <- convert2df(fScopusFile, dbsource = "isi", format = "bibtex")

# Move the cited by field from the csv file to the .bib. There is an error in loading that field
mArticles$CB <- dfArticleData$`Cited by`
# Replace NAs with zeros
mArticles$CB[is.na(mArticles$CB)] <- 0

# The first step is to perform a descriptive analysis of the bibliographic data frame.
results <- biblioAnalysis(mArticles, sep = ";")

options(width=100)

# Plot results
# 1. Most productive authors
# 2. Most productive countries
# 3. Scientific production (similar to Figure 1)
# 4. Average article citations (ignore)
# 5. Average total citations per year

#Pick select results
# fields authors, mostcitedpapers,sources,DE
plot(x = results, k = 15, pause = FALSE)
S <- summary(object = results, k = 10, pause = FALSE)

# To obtain the most frequent cited manuscripts:

nRows = 25

# Query and arrange the top citation counts
  
dfArticleCites <- dfArticleData %>% 
    ungroup() %>%
    arrange(desc(dfArticleData$`Cited by`))

#Formulate the paper lable : Author (Year) or Author et al (Year)
# Count the number of authors by the number of commas in the author field
dfArticleCites$NumAuthors <- str_count(dfArticleCites$Authors,",") + 1
dfArticleCites$FirstAuthorLN <- paste(word(dfArticleCites$Authors,start=1,end=1," ")," ")
dfArticleCites$SecondAuthorLN <- paste(word(dfArticleCites$Authors,start=3,end=3," ")," ")


#Single author => Last Name (Year)
dfArticleCites$PaperDesc <- paste0(dfArticleCites$FirstAuthorLN,"(",dfArticleCites$Year,")")
#Two author => First and Last (Year)
dfArticleCites$PaperDesc[dfArticleCites$NumAuthors == 2] <- paste0(dfArticleCites$FirstAuthorLN[dfArticleCites$NumAuthors ==2],"and ",dfArticleCites$SecondAuthorLN[dfArticleCites$NumAuthors == 2], "(",dfArticleCites$Year[dfArticleCites$NumAuthors == 2],")")
# Multi author => Last Name et al (Year)
dfArticleCites$PaperDesc[dfArticleCites$NumAuthors > 2] <- paste0(dfArticleCites$FirstAuthorLN[dfArticleCites$NumAuthors > 2],"et al. ", "(",dfArticleCites$Year[dfArticleCites$NumAuthors > 2],")")



#Plot barplot of Cited By
ggplot(dfArticleCites[0:nRows,], aes(x=reorder(PaperDesc,`Cited by`), y=`Cited by`)) +
  geom_col() +
  
  labs(y="Citations (count)", x="") +
  theme_bw() +
  theme(text = element_text(size=20), legend.text=element_text(size=18),
        axis.text.x = element_text(size=18, angle=90,hjust=1.1,vjust=0.5)) +
  coord_flip()


#Plot number of authors
dfNumAuthors <- ddply(dfArticleCites, .(NumAuthors), nrow) 
ggplot(data = dfNumAuthors, aes(x=NumAuthors,y=V1)) + 
  geom_bar( 
    stat = "identity",
    position = "identity") +
  
  labs(x="Number of Authors", y="Publications (count)") +
  #scale_x_continuous(breaks = seq(minYear,maxYear,1)) +
  #scale_fill_manual(values=cFills, name="Period", labels=cPeriods) +
  theme_bw() +
  theme(text = element_text(size=20), legend.text=element_text(size=18),
        axis.text.x = element_text(size=18, angle=90,hjust=1.1,vjust=0.5))

### Most frequent journals
# To obtain the most frequent cited manuscripts:

nRows = 25


dfJournalCounts <- ddply(dfArticleData, .(`Source title`), nrow) 

dfJournalCounts <- dfJournalCounts %>%
    ungroup() %>%
    arrange(desc(dfJournalCounts$V1))

#Grab the first 6 words for the source
dfJournalCounts$SourceTitleShort <- str_trunc(dfJournalCounts$`Source title`,50,side="right")

#Plot barplot
ggplot(data = dfJournalCounts[0:nRows,], aes(x=reorder(SourceTitleShort,V1),y=V1)) + 
  geom_bar( 
           stat = "identity",
           position = "identity") +
  
  labs(x="Journal", y="Publications (count)") +
  #scale_x_continuous(breaks = seq(minYear,maxYear,1)) +
  #scale_fill_manual(values=cFills, name="Period", labels=cPeriods) +
  theme_bw() +
  theme(text = element_text(size=20), legend.text=element_text(size=18),
        axis.text.x = element_text(size=18, angle=90,hjust=1.1,vjust=0.5)) +
  coord_flip()#,angle=90,


### Second approach for Journal Names
dfJournals <- as.data.frame(results$Sources, stringsAsFactors = FALSE)
#Grab the first 6 words for the source
dfJournals$SourceTitleShort <- str_trunc(dfJournals$SO,50,side="right")

#Plot barplot
ggplot(data = dfJournals[0:nRows,], aes(x=reorder(SourceTitleShort,Freq),y=Freq)) + 
  geom_bar( 
    stat = "identity",
    position = "identity") +
  
  labs(x="Journal", y="Publications (count)") +
  #scale_x_continuous(breaks = seq(minYear,maxYear,1)) +
  #scale_fill_manual(values=cFills, name="Period", labels=cPeriods) +
  theme_bw() +
  theme(text = element_text(size=20), legend.text=element_text(size=18),
        axis.text.x = element_text(size=18, angle=90,hjust=1.1,vjust=0.5)) +
  coord_flip()#,angle=90,


### Key word frequency

### Key word frequency FROM BIBLIOMETRIX
dfKeywords <- as.data.frame(results$DE, stringsAsFactors = FALSE)
#Grab the first 6 words for the source
#dfJournals$SourceTitleShort <- str_trunc(dfJournals$SO,50,side="right")

# Introduce arbitrary faceting to plot more keywords on the chart
nKeys <- nrow(dfKeywords)
nGroups <- round(nrow(dfKeywords)/nRows,0)
for(i in 1:nGroups) {
  dfKeywords$Group[(nRows*(i-1)+1):nKeys]   <- i
}

#Plot barplot
ggplot(data = dfKeywords[3:(3*nRows),], aes(x=reorder(Tab,Freq),y=Freq)) + 
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


# Keyword analysis from CSV file (not working as well)
lKeywordsList <- str_split(str_to_lower(dfArticleData$`Author Keywords`), "; ")
dfKeywords2 <- as.data.frame(unlist(lKeywordsList),stringsAsFactors = FALSE)
dfKeywords2$Keyword <- dfKeywords2$`unlist(lKeywordsList)`

dfKeywordCounts <- ddply(dfKeywords2, .(Keyword), nrow) 

dfKeywordCounts <- dfKeywordCounts %>%
  ungroup() %>%
  arrange(desc(dfKeywordCounts$V1))

# Introduce arbitrary faceting to plot more keywords on the chart
nKeys <- nrow(dfKeywordCounts)
nGroups <- round(nrow(dfKeywordCounts)/nRows,0)
for(i in 1:nGroups) {
  dfKeywordCounts$Group[(nRows*(i-1)+1):nKeys]   <- i
}

#Plot barplot of Keyword counts
ggplot(data = dfKeywordCounts[4:(3*nRows),], aes(x=reorder(Keyword,V1),y=V1)) + 
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




## Create a country collaboration network

mTag <- metaTagExtraction(mArticles, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(mTag, analysis = "collaboration", network = "countries", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, n = dim(NetMatrix)[1], Title = "Country Collaboration", type = "circle", size=TRUE, remove.multiple=FALSE,labelsize=0.7,cluster="none")


## Create a co-citation network

NetMatrix <- biblioNetwork(mArticles, analysis = "co-citation", network = "references", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, n = 30, Title = "Co-Citation Network", type = "fruchterman", size=T, remove.multiple=FALSE, labelsize=0.7,edgesize = 5)


##
# Conceptual Structure using keywords (method="CA")

CS <- conceptualStructure(mArticles,field="DE", method="CA", minDegree=4, clust=5, stemming=TRUE, labelsize=10, documents=10)

## Create a historical citation network
options(width=130)
histResults <- histNetwork(mArticles, min.citations = 1, sep = ";")

data(scientometrics)
histResults <- histNetwork(scientometrics, min.citations = 10, sep = ";")
histPlot(histResults, n = 20, size = 5, labelsize = 5,verbose = TRUE)