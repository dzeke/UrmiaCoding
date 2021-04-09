# BibliometrixUrmia2020.r
#
# Bibliometrix analysis of Scopus Papers with Keywords Lake Urmia
# Includes all articles from 1900 to September 2020
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
# April 8, 2021
# david.rosenberg@usu.edu
#

#install.packages("openxlsx")
install.packages("bibliometrix", repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))
install.packages("bibliometrix", dependencies = TRUE)

if (!require(readxl)) { 
  install.packages("readxl", repos="http://cran.r-project.org") 
  library(readxl) 
}

if (!require(RColorBrewer)) { 
  install.packages("RColorBrewer",repos="http://cran.r-project.org") 
  library(RColorBrewer) # 
}

library(bibliometrix)   ### load bibliometrix package
library(ggplot2)
library(dplyr)
library(lubridate)
library(plyr)
library(data.table)
library(stringr)
library(reshape)  #melt

# Number of items to print
lPrint <- 20

# Load the data
# From CSV

# The TiTLE, Abstract, or Keyword Query String
tArticleQuery <- 'TITLE-ABS-KEY ( ( "Lake"  AND  ( "Urmia"  OR  "orumiyeh"  OR  "Orumiyeh" ) ) )  AND  ( PUBDATETXT ( ( august  2019 )  OR  ( september  2019 )  OR  ( october  2019 )  OR  ( november  2019 )  OR  ( december  2019 ) )  OR  PUBYEAR  >  2019 ) ' 

#File with new additions from Aug 2019 to present
sFileName2020 <- "Scopus-LakeUrmia-Sep15-2020"
#Original file from 1900 to Aug 2019
sFileName2019 <- "Scopus-LakeUrmia-Aug8-2019"

#Read as a .bib file
#fScopusFile <- readFiles(paste0(sFileName,".bib"))

# <- fScopusFile
#D <- readFiles("http://www.bibliometrix.org/datasets/savedrecs.bib")

# Convert to dataframe
#M <- convert2df(D, dbsource = "scopus", format = "bibtex")
M <- convert2df(paste0(sFileName2020,".bib"), dbsource="scopus", format="bibtex")

M2019 <- convert2df(paste0(sFileName2019,".bib"), dbsource="scopus", format="bibtex")

#Add columns so the M and M2019 dataframes have the same columns
M2019$RP <- ""
M2019$AU1_UN <- ""
M2019$AU_UN_NR <- ""
M$ID <- ""
M$page_count <- 0

#Combine the data frames
M_comb <- rbind(M,M2019)

#Remove the duplicated Titles
M_comb <- M_comb[!duplicated(M_comb$TI), ]

# Conduct the bilio analysis
results <- biblioAnalysis(M_comb, sep = ";")

#Echo some summary results
results$Articles   #Number of articles
results$nAuthors   #Number of authors
results$AuMultiAuthoredArt #Number of multi-authored papers
results$AuSingleAuthoredArt #Number of single authored papers

# Articles over time
#Group years. Create as a new factor YearType
dfArticleData <- M_comb

cPeriods <- c('Before ULRP (left)', 'During ULRP (left)')
cPeriodBreak <- c(2013,2030)
#dfArticleData$YearPeriod <- cPeriods[3]

dfArticleData$YearPeriod <- ifelse(dfArticleData$PY <= cPeriodBreak[1],cPeriods[1],cPeriods[2])

# for (i in 2:1) {
#   index <- dfArticleData$PY <= cPeriodBreak[i]
#   dfArticleData$YearPeriod[index] = cPeriods[i]
# }
#Find the max year


barlines <- "#1F3552" #Black outline
cFills <- c('#6baed6', '#3182bd', '#08519c') #Fill colors for bars. Light blue, medium blue, dark blue
cFills <- c('#6baed6', 'Blue', '#08519c')
            
minYear <- min(dfArticleData$PY)
minYear <- 1984
maxYear <- 2021

### Figures coded by DER
### Figure 1 - Bar graph of number of journals per year by period
#Create version of the data set with YearType ordered so the legend and bar colors correctly plot.
# See https://ilari.scheinin.fi/ggplot-2-0-and-the-missing-order-aesthetic/ for particulars
dfArticleOrdered <- dfArticleData %>% filter(PY >= minYear) %>%
  mutate(YearPeriod=factor(YearPeriod, levels=cPeriods, ordered=TRUE))

#dfArticleOrdered <- dfArticleData %>% mutate(YearPeriod=factor(YearPeriod, levels=cPeriods, ordered=TRUE))

#Similarly order the datafame
dfArticleOrdered %>%
  ungroup() %>%
  arrange(-as.integer(YearPeriod))

#### Read in Lake level data from Excel file - Sima data (1995)

sExcelFile <- 'Lake_Urmia_data_1995-2015-SelectLevels.xlsx'
dfLakeLevels <- read_excel(sExcelFile, sheet = "Objectives timeseries",  range = "B5:H29")

#Clean up
#Rename first column
cNames <- colnames(dfLakeLevels)
cNames[1] <- "Year"
colnames(dfLakeLevels) <- cNames
#Remove 2nd column
dfLakeLevels <- dfLakeLevels[-c(2)]
#Remove rows of NA
dfLakeLevels <- na.omit(dfLakeLevels)
#Melt the data so get a max/min lake level for each year
dfLakeLevelsMelt <-melt(as.data.frame(dfLakeLevels[,c(1,5,6)]), id=c("Year"))
#Turn max level into July (7/12) and Min Year into January (0)
dfLakeLevelsMelt$YearMonth <- dfLakeLevelsMelt$Year + ifelse(dfLakeLevelsMelt$variable == "Maximum Lake level (m)", 7/12, 1/12)
#Reorder by YearMonth
dfLakeLevelsMelt <- dfLakeLevelsMelt %>% arrange(YearMonth)


#Plot barplot of number of articles per year on left axis. And lake level on right axis.

#Some parameters to calculate the right axis
cRightLims <- c(1268, 1278) #elevation in meters
cLeftLims <- c(0,80) # number of articles per year
cSlope <- (cRightLims[2] - cRightLims[1])/(cLeftLims[2] - cLeftLims[1]) #Slope

#Calculate Lake Level as articles
dfLakeLevels$LevelAsArticle <- (dfLakeLevels$`mean level (m)` - cRightLims[1])/cSlope + cLeftLims[1]
dfLakeLevelsMelt$LevelAsArticle <- (dfLakeLevelsMelt$value - cRightLims[1])/cSlope + cLeftLims[1]


pArticleCount <- ggplot(data = dfArticleOrdered, aes(x = PY)) + 
  #Histogram of publication counts
  geom_bar(breaks=seq(minYear,maxYear, by=1), 
           colour = barlines,
           position = "identity",
           aes(fill=YearPeriod)) +
  
  #Line of lake levels
  geom_line(data=dfLakeLevelsMelt, aes(x=YearMonth, y=LevelAsArticle, color="Level"), size=2) +

  labs(x="") +
  scale_x_continuous(limits = c(minYear,maxYear), breaks = seq(minYear,maxYear,2), minor_breaks = seq(minYear,maxYear,1)) +
  scale_y_continuous("Articles\n(number per year)",sec.axis = sec_axis(~ (. - cLeftLims[1])* cSlope  + cRightLims[1], name = "Lake Level (m)", breaks = seq(cRightLims[1],cRightLims[2],by=2)) ) +

  scale_fill_manual(values=cFills, name="", labels=cPeriods) +
  
  scale_color_manual(breaks=c("Level"), values=c("Red"), labels=c("Lake level (right)")) +
   
  theme_bw() +
  theme(text = element_text(size=20), legend.text=element_text(size=15),
        axis.text.x = element_text(size=18, angle=90,hjust=1.1,vjust=0.5),
        legend.title = element_blank(),
        legend.position = c(0.20, 0.6),
        legend.spacing.y = unit(-0.1, "cm"),
        #legend.margin = margin(-0.5,0,0,0, unit="cm"),
        legend.box.background = element_rect(color="black",size=1, fill="White", linetype="solid")) # +
        #legend.background = element_rect(fill="White",
         #                                size=0.5, linetype="solid", 
          #                               colour ="black")) #,angle=90,

print(pArticleCount)

ggsave("ArticleCountByYear.png", width=8, height=6)


### Now do the same figure using Vaheddost data back to 1966
#### Read in Lake level data from Excel file - Vaheddost data 
#### This is also Figure 1 for the synthesis paper

sExcelFile <- 'LakeUrmiaWaterLevel-1966-Vaheddoost.xlsx'
dfLakeLevelsVah <- read_excel(sExcelFile, sheet = "1966-2016_monthly",  range = "A1:E613")

# Calculate the date as year + decimal month
dfLakeLevelsVah$YearMonth <- dfLakeLevelsVah$Year + dfLakeLevelsVah$Month...3/(12+1)

#Calculate Lake Level as articles
dfLakeLevelsVah$LevelAsArticle <- (dfLakeLevelsVah$`Water Level` - cRightLims[1])/cSlope + cLeftLims[1]

minYear <- 1966


pArticleCount <- ggplot(data = dfArticleOrdered, aes(x = PY)) + 
  #Histogram of publication counts
  geom_bar(breaks=seq(minYear,maxYear, by=1), 
           colour = barlines,
           position = "identity",
           aes(fill=YearPeriod)) +
  
  #Line of lake levels
  geom_line(data=dfLakeLevelsVah, aes(x=YearMonth, y=LevelAsArticle, color="Level"), size=2) +
  
  labs(x="") +
  scale_x_continuous(limits = c(minYear,maxYear), breaks = seq(minYear,maxYear,2), minor_breaks = seq(minYear,maxYear,1)) +
  scale_y_continuous("Articles\n(number per year)",sec.axis = sec_axis(~ (. - cLeftLims[1])* cSlope  + cRightLims[1], name = "Lake Level (m)", breaks = seq(cRightLims[1],cRightLims[2],by=2)) ) +
  
  scale_fill_manual(values=cFills, name="", labels=cPeriods) +
  
  scale_color_manual(breaks=c("Level"), values=c("Red"), labels=c("Lake level (right)")) +
  
  theme_bw() +
  theme(text = element_text(size=20), legend.text=element_text(size=15),
        axis.text.x = element_text(size=18, angle=90,hjust=1.1,vjust=0.5),
        legend.title = element_blank(),
        legend.position = c(0.15, 0.4),
        legend.spacing.y = unit(-0.1, "cm"),
        #legend.margin = margin(-0.5,0,0,0, unit="cm"),
        legend.box.background = element_rect(color="black",size=1, fill="White", linetype="solid")) # +
#legend.background = element_rect(fill="White",
#                                size=0.5, linetype="solid", 
#                               colour ="black")) #,angle=90,

print(pArticleCount)

ggsave("ArticleCountByYear.png", width=8, height=6)


### Figure lake level vs precipitation plus ULRP plans
# Lake level on the right, precipitation on the left. 
# Everything plotted in precipitaiton units
# This figure goes in Section 4.7

## Read from Parsinejad data
sExcelFile <- 'Lake_Urmia_WaterLevel_Precip_data.xlsx'
dfParsinejad <- read_excel(sExcelFile, sheet = "Sheet2",  range = "A1:G64")

cElevLimits <- c(1268,1278)

#Some parameters to calculate the right axis
cPrecipLims <- c(-1000, 0) # mm

cSlopePrecipToElev <- (cPrecipLims[2] - cPrecipLims[1])/(cElevLimits[2] - cElevLimits[1]) #Slope
cSlopeElevToPrecip <- 1 / cSlopePrecipToElev

#Calculate Precipitation as Lake Level
dfParsinejad$PrecipAsLakeLevel <- (dfParsinejad$`precipitation (mm)` - cPrecipLims[1])/cSlopePrecipToElev + cElevLimits[1]

#Calculate Lake levels as precip
dfParsinejad$ActualAsPrecip <- (dfParsinejad$Actual - cElevLimits[1])/cSlopeElevToPrecip + cPrecipLims[1]
dfParsinejad$PlannedAsPrecip <- (dfParsinejad$Planned - cElevLimits[1])/cSlopeElevToPrecip + cPrecipLims[1]
dfParsinejad$WithoutAsPrecip <- (dfParsinejad$`Without the restoration plan` - cElevLimits[1])/cSlopeElevToPrecip + cPrecipLims[1]
nEcologicalLevelAsPrecip <- (1274.1 - cElevLimits[1])/cSlopeElevToPrecip + cPrecipLims[1]

#Define the Minimum x and y limits
minYear <- 2005
maxYear <- 2026

#Define the years of the three restoration phases
nStabilizeStart <- 2013
nRestoreStart <- 2016
nRestoreEnd <- 2021
nFinishEnd <- 2026

## Define the polygons that identify the three restoration phases, 2013-2016 and 2016-2021
# Define the polygons showing each tier to add to the plot. A polygon is defined by four points in the plot space. Lower-left, Lower-right, upper-right, upper left
# Polygon name
ids <- factor(c("Stabilize","Restore","Final"))
# Polygon corners (see above for defs)
dfPositions <- data.frame(id = rep(ids, each = 4),
                          Year = c(nStabilizeStart,nRestoreStart,nRestoreStart,nStabilizeStart,nRestoreStart,nRestoreEnd,nRestoreEnd,nRestoreStart, nRestoreEnd,nFinishEnd,nFinishEnd,nRestoreEnd),
                          PrecipVal = c(cPrecipLims[1],cPrecipLims[1],cPrecipLims[2],cPrecipLims[2],cPrecipLims[1],cPrecipLims[1],cPrecipLims[2],cPrecipLims[2],cPrecipLims[1],cPrecipLims[1],cPrecipLims[2],cPrecipLims[2]))
#Number of polygons
nPts <- nrow(dfPositions)/4

#Polygon labels
dfPolyLabel <- data.frame(id = ids,
                          Label = c("Stabilize", "Restore", "Final"),
                          DumVal = c(1:nPts))

#Calculate midpoints x (year) for each polygon. This is the average of left, right coordinates for
# the polygon

dfPolyLabel$MidYear <- c((nStabilizeStart + nRestoreStart)/2 ,(nRestoreStart + nRestoreEnd)/2, (nFinishEnd + nRestoreEnd)/2)
dfPolyLabel$PrecipPos <- (cPrecipLims[1] + 100)

# Currently we need to manually merge the two together
dfPolyAll <- merge(dfPolyLabel, dfPositions, by = c("id"))

#Add a variable for the annual precip mins and maxes
dfPolyAll$Precip <- c(cPrecipLims[1],cPrecipLims[2],cPrecipLims[2],cPrecipLims[1],cPrecipLims[1],cPrecipLims[2],cPrecipLims[2],cPrecipLims[1],cPrecipLims[1],cPrecipLims[2],cPrecipLims[2],cPrecipLims[1])
# Calculate the mid point of the precip values (not used)
dfPolyAll$MidPrecip <- (cPrecipLims[1] + cPrecipLims[2])/2

#Colors for the Resotration Phase polygons
palPurples <- brewer.pal(9, "Purples") #For plotting polygons


ggplot(data = dfParsinejad, aes(x = Year)) + 

  # Polygons for the different restoration phases
  geom_polygon(data = dfPolyAll, aes(x = Year, y = PrecipVal, group = id, fill = as.factor(dfPolyAll$DumVal)), show.legend = F) +
  # Text label the phases
  geom_text(data = dfPolyLabel, aes(x=MidYear, y=cPrecipLims[1] + 50, label=Label), angle = 0, size = 7, hjust="middle") +

  #Precipitation converted to lake level
  geom_col(aes(y=-`precipitation (mm)`, fill="Precipitation"),  position = "identity", color = "black") +
  
  #Lines of lake levels plotted in precipitation units
  geom_line(aes(y=ActualAsPrecip, color="Observed", group=1), size=2) +
  geom_line(aes(y=PlannedAsPrecip, color="Planned", group=1), linetype = "dashed", size=2) +
  geom_line(aes(y=WithoutAsPrecip, color="Without ULRP", group=1), linetype = "dotdash", size=2) +
  
  #Horizontal line and text label for ecological target 1274.1
  geom_hline(yintercept = nEcologicalLevelAsPrecip, color="red", linetype = "longdash", size = 1.5) +
  geom_text(aes(2007,nEcologicalLevelAsPrecip + 50, label="Ecological target"), color="red",size=6) +
  
  #No x axis title
  labs(x="") +
  #X-axis labels go from min year to max year in 5 year increments
  scale_x_continuous(limits = c(minYear,maxYear), breaks = seq(minYear,maxYear,5), minor_breaks = seq(minYear,maxYear,1)) +
  #scale_y_continuous("Lake Level (m)", limits = c(1268,1278), breaks = seq(1268,1278, by=2), sec.axis = sec_axis(~ (. - cElevLimits[1])* cSlopePrecipToElev  + cPrecipLims[1], name = "Precipitation (mm)", breaks = seq(cPrecipLims[1],cPrecipLims[2], by=100))) +
  #All axis units are in negative precipition. We are labeling the left axis from Lake level low to high.
  #We are labeling the right axis from Large precip (bottom) to 0 (top)
  scale_y_continuous("Lake Level (m)", breaks = seq(cPrecipLims[1],cPrecipLims[2], by=100), labels = seq(cElevLimits[1],cElevLimits[2],by=1) , sec.axis = sec_axis(~ . , name = "Precipitation\n(mm per year)", breaks = seq(cPrecipLims[1],cPrecipLims[2], by=100), labels = seq(-cPrecipLims[1],cPrecipLims[2], by=-100) )) +
  
  #Color the lines
  scale_color_manual(breaks=c("Observed", "Planned", "Without ULRP"), values=c("Black", "Blue", "Orange")) +
  #scale_fill_manual(breaks = c("Precipitation"), values=c("Grey")) +
  #Color the polygons and the precipitation bars
  scale_fill_manual(breaks = c("Precipitation"), values=c(palPurples[2],palPurples[3],palPurples[4], palPurples[8]), labels = c("Precipitation")) +
  
  theme_bw() +
  theme(text = element_text(size=20), legend.text=element_text(size=16),
        axis.text.x = element_text(size=18, angle=90,hjust=1.1,vjust=0.5),
        legend.title = element_blank(),
        legend.position = c(0.85, 0.825),
        legend.spacing.y = unit(-0.1, "cm"),
        #legend.margin = margin(-0.5,0,0,0, unit="cm"),
        legend.box.background = element_rect(color="black",size=1, fill="White", linetype="solid")) # +





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
pAuthorHist <- ggplot(data = dfNumAuthors, aes(x=NumAuthors,y=V1)) + 
  geom_bar( 
    stat = "identity",
    position = "identity",
    color ="black", fill = "Red") +
  
  labs(x="Number of Authors", y="Articles (count)") +
  scale_x_continuous(breaks = seq(0,20, by=4), minor_breaks = seq(0,20, by=2)) +
  
  theme_bw() +
  theme(text = element_text(size=20), legend.text=element_text(size=18),
        axis.text.x = element_text(size=18))

print(pAuthorHist)

#Calculate the number of papers with 8 or more authors
dfNumAuthors$CumSum <- cumsum(dfNumAuthors$V1)
dfNumAuthors$CumSumRev <- max(dfNumAuthors$CumSum) - dfNumAuthors$CumSum

## Construct a two panel plot of the article counts and author histogram
library(cowplot)
plot_grid(pArticleCount, pAuthorHist, labels = c('A', 'B'), label_size = 16)

ggsave("ArticleCounts.png", width=11, height = 6)


## Prepare dataframe to save to csv

## Sort the data frame from largest to smallest number of authors
dfArticleAuthorSort <- dfArticleCites %>% arrange(-NumAuthors, AU)

## Arrange the columns so they follow the earlier 1900 - 2020 data frame
cColOrder <- c("CR", "AU","CR","TI","PY","SO", "url","AB","C1","DE", "CR","DT","CR","CR", "DB","DI")
dfArticlePushOut <- dfArticleAuthorSort[,cColOrder]

##Rename the column headers so they match the earlier data frame

cNewColNames <- c("Team Member",	"Authors", "Author(s) ID", "Title", "Year", "Source title", "Link",	"Abstract", "Author(s) Keywords", "Index Keywords", "Publisher", "Document Type", "Publication Stage", "Access Type", "Source", "EID")
colnames(dfArticlePushOut) <- cNewColNames

#Clar the entry in the 1, 3, 11, 13, 14 columns
dfArticlePushOut[,c(1,3,11,13,14)] <- ""

#Turn the EID into a formal url
dfArticlePushOut$Link <- paste0("http://doi.org/",dfArticlePushOut$EID,"")

#Turn all upper case to proper
cColsToLower <- c(2,3,4,6,9,10,12,15)
for (i in cColsToLower){
  #Convert the all caps to titles
  dfArticlePushOut[,i] <- str_to_title(dfArticlePushOut[,i])
}

#Turn all upper case to lower (abstract)
dfArticlePushOut$Abstract <- str_to_lower(dfArticlePushOut$Abstract)


write.csv(dfArticlePushOut, "UrmiaArticlesSorted2020.csv")

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





### Remaining Figures by Bibliometrix


#Obtain most frequent citations
CR <- citations(M_comb, field = "article", sep = ";")
cbind(CR$Cited[1:lPrint])

#To obtain the most frequent cited first authors:

# Doesn't work  
CR <- citations(M_comb, field = "author", sep = ";")
cbind(CR$Cited[1:lPrint])

# Most frequent local citations
CR <- localCitations(M_comb, sep = ";")
CR$Authors[1:lPrint,]
CR$Papers[1:lPrint,]

#Top authors productivity over time
topAU <- authorProdOverTime(M_comb, k = lPrint, graph = TRUE)

# Bipartite networks
A <- cocMatrix(M_comb, Field = "SO", sep = ";")
dfJournalSort <- sort(Matrix::colSums(A), decreasing = TRUE)[1:lPrint]

# Citation network
A <- cocMatrix(M_comb, Field = "CR", sep = ".  ")
sort(Matrix::colSums(A), decreasing = TRUE)[1:lPrint]

# Author keyword network
#A <- cocMatrix(M, Field = "DE", sep = ";")

#NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "DE", sep = ".  ")
#net=networkPlot(NetMatrix,  normalize = "salton", weighted=NULL, n = 100, Title = "References' Coupling", type = "fruchterman", size=5,size.cex=T,remove.multiple=TRUE,labelsize=0.8,label.n=10,label.cex=F)


NetMatrix <- biblioNetwork(M_comb, analysis = "coupling", network = "authors", sep = ";")
net=networkPlot(NetMatrix,  normalize = "salton", weighted=NULL, n = 100, Title = "Authors' Coupling", type = "fruchterman", size=5,size.cex=T,remove.multiple=TRUE,labelsize=0.8,label.n=10,label.cex=F)

# Visualize networks
# Create a country collaboration network

#M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
#NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")

# Plot the network
#net=networkPlot(NetMatrix, n = dim(NetMatrix)[1], Title = "Country Collaboration", type = "circle", size=TRUE, remove.multiple=FALSE,labelsize=0.7,cluster="none")

#Create keyword co-occurrences network

NetMatrix <- biblioNetwork(M_comb, analysis = "co-occurrences", network = "keywords", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Keyword Co-occurrences", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)

# Conceptual Structure using keywords (method="CA")
#CS <- conceptualStructure(M,field="ID", method="CA", minDegree=4, clust=5, stemming=FALSE, labelsize=10, documents=10)

# Create a historical citation network
#options(width=130)
histResults <- histNetwork(M_comb, min.citations = 10, sep = ";")

## Articles analysed   84

# Plot a historical co-citation network
net <- histPlot(histResults, n=25, size = 10, labelsize=5)
