# FarmSizeDistribution.r
#
# Compares the farm size distributions in Utah, Urmia Basin, and Entire iran
#
# David E. Rosenberg 
# Utah State Univeristy
# Logan, Utah
# May 5, 2021
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

if (!require(pracma)) {  #interp1 and interpNA
  install.packages("pracma", repos="http://cran.r-project.org") 
  library(pracma) 
}

# Read in the excel data

sExcelFile <- 'FarmSizeDistributions.xlsx'
dfUrmiaIran <- read_excel(sExcelFile, sheet = "Iran",  range = "A3:C9")
dfUtah <- read_excel(sExcelFile, sheet = "Utah",  range = "A3:C11")

#Cumulative plot of Iran data streams
ggplot(dfUrmiaIran) +
  geom_line(aes(x=dfUrmiaIran$Hectares, y=cumsum(dfUrmiaIran$Urmia), color="Urmia"), size=2) + 
  geom_line(aes(x=dfUrmiaIran$Hectares, y=cumsum(dfUrmiaIran$Iran), color="Iran"), size=2) + 
  
  labs(y="Percent of Farms", x = "Area (Hectares)")


#Cumulative plot of Iran data streams
ggplot() +
  geom_line(data = dfUrmiaIran, aes(x=dfUrmiaIran$Hectares, y=cumsum(dfUrmiaIran$Urmia), color="Urmia"), size=2) + 
  geom_line(data = dfUrmiaIran, aes(x=dfUrmiaIran$Hectares, y=cumsum(dfUrmiaIran$Iran), color="Iran"), size=2) + 
  
  labs(y="Cumulative Percent", x = "Area (Hectares)") +

  scale_color_manual(breaks=c("Iran","Urmia"), values=c("Red","Pink")) +
  
  theme_bw() +
  theme(text = element_text(size=20), legend.text=element_text(size=16),
        legend.title = element_blank())


#Cumulative plot of Utah data streams
ggplot(dfUtah) +
  geom_line(aes(x=dfUtah$`Area (acres)`, y=cumsum(dfUtah$`Percent of Total Acres`), color="Utah Area"), size=2) + 
  geom_line(aes(x=dfUtah$`Area (acres)`, y=cumsum(dfUtah$`Percent of total number of farms`), color="Utah Farms"), size=2) + 
  
  labs(y="Cumulative Percent", x = "Area (Acres)") +
  
  scale_color_manual(breaks=c("Utah Area","Utah Farms"), values=c("Blue","Lightblue")) +
  
  theme_bw() +
  theme(text = element_text(size=20), legend.text=element_text(size=16),
        legend.title = element_blank())

#Cumulative plot of Iran and Utah data streams

nAcresToHectares = 0.405

ggplot() +
  geom_line(data = dfUrmiaIran, aes(x=dfUrmiaIran$Hectares, y=cumsum(dfUrmiaIran$Urmia), color="Urmia"), size=2) + 
  geom_line(data = dfUrmiaIran, aes(x=dfUrmiaIran$Hectares, y=cumsum(dfUrmiaIran$Iran), color="Iran"), size=2) + 
  geom_line(data = dfUtah, aes(x= `Area (acres)` * nAcresToHectares, y=cumsum(`Percent of Total Acres`), color="Utah"), size=2) +
  
  labs(y="Cumulative Percent", x = "Area (Hectares)") +
  
  scale_color_manual(breaks=c("Urmia","Iran","Utah"), values=c("Pink","Red","Blue")) +
  
  scale_x_continuous(limits = c(0,55)) + 
  
  theme_bw() +
  theme(text = element_text(size=20), legend.text=element_text(size=16),
        legend.title = element_blank())


