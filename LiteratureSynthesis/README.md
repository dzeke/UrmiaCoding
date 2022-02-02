# Data, Model, and Code
in Support of Parsinejad et al (2022) "[40-Years of Lake Urmia Restoration Research: Review, Synthesis and Next Steps](https://digitalcommons.usu.edu/cgi/viewcontent.cgi?article=1001&context=lake_urmia)."

## Contents
1. **[FigsFeb2022](https://github.com/dzeke/UrmiaCoding/tree/main/LiteratureSynthesis/FigsFeb2022)** - Data and code to produce all figures in the manuscript. See further instructions below.
    1. **[LakeUrmiaSynthesis-Figures-v6.pptx](https://github.com/dzeke/UrmiaCoding/raw/main/LiteratureSynthesis/FigsFeb2022/LakeUrmiaSynthesis-Figures-v6.pptx)** - Powerpoint file with final versions of all figures (direct download, check downloads in browser).
1. **[Libraries](https://github.com/dzeke/UrmiaCoding/tree/main/LiteratureSynthesis/Libraries)** - EndNote and XML files used to track articles reviewed and included in the synthesis.
1. **OldStuff** - Old stuff no longer used.

## Directions to Run
1. Switch into the **[FigsFeb2022](https://github.com/dzeke/UrmiaCoding/tree/main/LiteratureSynthesis/FigsFeb2022)** Folder.
1. Open a subfolder for a Figure.
1. For example, open a subfolder for Figures 1, 4, 9, or 12. Then open the Excel file in the subfolder.
1. Figures 3 and 7 were generated in R Studio. To run, see further directions **To Run Code and Generate Figures 3 and 7** below.

### Requirements
* R version 4.1.1. Download from https://cran.r-project.org/.
* R Studio 1.1.456. Download from https://www.rstudio.com/.

### To Run Code and Generate Figures 3 and 7.
1. Download and install R and RStudio (see requirements). 
1. Switch into the **[Figure3and7-ArticleCountsLakeLevel](https://github.com/dzeke/UrmiaCoding/tree/main/LiteratureSynthesis/FigsFeb2022/Figure3and7-ArticleCountsLakeLevel)** folder.
1. Open the file **[BibliometrixUrmia2020.pdf](https://github.com/dzeke/UrmiaCoding/blob/main/LiteratureSynthesis/FigsFeb2022/Figure3and7-ArticleCountsLakeLevel/BibliometrixUrmia2020.pdf)**. This markdown document explains how Figures 3 and 7 were generated and also shows the results. It also shows other figures that are not used in the review.
1. To generate the results in **BibliometrixUrmia2020.pdf**, open the file **FigsApril2021.Rproject**. R Studio will open.
1. Select the **BibliometrixUrmia2020.Rmd** file tab (R markdown file). Click the **Knit** button (below the file tab).
1. The following files will be generated:
  * **BibliometrixUrmia2020.pdf** - pdf file with all the figures
  * **UrmiaArticlesSorted2020.csv** - Comma separated values of all articles sorted by number
1. Open the **[BibliometrixUrmia2020.pdf](https://github.com/dzeke/UrmiaCoding/blob/main/LiteratureSynthesis/FigsFeb2022/Figure3and7-ArticleCountsLakeLevel/BibliometrixUrmia2020.pdf)** file to view figures and other ouput. 

## Requested Citation
David E. Rosenberg (2021). "Bibliometrix Analaysis for Lake Urmia Peer Reviewed Literature: 1900 to Sept. 2020", Utah State University, Logan, Utah. https://github.com/dzeke/UrmiaCoding/tree/main/LiteratureSynthesis.

The active version of this code is maintained at: https://github.com/dzeke/UrmiaCoding/tree/main/LiteratureSynthesis.
