# Political View Visualization in the US from the General Social Survey
# This visualization is modeled after the graphic at http://fivethirtyeight.com/datalab/there-are-few-libertarians-but-many-americans-have-libertarian-views/
#
# Features of interest: 
#     Homosexuals should have the right to marry (marhomo)
#     Should government reduce income differences (eqwlth)

setwd("~/Desktop/CSCI 183 Data Science/General Social Survey Political Views/")

library(foreign)
library(vcd)

data <- read.spss("GSS2014.sav")

# Features Of Interest
foi <- subset(as.data.frame(data), 
              (marhomo=="STRONGLY AGREE" | marhomo=="AGREE" | 
                 marhomo=="DISAGREE" | marhomo=="STRONGLY DISAGREE") & (eqwlth==1 | eqwlth==7), 
              select=c("marhomo", "eqwlth"))
levels(foi$marhomo) <- c("NA", "FAVOR", "FAVOR", "NA", "OPPOSE", "OPPOSE", "NA", "NA")
foi$eqwlth <- factor(foi$eqwlth, levels = c("1", "7"), labels = c("FAVOR", "OPPOSE"))
foi <- droplevels(foi)

mosaic(eqwlth ~ marhomo, data=foi, gp=shading_binary, shade=TRUE)
#prop.table
#labeling_cells
