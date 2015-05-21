# Converts SCU Bike Theft data from a csv format to a tsv format with the location as a row 
#   and the month as a column

library(lubridate)
library(reshape2)

setwd("~/Desktop/CSCI 183 Data Science/D3 Visualization/")

thefts <- read.csv("Bike Theft Log (From Sept07).csv")

# Unique Building Identifiers
buildings <- c("Campisi", "Walsh", "Casa", "Swig", "Sobrato", "Dunne", "Graham", "McLaughlin", 
               "Nobili", "[Ff]il+ip+o", "Villa", "Kenna", "Bannan", "Benson", "Daly", 
               "Bellarmine", "Domicilio", "Engineering|Eng", "Learning Common", "O'Connor", 
               "Malley", "Locatelli", "Leavey", "St. Joseph", "St. Clare", "Fine Arts", 
               "Bookstore", "Jesuit", "Facilities", "(A|Art(s)*) & (S|Science(s)*)", "Orradre",
               "Heafey", "Alumni", "Solar House", "Loyola", "Bergin", "Parking", "Mission", 
               "Music & Dance", "IT", "Washington", "Univ. Square")

### DATE cleaning: ###
# Translate the DATE column into dates in R
primary_date_format <- as.Date(thefts$DATE, format='%m/%d/%y')
odd_date_format_1 <- as.Date(thefts$DATE, format='%m/%d-%S/%S/%y')
# Since the year for this format is assumed to be the current year, we have to keep track of the records location to later correct it
odd_date_format_2 <- as.Date(thefts$DATE, format='%d-%b')

# Combine all the dates into primary_date_format
primary_date_format[which(!is.na(odd_date_format_2))] <- odd_date_format_2[which(!is.na(odd_date_format_2))]
primary_date_format[which(!is.na(odd_date_format_1))] <- odd_date_format_1[which(!is.na(odd_date_format_1))]
# Assign dates in odd_format_2 the year in the entry above them
year(primary_date_format[which(!is.na(odd_date_format_2))]) <- year(primary_date_format[which(!is.na(odd_date_format_2))-1])
thefts$MONTH <- format(primary_date_format, '%Y-%m')

thefts$BUILDING <- NA
for (building in buildings) {
  thefts[grep(building, thefts$LOCATION),]$BUILDING <- building
}
temp <- aggregate( CASE ~ BUILDING+MONTH, thefts, length)
tsv_thefts <- dcast(temp, BUILDING ~ MONTH, fill=0)

write.table(tsv_thefts, "thefts.tsv", sep = "\t", row.names = FALSE)
