# Want to find out:
#     Which dorm has the most bike thefts?           Finding: Swig
#     Which day of the week is the worst?            Finding: Monday
#     Which month of the year is the worst?          Finding: February

library(Amelia)
library(Hmisc)
library(lubridate)

thefts <- read.csv("~/Desktop/CSCI 183 Data Science/Class Activity 1/Bike Theft Log (From Sept07).csv")

missmap(thefts, col = c("yellow", "black"))


### LOCATION cleaning and analysis: ###
# Get a count of the thefts by dorm. Non-dorm locations are ignored
# The strings in dorms are regular expressions that represent each dorm. 
dorms <- c("Campisi", "Walsh", "Casa", "Swig", "Sobrato", "Dunne", "Graham", "McLaughlin", "Nobili", "[Ff]il+ip+o", "Villa")
thefts_by_dorm <- sapply(sapply(dorms, grep, x=thefts$LOCATION), length)


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


### DATE analysis: ###
# Thefts by day of week
thefts_by_dow <- summary(factor(weekdays(primary_date_format)))
# Thefts by month of year
thefts_by_moy <- summary(factor(months(primary_date_format)))
