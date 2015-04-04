# General Observations:
#       If someone is not signed in, their gender is assigned as female and their age is assigned as 0.
#       Female and male viewership is about the same across all age ranges (after accounting for not being signed in). 
#           The two exceptions are that there are roughly twice as many male viewers as female viewers in the under 18 group, 
#           and roughly twice as many female viewers as male viewers in the 65+ group.
#       The average (signed in) user age is about 40 and viewership is spread roughly evenly from ages 20 to 50.
#       The age distribution looks similar to a chi square distribution. 
#       Click through rate, clicks, and impressions are roughly equivalent across all ages and genders.
#       A majority of viewers have 5 impressions and 0 clicks.       
#       Impressions are independent of whether or not a user is signed in.
#       Signed in users are attributed roughly half the clicks of not signed in users.

library(ggplot2)
library(doBy)

data1 <- read.csv(url("http://stat.columbia.edu/~rachel/datasets/nyt1.csv"))

# Add a new column that breaks up the age into the following ranges:
#       <18, 18-24, 25-34, 35-44, 45-54, 55-64, 65+
data1$AgeRange <- cut(data1$Age, breaks=c(-Inf, 0, 18, 25, 35, 45, 55, 65, Inf), right=FALSE)

# Plot impressions and clicks distribution per AgeRange
qplot(factor(Impressions), data=data1, binwidth=1, xlab="Impressions", geom="histogram")
qplot(factor(Clicks), data=data1, binwidth=1, xlab="Clicks", geom="histogram")
# Plot Click-through Rate distribution per AgeRange
qplot(Clicks/Impressions, data=subset(data1, Signed_In==1), facets=.~AgeRange, xlab="Click-through Rate", geom="density")
qplot(Clicks/Impressions, data=data1, xlab="Click-through Rate", geom="density")

# Signed in vs. Not Signed in click behavior
qplot(Clicks, data=data1, facets=.~Signed_In, geom="bar", binwidth=1, ..ncount..)
qplot(Impressions, data=data1, facets=.~Signed_In, geom="bar", binwidth=1, ..ncount..)
qplot(Clicks/Impressions, data=subset(data1, Signed_In==0 & Clicks>0), xlab="Click-through Rate", geom="density")
qplot(Clicks/Impressions, data=subset(data1, Signed_In==1 & Clicks>0), xlab="Click-through Rate", geom="density")

# Plot Click-through Rate distribution per AgeRange excluding a click-through rate of 0
qplot(Clicks/Impressions, data=subset(data1, Clicks>0 & Signed_In==1), facets=AgeRange~., xlab="Click-through Rate (except 0)", geom="density")
qplot(Clicks/Impressions, data=subset(data1, Clicks>0 & Signed_In==1), xlab="Click-through Rate (except 0)", geom="density")

# Plot click-through rate per gender (excluding 0 rate)
qplot(Clicks/Impressions, data=subset(data1, Clicks>0 & Signed_In==1), facets=Gender~., geom="density")

# Plot click-through rate per gender and AgeRange (excluding 0 rate)
qplot(Clicks/Impressions, data=subset(data1, Clicks>0 & Signed_In==1), facets=AgeRange~Gender, geom="density")

# Plot Gender distributions
qplot(factor(Gender, labels=c("Female", "Male")), data=subset(data1, Signed_In==1), geom="bar")
qplot(factor(Gender, labels=c("Female", "Male")), data=data1, geom="bar")
# Plot Gender and Age distributions
qplot(factor(Gender, labels=c("Female", "Male")), data=subset(data1, Signed_In==1), geom="bar", facets=.~AgeRange)
qplot(factor(Gender, labels=c("Female", "Male")), data=data1, geom="bar", facets=.~AgeRange)

# If someone isn't signed in, their gender and age default to 0
qplot(Gender, Age, data=subset(data1, Signed_In==0))
NSI_table <- summaryBy(Gender+Age~Signed_In, data=data1, FUN=c(min, max))

# Plot Age distributions
qplot(AgeRange, data=data1, binwidth=1)
qplot(Age, data=data1, binwidth=1)
qplot(AgeRange, data=subset(data1, Signed_In==1), binwidth=1)
qplot(Age, data=subset(data1, Signed_In==1), binwidth=1)

# Click-through rates
data1$CTR <- cut(data1$Clicks/data1$Impressions, breaks=10)

summary(data1)
summary(subset(data1,Gender==0))
summary(subset(data1,Gender==1))
aggregate(subset(data1, select=c("Clicks", "Impressions", "CTR")), by=list(gender=data1$Gender), FUN=summary)
aggregate(subset(data1, select=c("Clicks", "Impressions", "CTR")), by=list(age_range=data1$AgeRange), FUN=summary)


# Most Important Plots from above

# Plot click-through rate per gender and AgeRange (excluding 0 rate)
qplot(Clicks/Impressions, data=subset(data1, Clicks>0 & Signed_In==1), facets=AgeRange~Gender, geom="density")

# Adjusted Age and Gender distributions
qplot(subset(data1, Signed_In==1)$AgeRange, data=subset(data1, Signed_In==1), binwidth=1)
qplot(factor(Gender, labels=c("Female", "Male")), data=subset(data1, Signed_In==1), geom="bar")
qplot(factor(Gender, labels=c("Female", "Male")), data=subset(data1, Signed_In==1), geom="bar", facets=.~AgeRange)

# Age and gender summaries
AG_table <- summaryBy(Gender~Signed_In+AgeRange, data=data1, FUN=mean)

# Click and Impression summaries
CI_table <- summaryBy(Impressions+Clicks~Signed_In+Gender+AgeRange,data=data1, FUN=c(mean, min, median, max))
CTR_table <- summaryBy(Clicks/Impressions~Signed_In+Gender+AgeRange,data=data1, FUN=summary)




# Repeats for each file
for (i in 1:31) {
  filename <- "http://stat.columbia.edu/~rachel/datasets/nyt"
  filename <- paste(filename, toString(i), sep="")
  filename <- paste(filename, ".csv", sep="")
  datan <- read.csv(url(filename))
  
  if (i==1) {
    # Age and gender summaries
    AG_table <- summaryBy(Gender~Signed_In+AgeRange, data=datan, FUN=mean)
    
    # Click and Impression summaries
    CI_table <- summaryBy(Impressions+Clicks~Signed_In+Gender+AgeRange,data=datan, FUN=c(mean, min, median, max))
    CTR_table <- summaryBy(Clicks/Impressions~Signed_In+Gender+AgeRange,data=datan, FUN=summary)
  }
  else{
    AG_table <- rbind(AG_table, summaryBy(Gender~Signed_In+AgeRange, data=datan, FUN=mean))
    CI_table <- rbind(CI_table, summaryBy(Impressions+Clicks~Signed_In+Gender+AgeRange,data=datan, FUN=c(mean, min, median, max)))
    CTR_table <- rbind(CTR_table, summaryBy(Clicks/Impressions~Signed_In+Gender+AgeRange,data=datan, FUN=summary))
  }
}

CTR_table <- renameCol(CTR_table, c("Clicks/Impressions.Min.", "Clicks/Impressions.1st Qu.", "Clicks/Impressions.Median", "Clicks/Impressions.Mean", "Clicks/Impressions.3rd Qu.", "Clicks/Impressions.Max.", "Clicks/Impressions.NA's"), 
          c("ctr.Min.", "ctr.1st Qu.", "ctr.Median", "ctr.Mean", "ctr.3rd Qu.", "ctr.Max.", "ctr.NA's"))

# Overall Gender
summary(subset(AG_table, Signed_In==1, select=Gender.mean))
g_table <- summaryBy(Gender.mean~., data=subset(AG_table, Signed_In==1), FUN=c(min, mean, max))
qplot(Gender.mean, data=subset(AG_table, Signed_In==1))

# Overall Clicks and Impressions
summary(subset(CI_table, select=c("Impressions.min", "Impressions.mean", "Impressions.max", "Clicks.min", "Clicks.mean", "Clicks.max")))
qplot(Impressions.mean, data=CI_table)
qplot(Impressions.mean, data=CI_table, facets=.~Signed_In)
qplot(Impressions.mean, data=subset(CI_table, Signed_In==1), facets=.~Gender)
qplot(Clicks.mean, data=CI_table)
qplot(Clicks.mean, data=CI_table, facets=.~Signed_In)
qplot(Clicks.mean, data=subset(CI_table, Signed_In==1), facets=.~Gender)

# Overall Click Through Rate
summary(subset(CTR_table, select=c("ctr.Min.", "ctr.Mean", "ctr.Max.")))
qplot(ctr.Mean, data=CTR_table)
qplot(ctr.Mean, data=CTR_table, facets=.~Signed_In)
qplot(ctr.Mean, data=subset(CTR_table, Signed_In==1), facets=.~Gender)
