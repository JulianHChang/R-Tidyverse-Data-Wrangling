# Data Wrangling in R
# 5.1 Detecting Outliers
#

# Load the tidyverse
library(tidyverse)

# Read in the Medicare payments dataset
names <- c("DRG", "ProviderID", "Name", "Address", "City", "State", "ZIP", "Region", "Discharges", "AverageCharges", "AverageTotalPayments", 
           "AverageMedicarePayments")
types = 'ccccccccinnn'
inpatient <- read_tsv('http://594442.youcanlearnit.net/inpatient.tsv', col_names = names, skip=1, col_types = types)

# Let's look at a histogram
ggplot(data=inpatient) + 
  geom_histogram(mapping=aes(x=AverageCharges))

# What if we change the limit of the y axis
ggplot(data=inpatient) + 
  geom_histogram(mapping=aes(x=AverageCharges)) +
  coord_cartesian(ylim=c(0,25))

# We could also view this with a single boxplot
ggplot(data=inpatient) + 
  geom_boxplot(mapping=aes("charges",AverageCharges))

# Or we can use a series of boxplots broken out by state
ggplot(data=inpatient) + 
  geom_boxplot(mapping=aes(State,AverageCharges))

# Plenty to investigate here, but let's dig in to those over $500,000
highCharges <- filter(inpatient, AverageCharges>500000)

unique(highCharges$DRG)

ggplot(data=highCharges) + 
  geom_point(mapping=aes(DRG,AverageCharges)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))

  
  
# Data Wrangling in R
# 5.2 Missing and Special Values in R
#

# Load the tidyverse and the food inspections dataset
library(tidyverse)

names <- c("ID", "DBAName", "AKAName", "License", "FacilityType", "Risk", "Address", 
           "City", "State", "ZIP", "InspectionDate", "InspectionType", "Results",
           "Violations", "Latitude","Longitude","Location")

inspections <- read_csv('http://594442.youcanlearnit.net/inspections.csv', 
                        col_names=names, skip=1)

# Look at a summary of the data
summary(inspections)

# Which inspections have NA values for license?

is.na(inspections$License)
which(is.na(inspections$License))
nolicense<-which(is.na(inspections$License))
inspections[nolicense,]

# Create two new tibbles, one for licensed facilities and one for unlicensed facilities
unlicensed <- inspections[nolicense,]
licensed <- inspections[-nolicense,]

# What happens if I divide a number by 0

badmath <- c(1,2,3,4/0,0/0,NA)
badmath
is.na(badmath)
is.nan(badmath)
is.infinite(badmath)
is.finite(badmath)



# Data Wrangling in R
# 5.2 Missing and Special Values in R, Part 2

badmath <- c(1,2,3,4/0,0/0,NA)

is.na(badmath)

is.nan(badmath)

is.infinite(badmath)

is.finite(badmath)


# Data Wrangling in R
# 5.3 Breaking Apart Columns With Separate
#

# Load the tidyverse and read in the Medicare payments dataset
library(tidyverse)
names <- c("DRG", "ProviderID", "Name", "Address", "City", "State", "ZIP", "Region", "Discharges", "AverageCharges", "AverageTotalPayments", 
           "AverageMedicarePayments")
types = 'ccccccccinnn'
inpatient <- read_tsv('http://594442.youcanlearnit.net/inpatient.tsv', col_names = names, skip=1, col_types = types)

# Take a look at the diagnosis-related group unique values
unique(inpatient$DRG)

# Let's try separating this on the hyphen
inpatient_separate <- separate(inpatient,DRG,c('DRGcode','DRGdescription'),'-')

# What's going on with those warning rows?  Let's look at row 45894
inpatient$DRG[45894]

# Let's separate with character position instead
inpatient_separate <- separate(inpatient,DRG,c('DRGcode','DRGdescription'),4)

# And take a look at the data now
glimpse(inpatient_separate)



# Data Wrangling in R
# 5.4 Combining Columns with unite()
#

# Load the tidyverse and the food inspections dataset
library(tidyverse)

names <- c("ID", "DBAName", "AKAName", "License", "FacilityType", "Risk", "Address", 
           "City", "State", "ZIP", "InspectionDate", "InspectionType", "Results",
           "Violations", "Latitude","Longitude","Location")

inspections <- read_csv('http://594442.youcanlearnit.net/inspections.csv', 
                        col_names=names, skip=1)

glimpse(inspections)

# Create a new column called Regions that combines City and State
regional_inspections <- unite(inspections,Region,City,State,sep=", ")

# Let's look at the data
glimpse(regional_inspections)

# Whoops. I didn't want to DELETE the City and State columns.  Let's try again.
regional_inspections <- unite(inspections,Region,City,State,sep=", ", remove=FALSE)
glimpse(regional_inspections)

# And take a look at the unique regions
unique(regional_inspections$Region)


# Data Wrangling in R
# 5.5 Manipulating Strings in R with stringr
#

# Load the tidyverse and the food inspections dataset
library(tidyverse)

names <- c("ID", "DBAName", "AKAName", "License", "FacilityType", "Risk", "Address", 
           "City", "State", "ZIP", "InspectionDate", "InspectionType", "Results",
           "Violations", "Latitude","Longitude","Location")

inspections <- read_csv('http://594442.youcanlearnit.net/inspections.csv', 
                        col_names=names, skip=1)

# Create a new column called Regions that combines City and State
regional_inspections <- unite(inspections,Region,City,State,sep=", ", remove=FALSE)

# And take a look at the unique regions
unique(regional_inspections$Region)

# We need to load stringr separately
library(stringr)

# Let's handle the uppercase/lowercase issues by converting everything to uppercase
regional_inspections$Region <- str_to_upper(regional_inspections$Region)

# What were the results of that?
unique(regional_inspections$Region)

# Let's take care of a few misspellings of Chicago
regional_inspections$Region <- str_replace(regional_inspections$Region,'CCHICAGO, IL', 'CHICAGO, IL')
regional_inspections$Region <- str_replace(regional_inspections$Region,'CHCICAGO, IL', 'CHICAGO, IL')
regional_inspections$Region <- str_replace(regional_inspections$Region,'CHICAGOCHICAGO, IL', 'CHICAGO, IL')
regional_inspections$Region <- str_replace(regional_inspections$Region,'CHCHICAGO, IL', 'CHICAGO, IL')
regional_inspections$Region <- str_replace(regional_inspections$Region,'CHICAGOI, IL', 'CHICAGO, IL')

# And see what's left
unique(regional_inspections$Region)

# There are some "CHICAGO, NA" values that we can clearly correct to "CHICAGO, IL"
regional_inspections$Region <- str_replace(regional_inspections$Region,'CHICAGO, NA', 'CHICAGO, IL')

# But we don't know what to do with "NA, IL", "NA, NA", or "INACTIVE, IL"
# so let's set those to missing values
regional_inspections$Region <- str_replace(regional_inspections$Region,'NA, ', NA)

# Whoops... we'll need to use str_detect to find those
NA_regions <- which(str_detect(regional_inspections$Region, "NA, "))
inactive_regions <- which(str_detect(regional_inspections$Region, "INACTIVE, IL"))

# And then set them to NA 
regional_inspections$Region[NA_regions] <- NA
regional_inspections$Region[inactive_regions] <- NA

# How did we do?
unique(regional_inspections$Region)



# Data Wrangling in R
# 5.5 Manipulating Strings in R with stringr
# Part 2

# Load the tidyverse and read in the Medicare payments dataset
library(tidyverse)
names <- c("DRG", "ProviderID", "Name", "Address", "City", "State", "ZIP", "Region", "Discharges", "AverageCharges", "AverageTotalPayments", 
           "AverageMedicarePayments")
types = 'ccccccccinnn'
inpatient <- read_tsv('http://594442.youcanlearnit.net/inpatient.tsv', col_names = names, skip=1, col_types = types)

# Separate at the fourth position
inpatient_separate <- separate(inpatient,DRG,c('DRGcode','DRGdescription'),4)

# And take a look at the data now
glimpse(inpatient_separate)

# Load stringr
library(stringr)

# Trim the DRGcode field
inpatient_separate$DRGcode <- str_trim(inpatient_separate$DRGcode)
glimpse(inpatient_separate)

# The DRGdescription field has a hyphen in front so we need to do something different
inpatient_separate$DRGdescription <- str_sub(inpatient_separate$DRGdescription, 3)
glimpse(inpatient_separate)
  