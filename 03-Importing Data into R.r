# Data Wrangling in R
# 3.2 Importing CSV Files into R

# First, I need to load the tidyverse
library(tidyverse)

# Now let's try loading in the CSV file without any special settings
inspections <- read_csv('http://594442.youcanlearnit.net/inspections.csv')

# And take a look at what we loaded in
glimpse(inspections)

# That actually looks pretty good.  I can see that R made some choices about variable names
# and I'd like to replace them with my own names.  First, I'll create a vector of my names.

names <- c("ID", "DBAName", "AKAName", "License", "FacilityType", "Risk", "Address", 
           "City", "State", "ZIP", "InspectionDate", "InspectionType", "Results",
           "Violations", "Latitude","Longitude","Location")

# I could simply rename the variables in the tibble that I already created, but I want to 
# show you how to supply variable names to read_csv

inspections <- read_csv('http://594442.youcanlearnit.net/inspections.csv', 
                        col_names=names)

# That changed the variable names, but it also imported the variable names that were in the
# file as the first data row.  So let me try this again and tell read_csv to skip the first line.

inspections <- read_csv('http://594442.youcanlearnit.net/inspections.csv', 
                        col_names=names, skip=1)

						
						# Data Wrangling in R
# 3.4 Importing TSV Files into R

# Read in TSV containing Medicare data
# This file is comparing the charges for the 100 most common inpatient services and 30 common outpatient services.  Providers determine what they will charge for items and services provided to patients and these charges are the amount the providers bills for an item or service.

# First, we need to load the tidyverse
library(tidyverse)

# Next, let's just try reading in the file and see what it looks like
inpatient <- read_tsv('http://594442.youcanlearnit.net/inpatient.tsv')

glimpse(inpatient)

# First issue that we need to correct is the variable names.  

names <- c("DRG", "ProviderID", "Name", "Address", "City", "State", 
           "ZIP", "Region", "Discharges", "AverageCharges", "AverageTotalPayments", 
           "AverageMedicarePayments")

inpatient <- read_tsv('http://594442.youcanlearnit.net/inpatient.tsv', col_names = names)

# Now the names are the first observation!
# Two ways I can deal with this, I can just delete the first row or I can fix... Let's fix

inpatient <- read_tsv('http://594442.youcanlearnit.net/inpatient.tsv', col_names = names,
                      skip=1)

# Let's specify our data types, reading only discharges as an integer for now

types = 'cccccccciccc'
inpatient <- read_tsv('http://594442.youcanlearnit.net/inpatient.tsv', col_names = names,
                      skip=1, col_types = types)
glimpse(inpatient)

# Now let's deal with the currency amounts
# Currency is floating point, so we might be tempted to try doubles

types = 'cccccccciddd'
inpatient <- read_tsv('http://594442.youcanlearnit.net/inpatient.tsv', col_names = names,
                      skip=1, col_types = types)
glimpse(inpatient)

# But that doesn't work quite right, so we can use numeric instead
# It finds the first number in the field.  Good for getting rid of $, % etc.

types = 'ccccccccinnn'
inpatient <- read_tsv('http://594442.youcanlearnit.net/inpatient.tsv', col_names = names,
                      skip=1, col_types = types)

glimpse(inpatient)


# Data Wrangling in R
# 3.5 Importing Delimited Files into R

# Accidental deaths associated with drug overdoses in Connecticut from 2012 to 2015
# Reading data from a carat-delimited file

# Load the tidyverse
library(tidyverse)

# Use the read_delim function to look at the file 
deaths <- read_delim(file='http://594442.youcanlearnit.net/drugdeaths.txt', delim='^')

glimpse(deaths)


# Data Wrangling in R
# 3.6 Importing Fixed Width Files into R

# Chicago employee salaries
# Reading in data from a fixed width file

# First we need to create a list of the column lengths
lengths <- c(32,50,24,NA)

# And a list of column names
names <- c("Name", "Title", "Department", "Salary")

# And then create the widths list using the fwf_widths function.
# Setting the last value to NA allows the last column to be "ragged" - variable length
widths <- fwf_widths(length, names)

# Then we read the data into a tibble by using the read_fwf function with the 
# filename and column widths as arguments.
employees <- read_fwf('http://594442.youcanlearnit.net/chicagoemployees.txt', widths)

# And let's take a look at what we got
glimpse(employees)

# The last correction we need to make is skipping the first line of the file so that we don't read
# the variable names as an observation
employees <- read_fwf('http://594442.youcanlearnit.net/chicagoemployees.txt', widths, skip=1)
glimpse(employees)


# Data Wrangling in R
# 3.7 Importing Excel Files into R

# School breakfast program data
# Reading data stored in Excel

# Load the tidyverse
library(tidyverse)

# The readxl package isn't part of the core tidyverse so we need to install and
# load it separately
install.packages("readxl")
library(readxl)

# Try just reading the file without other arguments
breakfast <- read_excel('breakfast.xls')
glimpse(breakfast)

# Try skipping three lines

breakfast <- read_excel('breakfast.xls', skip=3)
glimpse(breakfast)

# It looks like we need to skip five lines, which will remove the column names
# So lets create a vector with column names
names <- c("Year", "FreeStudents", "ReducedStudents", "PaidStudents", "TotalStudents", 
           "MealsServed", "PercentFree")

# And then try reading the file again
breakfast <- read_excel('breakfast.xls',skip=5, col_names = names)
glimpse(breakfast)

# I'll do a little quick manipulation of this tibble. 
# First, convert the numbers of students and meals to real values
breakfast$FreeStudents = breakfast$FreeStudents * 1000000
breakfast$ReducedStudents = breakfast$ReducedStudents * 1000000
breakfast$PaidStudents = breakfast$PaidStudents * 1000000
breakfast$TotalStudents = breakfast$TotalStudents * 1000000
breakfast$MealsServed = breakfast$MealsServed * 1000000

# And show the percentage as a decimal
breakfast$PercentFree = breakfast$PercentFree / 100
glimpse(breakfast)