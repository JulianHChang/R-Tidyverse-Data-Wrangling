# Data Wrangling in R
# Coal Consumption Case Study

# Load the tidyverse
library(tidyverse)

# Read in the coal dataset
coal <- read_csv("http://594442.youcanlearnit.net/coal.csv")
glimpse(coal)

# Skip the first two lines
coal <- read_csv("http://594442.youcanlearnit.net/coal.csv", skip=2)
glimpse (coal)

# Rename the first column as region
colnames(coal)[1] <- "region"
summary(coal)



# Data Wrangling in R
# Coal Consumption Case Study

# Load the tidyverse
library(tidyverse)

# Read in the coal dataset
coal <- read_csv("http://594442.youcanlearnit.net/coal.csv")
glimpse(coal)

# Skip the first two lines
coal <- read_csv("http://594442.youcanlearnit.net/coal.csv", skip=2)
glimpse (coal)

# Rename the first column as region
colnames(coal)[1] <- "region"
summary(coal)

# Convert from a wide dataset to a long dataset using gather
coal_long <- gather(coal, 'year', 'coal_consumption', -region)
glimpse(coal_long)

# Convert years to integers
coal_long$year <- as.integer(coal_long$year)
summary(coal_long)

# Convert coal consumption to numeric
coal_long$coal_consumption <- as.numeric(coal_long$coal_consumption)
summary(coal_long)





# Data Wrangling in R
# Coal Consumption Case Study

# Load the tidyverse
library(tidyverse)

# Read in the coal dataset
coal <- read_csv("http://594442.youcanlearnit.net/coal.csv")
glimpse(coal)

# Skip the first two lines
coal <- read_csv("http://594442.youcanlearnit.net/coal.csv", skip=2)
glimpse (coal)

# Rename the first column as region
colnames(coal)[1] <- "region"
summary(coal)

# Convert from a wide dataset to a long dataset using gather
coal_long <- gather(coal, 'year', 'coal_consumption', -region)
glimpse(coal_long)

# Convert years to integers
coal_long$year <- as.integer(coal_long$year)
summary(coal_long)

# Convert coal consumption to numeric
coal_long$coal_consumption <- as.numeric(coal_long$coal_consumption)
summary(coal_long)

# Look at region values - they contain both continents and countries
unique(coal_long$region)

# Create a vector of "noncountry" values that appear in the region variable
noncountries <- c("North America", "Central & South America", "Antarctica", "Europe", "Eurasia", 
                  "Middle East", "Africa", "Asia & Oceania", "World")

# Look for matches
matches <- which(!is.na(match(coal_long$region, noncountries)))

# create a tibble of country values
coal_country <- coal_long[-matches,]

# create a tibble of regional values
coal_region <- coal_long[matches,]

# check them out
unique(coal_region$region)
unique(coal_country$region)




# Data Wrangling in R
# Coal Consumption Case Study

# Load the tidyverse
library(tidyverse)

# Read in the coal dataset
coal <- read_csv("http://594442.youcanlearnit.net/coal.csv")
glimpse(coal)

# Skip the first two lines
coal <- read_csv("http://594442.youcanlearnit.net/coal.csv", skip=2)
glimpse (coal)

# Rename the first column as region
colnames(coal)[1] <- "region"
summary(coal)

# Convert from a wide dataset to a long dataset using gather
coal_long <- gather(coal, 'year', 'coal_consumption', -region)
glimpse(coal_long)

# Convert years to integers
coal_long$year <- as.integer(coal_long$year)
summary(coal_long)

# Convert coal consumption to numeric
coal_long$coal_consumption <- as.numeric(coal_long$coal_consumption)
summary(coal_long)

# Look at region values - they contain both continents and countries
unique(coal_long$region)

# Create a vector of "noncountry" values that appear in the region variable
noncountries <- c("North America", "Central & South America", "Antarctica", "Europe", "Eurasia", 
                  "Middle East", "Africa", "Asia & Oceania", "World")

# Look for matches
matches <- which(!is.na(match(coal_long$region, noncountries)))

# create a tibble of country values
coal_country <- coal_long[-matches,]

# create a tibble of regional values
coal_region <- coal_long[matches,]

# check them out
unique(coal_region$region)
unique(coal_country$region)


# Start with a scatterplot
ggplot(data=coal_region, mapping=aes(x=year, y=coal_consumption)) +
  geom_point()

# That looks like it should be a line graph
ggplot(data=coal_region, mapping=aes(x=year, y=coal_consumption)) +
  geom_line()

# Let's get a separate line for each region
ggplot(data=coal_region, mapping=aes(x=year, y=coal_consumption)) +
  geom_line(mapping=aes(color=region))

