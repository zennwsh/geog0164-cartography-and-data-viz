# This script is meant to categorise my two variables, black bear population and
# bigfoot sightings, for ease of mapping a bivariate choropleth. 
bears <- read.csv("Data/Black bear population.csv")

# Processing bear population --------

# Check the distribution of bear population
hist(bears$Black.Bear.Population)

# Rank them on a scale of 1-3, 3 being the most 
bears$rank <- ifelse(bears$Black.Bear.Population<1000, 1, 
                     ifelse(bears$Black.Bear.Population<20000, 2, 3)) 

# Processing bigfoot sightings --------
library(rgdal)
bigfoot <- readOGR("Data/Bigfoot/per-state", "bigfoot-count-perstate")

bigfoot.s <- bigfoot@data[c("NAME","NUMPOINTS")]

# Check distribution of bigfoot observations
hist(bigfoot.s$NUMPOINTS)

# Rank them on a scale of A-C, C being the most 
bigfoot.s$bigfoot.rank <- ifelse(bigfoot.s$NUMPOINTS<25, "A",
                         ifelse(bigfoot.s$NUMPOINTS<150, "B", "C"))

# Merge the two variables --------

# Merge datasets 
for.qgis <- merge(bears, bigfoot.s, by.x="State.Name", by.y="NAME")

# Combine the two ranks into a classification of LETTER/NUMBER e.g. A1 
for.qgis$final <- with(for.qgis, paste0(bigfoot.rank, rank))

# Save file 
write.csv(for.qgis, "Data/for-qgis.csv")

