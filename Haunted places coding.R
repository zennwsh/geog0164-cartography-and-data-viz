setwd("~/Documents/School/GEOG0164 - Cartography/Coursework")
haunted <- read.csv("Data/Haunted places/haunted_places.csv")
library(corpus)
library(rcorpora)

# This script is meant to perform a text mining analysis to investigate
# the most common descriptor for haunted places within each US state.

# Split into separate lists by state 
by.states <- split(haunted, f=haunted$state)

# Load this brilliant dataset of adjectives into a dataframe 
adjectives <- corpora("words/adjs") 

# Remove the word night cos every single state's most common descriptor
# is night... 
adjectives$adjs <- adjectives$adjs[adjectives$adjs!="night"] 

# Create blank dataframe 
state.descriptors <- as.data.frame(matrix(nrow=51,ncol=2))
colnames(state.descriptors) <- c("state", "descriptor")

# List the names of each state 
for (i in 1:51){
  state.descriptors[i,1] <- names(by.states[i])
} 

write.csv(state.descriptors, "haunted-bystate.csv")

# Loop through large list 
lapply(by.states, function(x){ 
  # Extract description (text) column from each list
  desc <- x$description 
  # Convert to corpus frame 
  summary <- corpus_frame(c(1:nrow(x)), text=desc)
  # Remove case 
  text_filter(summary, map_case=F, drop_punct=T)
  text_filter(summary)$map_case <- FALSE
  text_filter(summary)$drop_punct <- TRUE
  wordcount <- term_stats(summary, subset = term %in% adjectives$adjs)
  # Unfortunately I am not sure how to attach each term finding to the 
  # specific cell so I did this part manually lmao 
}
  )
