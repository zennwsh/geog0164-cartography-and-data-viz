setwd("~/Documents/School/GEOG0164 - Cartography/Coursework")
county <- read.csv("Data/US/cc-est2019-alldata.csv")
length(unique(county$STNAME))
length(unique(county$CTYNAME))
county2019 <- subset(county, YEAR==12)
county2019.agg <- aggregate(county2019$TOT_POP, by=list(county2019$CTYNAME), FUN=sum)
county2019.agg$pop <-county2019.agg$x/2
county2019.agg$Group.1 <- gsub(" County", "", county2019.agg$Group.1)

library(rgdal)
us.county <- readOGR("Data/US/cb_2018_us_county_500k", "cb_2018_us_county_500k")
us.county.pop <- merge(us.county, county2019.agg, by.x="NAME", by.y="Group.1")

library(tmap)

writeOGR(us.county.pop, layer="uscountypop", dsn="./Data/US", driver="ESRI Shapefile")

ufo <- read.csv("Data/UFO/nuforc_reports.csv")
ufo.points <- ufo[c(12, 11)]
ufo.points <- na.omit(ufo.points)
ufo.points <- SpatialPoints(ufo.points[,1:2], proj4string = CRS("+proj=longlat +datum=NAD83 +no_defs"))
us.county.pop <- spTransform(us.county.pop, CRSobj = "+proj=longlat +datum=NAD83 +no_defs")
ufo.points <- spTransform(ufo.points, CRSobj = "+proj=longlat +datum=NAD83 +no_defs")

library(GISTools)

n.ufo <- poly.counts(ufo.points, us.county.pop)
n.ufo <- setNames(n.ufo, us.county.pop@data$NAME)
df.ufo <- as.data.frame(n.ufo)
df.ufo$NAME <- us.county.pop@data$NAME
write.csv(df.ufo, "ufo-by-county.csv")

# corpus
library(corpus)
summary <- corpus_frame(c(1:88125), text=ufo$text)
text_tokens(summary)
text_filter(summary)
text_filter(summary)$map_case <- FALSE
text_filter(summary)$drop_punct <- TRUE
term_stats(summary, subset = !term %in% stopwords_en)

library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

docs <- Corpus(VectorSource(ufo[,9]))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# Extract summaries only 
desc <- ufo[,9]
summary <- corpus_frame(c(1:88125), text=desc)
text_filter(summary)$map_case <- FALSE
text_filter(summary)$drop_punct <- TRUE
wordcount <- term_stats(summary, subset =! term %in% stopwords_en)
state.descriptors[1,2] <- wordcount[1,1]

shapes <- c("airplane", "triangle", "round", "fireball", "oval", "unknown")
colors <- c ("orange", "blue", "yellow", "green", "purple", "white")
colors_count <- subset(wordcount, term %in% colors)
colors_count$dummy <- NA
unknown <- c("unknown", NA, nrow(ufo)-sum(colors_count$support))
colors_count <- rbind(colors_count, unknown)

shapes_count <- subset(wordcount, term %in% shapes)

library(ggplot2)
library(ggpubr)
shapes_count$term <- factor(shapes_count$term, levels=rev(unique(shapes_count$term)))
ggplot(data=shapes_count, aes(y=support, x=term)) +
  geom_col(fill="#90c78d", width=0.5) + coord_flip() 
#  theme(
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.background = element_rect(fill = "transparent",colour = NA),
#     plot.background = element_rect(fill = "transparent",colour = NA),
#     axis.title.x=element_blank(), axis.text.x=element_blank(),
#     axis.ticks.x=element_blank(),
#     axis.title.y=element_blank(),
#     axis.ticks.y=element_blank(),
#     axis.text.y=element_blank(),
# )
ggsave("ufo-shapes.png", bg="transparent")

colors_count$term <- factor(colors_count$term, levels=rev(unique(colors_count$term)))

ggplot(data=colors_count, aes(fill=term, y=support, x=dummy)) + 
  geom_bar(position="fill", stat="identity") + coord_flip() 

colors_count$percent <- round(colors_count$support / sum(colors_count$support)*100,3)
