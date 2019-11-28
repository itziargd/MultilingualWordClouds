library("tm")
library("wordcloud")
library("udpipe")
library("dplyr")

# read text
#text <- readLines(file.choose())
text <- readLines("FILE.txt")

# chose language and charge model in ud-pipes
udmodel <- udpipe_download_model(language = "basque")
udmodel <- udpipe_load_model(file = udmodel$file_model)

#analyse text
x <- udpipe_annotate(udmodel, x = text)

# covert analysis to data frame
x <- as.data.frame(x, detailed = TRUE)



# filter dataframe to get only content words 
x <- filter(x, upos %in% c("PROPN", "NOUN", "ADJ", "VERB")) #"ADV" 


# get the lemma
lemmas <- as.vector(x$lemma)

# convert lema vector to corpus
docs <- Corpus(VectorSource(lemmas))

# inspect corpus
#inspect(docs)

# replace special characthers
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))

# Remove numbers
docs <- tm_map(docs, removeNumbers)

#asd stop words (for Basquw)

docs <- tm_map(docs, removeWords, c( "egin", "egon", "izan", "ukan", "eman", "aurre", "nahi", "alde", "behar"))


#Build a term-document matrix
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

#create wordcloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
